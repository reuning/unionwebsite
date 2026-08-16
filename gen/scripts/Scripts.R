library(data.table)
library(magrittr)
library(stringr)
library(anytime)


## Match Labor_Union free text to a National affiliation code.
##
## Collects all *distinct* national codes matched across three tiers, then:
##   0 distinct matches  -> "" (becomes "Uncoded" downstream in Init.R)
##   1 distinct match    -> that code
##   >1 distinct matches -> National_Count > 1 (becomes "Multiple" in Init.R)
## This reproduces the original dictionary semantics: if multiple distinct
## national affiliations are identified, the row is flagged as Multiple rather
## than arbitrarily picking whichever was encountered first.
##
## Tiers:
##   1. Exact match against the reconstructed historical dictionary (fast hash
##      join). An exact hit is authoritative and skips Tiers 2/3, so a named
##      affiliate that also mentions its parent federation is not flagged
##      Multiple (e.g. "Workers United/SEIU" -> WU).
##   2. Substring/regex match against generic canonical union names (no local
##      number). Word-bounded so a short code like "IAM"/"CWA" can't match
##      inside a longer word.
##   3. Self-match on the National abbreviation itself, plus well-known legacy
##      names/abbreviations (aliases). Only applied to rows with no Tier 1/2
##      match.
match_national <- function(tmp_labor_union, dict_exact, dict) {
  n_rows <- length(tmp_labor_union)
  matched_codes <- vector("list", n_rows) # list of character vectors (distinct codes per row)
  add_match <- function(idx, code) {
    for (i in idx) {
      if (!(code %in% matched_codes[[i]])) {
        matched_codes[[i]] <<- c(matched_codes[[i]], code)
      }
    }
  }

  ## Tier 1: exact match
  norm_key <- function(x) {
    toupper(trimws(gsub("[[:space:]]+", " ", gsub("&", "and", x))))
  }
  exact_hit <- dict_exact$National[match(
    norm_key(tmp_labor_union),
    norm_key(dict_exact$Name)
  )]
  has_exact <- which(!is.na(exact_hit))
  if (length(has_exact)) {
    for (i in has_exact) {
      matched_codes[[i]] <- exact_hit[i]
    }
  }
  needs_fuzzy <- which(is.na(exact_hit)) # rows that still need Tier 2/3

  ## Tier 2: substring/regex. Word-bound patterns that start and end with a
  ## word character, so a 3-letter code like "IAM" can't match inside a longer
  ## word; phrases with trailing punctuation keep a literal match.
  has_sub <- logical(n_rows)
  for (ii in seq_len(nrow(dict))) {
    srch <- gsub("[[:space:]]|\u00A0", " ", gsub("&", "and", dict$Name[ii]))
    repl <- dict$National[ii]
    pat <- if (grepl("^[[:alnum:]]", srch) && grepl("[[:alnum:]]$", srch)) {
      paste0("\\b", srch, "\\b")
    } else {
      srch
    }
    hits <- which(grepl(pat, tmp_labor_union, ignore.case = TRUE))
    hits <- intersect(hits, needs_fuzzy)
    if (length(hits)) {
      add_match(hits, repl)
      has_sub[hits] <- TRUE
    }
  }

  ## Tier 3: self-match on the National abbreviation/code itself when it appears
  ## literally in the union text (e.g. "SEIU", "Teamsters").  Only applied to rows
  ## that did NOT get a Tier 1 exact hit AND did NOT get any Tier 2 substring match.
  tier3_eligible <- needs_fuzzy[!has_sub[needs_fuzzy]]
  nationals <- unique(c(dict_exact$National, dict$National))
  nationals <- nationals[nationals != ""]
  for (ii in seq_along(nationals)) {
    srch <- nationals[ii]
    hits <- which(grepl(
      paste0("(\\W|\\b|\\d)", srch, "(\\W|\\b|\\d)"),
      tmp_labor_union,
      ignore.case = TRUE
    ))
    hits <- intersect(hits, tier3_eligible)
    if (length(hits)) add_match(hits, srch)
  }

  ## Tier 3 aliases: legacy names / abbreviations that aren't themselves
  ## National codes but map clearly to one.  Patterns are word-bounded.  NOT
  ## bare "1199": that is a common local number (Teamsters/AFSCME/PATCO locals
  ## also use 1199) and would produce false SEIU codes; use the specific legacy
  ## names below instead.
  aliases <- list(
    list(pattern = "IBT", code = "Teamsters"), # Int'l Brotherhood of Teamsters
    list(pattern = "GCIU", code = "Teamsters"), # Graphic Communications Int'l (merged 2004)
    list(pattern = "PACE", code = "USW"), # Paper, Allied-Industrial, Chemical & Energy (merged 2005)
    list(pattern = "USWA", code = "USW"), # United Steelworkers of America (legacy)
    list(pattern = "AFTRA", code = "SAG-AFTRA"), # merged 2012 with SAG
    list(pattern = "IUE", code = "CWA"), # IUE-CWA division
    list(pattern = "ICWUC", code = "UFCW"), # Int'l Chemical Workers Union Council (UFCW)
    list(pattern = "MRC", code = "UBC"), # Metropolitan Regional Council of Carpenters
    list(pattern = "SMWIA", code = "SMART"), # Sheet Metal Workers Int'l Assoc (now SMART)
    list(pattern = "UTU", code = "SMART"), # United Transportation Union (merged 2014)
    list(pattern = "NYSUT", code = "AFT"), # NY State United Teachers (AFT affiliate)
    # 1199SEIU legacy names
    list(pattern = "National Health and Human Service", code = "SEIU"), # "1199 National Health & Human Service Employees Union"
    list(pattern = "New England Health", code = "SEIU"), # 1199 New England (1199NE)
    list(pattern = "Services Employees International Union", code = "SEIU") # SEIU full-name variant
  )
  for (a in aliases) {
    hits <- which(grepl(
      paste0("(\\W|\\b|\\d)", a$pattern, "(\\W|\\b|\\d)"),
      tmp_labor_union,
      ignore.case = TRUE
    ))
    hits <- intersect(hits, tier3_eligible)
    if (length(hits)) add_match(hits, a$code)
  }

  ## Collapse: National is the single matched code, or "" for 0 or >1 matches
  counts <- vapply(matched_codes, length, integer(1))
  national <- vapply(
    matched_codes,
    function(x) if (length(x)) x[1] else "",
    character(1)
  )
  national[counts != 1] <- ""

  list(National = national, National_Count = counts)
}


prep_data <- function(data = dt) {
  ## The original Name/National/International lookup lived in a Google Sheet that was
  ## accidentally deleted (link now returns HTTP 410). It has been reconstructed from the
  ## Labor_Union -> National mappings already present in historical cleaned_data.csv and
  ## split into two local files for performance:
  ##   - union_dictionary_exact.csv:       ~20k entries, matched by exact normalized text
  ##     (fast hash join; covers locality-specific strings, e.g. a specific local's name)
  ##   - union_dictionary_substrings.csv:  ~700 entries, matched by substring/regex search
  ##     (covers generic recurring canonical union names without a local number)
  ## See gen/scripts/rebuild_union_dictionary.R for how these were generated, and re-run it
  ## periodically to pick up newly-coded Labor_Union text as new data comes in.
  dict_exact <- read.csv(
    here::here("gen", "data", "union_dictionary_exact.csv"),
    stringsAsFactors = FALSE,
    encoding = "UTF-8"
  )
  dict <- read.csv(
    here::here("gen", "data", "union_dictionary_substrings.csv"),
    stringsAsFactors = FALSE,
    encoding = "UTF-8"
  )

  rename <- c(
    "Case_Name" = "Case Name",
    "Date_Closed" = "Date Closed",
    "Reason_Closed" = "Reason Closed",
    "Date_Filed" = "Date Filed",
    "Tally_Date" = "Tally Date",
    "Tally_Type" = "Tally Type",
    "Ballot_Type" = "Ballot Type",
    "Num_Eligible_Voters" = "No of Eligible Voters",
    "Labor_Union" = "Labor Union1",
    "Votes_For_Union" = "Votes for Labor Union1",
    "Votes_Against" = "Votes Against",
    "Total_Ballots_Counted" = "Total Ballots Counted"
  )
  cat("Renaming Data\n")
  names(data)[match(rename, names(data))] <- names(rename)

  ### Create New Variables
  cat("Creating New Variables\n")
  data[, Num_Eligible_Voters := as.numeric(Num_Eligible_Voters)]
  data[, Votes_Against := as.numeric(Votes_Against)]
  data[, Votes_For_Union := as.numeric(Votes_For_Union)]
  data[, Total_Ballots_Counted := as.numeric(Total_Ballots_Counted)]

  data[, Tally_Date := anydate(`Tally_Date`)]
  data[, Date_Filed := anydate(`Date_Filed`)]
  data[, Date_Closed := anydate(`Date_Closed`)]

  data[
    Tally_Date > as.Date("2080-01-01"),
    Tally_Date := Tally_Date - lubridate::dyears(100)
  ]
  data[
    Date_Filed > as.Date("2080-01-01"),
    Date_Filed := Date_Filed - lubridate::dyears(100)
  ]
  data[
    Date_Closed > as.Date("2080-01-01"),
    Date_Closed := Date_Closed - lubridate::dyears(100)
  ]

  data[, Length := Tally_Date - Date_Filed]
  data[, Tally_Quarter := anydate(cut(Tally_Date, breaks = "quarter"))]
  data[, Filed_Quarter := anydate(cut(Date_Filed, breaks = "quarter"))]

  data[
    is.na(Num_Eligible_Voters),
    Num_Eligible_Voters := `Employees on charge/petition`
  ]
  data$`Employees on charge/petition` <- NULL
  data[,
    size := cut(
      Num_Eligible_Voters,
      breaks = c(0, 5, 10, 25, 50, 100, 500, Inf),
      right = T,
      labels = c("<5", "6-10", "11-25", "26-50", "51-100", "101-500", "500>"),
      ordered_result = T
    )
  ]

  ### Get most recent
  cat("Identifying duplicates\n")
  data <- data[order(
    -Date_Filed,
    -Tally_Date,
    -Date_Closed,
    Num_Eligible_Voters
  )]
  data$Unique <- !duplicated(data, by = c("Case", "Unit ID"))
  to_drop <- !duplicated(
    data,
    by = c(
      'Case',
      'Tally_Date',
      'Tally_Type',
      'Date_Filed',
      'Ballot_Type',
      'Unit ID',
      'Total_Ballots_Counted',
      'Votes_Against',
      'Votes_For_Union',
      'Status',
      'Reason_Closed'
    )
  )
  data <- data[to_drop]

  # data <- data[Status=="Closed"]
  # data <- data[`Reason_Closed` %in% c("Certific. of Representative", "Certification of Results")]
  cat("Dropping elections with multiple\n")
  data <- data[
    `Ballot_Type` %in%
      c("Single Labor Organization", "Revised Single Labor Org", "")
  ]
  data[, Case_Type := substr(Case, 4, 5)]

  cat("Filling in NAs with 0s\n")

  data[, Election_Data := ifelse(is.na(Tally_Date), "No", "Yes")]
  data <- data[
    !(Election_Data == "No" & Case %in% data[Election_Data == "Yes", Case]),
  ]

  data[is.na(`Votes_Against`) & Election_Data == "Yes", `Votes_Against` := 0]
  data[
    is.na(`Votes_For_Union`) & Election_Data == "Yes",
    `Votes_For_Union` := 0
  ]
  data[
    is.na(`Total_Ballots_Counted`) & Election_Data == "Yes",
    `Total_Ballots_Counted` := 0
  ]
  data[
    is.na(`Num_Eligible_Voters`) & Election_Data == "Yes",
    `Num_Eligible_Voters` := 0
  ]

  #cat("Fixing Union Names")
  data[, Labor_Union := gsub("[[:space:]]|\u00A0", " ", Labor_Union)] ## cleaning in case
  data[, Plot_Labor_Union := Labor_Union]
  data[,
    tmp_Labor_Union := gsub(
      "[[:space:]]+|\u00A0",
      " ",
      gsub("&", "and", Labor_Union)
    )
  ]

  matched <- match_national(data$tmp_Labor_Union, dict_exact, dict)
  data[, National_Count := matched$National_Count]
  data[, National := matched$National]
  data$tmp_Labor_Union <- NULL

  data[,
    `Margin` := (`Votes_For_Union`) / (`Votes_For_Union` + `Votes_Against`)
  ]
  data[`Total_Ballots_Counted` == 0, `Margin` := NA]

  data[,
    `Union_Cer` := dplyr::case_when(
      `Status` == "Open" ~ "Still Open",
      `Reason_Closed` == "Certific. of Representative" ~ "Yes",
      `Reason_Closed` %in% c("Certification of Results", "Withdrawl") ~ "No",
      .default = "Other"
    )
  ]

  # data <- data[Case_Type %in% c("RC", "RD")]

  # data_rc <- data[Case_Type=="RC"]
  data$`Didnt_Vote` <- data$`Num_Eligible_Voters` -
    data$`Votes_For_Union` -
    data$`Votes_Against`

  return(data)
}
