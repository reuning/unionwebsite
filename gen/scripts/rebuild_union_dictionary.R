## Rebuilds gen/data/union_dictionary_exact.csv and gen/data/union_dictionary_substrings.csv
## from the Labor_Union -> National mappings already present in gen/data/cleaned_data.csv.
##
## Background: prep_data() in Scripts.R used to fetch a hand-curated Name/International/National
## lookup table from a published Google Sheet. That sheet was accidentally deleted (2026-07-29)
## with no cached copy or Wayback Machine snapshot available. Because every historical row in
## cleaned_data.csv already carries the National label the old dictionary assigned, this script
## reconstructs a *functionally equivalent* replacement by harvesting those labels directly,
## rather than the exact original file.
##
## Re-run this periodically (e.g. after a batch of newly-Uncoded rows get manually reviewed and
## re-coded) to fold newly-confirmed Labor_Union -> National mappings back into the dictionary.
##
## Design: split into two tiers for performance -- exact-match entries are looked up via a fast
## hash join (cheap regardless of table size), while substring/regex entries require an O(rows)
## grepl() per row and are kept to a small, high-value shortlist (generic recurring canonical
## union names without a local number, e.g. "International Association of Machinists ...").

library(data.table)
library(stringr)
library(here)

dt <- fread(here("gen", "data", "cleaned_data.csv"))

## Only harvest from historically unambiguous, real (non-placeholder) codes
coded <- dt[!National %in% c("Uncoded", "Other", "Multiple") & National_Count == 1]

escape_regex <- function(x) {
  str_replace_all(x, "([.\\\\|()\\[\\]{}^$*+?])", "\\\\\\1")
}

## Frequency of each (Name, National) pair, used to drop ambiguous conflicts and to pick
## high-value generic entries for the substring tier
freq_all <- coded[, .(Name = str_squish(Labor_Union), National)][,
  .N,
  by = .(Name, National)
]
freq_all[, Name_lower := str_to_lower(Name)]

## Drop genuine dual-affiliation conflicts (same text historically mapped to >1 National),
## e.g. AWPPU/UBC, NABET/CWA -- leave these to fall back to Uncoded/Multiple rather than
## baking in an arbitrary choice
conflicts <- freq_all[, .(n_national = uniqueN(National)), by = Name_lower][n_national > 1]
freq_all <- freq_all[!Name_lower %in% conflicts$Name_lower]
freq_all[, Name_lower := NULL]
freq_all[, has_digit := grepl("[0-9]", Name)]

## Tier 1: exact-match table (fast hash join; safe to keep large)
tier1 <- unique(freq_all[, .(Name, National)])[order(National, Name)]

## Tier 2: substring/regex table -- generic canonical names only (no digits/local numbers),
## appearing at least 3 times historically, to keep the O(rows) grepl loop fast
tier2 <- freq_all[!(has_digit) & N >= 3][order(-N)]
tier2[, Name := escape_regex(Name)]
tier2_out <- tier2[, .(Name, International = "", National)]

write.csv(tier1, here("gen", "data", "union_dictionary_exact.csv"), row.names = FALSE)
write.csv(
  tier2_out,
  here("gen", "data", "union_dictionary_substrings.csv"),
  row.names = FALSE
)

cat(
  "Wrote", nrow(tier1), "exact-match entries and", nrow(tier2_out),
  "substring entries covering", uniqueN(freq_all$National), "National codes\n"
)
