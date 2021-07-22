library(data.table)
library(here)
library(rvest)

retry_page <- function(url){
  check <- 0
  while(check < 20){
    con <- curl::curl(url)
    page <- try(read_html(con))
    if(class(page)[1]=="try-error"){
      check <- check + 1
      Sys.sleep(runif(1))
    } else {
      return(page)
    }
  }
  stop("Cannot Download page after 5 tries")
}

dt <- fread(here("gen", "data", "recent_election_results.csv"))
names(dt)[21] <- "Votes Against"

#downloader::download("csv-export",
#                     here("gen", "data", "temp.csv"))
dt_new <- fread(here("gen", "data", "temp.csv"))
dt_new$Election_Data <- "Yes"
names(dt_new)[21] <- "Votes Against"



tmp1 <- dt_new[Status == "Closed"]
tmp2 <- dt[Status == "Open"]

dt_newly_closed <- tmp1[na.omit(match(tmp2$Case, tmp1$Case))]


tmp1 <- dt_new[Status == "Closed"]
dt_missing_closed <- tmp1[!tmp1$Case %in% dt[Status == "Closed", Case]]

dt_out <- rbind(dt_new[Status == "Open"],
                dt[Status == "Closed"],
                dt_newly_closed,
                dt_missing_closed)

dt_out <- unique(dt_out, by = names(dt_out)[which(!names(dt_out) %in% c("Voting Unit (Unit A)",
                                 "Voting Unit (Unit B)",
                                 "Voting Unit (Unit C)",
                                 "Voting Unit (Unit D)"))] )



col_names <-which(apply(dt_out, 2, function(x) any(grepl('"', x))))
for (j in col_names) set(dt_out, j = j, value = gsub('"', '', dt_out[[j]]))


# dt_out


open_dt <- fread(here("gen", "data", "new_open_data.csv"))
open_dt <- open_dt[grepl("RC|RD|RM|UD",`Case Number`)]

open_dt$Voters[is.na(open_dt$Voters)] <-
  open_dt$`Employees on charge/petition`[is.na(open_dt$Voters)]

open_dt <- open_dt[, c("Case Name", "Case Number", "City", "Date Filed",
                       "State", "Unit Sought", "Voters" )]
names(open_dt)[c(2,6,7)] <- c("Case", "Voting Unit (Unit A)",
                                "No of Eligible Voters")

old_open <- fread(here("gen", "data", "open_petitions.csv"))


open_dt <- merge(open_dt, old_open[,c("Case", "Labor Union1") ], all.x=T)

open_dt <- open_dt[!open_dt$`Case` %in% dt_out$`Case`]


for(ii in 1:nrow(open_dt)){

  if(!is.na(open_dt$`Labor Union1`[ii])) next

  url <- paste0("https://www.nlrb.gov/case/", open_dt$`Case`[ii])
  cat(url, "\n")
  page <- retry_page(url)
  if(is.na(page %>% html_node("table.Participants"))) {
    cat("No Table Found \n")
    next
  }
  tab <-try(page %>% html_node("table.Participants") %>% html_table())
  union <- grep("Involved PartyUnion|PetitionerUnion", tab$Participant, value=T)
  if(length(union)==0) next
  union <- gsub("Involved PartyUnion|PetitionerUnion", "", union)
  union <- stringr::str_trim(union)
  if(length(union)>1){
    union <- union[which.max(nchar(union))]
  }

  open_dt$`Labor Union1`[ii] <- union
  Sys.sleep((runif(1, .1, .3)))
}


open_dt$Status <- "Open"
write.csv(open_dt, file=here("gen", "data", "open_petitions.csv"), row.names = F)
### Delete Temporary File

open_dt$Election_Data <- "No"
dt_out$Election_Data <- "Yes"

dt_out <- rbind(dt_out, open_dt, fill=T)
fwrite(dt_out, file = here("gen", "data", "recent_election_results.csv"), row.names = F)
