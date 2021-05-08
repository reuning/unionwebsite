
library(data.table)
library(here)



# dt <- fread(here("gen", "data", "recent_election_results.csv"))
# names(dt)[21] <- "Votes Against"

downloader::download("https://www.nlrb.gov/reports/graphs-data/recent-election-results/csv-export",
                     here("gen", "data", "temp.csv"))
dt_new <- fread(here("gen", "data", "temp.csv"))
names(dt_new)[21] <- "Votes Against"

# dt_new[Status == "Closed"]
# dt[Status == "Open"]
# 
# dt_tmp <- rbind(dt, dt_new[Status=="Open"])
# dt_tmp[,`Tally Date`:=as.Date(`Tally Date`, format="%m/%d/%Y")]
# dt_tmp[,`Date Filed`:=as.Date(`Date Filed`, format="%m/%d/%Y")]
# 
# 
# dt_tmp <- unique(dt_tmp, by=c("Case", "Tally Type", "Unit ID", "Tally Type", 
#                               "Status", "Reason Closed",
#                               "Challenges are Determinative", "Ballot Type"))
# dim(dt_tmp)
# 
# rm(dt_new)

col_names <-which(apply(dt_new, 2, function(x) any(grepl('"', x))))
for (j in col_names) set(dt_new, j = j, value = gsub('"', '', dt_new[[j]]))


fwrite(dt_new, file = here("gen", "data", "recent_election_results.csv"), row.names = F)



# dim(dt_new[Case %in%(dt[Status=="Closed", Case])])
# dim(dt[Status=="Closed"])
# tmp1 <-dt_new[Case %in%(dt[Status=="Closed", Case])] 
# tmp2 <- dt[Status=="Closed"]
# tmp3 <- unique(rbind(tmp1, tmp2), 
#                by=c("Case", "Tally Type", "Unit ID", "Tally Type", 
#                     "Status", "Reason Closed", "Tally Date",
#                     "No of Eligible Voters",
#                     "Challenged Ballots",
#                     "Votes for Labor Union1",
#                     "Total Ballots Counted",
#                     "Votes Against",
#                     "Runoff Required",
#                     "Challenges are Determinative", "Ballot Type"))
# anyDuplicated(rbind(tmp2, tmp3))
