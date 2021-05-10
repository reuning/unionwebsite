library(data.table)
library(here)

dt <- fread(here("gen", "data", "recent_election_results.csv"))
names(dt)[21] <- "Votes Against"

downloader::download("https://www.nlrb.gov/reports/graphs-data/recent-election-results/csv-export",
                     here("gen", "data", "temp.csv"))
dt_new <- fread(here("gen", "data", "temp.csv"))
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


fwrite(dt_out, file = here("gen", "data", "recent_election_results.csv"), row.names = F)
