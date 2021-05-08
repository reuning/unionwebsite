
library(data.table)
library(here)



dt <- fread(here("gen", "data", "recent_election_results.csv"))
names(dt)[21] <- "Votes Against"

downloader::download("https://www.nlrb.gov/reports/graphs-data/recent-election-results/csv-export", 
                     here("gen", "data", "temp.csv"))
dt_new <- fread(here("gen", "data", "temp.csv"))
names(dt_new)[21] <- "Votes Against"

dt <- unique(rbind(dt, dt_new))
rm(dt_new)

col_names <-which(apply(dt, 2, function(x) any(grepl('"', x))))
for (j in col_names) set(dt, j = j, value = gsub('"', '', dt[[j]]))


fwrite(dt, file = here("gen", "data", "recent_election_results.csv"), row.names = F)

