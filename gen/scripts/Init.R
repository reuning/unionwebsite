
library(data.table)
library(here)

source(here("gen", "scripts", "Scripts.R"))


dt <- fread(here("gen", "data", "recent_election_results.csv"))
dt_old <- fread(here("gen", "data", "old_nlrb.csv"))
dt_old$`Employees on charge/petition` <- NA

dt_old$Election_Data <- NULL
cols <- names(dt_old)
dt <- rbind(dt[,..cols], dt_old)
# dt[Case=="19-RC-195508",`Labor Union1`:= "SEIU Healthcare 1199NW"]
dt <- prep_data(dt)


dt[National %in% names(which(table(dt$National) < 20)), National:="Other"]
dt[National=="", National:="Uncoded"]
dt[National_Count>1, National:="Multiple"]
write.csv(dt, here("gen", "data", "cleaned_data.csv"), row.names = F)
