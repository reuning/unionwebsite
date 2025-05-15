
library(data.table)
library(here)
sysfonts::font_add_google("Crimson Pro")

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






library(tidygeocoder)


#source(here("gen", "scripts", "Scripts.R"))

dt <- fread(here("gen", "data", "recent_election_results.csv"))
open_dt <- dt[Status == "Open"]
open_dt <- unique(open_dt, by = c("Case", "City", "State"))
cols <- c(as.vector(which(apply(open_dt, 2,
                                function(x)
                                  all(!is.na(x))))), 32:42)
coded_dt <-
  fread(here("gen", "data", "elections_with_location.csv"))
tmp <- unique(coded_dt[, .(Case, `Case Name`, lat, 
                           long, formatted_address, 
                           accuracy, accuracy_type, 
                           source, address_components.city, 
                           address_components.county, 
                           address_components.state, 
                           address_components.zip, 
                           address_components.country,
                           seached, 
                           lat_jit, 
                           long_jit)])
full_dt <- merge(open_dt, tmp, all.x = T, by=c("Case", "Case Name"))
if (any(is.na(full_dt$seached))) {
  to_code_dt <- full_dt[is.na(seached)]
  done_dt <- full_dt[!is.na(seached)]
  
  to_code_dt <- tidygeocoder::geocode(
    as.data.frame(to_code_dt[,1:Voters]),
    city = "City",
    state = "State",
    full_results = T,
    # return_type = "geographies",
    method = "geocodio"
  )
  
  try(to_code_dt$address_components.street <- NULL)
  try(to_code_dt$address_components.formatted_street <- NULL)
  try(to_code_dt$address_components.suffix <- NULL)
  try(to_code_dt$address_components.predirectional <- NULL)
  try(to_code_dt$address_components.postdirectional <- NULL)
  
  to_code_dt$seached <- 1
  to_code_dt$lat_jit <- to_code_dt$lat + runif(nrow(to_code_dt), -.5, .5)
  to_code_dt$long_jit <- to_code_dt$long + runif(nrow(to_code_dt), -.5, .5)
  
  to_code_dt$address_lines <- NULL
  full_dt <- rbind(done_dt, to_code_dt, fill=TRUE)
  write.csv(
    full_dt,
    file = here("gen", "data", "elections_with_location.csv"),
    row.names = F
  )
}
#### Starbucks ####
dt <- fread(here("gen", "data", "recent_election_results.csv"))
st_dt <- dt[grepl("starbuck", `Case Name` , ignore.case = T)]
st_dt <- prep_data(st_dt)
st_dt <- st_dt[Unique==TRUE]
# cols <- c(as.vector(which(apply(open_dt, 2,
#                                 function(x)
#                                   all(!is.na(x))))), 32:42)
coded_dt <-
  fread(here("gen", "data", "starbucks_with_location.csv"))
tmp <- unique(coded_dt[, .(Case, Case_Name, lat, 
                           long, formatted_address, 
                           accuracy, accuracy_type, 
                           source, address_components.city, 
                           address_components.county, 
                           address_components.state, 
                           address_components.zip, 
                           address_components.country,
                           seached, 
                           lat_jit, 
                           long_jit)])
full_dt <- merge(st_dt, tmp, all.x = T, by=c("Case", "Case_Name"))
if (any(is.na(full_dt$seached))) {
  to_code_dt <- full_dt[is.na(seached)]
  done_dt <- full_dt[!is.na(seached)]
  
  to_code_dt <- tidygeocoder::geocode(
    as.data.frame(to_code_dt[,1:Didnt_Vote]),
    city = "City",
    state = "State",
    full_results = T,
    # return_type = "geographies",
    method = "geocodio"
  )
  
  try(to_code_dt$address_components.street <- NULL)
  try(to_code_dt$address_components.formatted_street <- NULL)
  try(to_code_dt$address_components.suffix <- NULL)
  try(to_code_dt$address_components.predirectional <- NULL)
  try(to_code_dt$address_components.postdirectional <- NULL)
  
  to_code_dt$seached <- 1
  to_code_dt$lat_jit <- to_code_dt$lat + runif(nrow(to_code_dt), -.5, .5)
  to_code_dt$long_jit <- to_code_dt$long + runif(nrow(to_code_dt), -.5, .5)
  
  to_code_dt$address_lines <- NULL
  full_dt <- rbind(done_dt, to_code_dt, fill=TRUE)
  write.csv(
    full_dt,
    file = here("gen", "data", "starbucks_with_location.csv"),
    row.names = F
  )
}
