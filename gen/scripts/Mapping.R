library(tidygeocoder)
library(here)

source(here("gen", "scripts", "Scripts.R"))

## Based on https://r-spatial.org/r/2018/10/25/ggplot2-sf-3.html

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
  
  
  full_dt <- rbind(done_dt, to_code_dt)
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
  
  
  full_dt <- rbind(done_dt, to_code_dt)
  write.csv(
    full_dt,
    file = here("gen", "data", "starbucks_with_location.csv"),
    row.names = F
  )
}



