library(yaml)

setwd(here::here("content/data/states"))
data <- read_yaml("states-template.yml")


state.abb
state.name
for(ii in seq_along(state.abb)){
    index <- gsub(" " , "_", tolower(state.name[ii]))
    tmp_list <- list(State=state.name[ii], 
        data=list(subset=paste0("data[data$State==\"", state.abb[ii], "\",]"),
                filename=paste0(state.abb[ii], "_data.csv")))

    data$items[[index]] <- tmp_list
}

write_yaml(data, "states-template.yml")


election_data <- readr::read_csv(here::here("gen", "data", "cleaned_data.csv"))
nationals <- sort(unique(election_data$National))


data <- read_yaml("states-template.yml")
data$items <- NULL

for(ii in seq_along(nationals)){
    index <- gsub(" " , "_", tolower(nationals[ii]))
    tmp_list <- list("Union_Short"=nationals[ii], 
                    "Union_Long"=nationals[ii], 
        data=list(subset=paste0("data[data$National==\"", nationals[ii], "\",]"),
                filename=paste0(nationals[ii], "_data.csv")))

    data$items[[index]] <- tmp_list
}

setwd(here::here("content/data/union"))

write_yaml(data, "unions-template.yml")
