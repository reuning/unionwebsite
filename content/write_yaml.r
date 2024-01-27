library(yaml)

setwd("content/data/states")
data <- read_yaml("states-template.yml")

data$items$Alabama

state.abb
state.name
for(ii in seq_along(state.abb)){
    tmp_list <- list(State=state.name[ii], 
        data=list(subset=paste0("data[data$State==\"", state.abb[ii], "\",]"),
                filename=paste0(state.abb[ii], "_data.csv")))

    data$items[[state.name[ii]]] <- tmp_list
}

write_yaml(data, "states-template.yml")
