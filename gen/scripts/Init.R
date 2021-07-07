
library(data.table)
library(here)

source(here("gen", "scripts", "Scripts.R"))


dt <- fread(here("gen", "data", "recent_election_results.csv"))

dt <- prep_data(dt)

# View(dt[National_Count > 1])
# View(table(dt[National_Count == 0, Labor_Union]))

dt[National %in% names(which(table(dt$National) < 25)), National:="Other"]

dt[National=="", National:="Uncoded"]
dt[National_Count>1, National:="Multiple"]

nationals <- unique(dt$National)

dir.create(here("content", "tables"))
### National Plots
create_page(title = "United States",  
                  data=dt, 
                  file_name = here("content", "data", "national",  
                                     "_index.md"), 
            type="national")
create_plot(data=dt, 
            file_name = here("content", "data", "national", 
                             paste0("United_States", "_10.png")))
create_time_plot(data=dt,
                 file_name = here("content", "data", "national",
                                  "United_States"))
create_table_open(data=dt, 
                  file_name=here("content", 
                                 "tables/national/", 
                                 paste0("United_States", "_open.html")))

## State Plots
for(state in state.abb){
  tmp_data = dt[dt$State == state]
  state_name <- state.name[state.abb == state]
  state_name_file <- gsub(" ", "_", state_name)
  
  create_page(title = state_name, 
                    data=tmp_data, 
                    file_name = here("content", "data", "states", 
                                     state_name_file, 
                                     "_index.md"))

  create_plot(data=tmp_data, 
              file_name = here("content", "data", "states", 
                               state_name_file, 
                               paste0(state_name_file, "_10.png")), 
              number=10)
  
  create_time_plot(data=tmp_data, 
                   file_name = here("content", "data", "states",
                                    state_name_file, 
                                    paste0(state_name_file)))

  create_table_open(data=tmp_data, 
                          file_name=here("content", "tables/states/", 
                                         paste0(state_name_file, "_open.html")))
  #
}



## State Plots
for(union in nationals){
  tmp_data = dt[dt$National == union]
  union_file <- gsub(" ", "_", union)
  weight <- ifelse(union %in% c("Other", "Uncoded","Multiple"), 2, 1)
  create_page(title = union, 
              data=tmp_data, 
              file_name = here("content", "data", "union", 
                               union_file, 
                               "_index.md"), 
              type = "union", weight=weight)
  
  create_plot(data=tmp_data, 
              file_name = here("content", "data", "union", 
                               union_file, 
                               paste0(union_file, "_10.png")), 
              number=10)
  
  create_time_plot(data=tmp_data, 
                   file_name = here("content", "data", "union",
                                    union_file, 
                                    paste0(union_file)))
  
  create_table_open(data=tmp_data, 
                    file_name=here("content", "tables/union/", 
                                   paste0(union_file, "_open.html")))
  #
}


# create_state_time_plot(data=dt)
# create_state_page(state_abb = "GU")
# create_state_plot(state_abb = "GU",
#                   number=10,
#                   data=dt)
# create_state_time_plot(state_abb = "GU",
#                        data=dt)
