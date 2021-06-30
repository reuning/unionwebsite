
library(data.table)
library(here)

source(here("gen", "scripts", "Scripts.R"))


dt <- fread(here("gen", "data", "recent_election_results.csv"))

dt <- prep_data(dt)

dir.create(here("content", "tables"))
### National Plots
create_state_page(title = "United States",  
                  data=tmp_data, 
                  file_name = here("content", "tables",
                                   "national", "open.html"))
create_state_plot(data=dt)
create_state_time_plot(data=dt)
create_state_table_open(data=dt)

## State Plots
for(state in state.abb){
  tmp_data = dt[dt$State == state]
  create_page(title = state.name[state.abb == state], 
                    data=tmp_data, 
                    file_name = here("content", "data", "states", 
                                     state.name[state.abb == state], 
                                     "_index.md"))

  create_plot(data=tmp_data, 
              file_name = here("content", "data", "states", 
                               state.name[state.abb == state], 
                               "_10.png"), 
              number=10)
  
  create_state_time_plot(state_abb = state,
                    data=tmp_data)

  create_state_table_open(state_abb = state, data=tmp_data)
  #
}

# create_state_time_plot(data=dt)
# create_state_page(state_abb = "GU")
# create_state_plot(state_abb = "GU",
#                   number=10,
#                   data=dt)
# create_state_time_plot(state_abb = "GU",
#                        data=dt)
