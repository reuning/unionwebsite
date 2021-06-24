
library(data.table)
library(here)

source(here("gen", "scripts", "Scripts.R"))


dt <- fread(here("gen", "data", "recent_election_results.csv"))

dt <- prep_data(dt)

dir.create(here("content", "tables"))
### National Plots
create_state_plot(data=dt)
create_state_time_plot(data=dt)
create_state_table_open(data=dt)

## State Plots
for(state in state.abb){
  create_state_page(state_abb = state,  data=dt)

  create_state_plot(state_abb = state,
                    number=10,
                    data=dt)
  create_state_time_plot(state_abb = state,
                    data=dt)

  create_state_table_open(state_abb = state, data=dt)
  #
}

# create_state_time_plot(data=dt)
# create_state_page(state_abb = "GU")
# create_state_plot(state_abb = "GU",
#                   number=10,
#                   data=dt)
# create_state_time_plot(state_abb = "GU",
#                        data=dt)
