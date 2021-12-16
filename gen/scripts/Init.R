
library(data.table)
library(here)

source(here("gen", "scripts", "Scripts.R"))


dt <- fread(here("gen", "data", "recent_election_results.csv"))
dt_old <- fread(here("gen", "data", "old_nlrb.csv"))

dt <- rbind(dt, dt_old)
# dt[Case=="19-RC-195508",`Labor Union1`:= "SEIU Healthcare 1199NW"]
dt <- prep_data(dt)



dt[National %in% names(which(table(dt$National) < 20)), National:="Other"]

dt[National=="", National:="Uncoded"]
dt[National_Count>1, National:="Multiple"]


#### Create quarterly reports ####

tmp <- seq.Date(lubridate::today()+1, lubridate::as_date("2010-01-01"), by = "-3 month")
quarters <- lubridate::quarter(tmp, type = "quarter")
years <- year(tmp)
starts <- lubridate::quarter(tmp, type="date_first")
ends <- lubridate::quarter(tmp, type="date_last")

for(ii in 2:length(quarters)){
  year <- years[ii]
  quarter <- quarters[ii]
  report_page(year=year, quarter=quarter)
  file_name <- here("content", "tables","reports", year, paste0(quarter, "union_filings.html"))
  
  report_table_filed(data = dt, 
               start_time=starts[ii], 
               end_time=ends[ii], 
               file_name=file_name)
  
  
  file_name <- here("content", "tables","reports", year, paste0(quarter, "union_closed.html"))
  
  report_table_closed(data = dt, 
                     start_time=starts[ii], 
                     end_time=ends[ii], 
                     file_name=file_name)

  
}


#### Create other pages ####
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



### Create National union table
dt <- fread(here("gen", "data", "recent_election_results.csv"))


dt <- prep_data(dt)

dt[National=="", National:="Uncoded"]
dt[National_Count>1, National:="Multiple"]
all_unions <- unique(dt$National)
all_unions <- all_unions[all_unions!=""]

create_front_page_table(data=dt, file_name="filed_ytd.html", 
                        column_name = "Date_Filed", all_groups = all_unions)
create_front_page_table(data=dt, file_name = "tally_ytd.html", column_name = "Tally_Date", 
                        all_groups = all_unions)

dt$Date_Closed <- as.Date(dt$Date_Closed, format="%m/%d/%Y")
create_front_page_table(data=dt, file_name = "closed_ytd.html", 
                        column_name = "Date_Closed", all_groups = all_unions)

all_states <- unique(dt$State)
all_states <- all_states[all_states!=""]
create_front_page_table(data=dt, file_name="filed_ytd.html", column_name = "Date_Filed", 
                        var="State",all_groups = all_states)
create_front_page_table(data=dt, file_name = "tally_ytd.html", column_name = "Tally_Date",  
                        var="State",all_groups = all_states)
create_front_page_table(data=dt, file_name = "closed_ytd.html", column_name = "Date_Closed",  
                        var="State",all_groups = all_states)

# create_state_time_plot(data=dt)
# create_state_page(state_abb = "GU")
# create_state_plot(state_abb = "GU",
#                   number=10,
#                   data=dt)
# create_state_time_plot(state_abb = "GU",
#                        data=dt)
