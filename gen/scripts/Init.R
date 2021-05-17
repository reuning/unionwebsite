
library(data.table)
library(here)

source(here("gen", "scripts", "Scripts.R"))

dict <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTQj4UzxBycuPUVmIXM9RUnTWq0dwHICOk-phgwyfjqZAm8lsjl3D4JTLz73aa4dnOJ7gXmhehPGfu8/pub?gid=0&single=true&output=csv")

dt <- fread(here("gen", "data", "recent_election_results.csv"))



names(dt)[c(2,6,7,8,9,10, 11,13,15,18,21,22)] <- c("Case_Name", "Date_Closed",
                                  "Reason_Closed", "Date_Filed",
                                  "Tally_Date", "Tally_Type", "Ballot_Type",
                                  "Num_Eligible_Voters",  "Labor_Union",
                                  "Votes_For_Union","Votes_Against",
                                  "Total_Ballots_Counted")


### Create New Variables
dt[,Tally_Date:=as.Date(`Tally_Date`, format="%m/%d/%Y")]
dt[,Date_Filed:=as.Date(`Date_Filed`, format="%m/%d/%Y")]
dt[,Length:=Tally_Date-Date_Filed]
dt[,Tally_Quarter := as.Date(cut(Tally_Date, breaks = "quarter"))]

dt[,size:=cut(Num_Eligible_Voters, breaks = c(0, 5, 10, 25, 50, 100, 500, Inf), right = T,
              labels=c("<5", "6-10", "11-25", "26-50", "51-100", "101-500", "500>"), ordered_result = T)]


### Get most recent
dt <- dt[order(-Tally_Date)]
dt$Unique <- !duplicated(dt, by='Case')

# dt <- dt[Status=="Closed"]
# dt <- dt[`Reason_Closed` %in% c("Certific. of Representative", "Certification of Results")]
dt <- dt[`Ballot_Type`%in% c("Single Labor Organization", "Revised Single Labor Org")]
dt[, Case_Type:=substr(Case, 4, 5)]

dt[is.na(`Votes_Against`),`Votes_Against`:=0 ]
dt[is.na(`Votes_For_Union`),`Votes_For_Union`:=0 ]
dt[is.na(`Total_Ballots_Counted`),`Total_Ballots_Counted`:=0 ]
dt[is.na(`Num_Eligible_Voters`),`Num_Eligible_Voters`:=0 ]

for(ii in 1:nrow(dict)){

  srch <- dict$Name[ii]
  repl <- ifelse(dict$Render.National.Union.As[ii]== "",
                 dict$Render.IU.As[ii], dict$Render.National.Union.As[ii])
  dt[,Plot_Labor_Union:=gsub(srch, repl, Labor_Union, ignore.case = T)]

}


dt[,`Margin`:=(`Votes_For_Union`)/(`Votes_For_Union`+`Votes_Against`)]
dt[`Total_Ballots_Counted`==0,`Margin`:=NA]
dt[,`Union_Cer`:=ifelse(`Reason_Closed`=="Certific. of Representative", "Yes", "No")]

# dt <- dt[Case_Type %in% c("RC", "RD")]

# dt_rc <- dt[Case_Type=="RC"]
dt$`Didnt_Vote` <- dt$`Num_Eligible_Voters` - dt$`Votes_For_Union` - dt$`Votes_Against`





dir.create(here("content", "tables"))
### National Plots
create_state_plot(data=dt)
create_state_time_plot(data=dt)
create_state_table_open(data=dt)

## State Plots
for(state in state.abb){
  create_state_page(state_abb = state)

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
