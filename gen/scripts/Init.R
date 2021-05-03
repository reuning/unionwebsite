setwd("~/Dropbox/Projects/unionwebsite/")

library(data.table)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(ggrepel)
library(magrittr)
library(stringr)
library(huxtable)
library(here)

dict <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTQj4UzxBycuPUVmIXM9RUnTWq0dwHICOk-phgwyfjqZAm8lsjl3D4JTLz73aa4dnOJ7gXmhehPGfu8/pub?gid=0&single=true&output=csv")

dt <- fread(here("gen", "data", "recent_election_results.csv"))
names(dt)[c(2,6,7,8,9,11,13,15,18,21,22)] <- c("Case_Name", "Date_Closed", 
                                  "Reason_Closed", "Date_Filed",
                                  "Tally_Date", "Ballot_Type",
                                  "Num_Eligible_Voters",  "Labor_Union", 
                                  "Votes_For_Union","Votes_Against",
                                  "Total_Ballots_Counted")

### Get most recent
dt <- dt[order(-Tally_Date)]
dt <- unique(dt, by='Case')

### Create New Variables
dt[,Tally_Date:=as.Date(`Tally_Date`, format="%m/%d/%Y")]
dt[,Date_Filed:=as.Date(`Date_Filed`, format="%m/%d/%Y")]

dt[,Length:=Date_Filed-Tally_Date]




dt <- dt[Status=="Closed"]
dt <- dt[`Reason_Closed` %in% c("Certific. of Representative", "Certification of Results")]
dt <- dt[`Ballot_Type`%in% c("Single Labor Organization")]
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

dt_rc <- dt[Case_Type=="RC"]
dt_rc$`Didnt_Vote` <- dt_rc$`Num_Eligible_Voters` - dt_rc$`Votes_For_Union` - dt_rc$`Votes_Against`




create_state_plot <- function(state_abb = "MN", number=10, data=NULL, 
                              file=NULL){
  

  state <- state.name[state.abb == state_abb]
  
  tmp_dt <- data[State==state_abb]
  tmp_dt$City <- tools::toTitleCase(tolower(tmp_dt$City))
  tmp_dt$`Case_Name` <-  str_to_title(tmp_dt$`Case_Name`) %>% str_trunc(width = 50)
  
  
  
  tmp_dt <- head(setorder(tmp_dt, -`Num_Eligible_Voters`), number) 
  
  
  x_lim = max(tmp_dt$`Num_Eligible_Voters`)*1.1
  
  tmp_dt %>%
    melt(measure.vars=c("Votes_For_Union", "Votes_Against", "Didnt_Vote"))  %>%
    ggplot(aes(x=value, y=reorder(paste(`Plot_Labor_Union`, "at",
                                        `Case_Name`, "in", `City`, "on",
                                        `Tally_Date`),
                                  `Num_Eligible_Voters`), 
               fill=variable)) +
    geom_col(position=position_stack(reverse=T), color="black") + 
    geom_text(x=x_lim, 
              aes(label=scales::percent(Margin, accuracy =2),
                  color=Union_Cer)) +
    scale_x_continuous(limits=c(0, x_lim)) +
    scale_y_discrete(label=scales::label_wrap(35)) +
    scale_fill_manual("", values=c("springgreen4", "orangered3", "grey")) + 
    guides(color=F) + 
    theme_minimal() + 
    scale_color_manual(values = c("orangered3", "springgreen4")) + 
    annotate("text", x=x_lim, y=number + .5, label="Margin") + 
    theme(legend.position = "bottom") + 
    guides(alpha=F) + 
    labs(y="", x="Votes", caption = "Includes only certification votes with a single union, data from NLRB") + 
    ggtitle(paste("Largest Private Union Elections Since 2007 in", state)) 
  
  ggsave(file, height=10*log10(number), width=8)
  
  
}

create_state_table <- function(state_abb = "MN", number=10, data=dt_rc){
  
  state <- state.name[state.abb == state_abb]
  tmp_dt <- data[State==state_abb]
  
  tmp_dt <- head(setorder(tmp_dt, -`Num_Eligible_Voters`), number) 
  
  tab <- huxtable(tmp_dt[,.(City, State, Case_Name, Labor_Union, Status, Date_Filed, Date_Closed, 
                     `Tally Type`, Union_Cer, Votes_For_Union, Votes_Against, Num_Eligible_Voters, 
                     `Challenged Ballots`, `Challenges are Determinative`, 
                     Didnt_Vote, Margin, `Voting Unit (Unit A)`, Case )])
  tab[1,] <- gsub("_", " ", (tab[1,]))
  tab[1,9] <- "Union Certified?"
  tab <- theme_striped(tab)
  
  retun(to_html(tab))
}


create_state_page <- function(state_abb = "MN", number=10, data=dt_rc){

  state <- state.name[state.abb == state_abb]
    
  dir.create(here("content",  "docs", "states", state))
  
  tmp <-c(paste("### 10 Largest Elections in", state),
          "", 
          paste0("{{< figure src=",state_abb, "_", number, ".png >}}")
          )

  writeLines(tmp, file(here("content", "docs", "states", state, "_index.md")))
  create_state_plot(state_abb = state_abb, 
                    number=number, 
                    data=data,file =here("content",  "docs", "states", state,
                                         paste0(state_abb, "_", number, ".png") ))
}

create_state_page(state_abb = "OH")
