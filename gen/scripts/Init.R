
library(data.table)
library(ggplot2)
library(ggthemes)
library(gridExtra)
# library(ggrepel)
library(magrittr)
library(stringr)
library(xtable)
library(here)
library(svglite)

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
dt$Unique <- !duplicated(dt, by='Case')

### Create New Variables
dt[,Tally_Date:=as.Date(`Tally_Date`, format="%m/%d/%Y")]
dt[,Date_Filed:=as.Date(`Date_Filed`, format="%m/%d/%Y")]
dt[,Length:=Tally_Date-Date_Filed]
dt[,Tally_Quarter := as.Date(cut(Tally_Date, breaks = "quarter"))]

dt[,size:=cut(Num_Eligible_Voters, breaks = c(0, 5, 10, 25, 50, 100, 500, Inf), right = T,
              labels=c("<5", "6-10", "11-25", "26-50", "51-100", "101-500", "500>"), ordered_result = T)]



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




create_state_plot <- function(state_abb = "MN", number=10, data=NULL,
                              file=NULL){


  state <- state.name[state.abb == state_abb]

  tmp_dt <- data[State==state_abb & Unique==TRUE & Case_Type == "RC" & Status=="Closed"]

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
    labs(y="", x="Votes", caption = "Includes only certification votes with a single union, data from NLRB")

  f <- here("content", "data", "states", state, paste0(state_abb, "_10.svg"))
  ggsave(f, height=10*log10(number), width=8)


}


create_state_time_plot <- function(state_abb = "MN", data=NULL,
                              file=NULL){


  state <- state.name[state.abb == state_abb]

  tmp_dt <- data[State==state_abb  & Case_Type == "RC" &
                   Ballot_Type != "Revised Single Labor Org" &
                   !is.na(size) ]

  # tmp_dt$City <- tools::toTitleCase(tolower(tmp_dt$City))

  ggplot(tmp_dt, aes(x=Tally_Quarter,
             fill=size)) +
  geom_bar(position=position_stack(reverse=T), color="black", size=.2, width=80) +
  scale_x_date(limits=c(as.Date("2008-01-01"), lubridate::today())) +
  theme_minimal() +
    scale_fill_colorblind("Size of Unit", drop=F) +
  theme(legend.position = "bottom") +
  labs(y="", x="Votes", caption = "Includes only certification votes with a single union, data from NLRB")

  f <- here("content", "data", "states", state, paste0(state_abb, "_hist_size.svg"))

  ggsave(f, height=6, width=8)

  ggplot(tmp_dt, aes(x=Tally_Quarter,
                     fill=Union_Cer, weight=Num_Eligible_Voters)) +
    geom_bar(position=position_stack(reverse=T), color="black", size=.2) +
    scale_x_date(limits=c(as.Date("2008-01-01"), lubridate::today())) +
    theme_minimal() +
    scale_fill_colorblind("Unionized?") +
    theme(legend.position = "bottom") +
    labs(y="", x="Votes", caption = "Includes only certification votes with a single union, data from NLRB")
  f <- here("content", "data", "states", state, paste0(state_abb, "_hist_vic.svg"))

  ggsave(f, height=6, width=8)


}


# create_state_table <- function(state_abb = "MN", number=10, data=dt_rc){
#
#   state <- state.name[state.abb == state_abb]
#   tmp_dt <- data[State==state_abb]
#
#   tmp_dt <- head(setorder(tmp_dt, -`Num_Eligible_Voters`), number)
#
#   tab <- xtable::xtable(tmp_dt[,.(City, State, Case_Name, Labor_Union, Status, Date_Filed, Date_Closed,
#                      `Tally Type`, Union_Cer, Votes_For_Union, Votes_Against, Num_Eligible_Voters,
#                      `Challenged Ballots`, `Challenges are Determinative`,
#                      Didnt_Vote, Margin, `Voting Unit (Unit A)`, Case )])
#   # tab[1,] <- gsub("_", " ", (tab[1,]))
#   tab[1,9] <- "Union Certified?"
#   # tab <- theme_striped(tab)
#
#   if(!dir.exists(here("content",  "tables", state))) dir.create(here("content",  "tables", state))
#
#   f <- file(here("content", "tables", state, "open.html"))
#   print(tab, type = "html",  file= f, comment=F)
#   close(f)
# }

create_state_table_open <- function(state_abb = "MN", data=NULL){

  state <- state.name[state.abb == state_abb]
  tmp_dt <- data[State==state_abb]
  tmp_dt <- tmp_dt[Status=="Open"]
  tmp_dt <- setorder(tmp_dt, -`Date_Filed`)

  tmp_dt$Date_Filed <- as.character(tmp_dt$Date_Filed, "%b %d, %Y")

  # tmp_dt$Case <- paste0("<a href='https://www.nlrb.gov/case/", tmp_dt$Case, "'>", tmp_dt$Case, "</a>")
  tab <- xtable(tmp_dt[,.(City, State, Case_Name, Labor_Union, Case_Type, Date_Filed,
                            Num_Eligible_Voters, Case )])
  align(tab)[8] <- "c"
  # tab[1,] <- gsub("_", " ", (tab[1,]))
  # tab[1,9] <- "Union Certified?"
  # tab <- theme_basic(tab)

  if(!dir.exists(here("content",  "tables", state))) dir.create(here("content",  "tables", state))

  f <- here("content", "tables", state, "open.html")
  print(tab, file = f, type="html",
        html.table.attributes="class='open-cases'",
        comment = F,include.rownames = F,
        sanitize.colnames.function = function(x) gsub("_", " ", x))
  file_contents <- readLines(f)
  cases <- unique(tmp_dt$Case)
  for(case in cases){


    file_contents <- gsub(case,
                          paste0("<a href='https://www.nlrb.gov/case/", case, "'>", case, "</a>"),
                          file_contents)

  }
  write(file_contents, f)
}

# create_state_table_open(data=dt, state_abb = "CA")

create_state_page <- function(state_abb = "CA"){

  state <- state.name[state.abb == state_abb]

  # dir.create(here("content",  "data", "states", state))


  tmp <-c(paste("## ", state),
          "",
          paste("### Number Employees in a Union Election by Outcome"),
          paste0("{{< image src=\"",state_abb, "_hist_vic.svg\" >}}"),
          "",
          paste("### Number of Elections by Unit Size"),
          paste0("{{< image src=\"",state_abb, "_hist_size.svg\" >}}"),
          "",
          paste("### Largest Private Union Elections"),
          paste0("{{< image src=\"",state_abb, "_10.svg\" >}}"),
          "",
          "### Open Election Related Cases",
          paste0("{{< readtable table=\"/tables/", state, "/open.html\" >}}"),
          "",
          "#### test"
          )

  f <- file(here("content", "data", "states", state, "_index.md"))
  writeLines(tmp, f)
  close(f)

}



for(state in state.abb){
  create_state_page(state_abb = state)

  # create_state_plot(state_abb = state,
  #                   number=10,
  #                   data=dt,file =here("content",  "data", "states", state,
  #                                        paste0(state_abb, "_", number, ".svg") ))
  # create_state_time_plot(state_abb = state,
  #                   data=dt)
  #
  # create_state_table_open(state_abb = state, data=dt)
  #
}

# create_state_table_open(state_abb = "CA", data=dt)

# create_state_plot(state_abb = "ND",
#                   number=10,
#                   data=dt)

# create_state_time_plot(state_abb = "MN",
#                   data=dt)
