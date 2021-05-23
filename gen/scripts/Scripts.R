library(data.table)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(magrittr)
library(stringr)
library(xtable)
library(curl)

sysfonts::font_add_google("Crimson Pro")
state.name <- c(state.name, "Puerto Rico", "Guam", "US Virgin Islands")
state.abb <- c(state.abb, "PR", "GU", "VI")

prep_data <- function(data=dt){
  dict <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTQj4UzxBycuPUVmIXM9RUnTWq0dwHICOk-phgwyfjqZAm8lsjl3D4JTLz73aa4dnOJ7gXmhehPGfu8/pub?gid=0&single=true&output=csv")
  
  
  rename <- c("Case_Name" = "Case Name", 
              "Date_Closed" = "Date Closed",
              "Reason_Closed" = "Reason Closed",
              "Date_Filed" = "Date Filed",
              "Tally_Date" = "Tally Date", 
              "Tally_Type" = "Tally Type",
              "Ballot_Type" = "Ballot Type",
              "Num_Eligible_Voters" = "No of Eligible Voters",
              "Labor_Union" = "Labor Union1",
              "Votes_For_Union" = "Votes for Labor Union1",
              "Votes_Against" = "Votes Against",
              "Total_Ballots_Counted" = "Total Ballots Counted")
  names(data)[match(rename, names(data))] <- names(rename)
  
  
  ### Create New Variables
  data[,Tally_Date:=as.Date(`Tally_Date`, format="%m/%d/%Y")]
  data[,Date_Filed:=as.Date(`Date_Filed`, format="%m/%d/%Y")]
  data[,Length:=Tally_Date-Date_Filed]
  data[,Tally_Quarter := as.Date(cut(Tally_Date, breaks = "quarter"))]
  
  data[,size:=cut(Num_Eligible_Voters, breaks = c(0, 5, 10, 25, 50, 100, 500, Inf), right = T,
                labels=c("<5", "6-10", "11-25", "26-50", "51-100", "101-500", "500>"), ordered_result = T)]
  
  
  ### Get most recent
  data <- data[order(-Tally_Date)]
  data$Unique <- !duplicated(data, by='Case')
  
  # data <- data[Status=="Closed"]
  # data <- data[`Reason_Closed` %in% c("Certific. of Representative", "Certification of Results")]
  data <- data[`Ballot_Type`%in% c("Single Labor Organization", "Revised Single Labor Org", "")]
  data[, Case_Type:=substr(Case, 4, 5)]
  
  data[is.na(`Votes_Against`),`Votes_Against`:=0 ]
  data[is.na(`Votes_For_Union`),`Votes_For_Union`:=0 ]
  data[is.na(`Total_Ballots_Counted`),`Total_Ballots_Counted`:=0 ]
  data[is.na(`Num_Eligible_Voters`),`Num_Eligible_Voters`:=0 ]
  
  for(ii in 1:nrow(dict)){
    
    srch <- dict$Name[ii]
    repl <- ifelse(dict$Render.National.Union.As[ii]== "",
                   dict$Render.IU.As[ii], dict$Render.National.Union.As[ii])
    data[,Plot_Labor_Union:=gsub(srch, repl, Labor_Union, ignore.case = T)]
    
  }
  
  
  data[,`Margin`:=(`Votes_For_Union`)/(`Votes_For_Union`+`Votes_Against`)]
  data[`Total_Ballots_Counted`==0,`Margin`:=NA]
  data[,`Union_Cer`:=ifelse(`Reason_Closed`=="Certific. of Representative", "Yes", "No")]
  
  # data <- data[Case_Type %in% c("RC", "RD")]
  
  # data_rc <- data[Case_Type=="RC"]
  data$`Didnt_Vote` <- data$`Num_Eligible_Voters` - data$`Votes_For_Union` - data$`Votes_Against`
  
  
  return(data)
}

# showtext::showtext_auto()
# 
# font <- svglite::font_face("Crimson Pro",
#                   ttf="https://fonts.googleapis.com/css2?family=Crimson+Pro&display=swap")

create_state_plot <- function(state_abb = NULL, number=10, data=NULL,
                              state = state.name[state.abb == state_abb],
                              file_name = here("content", "data", "states", 
                                               state, paste0(state_abb, "_10.png"))){

  if( is.null(state_abb )){
    tmp_dt <- data[Unique==TRUE & Case_Type == "RC" & Status=="Closed"]
    file_name = here("content", "data", "national",  paste0("national", "_10.png"))
  } else {
    tmp_dt <- data[State==state_abb & Unique==TRUE & Case_Type == "RC" & Status=="Closed"]
    
  }


  tmp_dt$City <- tools::toTitleCase(tolower(tmp_dt$City))
  tmp_dt$`Case_Name` <-  str_to_title(tmp_dt$`Case_Name`) %>% str_trunc(width = 50)



  tmp_dt <- head(setorder(tmp_dt, -`Num_Eligible_Voters`), number)


  x_lim = max(tmp_dt$`Num_Eligible_Voters`)*1.2

  tmp_dt %>%
    melt(measure.vars=c("Votes_For_Union", "Votes_Against", "Didnt_Vote"))  %>%
    ggplot(aes(x=value, y=reorder(paste(`Plot_Labor_Union`, "at",
                                        `Case_Name`, "in", `City`, "on",
                                        `Tally_Date`),
                                  `Num_Eligible_Voters`),
               fill=variable)) +
    geom_col(position=position_stack(reverse=T), color="black") +
    geom_text(x=x_lim*.95,
              aes(label=scales::percent(Margin, accuracy =2),
                  color=Union_Cer)) +
    scale_x_continuous(limits=c(0, x_lim), labels = scales::label_comma()) +
    scale_y_discrete(label=scales::label_wrap(50)) +
    scale_fill_manual("", values=c("#009E73", "#56B4E9", "grey"),
                      labels=c("Votes for", "Votes Against", "Didn't Vote")) +
    guides(color=F) +
    theme_minimal(base_family = "Crimson Pro") +
    theme(legend.position = "bottom", legend.margin=margin(l=-100),
          text = element_text(size=15, lineheight=.8), 
          panel.grid=element_blank()) +
    scale_color_manual(values = c("#56B4E9", "#009E73")) +
    annotate("text", x=x_lim*.95, y=number + .5, label="Margin") +
    guides(alpha=F) +
    labs(y="", x="Votes", caption = "Includes only certification votes with a single union, data from NLRB")

  ggsave(file_name, height=10*log10(number), width=10, type = "cairo", 
         units="in", dpi=200)


}


create_state_time_plot <- function(state_abb = NULL, data=NULL,
                                   state = state.name[state.abb == state_abb],
                                   file_name= here("content", "data", "states", 
                                                   state, paste0(state_abb))) {


  if( is.null(state_abb )){
    tmp_dt <- data[Case_Type == "RC" &
                     Ballot_Type != "Revised Single Labor Org" &
                     !is.na(size) ]
    file_name = here("content", "data",  "national", "national")
  } else {
    tmp_dt <- data[State==state_abb  & Case_Type == "RC" &
                     Ballot_Type != "Revised Single Labor Org" &
                     !is.na(size) ]
    
    
  }



  ggplot(tmp_dt, aes(x=Tally_Quarter,
             fill=size)) +
  geom_bar(position=position_stack(reverse=T), color="black", size=.2, width=80) +
  scale_x_date(limits=c(as.Date("2008-01-01"), lubridate::today())) +
    scale_y_continuous(labels=scales::label_comma()) + 
  theme_minimal(base_family = "Crimson Pro") +
    scale_fill_colorblind("Size of Unit", drop=F) +
    theme(legend.position = "bottom", 
          text = element_text(size=15, lineheight=.3)) +
    labs(x="Quarter", y="Number of Units", 
       caption = "Includes only certification votes with a single union, data from NLRB")

  f <- paste0(file_name, "_hist_size.png")

  ggsave(f, height=8, width=10, type = "cairo", 
         units="in", dpi=200)

  ggplot(tmp_dt, aes(x=Tally_Quarter,
                     fill=Union_Cer, weight=Num_Eligible_Voters)) +
    geom_bar(position=position_stack(reverse=T), color="black", size=.2) +
    scale_x_date(limits=c(as.Date("2008-01-01"), lubridate::today())) +
    scale_y_continuous(labels=scales::label_comma()) + 
    theme_minimal(base_family = "Crimson Pro") +
    scale_fill_colorblind("Unionized?") +
    theme(legend.position = "bottom",
          text = element_text(size=15, lineheight=.3)) +
    labs(x="Quarter", y="Number of Voters", caption = "Includes only certification votes with a single union, data from NLRB")
  
  f <- paste0(file_name, "_hist_vic.png")

  ggsave(f, height=8, width=10, type = "cairo",
         units="in", dpi=200)


}



create_state_table_open <- function(state_abb = NULL, data=NULL, 
                                    state = state.name[state.abb == state_abb],
                                    file_name=here("content", "tables", state, "open.html")){

  if( is.null(state_abb )){
    tmp_dt <- data[Case_Type == "RC"]
    file_name = here("content", "tables", "national", "open.html")
  } else {
    tmp_dt <- data[State==state_abb & Case_Type == "RC"]
    
  }

  tmp_dt <- tmp_dt[Status=="Open"]
  tmp_dt <- setorder(tmp_dt, -`Date_Filed`)

  tmp_dt <- unique(tmp_dt)
  tmp_dt$Date_Filed <- as.character(tmp_dt$Date_Filed, "%b %d, %Y")
  tmp_dt$Tally_Date <- as.character(tmp_dt$Tally_Date, "%b %d, %Y")
  
  # tmp_dt$Case <- paste0("<a href='https://www.nlrb.gov/case/", tmp_dt$Case, "'>", tmp_dt$Case, "</a>")
  tab <- xtable(tmp_dt[,.(City, State, Case_Name, Labor_Union, Date_Filed, Tally_Date,
                          Tally_Type, Ballot_Type, Votes_For_Union, Votes_Against,
                            Num_Eligible_Voters, Case )])
  align(tab)[6:12] <- "c"
  # tab[1,] <- gsub("_", " ", (tab[1,]))
  # tab[1,9] <- "Union Certified?"
  # tab <- theme_basic(tab)

  if(!dir.exists(dirname(file_name))) dir.create(dirname(file_name))

  print(tab, file = file_name, type="html",
        html.table.attributes="class='open-cases'",
        comment = F,include.rownames = F,
        sanitize.colnames.function = function(x) gsub("_", " ", x))
  file_contents <- readLines(file_name)
  cases <- unique(tmp_dt$Case)
  for(case in cases){


    file_contents <- gsub(case,
                          paste0("<a href='https://www.nlrb.gov/case/", case, "'>", case, "</a>"),
                          file_contents)

  }
  write(file_contents, file = file_name)
}


create_state_page <- function(state_abb = "CA", 
                              state = state.name[state.abb == state_abb],
                              file_name = here("content", "data", "states", state, "_index.md") ){


  if(!dir.exists(dirname(file_name))) dir.create( dirname(file_name))


  tmp <-c(paste("## ", state),
          "",
          paste("### Number Employees in a Union Election by Outcome"),
          paste0("{{< image src=\"",state_abb, "_hist_vic.png\" >}}"),
          "",
          paste("### Number of Elections by Unit Size"),
          paste0("{{< image src=\"",state_abb, "_hist_size.png\" >}}"),
          "",
          paste("### Largest Private Union Elections"),
          paste0("{{< image src=\"",state_abb, "_10.png\" >}}"),
          "",
          "### Open Election Related Cases",
          paste0("{{< readtable table=\"/tables/", state, "/open.html\" >}}"),
          ""
          )

  writeLines(tmp, file_name)

}

# create_state_page()
# create_state_table_open(data=dt)
# create_state_time_plot(data=dt)
# create_state_plot(data=dt)
