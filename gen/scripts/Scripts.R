library(data.table)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(magrittr)
library(stringr)
library(curl)
library(knitr)
options(knitr.kable.NA = "")

sysfonts::font_add_google("Crimson Pro")
state.name <- c(state.name, "Puerto Rico", "Guam", "US Virgin Islands")
state.abb <- c(state.abb, "PR", "GU", "VI")

clean_num <- function(x) scales::number(x, big.mark=",")

prep_data <- function(data=dt){
  dict <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTQj4UzxBycuPUVmIXM9RUnTWq0dwHICOk-phgwyfjqZAm8lsjl3D4JTLz73aa4dnOJ7gXmhehPGfu8/pub?gid=0&single=true&output=csv", 
                   encoding="UTF-8")


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
  cat("Renaming Data\n")
  names(data)[match(rename, names(data))] <- names(rename)


  ### Create New Variables
  cat("Creating New Variables\n")
  data[,Num_Eligible_Voters:=as.numeric(Num_Eligible_Voters)]
  data[,Votes_Against:=as.numeric(Votes_Against)]
  data[,Votes_For_Union:=as.numeric(Votes_For_Union)]
  data[,Total_Ballots_Counted:=as.numeric(Total_Ballots_Counted)]

  data[,Tally_Date:=as.Date(`Tally_Date`, format="%m/%d/%Y")]
  data[,Date_Filed:=as.Date(`Date_Filed`, format="%m/%d/%Y")]
  data[,Length:=Tally_Date-Date_Filed]
  data[,Tally_Quarter := as.Date(cut(Tally_Date, breaks = "quarter"))]

  data[,size:=cut(Num_Eligible_Voters, breaks = c(0, 5, 10, 25, 50, 100, 500, Inf), right = T,
                labels=c("<5", "6-10", "11-25", "26-50", "51-100", "101-500", "500>"), ordered_result = T)]


  ### Get most recent
  cat("Identifying duplicates\n")
  data <- data[order(-Tally_Date)]
  data$Unique <- !duplicated(data, by='Case')

  # data <- data[Status=="Closed"]
  # data <- data[`Reason_Closed` %in% c("Certific. of Representative", "Certification of Results")]
  cat("Dropping elections with multiple\n")
  data <- data[`Ballot_Type`%in% c("Single Labor Organization", "Revised Single Labor Org", "")]
  data[, Case_Type:=substr(Case, 4, 5)]

  cat("Filling in NAs with 0s\n")

  data[is.na(`Votes_Against`) & Election_Data=="Yes",`Votes_Against`:=0 ]
  data[is.na(`Votes_For_Union`) & Election_Data=="Yes",`Votes_For_Union`:=0 ]
  data[is.na(`Total_Ballots_Counted`) & Election_Data=="Yes",`Total_Ballots_Counted`:=0 ]
  data[is.na(`Num_Eligible_Voters`) & Election_Data=="Yes",`Num_Eligible_Voters`:=0 ]

  #cat("Fixing Union Names")
  data[,Labor_Union:=gsub("[[:space:]]", " ", Labor_Union)] ## cleaning in case
  data[,Plot_Labor_Union:=Labor_Union]
  data[,National:=character(nrow(data))]
  data[,National_Count:=0]
  data[,tmp_Labor_Union:=gsub("&", "and", Labor_Union)]
  
  doubles <- dict$National[dict$National!=""]
  names(doubles) <- dict$International[dict$National!=""]
  
  for(ii in 1:nrow(dict)){
    srch <- gsub("[[:space:]]|\u00A0", " ", dict$Name[ii]) ## annoyingly cleaning
    repl <- ifelse(dict$National[ii]== "",
                   dict$International[ii], dict$National[ii])

    # repl <- dict$International[ii]
    tmp <- 1*grepl(srch, data$tmp_Labor_Union, ignore.case = T)
    if(repl %in% names(doubles)) { 
      double_sel <- repl == names(doubles)
      tmp[(1*grepl(paste0(c(dict[dict$National %in% doubles[double_sel], "Name"],
                          dict[dict$National %in% doubles[double_sel], "National"]),
                          collapse = "|"), 
                   data$tmp_Labor_Union, 
                   ignore.case = T)) == tmp] <- 0 
    }
    
    data[tmp == 1 & National_Count==0, National := repl ]
    data[tmp == 1 & National_Count==0, National_Count := 1 ]

    data[tmp == 1 & National != repl, National_Count := National_Count + 1 ]

  }

  nationals <- unique(c(dict$International, dict$National))
  
  nationals <- nationals[nationals!=""]
  for(ii in 1:length(nationals)){
    srch <- nationals[ii]

    tmp <- 1*grepl(paste0("(\\W|\\b|\\d)",srch, "(\\W|\\b|\\d)"), 
                   data$tmp_Labor_Union, ignore.case = T)

    if(srch %in% names(doubles)) { 
      double_sel <- srch == names(doubles)
      
      tmp[(1*grepl(paste0(c(dict[dict$National %in% doubles[double_sel], "Name"],
                            dict[dict$National %in% doubles[double_sel], "National"]),
                          collapse = "|"), 
                   data$tmp_Labor_Union, 
                   ignore.case = T)) == tmp] <- 0 
    }
    
    data[tmp == 1 & National_Count==0, National := srch ]
    data[tmp == 1 & National_Count==0, National_Count := 1 ]

    data[tmp == 1 & National != srch, National_Count := National_Count + 1 ]
    # print(ii)
    # print(sum(data$National_Count))
  }

  # data$Labor_Union[(data$National_Count==0)]

  data$tmp_Labor_Union <- NULL


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

create_plot <- function(number=10, data=NULL,
                        file_name = NULL){

  if(is.null(file_name)) stop("Need file name")
  tmp_dt <- data[Unique==TRUE & Case_Type == "RC" & Status=="Closed"]



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
    labs(y="", x="Votes", caption = "Includes only certification votes with a single union, data from NLRB. https://unionelections.org")

  ggsave(file_name, height=10*log10(number), width=10, type = "cairo",
         units="in", dpi=200)


}


create_time_plot <- function(data=NULL,
                             file_name=NULL ) {


  if(is.null(file_name)) stop("Need file name")

  tmp_dt <- data[Case_Type == "RC" &
                   Ballot_Type != "Revised Single Labor Org" &
                   !is.na(size) ]





  ggplot(tmp_dt, aes(x=Tally_Quarter,
             fill=size)) +
  geom_bar(position=position_stack(reverse=T), color="black", size=.2, width=80) +
  scale_x_date(limits=c(as.Date("2008-01-01"), lubridate::ceiling_date(lubridate::today(), unit = "month"))) +
    scale_y_continuous(labels=scales::label_comma()) +
  theme_minimal(base_family = "Crimson Pro") +
    scale_fill_colorblind("Size of Unit", drop=F) +
    theme(legend.position = "bottom",
          text = element_text(size=15, lineheight=.3)) +
    labs(x="Quarter", y="Number of Units",
       caption = "Includes only certification votes with a single union, data from NLRB. https://unionelections.org")

  f <- paste0(file_name, "_hist_size.png")

  ggsave(f, height=8, width=10, type = "cairo",
         units="in", dpi=200)

  ggplot(tmp_dt, aes(x=Tally_Quarter,
                     fill=Union_Cer, weight=Num_Eligible_Voters)) +
    geom_bar(position=position_stack(reverse=T), color="black", size=.2, width=80) +
    scale_x_date(limits=c(as.Date("2008-01-01"), lubridate::ceiling_date(lubridate::today(), unit = "month"))) +
    scale_y_continuous(labels=scales::label_comma()) +
    theme_minimal(base_family = "Crimson Pro") +
    scale_fill_colorblind("Unionized?") +
    theme(legend.position = "bottom",
          text = element_text(size=15, lineheight=.3)) +
    labs(x="Quarter", y="Number of Voters", caption = "Includes only certification votes with a single union, data from NLRB. https://unionelections.org")

  f <- paste0(file_name, "_hist_vic.png")

  ggsave(f, height=8, width=10, type = "cairo",
         units="in", dpi=200)


}



create_table_open <- function(state_abb = NULL, data=NULL,
                                    file_name=NULL){

  if(is.null(file_name)) stop("Need file name")

  tmp_dt <- data[Case_Type == "RC" & Status=="Open"]
  tmp_dt <- setorder(tmp_dt, -`Date_Filed`)

  tmp_dt <- unique(tmp_dt)
  tmp_dt$Date_Filed <- as.character(tmp_dt$Date_Filed, "%b %d, %Y")
  tmp_dt$Tally_Date <- as.character(tmp_dt$Tally_Date, "%b %d, %Y")
  tmp_dt[,Ballot_Type:=ifelse(Ballot_Type == "Revised Single Labor Org", "Revised", "Initial")]
  tmp_dt[,Labor_Union:=Plot_Labor_Union ]
  # tmp_dt$Case <- paste0("<a href='https://www.nlrb.gov/case/", tmp_dt$Case, "'>", tmp_dt$Case, "</a>")
  
  tab <- tmp_dt[,.(City, State, Case_Name, Labor_Union, Date_Filed,  Tally_Date,
                   Tally_Type, Ballot_Type, Votes_For_Union, Votes_Against,
                   Num_Eligible_Voters, Case )]
  tab <- kable(
    tab, 
    format="html",
    col.names=gsub("_", " ", names(tab)), 
    align="llllcccccccc",
    digits=0, 
    table.attr="class='display summary-stats'"
  )
  
  
  if(!dir.exists(dirname(file_name))) dir.create(dirname(file_name))

  
  writeLines(tab, con = file_name)
  
  cases <- unique(tmp_dt$Case)
  for(case in cases){


    tab <- gsub(case,
                paste0("<a href='https://www.nlrb.gov/case/", case, "'>", case, "</a>"),
                tab)

  }
  write(tab, file = file_name)
}


create_page <- function(title = "California",
                              data=NULL,
                              file_name = NULL,
                              type="states",
                        weight=1){

  path <- gsub(" ", "_", title)
  if(!dir.exists(dirname(file_name))) dir.create( dirname(file_name))

  tmp_dt <- data[Case_Type == "RC"]



  tmp_dt[,Ballot_Type:=ifelse(Ballot_Type == "Revised Single Labor Org", "Revised", "Initial")]

  filed_last_year <- sum((tmp_dt$Unique == TRUE) & tmp_dt$Date_Filed > (Sys.Date() - 365))
  voted_last_year <- sum((tmp_dt$Unique == TRUE) & tmp_dt$Tally_Date > (Sys.Date() - 365), na.rm=T)
  if(is.na(voted_last_year)) voted_last_year <- 0
  cert_last_year <- sum((tmp_dt$Unique == TRUE) & tmp_dt$Tally_Date > (Sys.Date() - 365) &
                           tmp_dt$Union_Cer == "Yes", na.rm=T)


  tmp_dt <- tmp_dt[Status=="Open"]
  tmp_dt <- setorder(tmp_dt, -`Date_Filed`)
  tmp_dt <- unique(tmp_dt)
  open_cases <- nrow(tmp_dt)
  open_cases_waiting <- sum(tmp_dt$Election_Data == "No")


  if(type=="national"){
    description <- paste0("description: Data on recent union elections in the United States.")
    recent_stats <- sprintf("Excluding public employees, in the last year there have been %s union elections filed in the United States and %s union elections held. In %s of those elections a new unit was certified. There are currently %s open representation cases and %s of are still waiting to vote.",
                            clean_num(filed_last_year),
                            clean_num(voted_last_year), clean_num(cert_last_year),
                            clean_num(open_cases), clean_num(open_cases_waiting))
    open_table <-paste0("{{< readtable table=\"/tables/national/", path, "_open.html\" >}}")
    images <-paste0("images: [",
                    "'data/national/", path, "_hist_vic.png', ",
                    "'data/national/", path, "_hist_size.png', ",
                    "'data/national/", path, "_10.png']")
  } else if(type=="union") {
    description <- paste0("description: Data on recent union elections involving the ", title, ".")
    recent_stats <- sprintf("Excluding public employees, in the last year there have been %s union elections filed by the %s and %s union elections held. In %s of those elections a new unit was certified. There are currently %s open representation cases and %s of are still waiting to vote.",
                            clean_num(filed_last_year), title,
                            clean_num(voted_last_year), clean_num(cert_last_year),
                            clean_num(open_cases), clean_num(open_cases_waiting))
    open_table <-paste0("{{< readtable table=\"/tables/union/", path, "_open.html\" >}}")
    images <-paste0("images: [",
                    "'data/union/", path, "/", path, "_hist_vic.png', ",
                    "'data/union/", path, "/", path, "_hist_size.png', ",
                    "'data/union/", path, "/", path, "_10.png']")
  } else if(type=="states") {
    description <-paste0("description: Data on recent union elections in ", title, ".")
    recent_stats <- sprintf("Excluding public employees, in the last year there have been %s union elections filed in %s and %s union elections held. In %s of those elections a new unit was certified. There are currently %s open representation cases and %s of are still waiting to vote.",
                            clean_num(filed_last_year), title,
                            clean_num(voted_last_year), clean_num(cert_last_year),
                            clean_num(open_cases), clean_num(open_cases_waiting))
    open_table <-paste0("{{< readtable table=\"/tables/states/", path, "_open.html\" >}}")
    images <-paste0("images: [",
                    "'data/states/", path, "/", path, "_hist_vic.png', ",
                    "'data/states/", path, "/", path, "_hist_size.png', ",
                    "'data/states/", path, "/", path, "_10.png']")
  } else {
    stop("Type unknown")
  }

  tmp <-c("---",
          paste("title:", title),
          paste("pagetitle:", title, "Union Elections"),
          description,
          images,
          paste0('keywords: ["', title, ' union elections", "', title, ' unions","Union elections"]'),
          paste0("weight: ", weight),
          "---",
          paste("## ", title),
          "",
          recent_stats,
          "",
          paste("### Number Employees in a Union Election by Outcome"),
          paste0("{{< image src=\"",path, "_hist_vic.png\" >}}"),
          "",
          paste("### Number of Elections by Unit Size"),
          paste0("{{< image src=\"",path, "_hist_size.png\" >}}"),
          "",
          paste("### Largest Private Union Elections"),
          paste0("{{< image src=\"",path, "_10.png\" >}}"),
          "",
          "### Open Election Related Cases",
          open_table,
          ""
          )

  writeLines(tmp, file_name)

}

# create_state_page()
# create_state_table_open(data=dt)
# create_state_time_plot(data=dt)
# create_state_plot(data=dt)




create_front_page_table <- function(data=NULL,
                                    var="National",
                        weight=1, 
                        column_name = "Date_Filed", 
                        file_name = "all_ytd.html", 
                        all_groups = NULL){
  

  
  tmp_dt <- data[Case_Type == "RC"]
  tmp_dt <- setorder(tmp_dt, -`Date_Filed`)
  
  tmp_dt <- unique(tmp_dt)
  
  stat_tab <-  tmp_dt[  get(column_name) >(Sys.Date() - 365), .N, by= mget(c(var,"Status"))]
  stat_tab <- dcast(stat_tab, get(var) ~ Status, fill = 0)
  stat_tab <- rbind(stat_tab, data.table("var"=all_groups[!all_groups %in% stat_tab$var], 
             "Closed"=0, 
             "Open"=0))
  
  vic_tab <-  tmp_dt[  get(column_name) >(Sys.Date() - 365) & Status == "Closed",
                        .N, by= mget(c(var,"Union_Cer"))]
  vic_tab <- dcast(vic_tab, get(var) ~ Union_Cer, fill = 0)
  vic_tab[,Percentage:=scales::percent(Yes/(Yes+No))]
  vic_tab$No <- NULL
  
  emp_tab <-  tmp_dt[  get(column_name) >(Sys.Date() - 365) & Status == "Closed" 
                       & Union_Cer == "Yes",
                       .(median(Num_Eligible_Voters, na.rm=T),
                       sum(Num_Eligible_Voters, na.rm=T)), by= get(var)]
  names(emp_tab)[1:3] <- c("var","Median", "Total")
  full_tab <- merge(vic_tab, stat_tab, all=T)
  full_tab[is.na(Percentage), Percentage:= "-"]
  full_tab <- merge(full_tab, emp_tab, all=T)
  full_tab[is.na(full_tab)] <- 0
  
  # tmp_dt$Case <- paste0("<a href='https://www.nlrb.gov/case/", tmp_dt$Case, "'>", tmp_dt$Case, "</a>")
  tab <- kable(
    full_tab[,.(var, Open, Closed, Yes, Percentage, Median, Total)], 
    format="html",
    col.names=c(var, "Open Elections", "Closed Elections",
                "Union Certified", "Percent Certified", 
                "Median Successful BU", "Total Workers Unionized"), 
    align="lcccccc",
    digits=0, 
    table.attr="class='display summary-stats'"
  )

  if(var == "National"){
    file <- paste0("content/tables/union/", file_name)
  } else if(var == "State"){
    file <- paste0("content/tables/states/", file_name)
  } else {
    stop("'var' not recognized")
  }
  writeLines(tab, con = file)
  

  
  
}
