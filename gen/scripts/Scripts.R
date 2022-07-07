library(data.table)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(magrittr)
library(stringr)
library(curl)
library(knitr)
library(anytime)
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

addFormats(c("%m/%e/%y", "%m/%d/%Y"))
options(knitr.kable.NA = "")

sysfonts::font_add_google("Crimson Pro")
state.name <- c(state.name, "Puerto Rico", "Guam", "US Virgin Islands")
state.abb <- c(state.abb, "PR", "GU", "VI")

clean_num <- function(x) scales::number(x, big.mark=",")
change_sentence <- function(new_val, prev_val){
  change <- (new_val - prev_val)/prev_val*100
  if(new_val > prev_val){
    sprintf("a %2.2f%% increase from the previous", change)
  } else if (new_val < prev_val){
    sprintf("a %2.2f%% decrease from the previous", change)
  } else {
    sprintf("the same as the previous", change)
  }
}


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
  
  data[,Tally_Date:=anydate(`Tally_Date`)]
  data[,Date_Filed:=anydate(`Date_Filed`)]
  data[,Date_Closed:=anydate(`Date_Closed`)]
  
  data[Tally_Date> as.Date("2080-01-01"), Tally_Date:=Tally_Date- lubridate::dyears(100)]
  data[Date_Filed> as.Date("2080-01-01"), Date_Filed:=Date_Filed- lubridate::dyears(100)]
  data[Date_Closed> as.Date("2080-01-01"), Date_Closed:=Date_Closed- lubridate::dyears(100)]
  
  
  data[,Length:=Tally_Date-Date_Filed]
  data[,Tally_Quarter := anydate(cut(Tally_Date, breaks = "quarter"))]
  data[,Filed_Quarter := anydate(cut(Date_Filed, breaks = "quarter"))]
  
  data[is.na(Num_Eligible_Voters),Num_Eligible_Voters:=`Employees on charge/petition`]
  data$`Employees on charge/petition` <- NULL
  data[,size:=cut(Num_Eligible_Voters, breaks = c(0, 5, 10, 25, 50, 100, 500, Inf), right = T,
                  labels=c("<5", "6-10", "11-25", "26-50", "51-100", "101-500", "500>"), ordered_result = T)]
  
  
  ### Get most recent
  cat("Identifying duplicates\n")
  data <- data[order(-Date_Filed, -Tally_Date, -Date_Closed, Num_Eligible_Voters)]
  data$Unique <- !duplicated(data, by=c("Case", "Unit ID"))
  to_drop <- !duplicated(data, by=c('Case', 'Tally_Date',
                                    'Tally_Type', 'Date_Filed',
                                    'Ballot_Type', 'Unit ID',
                                    'Total_Ballots_Counted',
                                    'Votes_Against', 'Votes_For_Union',
                                    'Status', 'Reason_Closed'))
  data <- data[to_drop]
  
  # data <- data[Status=="Closed"]
  # data <- data[`Reason_Closed` %in% c("Certific. of Representative", "Certification of Results")]
  cat("Dropping elections with multiple\n")
  data <- data[`Ballot_Type`%in% c("Single Labor Organization", "Revised Single Labor Org", "")]
  data[, Case_Type:=substr(Case, 4, 5)]
  
  cat("Filling in NAs with 0s\n")
  
  data[,Election_Data:=ifelse(is.na(Tally_Date), "No","Yes")]
  data <- data[!(Election_Data=="No" & Case %in% data[Election_Data=="Yes", Case]), ]
  
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
  
  
  tmp_dt <- tmp_dt[!is.na(Num_Eligible_Voters) & !is.na(Tally_Date)]
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
    guides(color="none") +
    theme_minimal(base_family = "Crimson Pro") +
    theme(legend.position = "bottom", legend.margin=margin(l=-100),
          text = element_text(size=15, lineheight=.8),
          panel.grid=element_blank()) +
    scale_color_manual(values = c("#56B4E9", "#009E73")) +
    annotate("text", x=x_lim*.95, y=number + .5, label="Margin") +
    guides(alpha="none") +
    labs(y="", x="Votes", caption = "Includes only certification votes with a single union, data from NLRB. https://unionelections.org")
  
  ggsave(file_name, height=10*log10(number), width=10, type = "cairo",
         units="in", dpi=200)
  
  
}

create_time_plots <- function(data=NULL,
                              file_name=NULL){
  
  if(is.null(file_name)) stop("Need file name")
  
  create_time_plot(data=data,
                   file_name=paste0(file_name, "_hist_elections.png"),
                   type="units",
                   fill="size",
                   date="Tally_Quarter",
                   title="Number of Union Elections in a Quarter by Unit Size")
  
  create_time_plot(data=data,
                   file_name=paste0(file_name, "_hist_filings.png"),
                   type="units",
                   fill="size",
                   date="Filed_Quarter",
                   title="Number of Union Filings in a Quarter by Unit Size")
  
  create_time_plot(data=data,
                   file_name=paste0(file_name, "_hist_vic.png"),
                   type="indiv",
                   fill="Union_Cer",
                   date="Tally_Quarter",
                   title="Number of Union Elections in a Quarter by Election Outcome")
  
  create_time_plot(data=data,
                   file_name=paste0(file_name, "_hist_vic_union.png"),
                   type="units",
                   fill="Union_Cer",
                   date="Tally_Quarter",
                   title="Number of Union Elections in a Quarter by Election Outcome")
}

create_time_plot <- function(data=NULL,
                             file_name=NULL,
                             type="units",
                             fill="Union_Cer",
                             date="Tally_Quarter",
                             title="Number of Union Elections in a Quarter by Outcome") {
  
  
  date <- sym(date)
  tmp_dt <- data[Case_Type == "RC" &
                   Ballot_Type != "Revised Single Labor Org" &
                   !is.na(size) & Unique ==TRUE ]
  
  tmp_dt <- tmp_dt[,Union_Cer:=forcats::fct_rev(Union_Cer)]
  
  curr_quarter <- lubridate::floor_date(lubridate::today(),
                                        unit = "quarter")
  
  
  if(type=="units"){
    y_max <- tmp_dt[get(date)==curr_quarter, .N,]
    y_lab <- "Number of Units"
    weight <- 1
  } else {
    y_max <- tmp_dt[get(date)==curr_quarter, sum(Num_Eligible_Voters),]
    y_lab <- "Number of Workers"
    weight <- sym("Num_Eligible_Voters")
  }
  
  fill_label <- ifelse(fill=="Union_Cer", "Vote for Union?", "Unit Size")
  fill <- sym(fill)
  date <- sym(date)## STOP MIXING TIDY AND DATATABLE
  
  ggplot(tmp_dt, aes(x=!!date,
                     fill=!!fill,
                     weight=!!weight)) +
    geom_bar(position=position_stack(reverse=T), color="black", size=.2, width=80) +
    scale_x_date(limits=c(as.Date("1999-01-01"),
                          lubridate::ceiling_date(lubridate::today(),
                                                  unit = "quarter"))) +
    scale_y_continuous(labels=scales::label_comma()) +
    theme_minimal(base_family = "Crimson Pro") +
    annotate("text",
             x=curr_quarter,
             y=ifelse(y_max == 0, 1, y_max*1.10),label="*") +
    scale_fill_viridis_d(fill_label,
                         direction = -1,
                         begin = .15,
                         end=.85) +
    theme(legend.position = "bottom",
          text = element_text(size=15, lineheight=.3)) +
    ggtitle(title) +
    labs(x="Quarter",
         y=y_lab,
         caption = "* Current quarter, not all data complete\n\nIncludes only certification votes with a single union, data from NLRB. https://unionelections.org")
  
  ggsave(file_name, height=8, width=10, type = "cairo",
         units="in", dpi=200)
  
  
}


create_time_table <- function(data=NULL,
                             file_name=NULL,
                             margin="Union_Cer",
                             date="Tally_Quarter") {
  
  
  date <- sym(date)
  tmp_dt <- data[Case_Type == "RC" &
                   Ballot_Type != "Revised Single Labor Org" &
                   !is.na(size) & Unique ==TRUE ]
  
  tmp_dt <- tmp_dt[,Union_Cer:=forcats::fct_rev(Union_Cer)]
  
  
  tab <- tmp_dt[,.("Units"=scales::comma(.N, 1), 
                   "Voters"=scales::comma(sum(Num_Eligible_Voters)),1),
                by=.("Date"=get(date), "margin"=get(margin))]
  
  tab <- dcast(tab, Date~margin, value.var=c("Voters", "Units"), fill=0)
  setorder(tab, -Date)
  tab <- tab[Date > as.Date("2000-01-01")]
  names(tab)[1] <- c("Quarter")
  names(tab) <- gsub("_Yes", " - For", names(tab))
  names(tab) <- gsub("_No", " - Against", names(tab))
  

  tab_out <- kable(
    tab,
    format="html",
    align="lcccc",
    table.attr="class='paged summary-stats'"
  )
  

  dates <- as.character(tab$Quarter)
  for(date in dates){
    
    ordering = paste0("data-order=\"",
                      as.numeric(anytime(date)),
                      "\"")
    tab_out <- gsub(paste0("<td style=\"text-align:left;\"> ",date," </td>"),
                    paste0("<td style=\"text-align:left;\" ", ordering,"> ",date," </td>"),
                    tab_out)
    
    
  }
  
  write(tab_out, file = file_name)
  
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
  
  
  tab_out <- kable(
    tab,
    format="html",
    col.names=gsub("_", " ", names(tab)),
    align="llllcccccccc",
    digits=0,
    table.attr="class='display summary-stats'"
  )
  
  
  
  if(!dir.exists(dirname(file_name))) dir.create(dirname(file_name))
  
  
  # writeLines(tab_out, con = file_name)
  
  cases <- unique(tmp_dt$Case)
  for(case in cases){
    
    
    tab_out <- gsub(case,
                    paste0("<a href='https://www.nlrb.gov/case/", case, "'>", case, "</a>"),
                    tab_out)
    
  }
  dates <- tab$Date_Filed
  for(date in dates){
    
    ordering = paste0("data-order=\"",
                      as.numeric(anytime(date)),
                      "\"")
    tab_out <- gsub(paste0("<td style=\"text-align:center;\"> ",date," </td>"),
                    paste0("<td style=\"text-align:center;\" ", ordering,"> ",date," </td>"),
                    tab_out)
    
    
  }
  
  dates <- tab$Tally_Date
  for(date in dates){
    
    ordering = paste0("data-order=\"",
                      as.numeric(anytime(date)),
                      "\"")
    tab_out <- gsub(paste0("<td style=\"text-align:center;\"> ",date," </td>"),
                    paste0("<td style=\"text-align:center;\" ", ordering,"> ",date," </td>"),
                    tab_out)
    
    
  }
  
  write(tab_out, file = file_name)
}


create_table_sb <- function(data=NULL,
                            file_name=NULL){
  
  if(is.null(file_name)) stop("Need file name")

  tmp_dt <- data[Case_Type == "RC"]
  tmp_dt <- setorder(tmp_dt, -`Date_Filed`)
  
  tmp_dt <- tmp_dt[Unique==TRUE,]
  tmp_dt <- unique(tmp_dt)
  tmp_dt$Date_Filed <- as.character(tmp_dt$Date_Filed, "%b %d, %Y")
  tmp_dt$Tally_Date <- as.character(tmp_dt$Tally_Date, "%b %d, %Y")
  tmp_dt[,Ballot_Type:=ifelse(Ballot_Type == "Revised Single Labor Org", "Revised", "Initial")]
  tmp_dt[,Labor_Union:=Plot_Labor_Union ]
  tmp_dt[,Status := ifelse(Status=="Open", "Open",
                           ifelse(Reason_Closed=="Certific. of Representative", "Unionized",
                                  ifelse(Reason_Closed=="Certification of Results", "Voted Failed",
                                         ifelse(Reason_Closed%in%c("Withdrawal Non-adjusted", "Withdrawl Adjusted"),
                                                "Withdrawn", "Other"))))]
  srch <- paste0("\\d+(?!(.*located))(.|\\n)+?(", paste(c(state.abb, state.name), collapse="|"),")")
  tmp_dt[,Address:=stringr::str_extract(`Voting Unit (Unit A)`, srch)]
  
  #### Summary statsitics ###
  tab <- tmp_dt[,.(.N, sum(Num_Eligible_Voters),
                   sum(Votes_For_Union, na.rm=T),
                   sum(Votes_Against, na.rm=T)), by=Status]
  
  colnames(tab) <- c("Status", "Number", "Total Employees",
                     "Votes for Union", "Votes Against")
  
  tab_out <- kable(
    tab,
    format="html",
    col.names=gsub("_", " ", names(tab)),
    align="lcccc",
    digits=0,
    table.attr="class='summary-stats center'"
  )
  
  if(!dir.exists(file_name)) dir.create(file_name)
  write(tab_out, file = paste0(file_name, "/starbucks_stats.html"))
  
  # tmp_dt$Case <- paste0("<a href='https://www.nlrb.gov/case/", tmp_dt$Case, "'>", tmp_dt$Case, "</a>")
  
  
  tab <- tmp_dt[,.(Date_Filed, City, State, Address, Status, Case_Name, Labor_Union,
                   Tally_Date, Ballot_Type, Votes_For_Union, Votes_Against,
                   Num_Eligible_Voters, Case )]
  
  
  tab_out <- kable(
    tab,
    format="html",
    col.names=gsub("_", " ", names(tab)),
    align="lllllcccccccc",
    digits=0,
    table.attr="class='display summary-stats'"
  )
  
  
  
  
  
  # writeLines(tab_out, con = file_name)
  
  cases <- unique(tmp_dt$Case)
  for(case in cases){
    
    
    tab_out <- gsub(case,
                    paste0("<a href='https://www.nlrb.gov/case/", case, "'>", case, "</a>"),
                    tab_out)
    
  }
  dates <- tab$Date_Filed
  for(date in dates){
    
    ordering = paste0("data-order=\"",
                      as.numeric(anytime(date)),
                      "\"")
    tab_out <- gsub(paste0("<td style=\"text-align:left;\"> ",date," </td>"),
                    paste0("<td style=\"text-align:left;\" ", ordering,"> ",date," </td>"),
                    tab_out)
    
    
  }
  
  dates <- tab$Tally_Date
  for(date in dates){
    
    ordering = paste0("data-order=\"",
                      as.numeric(anytime(date)),
                      "\"")
    tab_out <- gsub(paste0("<td style=\"text-align:center;\"> ",date," </td>"),
                    paste0("<td style=\"text-align:center;\" ", ordering,"> ",date," </td>"),
                    tab_out)
    
    
  }
  
  write(tab_out, file = paste0(file_name, "/starbucks_open.html"))
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
    table_nav <- paste0('"tables/national/', path, '_stats.html:Raw Data"')
    
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
    
    nav_membership <- paste0("{{< nav images=\"",path,
                             "_membership:Membership\" width=\"1000\" height=\"800\" tables=\"tables/union/", path, "_membership.html:Raw Data\">}}")
    
    table_nav <- paste0('"tables/union/', path, '_stats.html:Raw Data"')
    
    
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
    
    table_nav <- paste0('"/tables/states/', path, '_stats.html:Raw Data"')
  } else {
    stop("Type unknown")
  }
  
  nav_outcome <-  paste0("{{< nav images=\"",path,
                         "_hist_vic:By Number of Workers,", path,
                         "_hist_vic_union:By Number of Units\" width=\"1000\" height=\"800\" tables=",
                         table_nav,
                         ">}}")
  
  nav_hist <-  paste0("{{< nav images=\"",path,
                      "_hist_filings:By Filing Date,", path,
                      "_hist_elections:By Tally Date\" width=\"1000\" height=\"800\" >}}")
  
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
          paste("### Union Elections by Outcome"),
          nav_outcome,
          "",
          paste("### Timeline of Activity by Unit Size"),
          nav_hist,
          "",
          if(type=="union"){ 
             c(paste("### Membership"), 
               nav_membership,
               "")
          },
          paste("### Largest Private Union Elections"),
          paste0("{{< image src=\"",path, "_10.png\" width=\"1000\" height=\"1000\"  >}}"),
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
                                         "Open"=0), fill=T)
  
  vic_tab <-  tmp_dt[  get(column_name) >(Sys.Date() - 365) & Status == "Closed",
                       .N, by= mget(c(var,"Union_Cer"))]
  vic_tab <- dcast(vic_tab, get(var) ~ Union_Cer, fill = 0)
  vic_tab[,Percentage:=scales::percent(Yes/(Yes+No), accuracy = 0.01)]
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




report_page <- function(year, quarter=NULL, data, start_time, end_time,
                        time_delta=months(-3)){
  
  if(is.null(quarter)){
    time_frame <- paste0(year)
    time_title <- paste0(year)
    page_title <- paste0(year)
    file_page <- "_index.md"
    quarter <- 0
    period <- "year"
    weight <- year - 2009
  } else {
    time_frame <- paste0("the ", scales::ordinal(quarter), " quarter of ", year)
    time_title <- paste0(year, ", ", scales::ordinal(quarter), " Quarter")
    page_title <- paste0(scales::ordinal(quarter), " Quarter")
    file_page <- paste0(quarter, ".md")
    period <- "quarter"
    weight <- quarter
  }
  
  
  
  tmp_data <- data[Case_Type=="RC" & Unique==TRUE & Date_Filed > start_time &
                     Date_Filed <= end_time]
  
  tot_file <- nrow(tmp_data)
  tot_file_workers <- sum(tmp_data$Num_Eligible_Voters, na.rm=T)
  med_file_workers <- median(tmp_data$Num_Eligible_Voters, na.rm=T)
  
  tmp_data <- data[Case_Type=="RC" & Unique==TRUE & Date_Closed > start_time &
                     Date_Closed <= end_time]
  tot_closed <- nrow(tmp_data)
  tot_succes <- sum(tmp_data$Union_Cer=="Yes", na.rm=T)
  tot_closed_workers <- sum(tmp_data$Num_Eligible_Voters[tmp_data$Union_Cer=="Yes"], na.rm=T)
  med_closed_workers <- median(tmp_data$Num_Eligible_Voters[tmp_data$Union_Cer=="Yes"], na.rm=T)
  
  prev_start_time <- lubridate::add_with_rollback(start_time, time_delta)
  prev_end_time <- lubridate::add_with_rollback(end_time, time_delta)
  
  tmp_data <- data[Case_Type=="RC" & Unique==TRUE & Date_Filed > prev_start_time &
                     Date_Filed <= prev_end_time]
  
  prev_tot_file <- nrow(tmp_data)
  prev_tot_workers <- sum(tmp_data$Num_Eligible_Voters, na.rm=T)
  prev_med_workers <- median(tmp_data$Num_Eligible_Voters, na.rm=T)
  
  tmp_data <- data[Case_Type=="RC" & Unique==TRUE & Date_Closed > prev_start_time &
                     Date_Closed <= prev_end_time]
  prev_tot_closed <- nrow(tmp_data)
  prev_tot_succes <- sum(tmp_data$Union_Cer=="Yes", na.rm=T)
  prev_tot_closed_workers <- sum(tmp_data$Num_Eligible_Voters[tmp_data$Union_Cer=="Yes"], na.rm=T)
  prev_med_closed_workers <- median(tmp_data$Num_Eligible_Voters[tmp_data$Union_Cer=="Yes"], na.rm=T)
  
  
  
  filing <- sprintf("%s petitions for new units were filed, %s %s The median size was %1.0f with a total of %s workers across all units, %s %s",
                    scales::comma(tot_file, 1), change_sentence(tot_file, prev_tot_file),
                    period,
                    med_file_workers, scales::comma(tot_file_workers, 1),
                    change_sentence(tot_file_workers, prev_tot_workers), period)
  
  closed <- sprintf("%s petitions for new units were closed (this includes petitions where no election was ever held), with %2.2f%% closed with a certification order, creating %d total new units. This was %s %s in successful union certifications. Overall this represents approximately %s workers, which is %s %s The median bargaining unit has %1.0f workers.",
                    scales::comma(tot_closed, 1), tot_succes/tot_closed*100,
                    tot_succes, change_sentence(tot_succes, prev_tot_succes), period,
                    scales::comma(tot_closed_workers, 1),
                    change_sentence(tot_closed_workers, prev_tot_closed_workers), period,
                    med_closed_workers)
  
  filing <- paste0("In ", time_frame," ", filing)
  closed <- paste0("In ", time_frame," ", closed)
  
  page <- paste0("---\n",
                 ifelse(period=="year" ,"bookCollapseSection: true\n",""),
                 paste0("weight: ", weight,"\n"),
                 paste0("title: ", page_title, "\n"),
                 paste0("pagetitle: ", time_title, " Union Filing Report\n"),
                 paste0("description: Data on union election filings in ", time_frame, "\n"),
                 "keywords: union filings\n",
                 "---\n\n",
                 paste0("## ", time_title ," filings\n\n"),
                 paste0("This report details the number of filings and closed units in ", time_frame, ". As a reminder, we are only showing filings and closures related to single union elections (rare multi-union elections are excluded).\n\n"),
                 paste0("### Filings by Union\n"),
                 paste0(filing, "\n"),
                 paste0("{{< readtable table=\"/tables/reports/", year, "/", quarter, "union_filings.html\" >}}\n\n"),
                 paste0("### Closed Elections by Union\n"),
                 paste0(closed, "\n"),
                 paste0("{{< readtable table=\"/tables/reports/", year, "/", quarter, "union_closed.html\" >}}"))
  
  try(dir.create(here("content", "data", "reports", year)))
  writeLines(page, con=here("content", "data", "reports", year, file_page))
}

report_table_filed <- function(data,
                               start_time,
                               end_time,
                               file_name, time_delta=months(-3)){
  
  filed_table_current <- data[Case_Type=="RC" & Unique==TRUE & Date_Filed > start_time &
                                Date_Filed <= end_time,.("Total Units Filed"=.N,
                                                         "Total Workers"=sum(Num_Eligible_Voters),
                                                         "Median Unit Size" = median(Num_Eligible_Voters)),
                              by=National]
  
  prev_start_time <- lubridate::add_with_rollback(start_time, time_delta)
  prev_end_time <- lubridate::add_with_rollback(end_time, time_delta)
  
  filed_table_prev <- data[Case_Type=="RC" & Unique==TRUE & Date_Filed > prev_start_time &
                             Date_Filed <= prev_end_time,.("Prev_Units"=.N,
                                                           "Prev_Workers"=sum(Num_Eligible_Voters),
                                                           "Prev_Size" = median(Num_Eligible_Voters)),
                           by=National]
  
  filed_table <- merge(filed_table_current, filed_table_prev, all=T)
  filed_table[is.na(`Total Units Filed`), `Total Units Filed`:=0]
  filed_table[is.na(`Prev_Units`), Prev_Units:=0]
  
  
  
  filed_table[, "Change in Units" := scales::comma(`Total Units Filed` - Prev_Units, accuracy = 1)]
  filed_table[, "Change in Total Workers" := scales::percent((`Total Workers` - Prev_Workers)/Prev_Workers, big.mark=",",
                                                             accuracy = 1.11)]
  filed_table[, "Change in Median Unit" := scales::percent((`Median Unit Size` - Prev_Size)/Prev_Size, big.mark = ",",
                                                           accuracy = 1.11)]
  filed_table <- filed_table[,.(National, `Total Units Filed`, `Change in Units`,
                                `Total Workers`, `Change in Total Workers`,
                                `Median Unit Size`, `Change in Median Unit`)]
  
  
  filed_table[is.na(`Total Workers`), `Total Workers`:=0]
  filed_table[is.na(`Median Unit Size`), `Median Unit Size`:=0]
  
  filed_table[is.na(`Change in Total Workers`), `Change in Total Workers`:="-"]
  filed_table[is.na(`Change in Units`), `Change in Units`:="-"]
  filed_table[is.na(`Change in Median Unit`), `Change in Median Unit`:="-"]
  filed_table[, `Total Workers`:=scales::comma(`Total Workers`, accuracy=1)]
  # filed_table[, `Total Units Filed`:=scales::comma(`Total Units Filed`, accuracy=1)]
  filed_table[, `Median Unit Size`:=scales::comma(`Median Unit Size`, accuracy=.1)]
  
  
  
  tab <- kable(
    filed_table[order(-`Total Units Filed`)],
    format="html",
    align="lcccccc",
    table.attr="class='display summary-stats'"
  )
  
  
  if(!dir.exists(dirname(file_name))) dir.create(dirname(file_name))
  
  
  writeLines(tab, con = file_name)
  
}



report_table_closed <- function(data,
                                start_time,
                                end_time,
                                file_name, time_delta=months(-3)){
  
  closed_table_current <- data[Case_Type=="RC" & Unique==TRUE & Date_Closed > start_time &
                                 Date_Closed <= end_time,.("Total Units Closed"=.N),
                               by=.(National)]
  
  prev_start_time <- lubridate::add_with_rollback(start_time, time_delta)
  prev_end_time <- lubridate::add_with_rollback(end_time, time_delta)
  
  closed_table_prev <- data[Case_Type=="RC" & Unique==TRUE & Date_Closed > prev_start_time &
                              Date_Closed <= prev_end_time,.("Prev_Units"=.N),
                            by=National]
  
  succesful_table <- data[Case_Type=="RC" & Unique==TRUE & Date_Closed > start_time &
                            Date_Closed <= end_time,.("Units Won"=sum(Union_Cer=="Yes"),
                                                      "Total Workers Added"=sum(Num_Eligible_Voters *
                                                                                  (Union_Cer == "Yes")),
                                                      "Median Successful Unit"=median(Num_Eligible_Voters[Union_Cer == "Yes"])),
                          by=National]
  
  closed_table <- merge(closed_table_current, closed_table_prev,  all=T)
  closed_table <- merge(closed_table, succesful_table, all=T)
  closed_table[is.na(`Total Units Closed`), `Total Units Closed`:=0]
  closed_table[is.na(Prev_Units), Prev_Units:=0]
  
  
  
  closed_table[, "Change in Closed" := scales::comma(`Total Units Closed` - Prev_Units, accuracy = 1)]
  closed_table <- closed_table[,.(National, `Total Units Closed`, `Change in Closed`,
                                  `Units Won`, `Total Workers Added`,
                                  `Median Successful Unit`)]
  
  closed_table[is.na(`Units Won`), `Units Won`:=0]
  closed_table[is.na(`Total Workers Added`), `Total Workers Added`:=0]
  closed_table[is.na(`Median Successful Unit`), `Median Successful Unit`:=0]
  
  
  closed_table[is.na(`Change in Closed`), `Change in Closed`:="-"]
  closed_table[, `Total Workers Added`:=scales::comma(`Total Workers Added`, accuracy=1)]
  # closed_table[, `Total Units Filed`:=scales::comma(`Total Units Filed`, accuracy=1)]
  closed_table[, `Median Successful Unit`:=scales::comma(`Median Successful Unit`, accuracy=.1)]
  
  
  
  tab <- kable(
    closed_table[order(-`Total Units Closed`)],
    format="html",
    align="lccccc",
    table.attr="class='display summary-stats'"
  )
  
  
  if(!dir.exists(dirname(file_name))) dir.create(dirname(file_name))
  
  
  writeLines(tab, con = file_name)
  
}
