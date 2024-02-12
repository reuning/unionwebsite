library(data.table)
library(magrittr)
library(stringr)
library(anytime)


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
