library(DBI)
library(data.table)
setwd(here::here())
con <- dbConnect(RSQLite::SQLite(),"nlrb.sqlite") ## From https://github.com/labordata/nlrb-cats
dbListTables(con)

tmp <- as.data.table(dbGetQuery(con, 
           paste("SELECT r_case_number, case_name, status, date_filed,
                 closing_date, closing_method, election_city, election_state, 
                 unit_id, election_type, tally_type, tally_date, 
                 num_employees_eligible, num_void_ballots, num_votes_against, 
                 votes_for, labor_organization, sort_order, num_valid_votes,
                 runoff_required, num_challenges, challenges_determinative, 
                 majority_for", 
                 "FROM election_tally", 
                 "LEFT JOIN nlrb_case using(r_case_number)",
                 "LEFT JOIN election using(election_id)", 
                 "JOIN elect_votes_for using(r_case_number, unit_id, election_id, tally_id)", 
                 "LEFT JOIN closed_case using(r_case_number) WHERE (closed_case.r_case_number, closed_case.action_sequence) IN",
                  "( SELECT closed_case.r_case_number, MAX(closed_case.action_sequence)",
                    "FROM closed_case",
                    "GROUP BY closed_case.r_case_number",
                  ")")))

tmp <- (unique(tmp))

tmp2 <- dcast(tmp, formula = ... ~ sort_order, value.var=c("labor_organization" , "votes_for"))
tmp2$region <- NA
tmp2[,missing_labor := is.na(labor_organization_1)]
tmp2[missing_labor==TRUE, labor_organization_1 := labor_organization_2]
tmp2[missing_labor==TRUE, labor_organization_2 := labor_organization_3]
tmp2[missing_labor==TRUE, labor_organization_3 := labor_organization_4]

tmp2[missing_labor==TRUE, votes_for_1 := votes_for_2]
tmp2[missing_labor==TRUE, votes_for_2 := votes_for_3]
tmp2[missing_labor==TRUE, votes_for_3 := votes_for_4]

tmp2[,missing_labor := is.na(labor_organization_1)]
tmp2[missing_labor==TRUE, labor_organization_1 := labor_organization_2]
tmp2[missing_labor==TRUE, labor_organization_2 := labor_organization_3]
tmp2[missing_labor==TRUE, labor_organization_3 := labor_organization_4]

tmp2[missing_labor==TRUE, votes_for_1 := votes_for_2]
tmp2[missing_labor==TRUE, votes_for_2 := votes_for_3]
tmp2[missing_labor==TRUE, votes_for_3 := votes_for_4]

tmp2 <- tmp2[,.(region, r_case_number, case_name, status, date_filed,
        closing_date, closing_method, election_city, election_state, 
        unit_id, election_type, tally_type, tally_date, 
        num_employees_eligible, num_void_ballots, labor_organization_1,
        votes_for_1, labor_organization_2, votes_for_2, labor_organization_3, 
        votes_for_3, labor_organization_4, votes_for_4,
        num_votes_against, num_valid_votes,
        runoff_required, num_challenges, challenges_determinative, 
        majority_for)]

names(tmp2)
names(tmp2) <- c("Region", "Case Number", "Case Name" ,"Status",
                   "Date Filed", "Date Closed", "Reason Closed",
                   "City", "State", "Unit ID", "Ballot Type",
                   "Tally Type", "Tally Date", "No of Eligible Voters", 
                   "Void Ballots", "Votes for Labor Union1", 
                   "Labor Union2", "Votes for Labor Union2", 
                   "Labor Union3", "Votes for Labor Union3", "Votes Against",
                   "Total Ballots Counted", "Runoff Required","Challenged Ballots", 
                   "Challenges are Determinative","Union to Certify")

# tmp2$r_case_number[tmp2[,"1"] == 2]
# View(tmp[tmp$r_case_number=="24-RC-08570",])
# table(tmp2[,"2"])
# table(tmp2[,"3"])
# 
# tmp <- dbGetQuery(con, "SELECT * from elect_votes_for")
# tail(sort(table(tmp$election_id)))
# 
# df1 <- dbGetQuery(con, 
#                   paste("SELECT *", 
#                         "FROM election_tally"))
# 
# df2 <- dbGetQuery(con, 
#                   paste("SELECT *", 
#                         "FROM nlrb_case"))
# 
# closed_case
#   
# names(df1)
# names(df2)
# 
# all_df <- merge(df1, df2, by="r_case_number")
# 
# tail(sort(table(all_df$r_case_number)))
# names(all_df)

new_df <- fread("~/Dropbox/Projects/unionwebsite/gen/data/recent_election_results.csv")
names(new_df)
names(tmp2)

tmp2$r_case_number <- unlist(lapply(tmp2$r_case_number, function(x){
  tmp <- strsplit(x, "-")[[1]]
  paste0(tmp[1], "-", tmp[2], "-0", tmp[3])
  }))

# table(tmp2$tally_type)
# View(tmp2[tally_type=="ISeco" & r_case_number %in% new_df$Case])


tmp2$`Tally Type` <- ifelse(tmp2$election_type == "Initi", "Initial", 
                            ifelse(tmp2$election_type == "Runof", "Run-off", "Rerun"))
tmp2$election_type <- NULL

types <- c("Single Labor Organization", "Two Labor Organizations", "Three Labor Organizations", 
           "Four Labor Organizations", "Revised Single Labor Org", "Revised Two Labor Orgs", 
           "Revised Three Labor Orgs", "Revised Four Labor Orgs")

tmp2$`Ballot Type` <-  types[(4*(substr(tmp2$tally_type, 0, 1) == "R"))  + rowSums(!is.na(tmp2[,.(labor_organization_1, labor_organization_2, labor_organization_3, labor_organization_4)]))]
tmp2$`Ballot Type`[(substr(tmp2$r_case_number, 4, 5))=="UD"] <- "UD"
tmp2$tally_type <- NULL

closing <- c("Admin" = "Administrative Closing", 
             "AmCrt" = "Amended Certification", 
             "Dismi" = "Dismissal", 
             "Rep" = "Certific. of Representative", 
             "Resul" = "Certification of Results", 
             "Trans" = "Transferred",
             "With" = "Withdrawal")
tmp2$`Reason Closed` <-  closing[(tmp2$closing_method)]
tmp2[is.na(`Reason Closed`), `Reason Closed`:= ""] 
tmp2$closing_method <- NULL
tmp2$Status <- "Closed"
tmp2$status <- NULL 


tmp <- as.data.table(dbGetQuery(con, "SELECT r_case_number, unit_id, description_determined FROM bargaining_unit"))

tmp <- dcast(formula = r_case_number~unit_id, data=tmp, value.var="description_determined", fill = "")
names(tmp)[2:5] <- paste0("Voting Unit (Unit ", names(tmp)[2:5], ")")
tmp <- tmp[,1:5]

tmp$r_case_number <- unlist(lapply(tmp$r_case_number, function(x){
  tmp <- strsplit(x, "-")[[1]]
  paste0(tmp[1], "-", tmp[2], "-0", tmp[3])
}))

tmp2 <- merge(tmp2, tmp, by="r_case_number", all.x=T)

names(tmp2)[1:25] <- c("Case", "Region", "Case Name", 
                 "Date Filed", "Date Closed", "City", 
                 "State", "Unit ID", "Tally Date", 
                 "No of Eligible Voters", "Void Ballots",
                 "Labor Union1", "Votes for Labor Union1", 
                 "Labor Union2", "Votes for Labor Union2", 
                 "Labor Union3", "Votes for Labor Union3", 
                 "Labor Union4", "Votes for Labor Union4", 
                 "Votes Against", "Total Ballots Counted", 
                 "Runoff Required", "Challenged Ballots", 
                 "Challenges are Determinative", "Union to Certify")
tmp2$`Labor Union4` <- NULL
tmp2$`Votes for Labor Union4` <- NULL

tmp2$`Date Closed` <- as.character(lubridate::as_date(tmp2$`Date Closed`), "%m/%d/%Y")
tmp2$`Date Filed` <- as.character(lubridate::as_date(tmp2$`Date Filed`), "%m/%d/%Y")
tmp2$`Tally Date` <- as.character(lubridate::as_date(tmp2$`Tally Date`), "%m/%d/%Y")

tmp2$Election_Data <- "Yes"

tmp2[is.na(`Labor Union2`), `Labor Union2`:=""]
tmp2[is.na(`Labor Union3`), `Labor Union3`:=""]
tmp2[is.na(`Runoff Required`), `Runoff Required`:=""]
tmp2[is.na(`Challenges are Determinative`), `Challenges are Determinative`:=""]
tmp2[is.na(`Union to Certify`), `Union to Certify`:=""]
tmp2[is.na(`Voting Unit (Unit A)`), `Voting Unit (Unit A)`:=""]
tmp2[is.na(`Voting Unit (Unit B)`), `Voting Unit (Unit B)`:=""]
tmp2[is.na(`Voting Unit (Unit C)`), `Voting Unit (Unit C)`:=""]
tmp2[is.na(`Voting Unit (Unit D)`), `Voting Unit (Unit D)`:=""]

write.csv(tmp2, file = here::here("gen","data", "old_nlrb.csv"), row.names = F)

