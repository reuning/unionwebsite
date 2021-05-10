library(data.table)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(magrittr)
library(stringr)
library(xtable)





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
    scale_fill_manual("", values=c("#009E73", "#56B4E9", "grey"),
                      labels=c("Votes for", "Votes Against", "Didn't Vote")) +
    guides(color=F) +
    theme_minimal() +
    scale_color_manual(values = c("#56B4E9", "#009E73")) +
    annotate("text", x=x_lim, y=number + .5, label="Margin") +
    theme(legend.position = "bottom") +
    guides(alpha=F) +
    labs(y="", x="Votes", caption = "Includes only certification votes with a single union, data from NLRB")

  ggsave(file_name, height=10*log10(number), width=8)


}


create_state_time_plot <- function(state_abb = NULL, data=NULL,
                                   state = state.name[state.abb == state_abb],
                                   file_name= here("content", "data", "states", state, paste0(state_abb))) {


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
  theme_minimal() +
    scale_fill_colorblind("Size of Unit", drop=F) +
  theme(legend.position = "bottom") +
  labs(y="", x="Votes", caption = "Includes only certification votes with a single union, data from NLRB")

  f <- paste0(file_name, "_hist_size.png")

  ggsave(f, height=6, width=8)

  ggplot(tmp_dt, aes(x=Tally_Quarter,
                     fill=Union_Cer, weight=Num_Eligible_Voters)) +
    geom_bar(position=position_stack(reverse=T), color="black", size=.2) +
    scale_x_date(limits=c(as.Date("2008-01-01"), lubridate::today())) +
    theme_minimal() +
    scale_fill_colorblind("Unionized?") +
    theme(legend.position = "bottom") +
    labs(y="", x="Votes", caption = "Includes only certification votes with a single union, data from NLRB")
  
  f <- paste0(file_name, "_hist_vic.png")

  ggsave(f, height=6, width=8)


}



create_state_table_open <- function(state_abb = NULL, data=NULL, 
                                    state = state.name[state.abb == state_abb],
                                    file_name=here("content", "tables", state, "open.html")){

  if( is.null(state_abb )){
    tmp_dt <- data
    file_name = here("content", "tables", "national", "open.html")
  } else {
    tmp_dt <- data[State==state_abb]
    
    
  }

  tmp_dt <- tmp_dt[Status=="Open"]
  tmp_dt <- setorder(tmp_dt, -`Date_Filed`)

  tmp_dt <- unique(tmp_dt)
  tmp_dt$Date_Filed <- as.character(tmp_dt$Date_Filed, "%b %d, %Y")

  # tmp_dt$Case <- paste0("<a href='https://www.nlrb.gov/case/", tmp_dt$Case, "'>", tmp_dt$Case, "</a>")
  tab <- xtable(tmp_dt[,.(City, State, Case_Name, Labor_Union, Case_Type, Date_Filed,
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
