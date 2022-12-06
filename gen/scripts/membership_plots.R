setwd(here::here("gen/data/membership/"))
# library(data.table)
files <- dir()




library(ggplot2)
library(dplyr)
library(knitr)
library(readr)
library(tidyr)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

options(knitr.kable.NA = "")

sysfonts::font_add_google("Crimson Pro")
showtext::showtext_auto()
####
dt_out <- read_delim(grep("data_data", files, value = T),
                     delim = "|", guess_max = 5000, trim_ws = T)

dt_memb <- read_delim(grep("membership_data", files, value = T),
                     delim = "|", guess_max = 5000, trim_ws = T, quote = "")



dict <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTQj4UzxBycuPUVmIXM9RUnTWq0dwHICOk-phgwyfjqZAm8lsjl3D4JTLz73aa4dnOJ7gXmhehPGfu8/pub?gid=1590219215&single=true&output=csv",
                 encoding="UTF-8")
clean_categories <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTQj4UzxBycuPUVmIXM9RUnTWq0dwHICOk-phgwyfjqZAm8lsjl3D4JTLz73aa4dnOJ7gXmhehPGfu8/pub?gid=250121075&single=true&output=csv")


dict$id <- as.numeric(gsub("-", "", dict$LM_ID))
dt_memb$CATEGORY <-  tolower(dt_memb$CATEGORY)
names(clean_categories)[2:3] <- c("F_NUM", "CATEGORY")
clean_categories$F_NUM <- as.numeric(gsub("-", "", clean_categories$F_NUM))

dt_memb <- dt_memb %>% left_join(dt_out[,c("RPT_ID","YR_COVERED", "F_NUM")])  %>% 
  left_join(clean_categories)

all_unions <- dt_out %>% filter(F_NUM %in% dict$id) %>% pull(RPT_ID)

dt_memb %>% filter(RPT_ID %in% all_unions & is.na(Real_Category) & YR_COVERED>2015) %>% 
write_csv(here::here("gen", "data", "membership", "new_cats.csv"))


out <- list()
for(ii in 1:nrow(dict)){
  ids <- dt_out %>% filter(F_NUM == dict$id[ii]) %>% pull(RPT_ID)
  
  membs <- dt_memb %>% filter(RPT_ID %in% ids) %>% pivot_wider(id_cols = c(RPT_ID, YR_COVERED), 
                                                               names_from = Real_Category, 
                                                               values_from = NUMBER, 
                                                              values_fn=sum) %>% 
    relocate(YR_COVERED, .before = RPT_ID) %>% 
    mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
    select(where(~sum(.x)>0))
  
  out[[ii]] <- membs
  names(out)[ii] <- gsub(" " , "_", dict$International[ii])
}

unions <- dir(here::here("content", "data", "union"))

dict <- dict %>% mutate(International = gsub(" ", "_", International), 
                        LM_ID = as.numeric(gsub("-", "", LM_ID)))


for(ii in 1:length(unions)){
  if(!unions[ii] %in% names(out)) next
  fnum <- filter(dict, International == unions[ii]) %>% pull(LM_ID)
  if(is.na(fnum)) next
  if(nrow(out[[unions[ii]]] ) != 0) {
    all_data <- filter(dt_out, F_NUM == fnum) %>% 
      select(YR_COVERED, MEMBERS) %>% 
      distinct() %>% 
      left_join(out[[unions[ii]]]) %>% 
      pivot_longer(4:last_col(), names_to="Category") %>% 
      filter(Category !="NA")
  } else {
    all_data <- filter(dt_out, F_NUM == fnum) %>% 
      select(YR_COVERED, MEMBERS) 
  }
  
  if(nrow(all_data) == 0 ){
    all_data <- filter(dt_out, F_NUM == fnum) %>% 
      select(YR_COVERED, MEMBERS) 
  }

  detailed_data <- "Category" %in% names(all_data)
  ggplot(all_data, aes(x=YR_COVERED)) + 
    {if(detailed_data)geom_col(aes(y=value, fill=Category))} + 
    geom_line(aes(y=MEMBERS, color="Total Members")) +
    theme_minimal(base_family = "Crimson Pro") +
    ggtitle(paste("Membership Data for", unions[ii])) + 
    scale_y_continuous("", labels=scales::label_comma()) + 
    scale_color_manual("", values = c("Total Members"="black")) + 
    scale_fill_brewer(type = "qual", palette=3) +
    scale_x_continuous("Year", limits = c(2000, 2023)) + 
    theme(text = element_text(size=15, lineheight=.3)) +
    labs(caption = "Data from OLMS reports. Not all unions are required to report membership data. https://unionelections.org")
  
  file_name <- here::here("content", "data", "union", unions[ii],
                          paste0(unions[ii], "_membership.png"))
  ggsave(file_name, height=6, width=8, type = "cairo",
         units="in", dpi=120)
  if(detailed_data){
    all_data <- all_data %>% pivot_wider(names_from = Category, values_from = value ) %>%  
      select(-RPT_ID)
  }
  tab <- all_data %>% 
    rename("Year"="YR_COVERED", 
           "Total Members"="MEMBERS") %>% 
    mutate(across(2:last_col(), ~scales::comma(.x, 1)))
  
  tab_out <- kable(
    tab,
    format="html",
    align="l",
    table.attr="class='display summary-stats'"
  )
  
  file_name <- here::here("content", "tables", "union", 
                          paste0(unions[ii],"_membership.html"))
  write(tab_out, file = file_name)
  
  
}
    


