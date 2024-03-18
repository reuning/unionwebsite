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



unions <- yaml::read_yaml(here::here("data", "union", "unions-template.yml"))

unions <- names(unions$items)
dict <- dict %>% mutate(International = gsub(" ", "_", International), 
                        LM_ID = as.numeric(gsub("-", "", LM_ID)))

all_member_out <- list()
for(ii in seq_along(unions)){
  if(!unions[ii] %in% tolower(names(out))) next
  union_proper_name <- names(out)[match(unions[ii], tolower(names(out)))]
  fnum <- filter(dict, International == union_proper_name) %>% pull(LM_ID)
  if(is.na(fnum)) next
  if(nrow(out[[union_proper_name]] ) != 0) {
    all_data <- filter(dt_out, F_NUM == fnum) %>% 
      select(YR_COVERED, MEMBERS) %>% 
      distinct() %>% 
      left_join(out[[union_proper_name]]) %>% 
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

  all_member_out[[unions[ii]]] <- all_data

  
}
# all_member_out    


json_version <- jsonlite::toJSON(all_member_out, pretty=T,  dataframe = 'rows')

jsonlite::write_json(json_version, here::here("gen", "data", "members.json"))
