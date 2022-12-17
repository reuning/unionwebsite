library(tidygeocoder)
library(here)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(rgeos)
library(tidyr)
library(dplyr)
library(gganimate)

source(here("gen", "scripts", "Scripts.R"))

## Based on https://r-spatial.org/r/2018/10/25/ggplot2-sf-3.html

dt <- fread(here("gen", "data", "recent_election_results.csv"))

open_dt <- dt[grepl("starbuck", `Case Name`, ignore.case = T)]

open_dt <- unique(open_dt, by = c("Case", "City", "State"))

cols <- c(as.vector(which(apply(open_dt, 2,
                                function(x)
                                  all(!is.na(x))))), 32:42)
coded_dt <-
  fread(here("gen", "data", "elections_with_location.csv"))
tmp <- unique(coded_dt[, .(Case, `Case Name`, lat, 
                           long, formatted_address, 
                           accuracy, accuracy_type, 
                           source, address_components.city, 
                           address_components.county, 
                           address_components.state, 
                           address_components.zip, 
                           address_components.country,
                           seached, 
                           lat_jit, 
                           long_jit)])
full_dt <- merge(open_dt, tmp, all.x = T, by=c("Case", "Case Name"))

if (any(is.na(full_dt$seached))) {
  to_code_dt <- full_dt[is.na(seached)]
  done_dt <- full_dt[!is.na(seached)]
  
  to_code_dt <- tidygeocoder::geocode(
    as.data.frame(to_code_dt[,1:Voters]),
    city = "City",
    state = "State",
    full_results = T,
    # return_type = "geographies",
    method = "geocodio"
  )
  
  try(to_code_dt$address_components.street <- NULL)
  try(to_code_dt$address_components.formatted_street <- NULL)
  try(to_code_dt$address_components.suffix <- NULL)
  try(to_code_dt$address_components.predirectional <- NULL)
  try(to_code_dt$address_components.postdirectional <- NULL)
  
  to_code_dt$seached <- 1
  to_code_dt$lat_jit <- to_code_dt$lat + runif(nrow(to_code_dt), -.5, .5)
  to_code_dt$long_jit <- to_code_dt$long + runif(nrow(to_code_dt), -.5, .5)
  
  
  full_dt <- rbind(done_dt, to_code_dt)
}

full_dt[,Election_Data :=ifelse(`Tally Date`=="", "No", "Yes") ]
full_dt[City == "Amherst" & State == "NY", lat_jit:=42.978333]
full_dt[City == "Amherst" & State == "NY", long_jit:=-78.8 ]


usa <- ne_states(returnclass = 'sf',
                 country = c("united states of america", 
                             "puerto rico"))
full_dt <- prep_data(full_dt)
# full_dt <- full_dt[grepl("RC", Case)]
# full_dt[, Date := as.Date(`Date Filed`, format = "%m/%d/%Y")]
full_dt <- full_dt[Case_Type == "RC"]
st_dt <- full_dt[grepl("starbuck", Case_Name, ignore.case = T)]

sub_dt <- st_dt[,.(Case, Date_Filed, Date_Closed, Reason_Closed, Tally_Date, Margin, long_jit, lat_jit, State)]

sub_dt <- melt(sub_dt, measure = patterns("Date"), na.rm = T )
sub_dt[variable=="Date_Filed", Reason_Closed:="Filed"]
sub_dt[Reason_Closed=="Withdrawal Non-adjusted", Reason_Closed:="Withdrawn/Dismissed" ]
sub_dt[Reason_Closed=="Dismissal Non-adjusted", Reason_Closed:="Withdrawn/Dismissed" ]
sub_dt[Reason_Closed=="Certific. of Representative", Reason_Closed:="Voted to Unionize" ]
sub_dt[Reason_Closed=="Certification of Results", Reason_Closed:="Voted Against" ]

sub_dt[Margin>.5 & variable =="Tally_Date", Reason_Closed:="Voted to Unionize" ]
sub_dt[Margin<=.5 & variable =="Tally_Date", Reason_Closed:="Voted Against" ]

sub_dt[Reason_Closed=="", Reason_Closed:="Filed"]

names(sub_dt)[2] <- "Status"
 


usa <- ne_states(returnclass = 'sf',
                 country = c("united states of america", 
                             "puerto rico"))

# ESRI:102003
crs_lower48 <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# EPSG:3338
crs_alaska <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
# ESRI:102007
crs_hawaii <- "+proj=aea +lat_1=8 +lat_2=18 +lat_0=13 +lon_0=-157 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

crs_pr <- "+proj=lcc +lat_1=18.43333333333333 +lat_2=18.03333333333333 +lat_0=17.83333333333333 +lon_0=-66.43333333333334 +x_0=200000 +y_0=200000 +ellps=GRS80 +units=m +no_defs"

site <- as_tibble(sub_dt) %>% group_by(Case) %>% 
  arrange(value) %>% 
  group_modify(~ add_row(.x, value=as.Date(Sys.Date()))) %>% 
  # ungroup() %>%
  complete(value=full_seq(value, 1)) %>% 
  #arrange(Case) %>% 
  fill(Status:variable) %>% 
  filter(!is.na(lat_jit))

site %>% filter(State=="HI") %>% View()


site <- st_as_sf(
  unique(site, by = c("Case", "Status")),
  coords = c("long_jit", "lat_jit"),
  crs = 4326,
  agr = "constant"
)

site <- site %>% st_transform(crs_lower48)
us_states <- as(usa, "sf") %>%
  st_transform(crs_lower48)# remove Puerto Rico

# now remove Alaska and Hawaii for lower 48, and calculate bounding box
us_lower48 <- dplyr::filter(us_states, !fips %in% c("US02", "US15", "72033"))
site_lower48 <- dplyr::filter(site, !State %in% c("HI", "AK", "PR"))
bb <- st_bbox(us_lower48)

place_geometry <- function(geometry, position, scale = 1, 
                           center=st_centroid(geometry)) {
  (geometry - center) * scale +
    st_sfc(st_point(position))
}

# move Alaska
us_alaska <- dplyr::filter(us_states, postal == "AK")
us_alaska2 <- st_transform(us_alaska, crs_alaska)
st_geometry(us_alaska2) <- place_geometry(
  st_geometry(us_alaska2),
  c(bb$xmin + 0.1*(bb$xmax - bb$xmin),
    bb$ymin - 0*(bb$ymax - bb$ymin)), 
  scale=.4
)
# we're cheating here by assigning a projection that isn't correct; we
# need to do this because the final compound map needs to have a single
# projection
st_crs(us_alaska2) <- crs_lower48

# move Hawaii
us_hawaii <- dplyr::filter(us_states, postal == "HI")
us_hawaii2 <- st_transform(us_hawaii, crs_hawaii)
hi_center <- st_centroid(st_geometry(us_hawaii2))
st_geometry(us_hawaii2) <- place_geometry(
  st_geometry(us_hawaii2),
  c(bb$xmin + 0.3*(bb$xmax - bb$xmin),
    bb$ymin + 0.*(bb$ymax - bb$ymin))
)
# cheating, as before
st_crs(us_hawaii2) <- crs_lower48

site_hi <- dplyr::filter(site, State == "HI") %>% 
  st_transform(crs_hawaii) 
st_geometry(site_hi) <-
  place_geometry(st_geometry(site_hi),
                 center = hi_center,
                 c(bb$xmin + 0.3*(bb$xmax - bb$xmin),
                   bb$ymin + 0.*(bb$ymax - bb$ymin)))
st_crs(site_hi) <- crs_lower48



## Puerto Rico
us_pr <- dplyr::filter(us_states, fips == "72033")
us_pr2 <- st_transform(us_pr, crs_pr)
st_geometry(us_pr2) <- place_geometry(
  st_geometry(us_pr2),
  c(bb$xmin + 0.5*(bb$xmax - bb$xmin),
    bb$ymin + -0.1*(bb$ymax - bb$ymin)), 
  scale=2
)
# cheating, as before
st_crs(us_pr2) <- crs_lower48


x3 <- rbind(us_lower48, us_alaska2, us_hawaii2, us_pr2)
site_map <- rbind(site_lower48, site_hi)

stats <- site %>% ungroup() %>% 
  group_by(value, Status) %>% summarize(n=n()) %>%
  select(value, Status, n)
stats$geometry <- NULL



site_map <- mutate(site_map, 
                   label=case_when(
                     Status == "Voted to Unionize" ~ "\uf6de", 
                     Status == "Filed" ~ "\uf7b6",
                     Status == "Withdrawn/Dismissed" ~ "\uf00d",
                     Status == "Voted Against"  ~ "\uf05e"
                   ))

site_map <- site_map %>% 
  arrange(case_when(
    Status == "Voted Against" ~ 4, 
    Status == "Voted to Unionize" ~ 3, 
    Status == "Filed" ~ 2,
    Status == "Withdrawn/Dismissed" ~ 1
  )) %>% ungroup() %>% 
  mutate(Status = ordered(Status, levels=(c("Voted to Unionize", 
                                               "Filed", "Withdrawn/Dismissed", 
                                               "Voted Against"))), 
         label= ordered(label, levels=(c("\uf6de", "\uf7b6", "\uf00d", "\uf05e") )))


sysfonts::font_add("font-awesome", regular = "~/Library/Fonts/Font Awesome 6 Free-Solid-900.otf")
library(showtext)
showtext::showtext_auto()
# tmp <- site_map[site_map$Case %in% names(which(table(site_map$Case) > 1)),]
# tmp <- tmp[tmp$State == "NY",]
mainland <- ggplot(data = x3) +
  geom_sf(fill = "white") + 
  theme_void() + 
  # theme_void(base_family = "Crimson Pro")  +
  geom_sf_text(
    family="font-awesome",
    data = site_map,
    # data = filter(site_map, value ==Sys.Date()),
    size = 2.5,
    aes(label=label,
        color=Status, 
        alpha=Status)
    # aes(fill = size, shape=Election_Data)
  ) + 
  scale_alpha_manual(values = (c("Voted to Unionize"= 1, 
                                "Filed"=.5, 
                                "Withdrawn/Dismissed"=.5, 
                                "Voted Against"=1))) + 
  scale_color_manual(values =(c("Voted to Unionize"= "black", 
                                "Filed"= "orangered3", 
                                "Withdrawn/Dismissed"="yellow3", 
                                "Voted Against"="blue"))) + 
  guides(color = 
           guide_legend(override.aes = 
                          list(label = 
                                 c("\uf6de", "\uf7b6", "\uf00d", 
                                   "\uf05e")
                              #color=c("black", "oranegred3", "yellow3", "blue"))), 
                          )),
         alpha="none") + 
  geom_text(x=2500000, y=200000, aes(label=paste(n)), 
            data=stats[stats$Status=="Voted to Unionize",]) +
  geom_text(x=2500000, y=-100000, aes(label=paste(n)), 
            data=stats[stats$Status=="Filed",]) + 
  geom_text(x=2500000, y=-425000, aes(label=paste(n)), 
             data=stats[stats$Status=="Withdrawn/Dismissed",]) + 
  geom_text(x=2500000, y=-750000, aes(label=paste(n)), 
            data=stats[stats$Status=="Voted Against",]) + 
  transition_manual(value)+
  # shadow_mark() +
  # enter_grow() +
  # exit_recolor(color = "orangered3") +
  # ggtitle("Union Filings at Starbucks", subtitle = "Date: {frame_time}") + 
  labs(title="Union Filings at Starbucks", 
       subtitle = "Date: {current_frame}",
    caption = "Location is approximate based on cities. Each is jittered so overlapping cases are obvious.\nNote: Decisions are not final in some elections as challenges are pending.") +
  # coord_sf(
  #   crs = st_crs(2163),
  #   xlim = c(-2500000, 2500000),
  #   ylim = c(-2300000,  730000)
  # ) +
  theme(plot.caption.position = "plot") +
  coord_sf(clip = 'off') + 
  NULL
p <- animate(mainland, 
             nframes=200,
             end_pause=20, 
             height=4, width=6, 
             units="in", res=150, 
             renderer=gifski_renderer())

anim_save("starbucks_filings.gif", animation=p)

