library(tidygeocoder)
library(here)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(rgeos)

source(here("gen", "scripts", "Scripts.R"))

## Based on https://r-spatial.org/r/2018/10/25/ggplot2-sf-3.html

dt <- fread(here("gen", "data", "recent_election_results.csv"))


open_dt <- dt[Status == "Open"]

open_dt <- unique(open_dt, by = c("Case", "City", "State"))

cols <- c(as.vector(which(apply(open_dt, 2,
                                function(x)
                                  all(!is.na(x))))), 32:42)
coded_dt <-
  fread(here("gen", "data", "elections_with_location.csv"))

full_dt <- merge(open_dt, unique(coded_dt[, c(3, 4, 33:44)]), all.x = T)

if (any(is.na(full_dt$seached))) {
  to_code_dt <- full_dt[is.na(seached)]
  done_dt <- full_dt[!is.na(seached)]
  
  to_code_dt <- tidygeocoder::geocode(
    as.data.frame(to_code_dt[,1:`Labor Union1`]),
    city = "City",
    state = "State",
    full_results = T,
    # return_type = "geographies",
    method = "geocodio"
  )
  
  try(to_code_dt$address_components.street <- NULL)
  try(to_code_dt$address_components.formatted_street <- NULL)
  try(to_code_dt$address_components.suffix <- NULL)
  
  to_code_dt$seached <- 1
  
  full_dt <- rbind(done_dt, to_code_dt)
  write.csv(
    full_dt,
    file = here("gen", "data", "elections_with_location.csv"),
    row.names = F
  )
}





usa <- ne_states(returnclass = 'sf',
                 country = c("united states of america", 
                             "puerto rico"))
full_dt <- prep_data(full_dt)
# full_dt <- full_dt[grepl("RC", Case)]
# full_dt[, Date := as.Date(`Date Filed`, format = "%m/%d/%Y")]
full_dt <- full_dt[Case_Type == "RC"]
full_dt <- unique(full_dt, by=c("Case", "City", "State"))
full_dt[is.na(size), size:="Unknown"]

site <- st_jitter(st_as_sf(
  unique(full_dt[!is.na(lat)], by = "Case"),
  coords = c("long", "lat"),
  crs = 4326,
  agr = "constant"
), .5)


mainland <- ggplot(data = usa) +
  geom_sf(fill = "white") + 
  theme_void(base_family = "Crimson Pro")  +
  geom_sf(
    data = site,
    # shape = 21,
    color = "gray",
    size = 3,
    aes(fill = size, shape=Election_Data)
  ) +
  scale_shape_manual("Already Voted?", values=c("Yes"=21, "No"=24)) + 
  scale_fill_colorblind("Unit Size",
    guide=guide_legend(override.aes = list(shape = 21)), 
    drop=F) + 
  # scale_fill_date(name = "Filing\nDate", 
  #                 low = "#0072B2",
  #                 high = "#D55E00") +
  ggtitle("Open Election Cases") + 
  labs(caption = "Location is approximate based on cities. Each is jittered so overlapping cases are obvious.") +
  # ggrepel::geom_text_repel(
  #   data = site,
  #   aes(label = Case, geometry = geometry),
  #   stat = "sf_coordinates",
  #   min.segment.length = 0,
  #   size=2
  #
  # ) +
  # theme(legend.position = "bottom") +
  coord_sf(
    crs = st_crs(2163),
    xlim = c(-2500000, 2500000),
    ylim = c(-2300000,  730000)
  ) +
  NULL

# mainland

alaska <- ggplot(data = usa) +
  geom_sf(fill = "white") + 
  theme_void(base_family = "Crimson Pro")  +
  geom_sf(
    data = site,
    # shape = 21,
    color = "gray",
    size = 3,
    aes(fill = size, shape=Election_Data)
  ) +
  scale_shape_manual("Already Voted?", values=c("Yes"=21, "No"=24)) + 
  scale_fill_colorblind("Unit Size",
                        guide=guide_legend(override.aes = list(shape = 21)), 
                        drop=F) + 
  # scale_fill_date(name = "Filing\nDate", 
  #                 low = "#0072B2",
  #                 high = "#D55E00") +
  guides(fill = F, shape=F) +
  coord_sf(
    crs = st_crs(3467),
    xlim = c(-2400000, 1600000),
    ylim = c(200000, 2500000),
    expand = FALSE,
    datum = NA
  )


puerto <- ggplot(data = usa) +
  geom_sf(fill = "white") + 
  theme_void(base_family = "Crimson Pro")  +
  geom_sf(
    data = site,
    # shape = 21,
    color = "gray",
    size = 3,
    aes(fill = size, shape=Election_Data)
  ) +
  scale_shape_manual("Already Voted?", values=c("Yes"=21, "No"=24)) + 
  scale_fill_colorblind("Unit Size",
                        guide=guide_legend(override.aes = list(shape = 21)), 
                        drop=F) + 
  # scale_fill_date(name = "Filing\nDate", 
  #                 low = "#0072B2",
  #                 high = "#D55E00") +
  guides(fill = F, shape=F) +
  coord_sf(
    crs = st_crs(3991),
    xlim = c(-50000, 1000000),
    ylim = c(0, 260000),
    expand = FALSE,
    datum = NA
  )

# puerto

hawaii  <- ggplot(data = usa) +
  geom_sf(fill = "white") +  
  theme_void(base_family = "Crimson Pro")  +
  geom_sf(
    data = site,
    # shape = 21,
    color = "gray",
    size = 3,
    aes(fill = size, shape=Election_Data)
  ) +
  scale_shape_manual("Already Voted?", values=c("Yes"=21, "No"=24)) + 
  scale_fill_colorblind("Unit Size",
                        guide=guide_legend(override.aes = list(shape = 21)), 
                        drop=F) + 
  # scale_fill_date(name = "Filing\nDate", 
  #                 low = "#0072B2",
  #                 high = "#D55E00") +
  guides(fill = F, shape=F) +
  coord_sf(
    crs = st_crs(4135),
    xlim = c(-161,-154),
    ylim = c(18, 23),
    expand = FALSE,
    datum = NA
  )

p <- mainland +
  annotation_custom(
    grob = ggplotGrob(alaska),
    xmin = -2750000,
    xmax = -2750000 + (1600000 - (-2400000)) / 2.5,
    ymin = -2450000,
    ymax = -2450000 + (2500000 - 200000) / 2.5
  ) +
  annotation_custom(
    grob = ggplotGrob(hawaii),
    xmin = -1250000,
    xmax = -1250000 + (-154 - (-161)) * 120000,
    ymin = -2450000,
    ymax = -2450000 + (23 - 18) * 120000
  ) +
  annotation_custom(
    grob = ggplotGrob(puerto),
    xmin = 750000,
    xmax = 750000 + (1000000 - (-50000)) / 2,
    ymin = -2250000,
    ymax = -2250000 + (260000 - 0) /2
  ) 


ggsave(here("content", "data",  "national", "map.png"),
       height=6, width=10, type = "cairo",
       units="in", dpi=200)
