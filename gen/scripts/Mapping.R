library(tidygeocoder)
library(data.table)
library(here)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(rgeos)
sysfonts::font_add_google("Crimson Pro")
## Based on https://r-spatial.org/r/2018/10/25/ggplot2-sf-3.html

dt <- fread(here("gen", "data", "recent_election_results.csv"))


open_dt <- dt[Status == "Open"]

open_dt <- unique(open_dt, by = c("Case", "City", "State"))

cols <- c(as.vector(which(apply(open_dt, 2,
                                function(x)
                                  all(!is.na(x))))), 32:42)
coded_dt <-
  fread(here("gen", "data", "elections_with_location.csv"))

full_dt <- merge(open_dt, unique(coded_dt[, c(3, 4, 32:43)]), all.x = T)

if (any(is.na(full_dt$seached))) {
  to_code_dt <- full_dt[is.na(seached)]
  done_dt <- full_dt[!is.na(seached)]
  
  to_code_dt <- tidygeocoder::geocode(
    as.data.frame(to_code_dt),
    city = "City",
    state = "State",
    full_results = T,
    # return_type = "geographies",
    method = "geocodio"
  )
  
  to_code_dt$seached <- 1
  
  full_dt <- rbind(done_dt, to_code_dt)
  write.csv(
    full_dt,
    file = here("gen", "data", "elections_with_location.csv"),
    row.names = F
  )
}





usa <- ne_states(returnclass = 'sf',
                 country = "united states of america")

full_dt[, Date := as.Date(`Date Filed`, format = "%m/%d/%Y")]
# full_dt <- unique(full_dt, by=c("Case", "City", "State"))

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
    shape = 21,
    color = "white",
    size = 3,
    aes(fill = Date)
  ) +
  scale_fill_date(name = "Filing\nDate", low = "#0072B2",
                  high = "#D55E00") +
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
    shape = 21,
    color = "white",
    size = 3,
    aes(fill = Date)
  ) +
  scale_fill_date(low = "#0072B2",
                  high = "#D55E00") +
  guides(fill = F) +
  coord_sf(
    crs = st_crs(3467),
    xlim = c(-2400000, 1600000),
    ylim = c(200000, 2500000),
    expand = FALSE,
    datum = NA
  )

hawaii  <- ggplot(data = usa) +
  geom_sf(fill = "white") +  
  theme_void(base_family = "Crimson Pro")  +
  geom_sf(
    data = site,
    shape = 21,
    color = "white",
    size = 3,
    aes(fill = Date)
  ) +
  scale_fill_date(low = "#0072B2",
                  high = "#D55E00") +
  guides(fill = F) +
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
  )

ggsave(here("content", "data",  "national", "national", "map.png"),
       height=8, width=10, type = "cairo",
       units="in", dpi=200)