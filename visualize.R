# Pete Nelson, PhD
# Department of Water Resources
# Predictability of CV Watersheds

# purpose: visualize environmental predictability

# created 17 December 2024

# set up -----

library(tidyverse)
library(ggplot2)
library(forcats)
library(tsibble)
library(fable)

# devtools::install_github("fishsciences/fishpals")
library(fishpals)

# using google maps
library(ggmap)
register_google(AIzaSyAqFBzCDuhOnDKzVnRqPjpRUpurvArqc_Y,
                account_type = "standard",
                write = TRUE)

# load data ----
# environmental predictability data for flow
## all CV sites ----
temp <- read_rds("results/flow_env_pred.rds") %>% 
  print(n = dim(.)[1])

flow_meta <- read_rds("results/flow_meta.rds") %>% 
  print()

# check match
left_join(temp, flow_meta, by = "site") %>% 
  select(site, location.x, location.y) %>% 
  print(n = Inf)

# combine env pred data with geographic data

site_chr <-
  left_join(temp,
            flow_meta,
            by = "site") %>% 
  select(-location.y, -station_nm) %>% # station_nm may be useful in other contexts
  mutate(location = location.x, 
         site = fct_inorder(site), # sets order of levels as given (N-S)
         .keep = "unused",
         .after = "site") %>% 
  relocate(c(dec_lat_va:drain_area_va), .after = location) %>% 
  print(n = dim(temp)[1])

write_rds(site_chr, "data/site_chr.rds")
  
## tributaries ----
# tribs w multiple sites, how does env pred change as you move downstream on a single trib?

battle <- read_rds("results/battle_flow_env_pred.rds") %>% 
  print()

## combine data ----
flow_env_pred <- read_rds("results/flow_env_pred.rds") 
flow_meta <- read_rds("results/flow_meta.rds")
flow <- 
  full_join(flow_env_pred,
            flow_meta,
            keep = FALSE) %>% 
  mutate(location = as_factor(location),
         lon = dec_long_va,
         lat = dec_lat_va,
         constancy = colwell_c/colwell_p,       # standardize C & P for pie charts
         contingency = colwell_m/colwell_p) %>%   # note: M/P = seasonality
  filter(site != "yreka" & site != "butte_forks" & site != "putah_s") %>% 
  relocate(station_nm) %>% 
  relocate(lon:lat, .after = site) %>% 
  relocate(constancy:contingency, .after = colwell_p)

write_rds(flow, "results/flow.rds")
flow <- read_rds("results/flow.rds")

# graphics -----
ggplot(site_chr, 
       aes(site, colwell_p, color = location)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

temp <- 
  site_chr %>% 
  select(!dec_lat_va:dec_long_va) %>% 
  pivot_longer(
    cols = env_col:drain_area_va,
    names_to = "variable",
    values_to = "values") %>% 
  select(site:colwell_p)

temp1 <- 
  site_chr %>% 
  select(!location:bounded_seasonality) %>% 
  pivot_longer(
    cols = env_col:colwell_p,
    names_to = "variable",
    values_to = "values")

temp1 %>% 
  filter(variable == "colwell_c" | variable == "colwell_m") %>%
  ggplot(
    aes(x = site, y = values, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1))

temp2 <- 
  site_chr %>% 
  select(!dec_lat_va:bounded_seasonality) %>% 
  pivot_longer(
    cols = env_col:colwell_p,
    names_to = "variable",
    values_to = "values")

temp2 %>% 
  filter(variable == "colwell_c" | variable == "colwell_m") %>% 
  ggplot(
    aes(x = site, y = values, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1)) +
  facet_wrap(vars(location))

## battle ----
temp <- 
  battle %>% 
  select(!dec_lat_va:dec_long_va) %>% 
  pivot_longer(
    cols = env_col:drain_area_va,
    names_to = "variable",
    values_to = "values") %>% 
  select(site:colwell_p)

temp1 <- 
  site_chr %>% 
  select(!location:bounded_seasonality) %>% 
  pivot_longer(
    cols = env_col:colwell_p,
    names_to = "variable",
    values_to = "values")



# factors: location, drainage area, altitude

## C vs M ----
plot(site_chr$colwell_c ~ site_chr$colwell_m)

ggplot(data = site_chr,
       aes(x = colwell_m, 
           y = colwell_c, 
           size = drain_area_va)) + # can't include key outlier
  stat_smooth(method = lm, linewidth = 1.5) +
  geom_point(shape = 19, na.rm = TRUE)

ggplot(data = site_chr,
       aes(x = colwell_m, y = colwell_c)) +
  geom_point(shape = 19) + stat_smooth(method = lm, linewidth = 1.5)

plot(data = site_chr, colwell_p ~ drain_area_va, pch = 16)
plot(data = site_chr, colwell_p ~ alt_va, pch = 16)

plot(colwell_c ~ unbounded_seasonality, data = site_chr[-23,]) # removed putah_s, the canal

## Putah Creek time series -----
# below Monticello Dam and Berryessa Lake
ggplot(
  read_rds("data/putah_n.rds"), 
  aes(date, flow)) + 
  geom_line() +
  ggtitle("Putah Creek", subtitle = "below Monticello Dam")

putah_dam <- read_rds("data/putah_n.rds") %>% 
  filter(date >= "1951-10-01" & date <= "1966-09-30")

ggplot(putah_dam, aes(date, flow)) + 
  geom_line() +
  xlab("") + ylab("")

# Putah South Canal time series
ggplot(
  read_rds("data/putah_s.rds"), 
  aes(date, flow)) + 
  geom_line() +
  ggtitle("Putah South Canal")

##

dat_envpred <-
  site_chr %>% 
  select(site, colwell_c, colwell_m) %>% 
  mutate(diff = colwell_c - colwell_m) %>% 
  pivot_longer(cols = c(colwell_c, colwell_m)) %>% 
  rename(component = name,
         predictability = value) %>% 
  mutate(direction = case_when(diff <0 ~ "C < M",
                               TRUE ~ "C > M")) %>% 
  within(., component[component == "colwell_c"] <- "C") %>% 
  within(., component[component == "colwell_m"] <- "M")

# alt method to replace component data
dat_envpred <-
  site_chr %>% 
  select(site, colwell_c, colwell_m) %>% 
  mutate(diff = colwell_c - colwell_m) %>% 
  pivot_longer(cols = c(colwell_c, colwell_m)) %>% 
  rename(component = name,
         predictability = value) %>% 
  mutate(direction = case_when(diff <0 ~ "C < M",
                               TRUE ~ "C > M"),
         component = replace(component,
                             component == "colwell_c", "C"),
         component = replace(component,
                             component == "colwell_m", "M"))

head(dat_envpred)

write_rds(dat_envpred, "data/dat_envpred.rds")

constancy <- dat_envpred %>% filter(component == "C") %>% print(n = 10)
contingency <- dat_envpred %>% filter(component == "M") %>% print(n = 10)

write_rds(constancy, "data/constancy.rds")
write_rds(contingency, "data/contingency.rds")

p <- ggplot(dat_envpred) +
  geom_line(aes(x = predictability, # env predictability, 0-1
                y = fct_rev(site), # sites from N to S
                color = direction), # tie color to diff btwn C&M +/-
            linewidth = 4.5, alpha = 0.4) +
  geom_point(aes(x = predictability, 
                 y = site, 
                 color = component),
             size = 5,
             show.legend = TRUE) +
  xlim(0, 1) +
  labs(x = "Environmental Predictability (C + M)",
       y = "Sites",
       title = "Central Valley Monthly Stream Flow")

p

p + scale_color_fishpals("adultchinook", # 'color' not 'fill'!
                         discrete = TRUE,
                         name = "legend") + # legend title
  theme_bw()

p + scale_color_viridis_d(name = "legend") +
  theme_bw()

# maps -----
library(sf)
# and maybe...
library(rnaturalearth)
library(rnaturalearthdata)
# library(rnaturalearthhires)
library(ggmap)
theme_set(theme_bw()) # recommended theme for maps

## sites -----
sites <- read_rds('results/flow_meta.rds') %>% 
  mutate(lat = dec_lat_va,
         lng = dec_long_va) %>% 
  filter(site != "yreka") %>% # yreka site is on Shasta R, trib of Klamath
  select(site, lat, lng)

# site coordinates
sites_sf <- st_as_sf(sites,                    # convert foreign object to sf object
                     coords = c("lng", "lat"), # where to find coordinate data
                     crs = 4326,               # which coord reference system to use
                     agr = "constant")         # character vector, see details '?st_sf'

## base map -----
base <- 
  ggplot(data = ne_states(returnclass = "sf")) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-125, -114),    # California longitudes +
           ylim = c(32, 43),        # California latitudes +
           expand = FALSE) +
  theme(panel.grid.major = element_line(color = gray(0.1),
                                        linetype = "dashed",
                                        linewidth = 0.1),
        panel.background = element_rect(fill = "aliceblue"))

## add sites -----

base + geom_sf(data = sites_sf) # places sites on world map

base +
  geom_sf(data = sites_sf,         # plot layer w sites
          size = 3,
          shape = 23, 
          fill = "darkred") +
  coord_sf(xlim = c(-125, -118),   # limit map to northern California
           ylim = c(37, 42.2),
           expand = FALSE) +
  annotate(geom = "text",          # add ocean label
           x = -122, y = 41.5, 
           label = "Northern California",
           fontface = "italic", color = "grey22", size = 6)


## add waterways -----

### NHD source -----
# "~/Documents/NHD_Major_Rivers_and_Creeks/NHD_Major_Rivers_and_Creeks.shp"
temp <- read_sf("~/Documents/NHD_Major_Rivers_and_Creeks/NHD_Major_Rivers_and_Creeks.shp")
names(temp)
typeof(temp)
head(temp)
dim(temp)
filter(temp, lengthkm >= 0.298)
str_subset(temp$gnis_name, "Eel River")
# str_match(temp, "Eel River")
str_detect(temp$gnis_name, "Eel River")
temp <- temp[1:20,]
str_subset(temp$gnis_name, "Eel River")

temp %>% 
  filter(ftype == 558) %>%  # ArtificialPath
  print(n=100)

filter(temp, fcode == 493)

### waterway selection ----

sjr <- filter(temp, gnis_id == "00273488") %>% mutate(sys_code = "sjr", .after = gnis_name)
yreka <- filter(temp, gnis_id == "00267231") %>% mutate(sys_code = "yreka", .after = gnis_name)
mccld <- filter(temp, gnis_id == "00266985") %>% mutate(sys_code = "mccld", .after = gnis_name)
sac <- filter(temp, gnis_id == "01654949") %>% mutate(sys_code = "sac", .after = gnis_name)
battle <- filter(temp, gnis_id == "00218740") %>% mutate(sys_code = "battle", .after = gnis_name)
clear <- filter(temp, gnis_id == "00258433") %>% mutate(sys_code = "clear", .after = gnis_name)
mill <- filter(temp, gnis_id == "01655086") %>% mutate(sys_code = "mill", .after = gnis_name)
deer <- filter(temp, gnis_id == "01655075") %>% mutate(sys_code = "deer", .after = gnis_name)
butte <- filter(temp, gnis_id == "00266522") %>% mutate(sys_code = "butte", .after = gnis_name)
feather <- filter(temp, gnis_id == "00223423") %>% mutate(sys_code = "feather", .after = gnis_name)
yuba <- filter(temp, gnis_id == "00238295") %>% mutate(sys_code = "yuba", .after = gnis_name)
putah <- filter(temp, gnis_id == "00234522") %>% mutate(sys_code = "putah", .after = gnis_name)
tuol <- filter(temp, gnis_id == "00255171") %>% mutate(sys_code = "tuol", .after = gnis_name)
merc <- filter(temp, gnis_id == "00272412") %>% mutate(sys_code = "merc", .after = gnis_name)

sac_sjr <- 
  bind_rows(sjr, sac) %>% 
  select(gnis_id:reachcode, wbarea_per, fdate)

sac_sjr_ws <- 
  bind_rows(sjr, yreka, mccld, sac, battle, clear, mill, deer, butte,
            feather, yuba, putah, tuol, merc) %>% 
  select(gnis_id:reachcode, wbarea_per, fdate)

write_rds(sac_sjr_ws, "data/sac_sjr_ws.rds")

sac_sjr_ws <- 
  read_rds("data/sac_sjr_ws.rds")

(test <- 
    filter(temp, gnis_name == "Feather River") %>% 
    select(gnis_id:reachcode) %>% 
    arrange(gnis_id))

test %>% group_by(gnis_id) %>% 
  summarise(length = sum(lengthkm)) %>% 
  arrange(desc(length)) %>% print(n = Inf)

(test <- 
    temp %>% 
    filter(grepl("Clear", gnis_name)) %>% 
    select(gnis_id:reachcode) %>% 
    arrange(gnis_id))

## rivers + sites ----
base +
  geom_sf(data = sac_sjr_ws,
          color = "blue") +
  geom_sf(data = sites_sf,    
          size = 2,
          shape = 23, 
          fill = "red") +
  coord_sf(xlim = c(-125, -119),  
           ylim = c(36.5, 42.2),
           expand = FALSE) +
  annotate(geom = "text",
           x = -122.87, y = 40,
           label = "Sacramento",
           fontface = "italic", color = "darkblue", size = 6) + 
  annotate(geom = "text",
           x = -122.5, y = 39.8,
           label = "River",
           fontface = "italic", color = "darkblue", size = 6) + 
  annotate(geom = "text",
           x = -121.62, y = 37.2,
           label = "San Joaquin",
           fontface = "italic", color = "darkblue", size = 6) + 
  annotate(geom = "text",
           x = -121.09, y = 37.0,
           label = "River",
           fontface = "italic", color = "darkblue", size = 6) +   
  annotate(geom = "text",       
           x = -124, y = 37.5, 
           label = "Pacific Ocean",
           color = "grey22", size = 6)

watershed <-
  base +
  geom_sf(data = sac_sjr_ws,
          color = "blue") +
  coord_sf(xlim = c(-125, -119),  
           ylim = c(36.5, 42.2),
           expand = FALSE)

watershed +
  geom_point(aes(x = lon, y = lat, 
                 size = colwell_p), # scale point size to predictability (Colwell's P)
             data = flow,
             alpha = 0.3, color = "red")

## pie charts -----
library(ggplot2)
library(maps)
library(PieGlyph)

## mapping pies -----
### upper Sac R ----
(upper_sac <-
  base +
  geom_sf(data = sac_sjr_ws,
          color = "blue") +
  coord_sf(xlim = c(-123, -121),  
           ylim = c(40, 41.3),
           expand = FALSE) +
  ggtitle("Upper Sacramento River Watershed"))

(upper_sac +
  geom_pie_glyph(aes(x = lon, y = lat,
                     radius = colwell_p), # size of pies
                 data = filter(flow, lat > 40.2),
                 slices = c("constancy", "contingency"),  # standardized (C/P, M/P)
                 color = "grey22",
                 alpha = 0.8) +
  labs(fill = "Flow", radius = "Predictability") +
  scale_fill_manual(values = c("#39BEB1", "#ACA4E2")))

### central Sac R ----
(central_sac <-
   base +
   geom_sf(data = sac_sjr_ws,
           color = "blue") +
   coord_sf(xlim = c(-122.7, -120.5),  
            ylim = c(38.6, 40.5),
            expand = FALSE) +
   ggtitle("Central Sacramento River Watershed"))

(central_sac +
    geom_pie_glyph(aes(x = lon, y = lat,
                       radius = colwell_p), # size of pies
                   data = filter(flow, lat < 40.2),
                   slices = c("constancy", "contingency"),  # standardized (C/P, M/P)
                   color = "grey22",
                   alpha = 0.8) +
    labs(fill = "Flow", radius = "Predictability") +
    scale_fill_manual(values = c("#39BEB1", "#ACA4E2")))

### Delta + San Joaquin River Watershed ----
(delta <-
   base +
   geom_sf(data = sac_sjr_ws,
           color = "blue") +
   coord_sf(xlim = c(-123, -119),  
            ylim = c(36.5, 39),
            expand = FALSE) +
   ggtitle("Delta + San Joaquin River Watershed"))

(delta +
    geom_pie_glyph(aes(x = lon, y = lat,
                       radius = colwell_p), # size of pies
                   data = filter(flow, lat < 39),
                   slices = c("constancy", "contingency"),  # standardized (C/P, M/P)
                   color = "grey22",
                   alpha = 0.8) +
    labs(fill = "Flow", radius = "Predictability") +
    scale_fill_manual(values = c("#39BEB1", "#ACA4E2")))

# SCRATCH -----
# from PieGlyph
# https://cran.r-project.org/web/packages/PieGlyph/vignettes/PieGlyph.html

library(tidyverse)
library(ggplot2)
library(PieGlyph)
library(ggiraph)

### simulate raw data -----
set.seed(123)
plot_data <- data.frame(response = rnorm(30, 100, 30),
                        system = 1:30,
                        group = sample(size = 30, x = c('G1', 'G2', 'G3'), replace = T),
                        A = round(runif(30, 3, 9), 2),
                        B = round(runif(30, 1, 5), 2),
                        C = round(runif(30, 3, 7), 2),
                        D = round(runif(30, 1, 9), 2))
# The data has 30 observations and seven columns. response is a continuous variable 
# measuring system output while system describes the 30 individual systems of 
# interest. Each system is placed in one of three groups shown in group. Columns 
# A, B, C, and D measure system attributes.

### basic plot of pies -----
ggplot(data = plot_data, aes(x = system, y = response))+
  geom_pie_glyph(slices = c('A', 'B', 'C', 'D'))+
  theme_classic()

### pie radius & border -----
ggplot(data = plot_data, aes(x = system, y = response)) +
  geom_pie_glyph(slices = 4:7, color = "black", radius = 0.5) +
  theme_classic()

### radius mapped to variable ----
p <- ggplot(data = plot_data, aes(x = system, y = response)) +
  geom_pie_glyph(aes(radius = group),
                 slices = c("A", "B", "C", "D"),
                 color = "black") +
  theme_classic()

p

### adjust radius for groups ----
p <- p + scale_radius_manual(values = c(0.25, 0.5, 0.75), unit = "cm")

p

### change category colors -----
library(colorspace)
p + scale_fill_manual(values = c("#E495E5", "#ABB065", "#39BEB1", "#ACA4E2"))
p + scale_fill_manual(values = rainbow_hcl(4))
p + scale_fill_manual(values = c("red", "green", "blue", "orange"))

### example dataset
library(scatterpie)

set.seed(123)
long <- rnorm(50, sd = 100)
lat <- rnorm(50, sd = 50)
d <- data.frame(long = long, lat = lat)
d <- with(d, d[abs(long) < 150 & abs(lat) < 70, ]) # removed out of bounds or aberrant lat/long
n <- nrow(d)
d$region <- factor(1:n) # add region as factors 1:n
d$A <- abs(rnorm(n, sd = 1)) # adding random numbers (data) for variables A:D
d$B <- abs(rnorm(n, sd = 2))
d$C <- abs(rnorm(n, sd = 3))
d$D <- abs(rnorm(n, sd = 4))
head(d)
d[1, 4:7] <- d[1, 4:7] * 3 # triple A:D for region 1
head(d)
d1 <- d[1:6,]
d1$radius <- c(4, 8, 8, 3, 11, 6)

# plot
world <- map_data('world')
p <- ggplot(world,
            aes(long, lat)) +
  geom_map(map = world,
           aes(map_id = region),
           fill = NA,
           color = "black") +
  coord_quickmap()

p + geom_scatterpie(aes(x = long, y = lat,
                        group = region,
                        r = radius), # size of pies based on 'radius'
                    data = d1,
                    cols = LETTERS[1:4],
                    color = NA,
                    alpha = 0.8) +
  geom_scatterpie_legend(d1$radius, x = -160, y = -55)

p + geom_scatterpie(aes(x = long, y = lat,
                        group = region,
                        r = radius), # size of pies based on 'radius'
                    data = d1,
                    cols = c("A", "B", "C", "D"),
                    color = NA,
                    alpha = 0.8) +
  geom_scatterpie_legend(d1$radius, x = -160, y = -55)

# SCRATCH #####
# find all rows referring to "Butte Creek"
tbl <- tibble(name = c("N Fork Butte Creek", "M Fork Butte Creek", "Butte Creek", "Middle Fork Salmon River", "S Fork Eel River"),
              length = c(20, 16, 42, 33, 28))

str_subset(tbl$name, "Creek")

tbl[1:3,]

tbl %>% filter(str_detect(name, "Butte"))

# scaling points on a map with data
https://stackoverflow.com/questions/51548812/ggmap-package-making-point-sizes-larger-when-scaled-by-a-variable-value

base +
  geom_sf(data = sac_sjr) +
  geom_sf(data = sites_sf,         # plot layer w sites
          size = 2,
          shape = 23, 
          fill = "darkred") +
  coord_sf(xlim = c(-125, -118),   # limit map to northern California
           ylim = c(36.5, 42.2),
           expand = FALSE)

p <- ggplot(dat_envpred) +
  geom_line(aes(x = predictability, # env predictability, 0-1
                y = fct_rev(site), # sites from N to S
                color = direction), # tie color to diff btwn C&M +/-
            linewidth = 4.5, alpha = 0.4) +
  geom_point(aes(x = predictability, y = site, color = component),
             size = 5,
             show.legend = TRUE) +
  xlim(0, 1) +
  labs(x = "Environmental Predictability",
       y = "Central Valley Sites",
       title = "Predictability of Monthly Stream Flow")

p


head(putah_n)
head(putah_s)
temp <- putah_n %>% filter(date >= "1994-10-01")
head(temp)
putah <-
  left_join(temp, putah_s, by = "date") %>% 
  mutate(north = flow.x,
         south = flow.y) %>% 
  select(date, north, south) %>% 
  as_tsibble()
head(putah)

autoplot(putah)

putah %>% autoplot(vars(north, south))

library(tsibbledata)
library(tsibble)

tsibbledata::gafa_stock %>%
  autoplot(vars(Close, log(Close)))

site_chr %>% 
  select(!dec_lat_va:dec_long_va) %>% 
  pivot_longer(
    cols = env_col:drain_area_va,
    names_to = "variable",
    values_to = "values") %>% 
  select(!series_n:bounded_seasonality)

hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')
head(hbcu_all)
hbcu_all %>% 
  filter(Year >= 1990) %>% 
  select(Year, Males, Females) %>% 
  mutate(diff = Females - Males) %>% 
  pivot_longer(cols = c(Males, Females)) %>% 
  rename(Gender = name,
         Enrollments = value) -> dat_gender
head(dat_gender)
Males <- dat_gender %>% filter(Gender == "Males")
Females <- dat_gender %>% filter(Gender == "Females")
head(Females)

p <- ggplot(dat_gender) +
  geom_segment(data = Males,
               aes(x = Enrollments, y = Year,
                   yend = Females$Year, xend = Females$Enrollments),
               color = "#aeb6bf",
               linewidth = 4.5,
               alpha = 0.5) +
  geom_point(aes(x = Enrollments, y = Year, color = Gender),
             size = 4,
             show.legend = TRUE) +
  ggtitle("Enrollment Trends at HBCUs")
