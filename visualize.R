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

plot(colwell_c ~ unbounded_seasonality, data = site_chr)

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

constancy <- dat_envpred %>% filter(component == "colwell_c") %>% print(n = 10)
contingency <- dat_envpred %>% filter(component == "colwell_m") %>% print(n = 10)

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

# SCRATCH #####

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
