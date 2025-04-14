library(tidyverse)
library(zoo)

A <- tibble(
  date = seq(ymd("1927-10-01"), ymd("1927-10-07"), by = "days"),
  flow = sample.int(100, 7))
a <- zoo(A, index(date))

B <- tibble(
  date = seq(ymd("1927-10-01"), ymd("1927-10-07"), by = "days"),
  flow = c(48, 71, NA, 8, 92, 22, 49))
b <- zoo(B, index(date))

C <- tibble(
  date = seq(ymd("1927-10-03"), ymd("1927-10-05"), by = "days"),
  flow = sample.int(100, 3))
c <- zoo(C, index(date))

D <- tibble(
  date = ymd("1927-10-02", "1927-10-03", "1927-10-05", "1927-10-06", "1927-10-07"),
  flow = sample.int(100, 5))
d <- zoo(D, index(date))

L <- lst(A, B, C, D)

df <-
  L %>% 
  lapply(read.zoo) %>% # create list of zoo objects
  do.call(what = "merge") 
# apply 'fortify.zoo(name = "date")' to convert zoo to data frame

# trim NAs from one site
na.trim(df$D)
