library(ggplot2)



# Sample data

data <- data.frame(x = seq(1, 10, by = 0.1), y =  x^2 + rnorm(length(x), mean = 0, sd = 5))



# Plot with LOESS smoothed line

ggplot(data, aes(x = x, y = y)) + 
  
  geom_point() + 
  
  geom_smooth(method = "loess") 

# 10 years, 12 months each
library(tidyverse)
library(lubridate)
tibble(month = rep(month.abb, 2),
       year = c(rep(2011, 12), rep(2012, 12)))

set.seed(20)
whitenoise <- ts(rnorm(273, mean = 0.18))

plot(whitenoise, main = "white noise")
abline(h = 0.18)

rep(month.abb, 3)
month <- format(ISOdate(2025, c(10:12, 1:9), 1), "%b")
rainfall <- c(0.95, 2.08, 3.25, 3.64, 3.47, 2.75, 1.15, 0.68, 0.21, 0.0, 0.05, 0.29)

precip_means <- 
  tibble(month = format(ISOdate(2025, c(10:12, 1:9), 1), "%b"),
       Sacramento = c(0.95, 2.08, 3.25, 3.64, 3.47, 2.75, 1.15, 0.68, 0.21, 0.0, 0.05, 0.29),
       Blythe = c(0.2, 0.23, 0.58, 0.56, 0.58, 0.4, 0.09, 0.04, 0.0, 0.22, 0.42, 0.45),
       Crescent_City = c(4.51, 10.18, 13.7, 10.82, 8.92, 9.11, 6.34, 3.54, 2.01, 0.35, 0.57, 1.19))

