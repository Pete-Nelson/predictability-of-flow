# If I calculate C, M & P sensu Colwell 1974 and seasonality as M/P, can I plot 
# predictability as a function of seasonality, like Peek et al 2021, fig 4, or  
# am I going to run afoul of autocorrelation? Note that Peek used wavelet power
# to estimating seasonality following Tonkin et al 2017.

set.seed(95060)
M <- round(runif(500), 3) # random numbers between 0 & 1, uniform distribution
P <- round(runif(500, M, 1), 3) # P is defined as the sum of M+C so P>M but less than 1
seasonality <- round(M / P, 3)

m <- lm(P ~ seasonality)
summary(m)

plot(seasonality, P)
abline(m)

# Interpretation of results:
# The slope (seasonality estimate: 0.2374) is positive and > 0 (p < 0.0001), so
# seasonality has a positive effect on P (to put things in an assbackwards sort
# of way). But if you think about it logically, that's what you ought to expect,
# regardless of how you're estimating P. Here, we used contingency (M) to 
# determine P (so there's probably be something wrong if there wasn't a stat-
# istical relationship), and P is always going be >= M (P = M + C), so you
# have to expect that cluster of results in the upper right. 

# I did this because I saw the Peek et al 2021 figure and thought I'd like to
# generate something similar with Central Valley tributary data. I haven't yet
# used wavelets to estimate P, so I wondered if I could just use Colwell's P
# but worried about autocorrelation. 

# I think it'd be okay to do this but not ideal. To publish the results, I'd 
# want to generate a quasi-independent estimate of P (independent from the 
# method used to determine seasonality). 

# Peek, R. A., S. M. O'Rourke, and M. R. Miller. 2021. Flow modification 
# associated with reduced genetic health of a river-breeding frog, Rana boylii. 
# Ecosphere 12(5):e03496. 10.1002/ecs2.3496

# Tonkin, Jonathan D., Michael T. Bogan, Núria Bonada, Blanca Rios-Touma, and 
# David A. Lytle. “Seasonality and Predictability Shape Temporal Species 
# Diversity.” Ecology 98, no. 5 (2017): 1201–16.
