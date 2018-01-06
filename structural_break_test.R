### STRUCTURAL BREAK TESTS

# 1. Testing for a structural break (with a priori knowledge)

  # Given a time series and an a priori point where a "change" happened, test the significance of it using a Chow Test
  # From Wikipedia, The Chow test, proposed by econometrician Gregory Chow in 1960, is a test of whether the coefficients   
  # in two linear regressions on different data sets are equal. In econometrics, it is most commonly used in time series analysis 
  # to test for the presence of a structural break at a period which can be assumed to be known a priori 
  # (for instance, a major historical event such as a war). 


  ## Load Libraries
  library(strucchange)
  
  ## Download dataset "deaths-from-homicides-and-suicid" with Deaths from homicides and suicides in Australia per 100.000 people
  ## https://datamarket.com/data/set/238x/deaths-from-homicides-and-suicides-in-australia
  
  ## Load dataset "deaths-from-homicides-and-suicid" from current working directory
  deaths <- read.csv(file="data/deaths-from-homicides-and-suicid.csv", header=TRUE, sep=",")[-91,]
  
  
  ## Create timeseries object for homicides by firearms 
  homicides_firearm <- ts(deaths$Firearm..Homicide, start = 1915, end = 2004)
  
  ## store the breakdates
  bp_ts <- breakpoints(homicides_firearm ~ 1)
  
  # this will give you the break dates and their confidence intervals
  summary(bp_ts) 
  
  # store the confidence intervals
  ci_ts <- confint(bp_ts)
  
  ## to plot the breakpoints with confidence intervals
  plot(homicides_firearm)
  lines(bp_ts)
  lines(ci_ts)
  
  #----
  
  ## Create timeseries object for suicides by firearms 
  suicides <- ts(deaths$Firearm..Suicide, start = 1915, end = 2004)
  
  ## store the breakdates
  bp_ts <- breakpoints(suicides ~ 1)
  
  # this will give you the break dates and their confidence intervals
  summary(bp_ts) 
  
  # store the confidence intervals
  ci_ts <- confint(bp_ts)
  
  ## to plot the breakpoints with confidence intervals
  plot(suicides)
  lines(bp_ts)
  lines(ci_ts)
  
  
  ## test the model null hypothesis that the average homicide by firearm rate remains constant
  ## over the years
  ## compute OLS-CUSUM fluctuation process
  homfa.cus <- efp(homicides_firearm ~ 1, type = "OLS-CUSUM")
  ## plot the process without boundaries
  plot(homfa.cus, alpha = 0.05, boundary = FALSE)
  ## add the boundaries in another colour
  bound <- boundary(homfa.cus, alpha = 0.05)
  lines(bound, col=4)
  lines(-bound, col=4)
  
  