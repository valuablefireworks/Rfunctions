##Loading in Usefull Tools
##Load in gss & wb before running
library(DescTools)
library(moments)
library(qualvar)

## load my functions
sandy <- function (z,x) {

  if (z == 'Nominal') {
    ##Make sure im being appropriate##
    mode <- c(1)
    modef <- data.frame(mode)
    modef$mode <- DescTools::Mode(x, na.rm = TRUE)
    gf <- modef
    return(gf)
  }

  if (z == 'Ordinal') {
    mode <- c(1)
    modef <- data.frame(mode)
    ##Now we throw in the median
    modef$mode <- DescTools::Mode(x, na.rm = TRUE)
    modef$median <- DescTools::Median(x, na.rm = TRUE)
    gf <- modef
    return(gf)
  }

  if (z == 'IR') {
    median <- c(1)
    medianf <- data.frame(median)
    medianf$median <- DescTools::Median(x, na.rm = TRUE)
    medianf$mean <- DescTools::Mean(x, na.rm = TRUE)
    medianf$skwd <- moments::skewness(x, na.rm = TRUE)
    medianf$skw[medianf$skwd >= -1.5 & medianf$skwd <= 1.5] = 'USE MEAN'
    medianf$skw[medianf$skwd != 'USE MEAN'] = 'USE MEDIAN'
    ## Hair et al. (2010) and Bryne (2010) Kline (2011)
    ## are still debating 'normalcy' rule of thumb cutoffs
    ## For skewedness, But I thought it'd be fun to include
    gf <- medianf
    return(gf)
    ##You could also add more detailed Mean Median advice
  }
}

##Quick find the RangeVariance&standardDeviation
RVsd <- function (x) {
g <- c(x)
range <- g
df <- data.frame(range)
df$range <- range(g, na.rm = TRUE)
df$variance <- var(g, na.rm = TRUE)
df$sd <- sd(g, na.rm = TRUE)
gf <- df
return(gf[1:2,])
## Im sure there are better ways to display this range
## But I thought this was a informative dataframe
}

##My pride and joy
## A function that automatically applies Appropriate
## Central tendency and variance measurements to vector objects
## Based on User Classifcation, these are custom summary functions
sandyCombo <- function (z,x) {

  if (z == 'Nominal') {
    mode <- c(1)
    modef <- data.frame(mode)
    modef$mode <- DescTools::Mode(x, na.rm = TRUE)
    modef$IQV <- qualvar::VA(x, na.rm = TRUE)
    gf <- modef
    return(gf)
  }

  if (z == 'Ordinal') {
    mode <- c(1)
    modef <- data.frame(mode)
    modef$mode <- DescTools::Mode(x, na.rm = TRUE)
    modef$median <- DescTools::Median(x, na.rm = TRUE)
    modef$IQV <- qualvar::VA(x, na.rm = TRUE)
    modef$range <- max(x, na.rm = TRUE)-min(x, na.rm = TRUE)
    modef$IQR <- IQR(x, na.rm = TRUE)
    gf <- modef
    return(gf)
  }

  if (z == 'IR') {
    median <- c(1)
    medianf <- data.frame(median)
    medianf$median <- DescTools::Median(x, na.rm = TRUE)
    medianf$mean <- DescTools::Mean(x, na.rm = TRUE)
    medianf$skwd <- moments::skewness(x, na.rm = TRUE)
    medianf$skw[medianf$skwd >= -0.5 & medianf$skwd <= 0.5] = 'USE MEAN'
    medianf$skw[medianf$skwd != 'USE MEAN'] = 'USE MEDIAN'
    medianf$range <- max(x, na.rm = TRUE)-min(x, na.rm = TRUE)
    medianf$IQR <- IQR(x, na.rm = TRUE)
    medianf$var <- var(x, na.rm = TRUE)
    medianf$sd <- sd(x, na.rm = TRUE)
    gf <- medianf
    return(gf)
    ##You can add a skew measurment too
  }
}



#tvhours - I/R
#age - I/R
#happy - Ordinal
#sex - Nominal
#agekdborn - I/R



sandy('IR', gss$tvhours)
#Of all the individuals in the dataset ranked by tv hours watched,
#the 50th percentile watched 2 hours
sandy('IR', gss$age)
#Of all the individuals in the dataset by age,
#the average age was = 49 years
sandy('Ordinal', gss$happy)
#Nuetral (2) was the Median and Mode for happy scores,
#showing it was the most frequent choice among respondents
sandy('Nominal', gss$sex)
#There were more women then men in the data
sandy('IR', gss$agekdbrn)
# Because of the skewness we could probably use either
#the median 23, or mean 24.3 to represent the average age when kid was born

#range, variance and standard deviation for tvhours, age, agekdbrn, and educ
RVsd(gss$educ)
RVsd(gss$tvhours)
RVsd(gss$agekdbrn)
RVsd(gss$educ)
# Tells us the difference between the maximum and minimum values
# Shows us the averaged summed variance from the mean for individual data points
# Shows the standard unit deviation from the mean


##Comparing means across class variables with aggregate
aggregate(x = gss$agekdbrn,
          by = list(gss$class),
          FUN = mean,
          na.rm = TRUE)
##OUTPUT
# 1 | 21.74534
# 2 | 23.39018
# 3 | 25.62821
# 4 | 26.63492

#Seems like higher class individuals have children later in life on average
#Higher class individuals typically were brought up with more comprehensive sex education

##BoxPlots
boxplot(gss$educ~gss$race,
        main = "Boxplot of Education",
        xlab = "Race",
        ylab = "Years",
        col = "seagreen2")
##Education years fell across races 1,2,3 respectivly

boxplot(gss$educ~gss$health,
        main = "Boxplot of Education",
        xlab = "Health",
        ylab = "Years",
        col = "steelblue1")

## Apporopriate measures of central tendency
## GDP per capita Ratio expressed as %
## electric Ratio expressed as %
## exports Ratio expressed as %

sandyCombo("IR", wb$gdppc)
#Average country gdp is median 6433 per captia
sandyCombo("IR", wb$electric)
#50 percentile is 99.58
sandyCombo("IR", wb$exports)
#has a average deviation of 30 from the mean

aggregate(x = wb$lifeexpect,
          by = list(wb$highincome),
          FUN = mean,
          na.rm = TRUE)

boxplot(wb$lifeexpect~wb$highincome,
        main = "Boxplot of Life Expectancy",
        xlab = "High Income",
        ylab = "Years",
        col = "steelblue1")
#Higher income countries live 12 years longer on average

##INSERT
wb$incomecat = wb$gdppc
wb$incomecat[wb$incomecat < 2037] = 1
wb$incomecat[wb$incomecat >= 2037 & wb$incomecat <= 6434] = 2
wb$incomecat[wb$incomecat > 6434 & wb$incomecat <= 19745 ] = 3
wb$incomecat[wb$incomecat > 19745] = 4
##END INSERT

boxplot(wb$infmort~wb$incomecat,
        main = "Infant Mortality Rates",
        xlab = "Country Income Class",
        ylab = "Mortality %",
        col = "steelblue1")
##Infant mortality falls as countries get wealthier
