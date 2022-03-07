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

