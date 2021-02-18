require(foreign);require(lmtest);require(sandwich);require(oddsratio)
require(dplyr);require(effsize);require(car);require(pscl);require(pastecs)
require(MASS)
require(foreign)
require(lmtest)
require(sandwich)
require(oddsratio)
require(dplyr)
require(effsize)
require(car)
require(pscl)
require(pastecs)
require(MASS)
library(Hmisc)
library(randomForest)
options(scipen = 4)
options(digits = 4)
library(ggplot2)



minmax_scaler <- function(x) {
  "
  x: data. numeric vector of values to be scaled
  a: desired minimum after scaling takes place
  b: desired maximum after scaling takes place
  
  e.g. f(c(1,2,3,4), 1, 17)
  [1]  1.000000  6.333333 11.666667 17.000000
  "
  (x - min(x,na.rm = TRUE)) / (max(x,na.rm = TRUE) - min(x,na.rm = TRUE))
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


dataset <- read.csv(paste("../RQ2_detailed.csv", sep = ""), sep = ";", header = T)

dataset = dataset %>%
  filter(
    test.suite != 'NO-TEST'
  )

#RQ2
summary(remove_outliers(na.omit(dataset$t_loc)))
summary(remove_outliers(na.omit(dataset$t_wmc)))
summary(remove_outliers(na.omit(dataset$t_rfc)))
summary(na.omit(dataset$t_ifc))
summary(na.omit(dataset$t_lcom))
summary(na.omit(dataset$t_tcc))
summary(na.omit(dataset$t_lcc))
summary(na.omit(dataset$readability))
summary(na.omit(dataset$commentRatio))

lcom = dataset$t_lcom
tcc = dataset$t_tcc
lcc = dataset$t_lcc
y = dataset$t_loc

cor(lcom,y,use="complete.obs", method = 'pearson')
