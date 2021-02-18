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

#LOADING DATASET
df <- read.csv("../RQ4_detailed.csv", sep = ";", header = T)

#CHECKNIG FOR CORRELATION
checkCorrelation = function(form, dataset){
  vc = varclus(form, data=dataset)
  plot(vc)
  threshold <- 0.6
  abline(h=1-threshold, col = "red", lty = 2)
}

#FUNCTION TO PRINT RESULTS OF THE LINEAR REGRESSION MODEL
printResults = function(lr){
  print(summary(lr))
  print(vif(lr))
}

#FUNCTION TO SHOW RESULTS OF THE RANDOM FOREST MODEL
plot.rf <- function(rf) {
  print(rf)
  plot(rf)
  #varImp(rf)
  varImpPlot(rf)
  
}

df = df %>%
  filter(!is.na(df$defects)) %>% 
  droplevels()


#BEGIN LINEAR REGRESSION
form = df$defects ~ df$t_loc + df$t_lcom + df$t_wmc + df$t_ifc + df$t_lcc + df$t_rfc + df$t_tcc + df$isAssertionRoulette + df$isEagerTest + df$isMysteryGuest + df$isResourceOptimism + df$isIndirectTesting + df$readability + df$commentRatio + df$line.coverage + df$assertion.density + df$p_loc + df$p_wmc + df$p_ifc + df$p_lcc + df$p_rfc + df$p_tcc + df$isGodClass + df$isCDSBP + df$isComplexClass + df$isFunctionalDecomposition + df$isDW + df$isIS + df$changes
checkCorrelation(form, df)


#REMOVING isFunctionalDecomposition, p_wmc, t_wmc, t_rfc
form = df$defects ~ df$t_loc + df$t_lcom + df$t_ifc + df$t_lcc + df$t_tcc + df$isAssertionRoulette + df$isEagerTest + df$isMysteryGuest + df$isResourceOptimism + df$isIndirectTesting + df$readability + df$commentRatio + df$line.coverage + df$assertion.density + df$p_loc + df$p_ifc + df$p_lcc + df$p_rfc + df$p_tcc + df$isGodClass + df$isComplexClass + df$changes
checkCorrelation(form, df)


#BUILDING MODEL
lr =  glm(form)
printResults(lr)



