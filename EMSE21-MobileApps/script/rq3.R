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
library(tidyverse)
install.packages("directlabels", repo="http://r-forge.r-project.org")
library(lattice)
library(directlabels)


dataset <- read.csv(paste("../RQ3_detailed.csv", sep = ""), sep = ",", header = T)


#RQ3
a = data.frame(Metrics = "Line Coverage", Value = dataset$line.coverage)
b = data.frame(Metrics = "Assertion Density", Value = dataset$assertion.density)

plot.data = rbind(a, b) # this function will bind or join the rows. See data at bottom.

p <- ggplot(plot.data, aes(x=Metrics, y=Value, fill=Metrics)) +  # This is the plot function
  geom_boxplot()      # This is the geom for box plot in ggplot.

p + theme(
  # Change legend background color
  # Change legend key size and key width
  #legend.key.size = unit(2.0, "cm"),
  #legend.key.width = unit(0.5,"cm"),
  #legend.title = element_blank(),
  #legend.spacing.x = unit(5, 'cm'),
  #legend.spacing.y = unit(0.5, 'cm'),
  #legend.position="bottom",
  #legend.text=element_text(size=24)
  legend.position = "none",
  axis.title.x = element_text(size=0),
  axis.title.y = element_text(size=0),
  axis.text.x = element_text(color = "grey20", size = 20, hjust = .5, vjust = .5, face = "plain"),
  axis.text.y = element_text(color = "grey20", size = 20, hjust = .5, vjust = .5, face = "plain")
) #+ guides(fill = guide_legend(label.position = "bottom", label.hjust = 1))
