library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)

#load user/it labor,epi data
user.all.labor.ratio <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/user_all_labor_ratio.csv",header=TRUE)
user.all.ratio <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/user_all_ratio.csv",header=TRUE)

#regress ratios against epi
fit.user.labor.ratio.epi <- lm(user.labor~epi,data=user.all.labor.ratio)
summary(fit.user.labor.ratio.epi)


# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit.user.labor.ratio.epi)