###############################
## compare_country_skill_profiles.R
## Author: Mark Huberty
## Date begun: 8 April 2012
## Code and procedures for exploring the user:skill mapping data
## produced by calculate_country_skill_profiles.py
## Currently DRAFT--incomplete.
###############################

library(reshape)
library(ggplot2)
library(stringr)
library(foreach)


format.date <- function(d){
  d<- str_split(d,"T")
  d<- sapply(d,function(x){x[[1]][1]})
  d<- as.Date(d)
  return(d)
}

format.date2 <- function(x, f, t){
  caldate <- str_sub(x, start=f, end=t)
  out <- as.Date(caldate)
  return(out)

}

setwd("~/Documents/Research/Papers/stackexchange/")
country.user.skill <- read.csv("./data/users_skills_geo.csv",
                               header=TRUE
                               )

country.user.skill$CreationDate <-
  format.date2(country.user.skill$CreationDate, f=1, t=10)
country.user.skill$LastAccessDate <-
  format.date2(country.user.skill$LastAccessDate, f=1, t=10)

country.user.skill$duration <-
  country.user.skill$LastAccessDate - country.user.skill$CreationDate

country.user.skill$duration.quantile <-
  cut(as.integer(country.user.skill$duration), 10)

country.user.skill <-
  country.user.skill[country.user.skill$country.code != "",]

lme <- c("US", "AU", "NZ", "IE", "GB", "CA")
cme <- c("DE", "AT", "JP", "BE", "FR", "IT", "NL", "CH", "JP")
scand <- c("DK", "SE", "NO", "FI")

country.user.skill$voc <- "Other"
country.user.skill$voc[country.user.skill$country.code %in% lme] <- "LME"
country.user.skill$voc[country.user.skill$country.code %in% cme] <- "CME"
country.user.skill$voc[country.user.skill$country.code %in% scand] <- "Scand."



plot.skill.value <-
                           ggplot(country.user.skill[country.user.skill$skill_value > 0,],
                           aes(x=voc,
                               y=skill_value,
                               colour=voc
                               )
                           ) +
  geom_boxplot() +
  facet_wrap( ~ duration.quantile, scales="free")
  
print(plot.skill.value)

user.skill.count <- tapply(country.user.skill$userid,
                           country.user.skill$userid,
                           length
                           )
user.skill.mean <- tapply(country.user.skill$userid,
                           country.user.skill$userid,
                           mean
                           )

df <- data.frame(names(user.skill.count),
                 user.skill.count,
                 user.skill.mean
                 )
names(df) <- c("userid", "skillcount", "skillmean")
df$userid <- as.integer(df$userid)
df <- df[order(df$userid),]

user.country <- unique(country.user.skill[,c("userid", "country.code",
                                             "voc", "duration.quantile")])
user.country <- user.country[order(user.country$userid),]

df <- cbind(df, user.country[,2:4])

  
plot.country.skillcount <- ggplot(df,
                                  aes(x=log10(skillcount),
                                      group=voc,
                                      colour=voc
                                      )
                                  ) +
  geom_density() +
  facet_wrap( ~ duration.quantile)## +
  ## scale_x_continuous(limits=c(0,20))
                     
print(plot.country.skillcount)

plot.country.skillcount <- ggplot(df,
                                  aes(x=skillmean,
                                      group=voc,
                                      colour=voc
                                      )
                                  ) +
  geom_density() +
  facet_wrap( ~ duration.quantile)## +
  ## scale_x_continuous(limits=c(0,20))
                     
print(plot.country.skillcount)


plot.country.skillcount <- ggplot(df,
                                  aes(x=voc,
                                      y=log10(skillcount),
                                      colour=voc
                                      )
                                  ) +
  geom_boxplot() +
  facet_wrap( ~ duration.quantile)## +
  ## scale_x_continuous(limits=c(0,20))
                     
print(plot.country.skillcount)

plot.country.skillmean <- ggplot(df,
                                  aes(x=voc,
                                      y=skillmean,
                                      colour=voc
                                      )
                                  ) +
  geom_boxplot() +
  facet_wrap( ~ duration.quantile)## +
  ## scale_x_continuous(limits=c(0,20))
                     
print(plot.country.skillcount)


## Split into duration-quantile data frames w/ only columns
## needed for rxa calculation
country.user.skill.split <-
  split(country.user.skill[,c("country.code", "skillid", "skill_value")],
        country.user.skill$duration.quantile
        )

calc.rta <- function(mat){

  tp.class <- colSums(mat, na.rm=TRUE)
  tp.world <- sum(mat, na.rm=TRUE)

  tp.country <- rowSums(mat, na.rm=TRUE)

  numerator <- mat / tp.country

  denominator <- tp.class / tp.world

  rta <- t(t(numerator) / denominator)

  print(dim(rta))

  rownames(rta) <- rownames(mat)
  colnames(rta) <- colnames(mat)

  rta[is.nan(rta)] <- 0

  return(rta)
        
}

## then melt/cast and rxa each chunk
test <- lapply(country.user.skill.split, function(x){

  m <- melt(x, id.vars=c("country.code", "skillid"))
  cm <- cast(x, country.code ~ skillid, fun.aggregate="mean")
  rownames(cm) <- cm$country.code
  cm <- cm[,-1]
  rxa <- calc.rta(cm)
  return(rxa)

})

test.diversification <- foreach(x=1:length(test), .combine=rbind) %do% {
  
  div <- sort(rowSums(test[[x]] > 1))
  dec <- rep(x, length(div))
  ctry <- names(div)

  out <- cbind(ctry, dec, div)
  return(out)

}
test.diversification <- as.data.frame(test.diversification)
names(test.diversification) <- c("country", "duration.decile", "div")
test.diversification$div <-
  as.integer(as.character(test.diversification$div))
test.diversification$duration.decile <-
  as.factor(as.integer(as.character(test.diversification$duration.decile)))

plot.td <- ggplot(test.diversification,
                  aes(x=country,
                      y=div,
                      colour=duration.decile
                      )
                  ) +
  geom_point() +
  opts(axis.text.x=theme_text(size=3))

print(plot.td)
