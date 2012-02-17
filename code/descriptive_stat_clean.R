library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)


#load user data
user <-
  read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/users_geocoded_final.csv",header=TRUE)

user <-
  read.csv("~/Documents/Research/Papers/stackexchange/data/users_geocoded_final.csv",
           header=TRUE
           )

#take out user location
user$Location <-NULL
user$Id <- NULL
user$DisplayName <- NULL

#take out uninteresting countries
country.sub <- c("AU",
                 "US",
                 "GB",
                 "DE",
                 "NO",
                 "SE",
                 "SK",
                 "NL",
                 "LV",
                 "LI",
                 "ES",
                 "EE",
                 "PL",
                 "IT",
                 "PT",
                 "FI",
                 "DK",
                 "FR",
                 "CA",
                 "IE",
                 "NZ",
                 "IL",
                 "LU",
                 "BE",
                 "SI",
                 "CH",
                 "BG",
                 "RO",
                 "HU",
                 "GR",
                 "AT",
                 "MT",
                 "CY",
                 "CZ",
                 "RU",
                 "MX"
                 )


user.sub <- drop.levels(user[user$country.code%in% country.sub,])




#assign binary values to website and dataframe it
user.sub$web.binary <- ifelse(is.na(user.sub$WebsiteUrl),0,1)
user.sub.mean.web <- data.frame(tapply(user.sub$web.binary,user.sub$country.code,mean))

#calculate duration

format.date <- function(d){
  d<- str_split(d,"T")
  d<- sapply(d,function(x){x[[1]][1]})
  d<- as.Date(d)
  return(d)
}

access.date <- format.date(user.sub$LastAccessDate)
creation.date <- format.date(user.sub$CreationDate)
user.sub$LastAccD <- access.date
user.sub$CreatD <- creation.date

user.sub$duration <- as.integer(difftime(user.sub$LastAccD,
                                         user.sub$CreatD,
                                         units="days")
                                )

#calculate mean,med and 95% CI

calc.boot <- function(value, 
                      country, 
                      n.boot=10, 
                      probs=c(0.025, 0.975),
                      fun="mean"){
  
  val.country <- split(value, country)
  
  out <- lapply(val.country, function(x){
    
    boot.val(x, n.boot, probs, fun)
    
  })
  
  mean.out = sapply(out, function(x) mean(x, na.rm=TRUE))
  quantile.out = sapply(out, function(x) quantile(x, probs=probs))
  out = data.frame(names(mean.out), round(mean.out, 4), t(round(quantile.out, 4)))
  names(out) <- c("country", "sum.stat", "ci.lower", "ci.upper")
  return(out)
  
  
}

boot.val <- function(val, n.boot, probs, fun="mean"){
  
  out <- sapply(1:n.boot, function(x){
    
    sample.vec <- sample(1:length(val), length(val), replace=TRUE)
    val.boot = val[sample.vec]
    
    this.fun <- match.fun(fun)
    this.fun(val.boot, na.rm=TRUE)
    
  })  
  
  return(out)
  
}


user.sub.mean.rep <- calc.boot(log(user.sub$Reputation[user.sub$Reputation > 1]),
                               user.sub$country.code[user$Reputation > 1],
                               n.boot=500,
                               fun="mean"
                               )

user.sub.med.rep <- calc.boot(log(user.sub$Reputation[user.sub$Reputation > 1]),
                              user.sub$country.code[user.sub$Reputation > 1],
                              n.boot=500,
                              fun="median"
                              )

user.sub.mean.up <- calc.boot(log(user.sub$UpVotes[user.sub$Reputation
                                                   > 1] + 1
                                  ),
                              user.sub$country.code[user.sub$Reputation > 1],
                              n.boot=500,
                              fun="mean"
                              )

user.sub.med.up <- calc.boot(log(user.sub$UpVotes[user.sub$Reputation
                             > 1] + 1),
                             user.sub$country.code,
                             n.boot=500,
                             fun="median"
                             )

user.sub.mean.dn <-
  calc.boot(log(user.sub$DownVotes[user.sub$Reputation > 1] + 1),
            user.sub$country.code,
            n.boot=500,
            fun="mean"
            )

user.sub.med.dn <-
  calc.boot(log(user.sub$DownVotes[user.sub$Reputation > 1] + 1),
            user.sub$country.code,
            n.boot=500,
            fun="median"
            )

user.sub.mean.duration <-
  calc.boot(user.sub$duration,
            user.sub$country.code,
            n.boot=500,
            fun="mean"
            )

user.sub.med.duration <-
  calc.boot(user.sub$duration,
            user.sub$country.code,
            n.boot=500,
            fun="median"
            )

#plotting each variable


plot.mean.rep <- ggplot(user.sub.mean.rep,
                        aes(x=country,
                            y=sum.stat,
                            ymin=ci.lower,
                            ymax=ci.upper,
                            colour=cut)
                        ) +
  geom_pointrange() +
  opts(title="mean.rep")
print(plot.mean.rep)

plot.mean.up <- ggplot(user.sub.mean.up,
                        aes(x=country,
                            y=sum.stat,
                            ymin=ci.lower,
                            ymax=ci.upper,
                            colour=cut)
                       ) + geom_pointrange() +
  opts(title="mean.up") +
  coord_flip()
print(plot.mean.up)

plot.mean.dn <- ggplot(user.sub.mean.dn,
                       aes(x=country,
                           y=sum.stat,
                           ymin=ci.lower,
                           ymax=ci.upper,
                           colour=cut)
                       ) +
  geom_pointrange() +
  opts(title="mean.dn") +
  coord_flip()
print(plot.mean.dn)

plot.mean.duration <- ggplot(user.sub.mean.duration,
                             aes(x=country,y=sum.stat,ymin=ci.lower,ymax=ci.upper,
                                 colour=cut)
                             ) +
  geom_pointrange() +
  opts(title="mean.duration") +
  coord_flip()
print(plot.mean.duration)

plot.med.up <- ggplot(user.sub.med.up,
                        aes(x=country,
                            y=sum.stat,
                            ymin=ci.lower,
                            ymax=ci.upper,
                            colour=cut)
                      ) +
  geom_pointrange() +
  opts(title="med.up") +
  coord_flip()
print(plot.med.up)

plot.med.rep <- ggplot(user.sub.med.rep,
                       aes(x=country,
                           y=sum.stat,
                           ymin=ci.lower,
                           ymax=ci.upper,
                           colour=cut)
                       ) +
  geom_pointrange() +
  opts(title="med.rep") +
  coord_flip()
print(plot.med.rep)



plot.med.dn <- ggplot(user.sub.med.dn,
                      aes(x=country,
                          y=sum.stat,
                          ymin=ci.lower,
                          ymax=ci.upper,
                          colour=cut)
                      ) +
  geom_pointrange() +
  opts(title="med.dn") +
  coord_flip()
print(plot.med.dn)


plot.med.duration <- ggplot(user.sub.med.duration,
                            aes(x=country,
                                y=sum.stat,
                                ymin=ci.lower,
                                ymax=ci.upper,
                                colour=cut)) +
  geom_pointrange() +
  opts(title="med.diffdate") +
  coord_flip()
print(plot.med.duration)


## Two plots to look at the numeric variables
## This gets at similar ideas in a different way using
## ggplot to do the summarization.
## First construct the necessary dataframe
## Grab just the numeric data and the country code
user.sub.numeric <- user.sub[,c("country.code", "Reputation",
                                "UpVotes", "DownVotes", "CreatD",
                                "LastAccD",
                                "duration")
                             ]
## Subset the whole thing on reputation
user.sub.numeric <-
  drop.levels(user.sub.numeric[user.sub.numeric$Reputation > 1,])
## Take logs as necessary, then melt
user.sub.numeric$Reputation <- log10(user.sub.numeric$Reputation)
user.sub.numeric$UpVotes <- log10(user.sub.numeric$UpVotes + 1)
user.sub.numeric$DownVotes <- log10(user.sub.numeric$DownVotes + 1)
user.sub.numeric.melt <- melt(user.sub.numeric, id.var="country.code")


## First, boxplots for each numeric variable
## by country, on a single plot
plot.numeric.box <- ggplot(user.sub.numeric.melt,
                           aes(x=country.code,
                               y=value
                               )
                           ) +
  geom_boxplot() +
  facet_wrap(~ variable, scales="free") 
print(plot.numeric.box)

## Second, the same data, portrayed as densities rather than
## ranges. The value for the entire data set is overlaid in red
## on the values for each country.
plot.numeric.density <- ggplot(user.sub.numeric.melt,
                               aes(x=value,
                                   group=country.code
                                   )
                               ) +
  geom_density() +
  geom_density(aes(x=value, group=NULL), col="red") + 
  facet_wrap( ~ variable, scales="free")
print(plot.numeric.density)
