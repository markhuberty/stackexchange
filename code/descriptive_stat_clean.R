library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)


#load user data
user <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/users_geocoded_final.csv",header=TRUE)

#take out user location
user$Location <-NULL
user$Id <- NULL
user$DisplayName <- NULL

#drop countries that we do not care about 

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


user.sub <- drop.levels(user[user$country.code %in% country.sub,])


user.sub$country.code <-
  factor(user.sub$country.code,
         levels=user.sub$country.code)

#assign binary values to website url and dataframe it
user.sub$web.binary <- ifelse(is.na(user.sub$WebsiteUrl),0,1)
user.sub.mean.web <- data.frame(tapply(user.sub$web.binary,user.sub$country.code,mean))

#deal with dates, calculate the duration of access 

format.date <- function(d){
  d<- str_split(d,"T")
  d<- sapply(d,function(x){x[[1]][1]})
  d<- as.Date(d)
  return(d)
}

access.date<- format.date(user.sub$LastAccessDate)
creation.date <- format.date(user.sub$CreationDate)
user.sub$LastAccD<- access.date
user.sub$CreatD<- creation.date

user.sub$diffdate <-difftime(user.sub$LastAccD,user.sub$CreatD,units="days")

plot(log(user.sub$Reputation) ~ user.sub$diffdate, pch=".")

#list countries that we are interested in



#generate descriptive statistics

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
  out = t(rbind(names(mean.out), round(mean.out, 4), round(quantile.out, 4)))
  
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


user.sub.mean.rep <- data.frame(calc.boot(log(user.sub$Reputation[user.sub$Reputation > 1]),
                       drop.levels(user.sub$country.code[user.sub$Reputation > 1]),
                       n.boot=500,
                       fun="mean"
                       ))

user.sub.med.rep <- data.frame(calc.boot(log(user.sub$Reputation[user.sub$Reputation > 1]),
                      drop.levels(user.sub$country.code[user.sub$Reputation > 1]),
                      n.boot=500,
                      fun="median"
                      ))

user.sub.mean.up <- data.frame(calc.boot(log(user.sub$UpVotes),
                       drop.levels(user.sub$country.code),
                       n.boot=500,
                       fun="mean"
                       ))

user.sub.med.up <- data.frame(calc.boot(log(user.sub$UpVotes),
                      drop.levels(user.sub$country.code),
                      n.boot=500,
                      fun="median"
                      ))
user.sub.mean.dn <- data.frame(calc.boot(log(user.sub$DownVotes),
                       drop.levels(user.sub$country.code),
                       n.boot=500,
                       fun="mean"
                       ))

user.sub.med.dn <- data.frame(calc.boot(log(user.sub$DownVotes),
                      drop.levels(user.sub$country.code),
                      n.boot=500,
                      fun="median"
                      ))
user.sub.mean.diffdate <- data.frame(calc.boot(user.sub$diffdate,
                               drop.levels(user.sub$country.code),
                               n.boot=500,
                               fun="mean"
                               ))
user.sub.med.diffdate <- data.frame(calc.boot(user.sub$diffdate,
                               drop.levels(user.sub$country.code),
                               n.boot=500,
                               fun="median"
                               ))
#plot


plot.mean.rep <- ggplot(user.sub.mean.rep,
                    aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
                    colour=cut))+geom_pointrange()+opts(title="mean.rep")


plot.med.rep <- ggplot(user.sub.med.rep,
                        aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
                            colour=cut))+geom_pointrange()+opts(title="med.rep")


plot.mean.up <- ggplot(user.sub.mean.up,
                        aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
                            colour=cut))+geom_pointrange()+opts(title="mean.up")


plot.med.up <- ggplot(user.sub.med.up,
                        aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
                            colour=cut))+geom_pointrange()+opts(title="med.up")


plot.mean.dn <- ggplot(user.sub.mean.dn,
                        aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
                            colour=cut))+geom_pointrange()+opts(title="mean.dn")


plot.med.dn <- ggplot(user.sub.med.dn,
                        aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
                            colour=cut))+geom_pointrange()+opts(title="med.dn")


plot.mean.diffdate <- ggplot(user.sub.mean.diffdate,
                        aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
                            colour=cut))+geom_pointrange()+opts(title="mean.diffdate")


plot.med.diffdate <- ggplot(user.sub.med.diffdate,
                        aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
                            colour=cut))+geom_pointrange()+opts(title="med.diffdate")
  