#Formated user data and dropped countries that we do not care about
#Bootstrapped (n=500, CI=95%) to find mean and medians for 
#reputation (log, for high reputation users >1), upvotes(log), downvotes(log),and duration between
#account creation date and last access date

#Calculated densities

library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)


#load user data
user <-
  read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/users_geocoded_final.csv",header=TRUE)

#take out user location
user$Location <-NULL
user$Id <- NULL
user$DisplayName <- NULL

#take out uninteresting countries
country.sub <- c("AU",
                 "JP",
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
  opts(title="User mean reputation (log), for users that have reputation >1
       Bootstrapping 95% confidence interval",axis.text.x=theme_text(size=6))+
  labs(x="countries",y="reputation (log)")
print(plot.mean.rep)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_mean_rep.pdf")

plot.mean.up <- ggplot(user.sub.mean.up,
                        aes(x=country,
                            y=sum.stat,
                            ymin=ci.lower,
                            ymax=ci.upper,
                            colour=cut)
                       ) + geom_pointrange() +
  opts(title="User mean upvotes (log), for users that have reputation >1
       Bootstrapping 95% confidence interval",
       axis.text.x=theme_text(size=6))+
         labs(x="countries",y="upvoates(log)")
print(plot.mean.up)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_mean_up.pdf")

plot.mean.dn <- ggplot(user.sub.mean.dn,
                       aes(x=country,
                           y=sum.stat,
                           ymin=ci.lower,
                           ymax=ci.upper,
                           colour=cut)
                       ) +
  geom_pointrange() +
  opts(title="User mean downvotes (log), for users that have reputation >1
       Bootstrapping 95% confidence interval",
       axis.text.x=theme_text(size=6))+
         labs(x="countries",y="downvotes(log)")
  
print(plot.mean.dn)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_mean_dn.pdf")


plot.mean.duration <- ggplot(user.sub.mean.duration,
                             aes(x=country,y=sum.stat,ymin=ci.lower,ymax=ci.upper,
                                 colour=cut)
                             ) +
  geom_pointrange() +
  opts(title="User mean duration between account creation date and last access date
       Bootstrapping 95% confidence interval",axis.text.x=theme_text(size=6))+
  labs(x="countries",y="duration")

print(plot.mean.duration)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_mean_duration.pdf")

plot.med.up <- ggplot(user.sub.med.up,
                        aes(x=country,
                            y=sum.stat,
                            ymin=ci.lower,
                            ymax=ci.upper,
                            colour=cut)
                      ) +
  geom_pointrange() +
  opts(title="User median upvotes",axis.text.x=theme_text(size=6)) 

print(plot.med.up)

plot.med.rep <- ggplot(user.sub.med.rep,
                       aes(x=country,
                           y=sum.stat,
                           ymin=ci.lower,
                           ymax=ci.upper,
                           colour=cut)
                       ) +
  geom_pointrange() +
  opts(title="Uswer median reputation",axis.text.x=theme_text(size=6)) 
print(plot.med.rep)

plot.med.dn <- ggplot(user.sub.med.dn,
                      aes(x=country,
                          y=sum.stat,
                          ymin=ci.lower,
                          ymax=ci.upper,
                          colour=cut)
                      ) +
  geom_pointrange() +
  opts(title="User median downvotes",axis.text.x=theme_text(size=6)) 

print(plot.med.dn)


plot.med.duration <- ggplot(user.sub.med.duration,
                            aes(x=country,
                                y=sum.stat,
                                ymin=ci.lower,
                                ymax=ci.upper,
                                colour=cut)) +
  geom_pointrange() +
  opts(title="User median duration between account creation date and last access date",axis.text.x=theme_text(size=6))+
  labs(x="Countires",y="Median Duration")
print(plot.med.duration)

#plot rep, up, dn on the same plot to see how upvotes and downvotes affect reputation

plot.rep.up.dn.mean<-ggplot()+geom_pointrange(data=user.sub.mean.rep,
                                         aes(x=country,y=sum.stat,ymin=ci.lower,ymax=ci.upper,col="reputation(mean,log)"))+
                                           geom_pointrange(data=user.sub.mean.up,aes(x=country,y=sum.stat,
                                                                                ymin=ci.lower,
                                                                                ymax=ci.upper,col="upvotes(mean,log)"))+
                                           geom_pointrange(data=user.sub.mean.dn,aes(x=country,y=sum.stat,
                                                                                ymin=ci.lower,
                                                                                ymax=ci.upper,col="downvotes(mean,log)"))+
                                           opts(title="User mean reputation, upvotes and downvotes (log) 
for users that have reputation >1
       Bootstrapping 95% confidence interval",
                                                axis.text.x=theme_text(size=6))+
                                                  labs(x="countries",y="reputation,upvotes and downvotes(log)")
print(plot.rep.up.dn.mean)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/user_rep_up_dn_mean.pdf")


plot.rep.up.dn.med<-ggplot(data=user.sub.med.rep,
                                              aes(x=country,y=sum.stat,ymin=ci.lower,ymax=ci.upper,col="reputation(median,log)"))+geom_pointrange()+
                                                geom_pointrange(data=user.sub.med.up,aes(x=country,y=sum.stat,
                                                                                          ymin=ci.lower,
                                                                                          ymax=ci.upper,col="upvotes(median,log)"))+
                                                                                            geom_pointrange(data=user.sub.med.dn,aes(x=country,y=sum.stat,
                                                                                                                                      ymin=ci.lower,
                                                                                                                                      ymax=ci.upper,col="downvotes(median,log)"))+
                                                                                                                                        opts(title="User median reputation, upvotes and downvotes (log) 
for users that have reputation >1
       Bootstrapping 95% confidence interval",axis.text.x=theme_text(size=6))+
                                                  labs(x="countries",y="reputation,upvotes and downvotes(log)")
print(plot.rep.up.dn.med)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/user_rep_up_dn_med.pdf")      

## Two plots to look at the numeric variables
## This gets at similar ideas in a different way using
## ggplot to do the summarization.
## First construct the necessary dataframe
## Grab just the numeric data and the country code
user.sub.numeric <- user.sub[,c("country.code", "Reputation",
                                "UpVotes", "DownVotes", "CreatD",
                                "LastAccD",
                                "duration")]

user.sub.numeric.all <- user.sub.numeric
                             
names(user.sub.numeric.all) <- c("Countries","Reputation.log","UpVotes.log", "DownVotes.log", 
                             "CreationDate", "LastAccessDate", "Duration")
user.sub.numeric.all$Reputation.log <- log10(user.sub.numeric.all$Reputation.log)
user.sub.numeric.all$UpVotes.log <- log10(user.sub.numeric.all$UpVotes.log + 1)
user.sub.numeric.all$DownVotes.log <- log10(user.sub.numeric.all$DownVotes.log + 1)
user.sub.numeric.all.melt <- melt(user.sub.numeric.all, id.var="Countries")


plot.numeric.density <- ggplot(user.sub.numeric.all.melt,aes(x=value,group=Countries)) +
                                geom_density() +
                                geom_density(aes(x=value, group=NULL), col="red") + 
                                facet_wrap( ~ variable, scales="free")+
                                opts(title="Densities (reputation >=0),entire data in red"
                                     )
                               
print(plot.numeric.density)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/user_density_all.pdf")

## Subset the whole thing on reputation
user.sub.numeric.h <-
  drop.levels(user.sub.numeric[user.sub.numeric$Reputation > 1,])
names(user.sub.numeric.h) <- c("Countries","Reputation.log","UpVotes.log", "DownVotes.log", 
                                 "CreationDate", "LastAccessDate", "Duration")

## Take logs as necessary, then melt
user.sub.numeric.h$Reputation.log <- log10(user.sub.numeric.h$Reputation.log)
user.sub.numeric.h$UpVotes.log <- log10(user.sub.numeric.h$UpVotes.log + 1)
user.sub.numeric.h$DownVotes.log <- log10(user.sub.numeric.h$DownVotes.log + 1)
user.sub.numeric.h.melt <- melt(user.sub.numeric.h, id.var="Countries")


plot.numeric.density.h <- ggplot(user.sub.numeric.h.melt,
                               aes(x=value,
                                   group=Countries
                                   )
                               ) +
  geom_density() +
  geom_density(aes(x=value, group=NULL), col="red") + 
  facet_wrap( ~ variable, scales="free")+
  opts(title="Densities (reputation>1), entire data in red")
print(plot.numeric.density.h)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/user_density_high_rep.pdf")