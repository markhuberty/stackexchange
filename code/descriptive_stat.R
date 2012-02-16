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

#assign binary values to website and dataframe it
user$web.binary <- ifelse(is.na(user$WebsiteUrl),0,1)
user.mean.web <- data.frame(tapply(user$web.binary,user$country.code,mean))

#deal with dates

# format.date <- function(d){
#   d<- str_split(d,"T")
#   d<- sapply(d,function(x){x[[1]][1]})
#   d<- as.Date(d)
#   return(d)
# }
# 
# access.date<- format.date(user$LastAccessDate)
# creation.date <- format.date(user$CreationDate)
# user$LastAccD<- access.date
# user$CreatD<- creation.date


#calculate basic statistics
# user.mean.last.acc<-tapply(user$LastAccD,user$country.code,mean)
# user.sd.last.acc<-tapply(user$LastAccD,user$country.code,sd)
# user.qu.last.acc<-tapply(user$LastAccD,user$country.code,quantile)
# 
# user.mean.creat<-tapply(user$CreatD,user$country.code,mean)
# user.sd.creat<-tapply(user$CreatD,user$country.code,sd)
# user.qu.creat<-tapply(user$CreatD,user$country.code,quantile)

user.mean.rep <- tapply(user$Reputation,user$country.code,mean)
user.sd.rep <- tapply(user$Reputation,user$country.code,sd)
user.qu.rep.low <- tapply(user$Reputation,user$country.code,quantile,probs=0.025))
user.qu.rep.high <- tapply(user$Reputation,user$country.code,quantile,probs=0.975))

user.mean.up <- tapply(user$UpVotes,user$country.code,mean)
user.sd.up <- tapply(user$UpVotes,user$country.code,sd)
user.qu.up.low<-  tapply(user$UpVotes,user$country.code,quantile,probs=0.025)
user.qu.up.high<- tapply(user$UpVotes,user$country.code,quantile,probs=0.975)


user.mean.dn <-  tapply(user$DownVotes,user$country.code,mean)
user.sd.dn <- tapply(user$DownVotes,user$country.code,sd)
user.qu.dn.low<- tapply(user$DownVotes,user$country.code,quantile,probs=0.025)
user.qu.dn.high<- tapply(user$DownVotes,user$country.code,quantile,probs=0.975)

#data frame and unlist lists
user.new <- data.frame(unlist(user.mean.rep),
                   unlist(user.sd.rep),
#                    unlist(user.qu.rep),
                   unlist(user.mean.up),
                   unlist(user.sd.up),
#                    unlist(user.qu.up),
                   unlist(user.mean.dn),
                   unlist(user.sd.dn)
#                    unlist(user.qu.dn),
#                    unlist(user.mean.last.acc,
#                    unlist(user.sd.last.acc),
#                    unlist(user.mean.creat),
#                    unlist(user.sd.creat)     
                  )

user.new$country.code <- rownames(user.new)
names(user.new) <- c("mean.rep","sd.rep","mean.up","sd.up","mean.dn","sd.dn","country.code")


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

#reshape
user.new.melt <- melt(user.new,id.vars="country.code")



#drop countries that we are not interested in
user.new.melt.sub <- drop.levels(user.new.melt[user.new.melt$country.code
                                     %in% country.sub,])


user.new.melt.sub$country.code <-
  factor(user.new.melt.sub$country.code,
         levels=user.new.melt.sub$country.code[order(user.new.melt.sub$value)])



  
#ggplot
plot.stat <- ggplot(user.new.melt.sub,
       aes(x=country.code,y=value,group=variable))
plot.stat+geom_point()+facet_grid(variable~.,scales="free")+
         scale_x_discrete("Country")+
         opts(strip.text.y=theme_text())
      
pdf("C:/Users/miaomiaocui/Documents/test/stackexchange/figures/user_stat_plot.pdf")

# 
# plot.mean.rep <- ggplot(user.m.sub,aes(x=country.code,y=Reputation)
#                          )+
#                          stat_summary(fun.y="mean",geom="point",colour="black")+
#                          stat_summary(fun.data="mean_cl_boot",geom="linerange",colour="red")+
#                          scale_x_discrete("Country")+
#                          scale_y_continuous("User reputation")+
#                          coord_cartesian(ylim=c(0,2000))
                        
user.raw <- data.frame(user$country.code,user$UpVotes,user$DownVotes,user$Reputation)
names(user.raw) <- c("country.code","up.votes","down.votes","reputation")
user.raw <-melt(user.raw,id.vars="country.code")

user.raw.sub <- drop.levels(user.raw[user.raw$country.code
                                     %in% country.sub,])

user.raw.sub$country.code <-
  factor(user.raw.sub$country.code,
         levels=user.raw.sub$country.code[order(user.raw.sub$value)])

plot.raw.data <- ggplot(user.raw.sub,aes(x=country.code,y=value,group=variable,
                                       ymin=min(value),
                                       ymax=max(value),
                                       colour=cut))
plot.raw.data+geom_pointrange()

#ggplot with mean and ci
user.new.df <- data.frame(unlist(user.mean.rep),
                       unlist(user.mean.up),
                       unlist(user.mean.dn))

user.new.df$country.code <- rownames(user.new.df)

user.new.df<- melt(user.new.df,id.vars="country.code")

user.new.df.sub <-drop.levels(user.new.df[user.new.df$country.code
                                    %in% country.sub,])
user.new.df.sub$country.code <- factor(user.new.df.sub$country.code,levels=user.new.df.sub$country.code)

#quantile
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


test.mean <- calc.boot(log(user$Reputation[user$Reputation > 1]),
                  drop.levels(user$country.code[user$Reputation > 1]),
                  n.boot=500,
                  fun="mean"
                  )

test.med <- calc.boot(log(user$Reputation[user$Reputation > 1]),
                       drop.levels(user$country.code[user$Reputation > 1]),
                       n.boot=500,
                       fun="median"
                       )

user.qu.rep=t(sapply(tapply(user$Reputation,
                            user$country.code,
                            quantile,
                            probs=c(0.025,0.975)
                            ),
                     function(x) x
                     )
              )
user.qu.up=t(sapply(tapply(user$UpVotes,
                           user$country.code,
                           quantile,
                           probs=c(0.025,0.975)
                           ),
                    function(x) x
                    )
             )
user.qu.dn = t(sapply(tapply(user$DownVotes,
                             user$country.code,
                             quantile,
                             probs=c(0.025, 0.975)
                             ),
                      function(x) x
                      )
               )

user.qu.rep<-data.frame(user.qu.rep)
user.qu.rep$country.code<-rownames(user.qu.rep)
user.qu.rep$variable <- "unlist.user.mean.rep."

user.qu.up<-data.frame(user.qu.up)
user.qu.up$country.code <-rownames(user.qu.up)
user.qu.up$variable <- "unlist.user.mean.up."

user.qu.dn <- data.frame(user.qu.dn)
user.qu.dn$country.code <- rownames(user.qu.dn)
user.qu.dn$variable <- "unlist.user.mean.dn."

user.quantiles <- rbind(user.qu.rep,
                        user.qu.up,
                        user.qu.dn
                        )

#merge to the user.new.df.sub

user.new.df.sub.new<-merge(user.new.df.sub,
                           user.quantiles,
                       by.x=c("country.code","variable"),
                       by.y=c("country.code","variable"),
                       all.x=TRUE,
                       all.y=FALSE)

user.new.df.sub.new<-merge(user.new.df.sub.new,user.qu.up,
                           by.x=c("country.code","variable"),
                           by.y=c("country.code","variable"),
                           all.x=TRUE,
                           all.y=FALSE)

user.new.df.sub.new<-merge(user.new.df.sub.new,user.qu.dn,
                           by.x=c("country.code","variable"),
                           by.y=c("country.code","variable"),
                           all.x=TRUE,
                           all.y=FALSE)
                       


plot.stat <- ggplot(user.new.sub,
                    aes(x=country.code,y=value,group=variable,ymin=,ymax=))
plot.stat+geom_point()+facet_grid(variable~.,scales="free")+
  scale_x_discrete("Country")+
  opts(strip.text.y=theme_text())

