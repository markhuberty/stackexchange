library(reshape)
#load user data
user <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/users_geocoded_final.csv",header=TRUE)

#take out user location
user$Location <-NULL

#assign binary values to website and dataframe it
user$web.binary <- ifelse(is.na(user$WebsiteUrl),0,1)
user.mean.web <- data.frame(tapply(user$web.binary,user$country.code,mean))


#calculate basic statistics
user.mean.rep <- tapply(user$Reputation,user$country.code,mean)
user.sd.rep <- tapply(user$Reputation,user$country.code,sd)
user.qu.rep <- tapply(user$Reputation,user$country.code,quantile)

user.mean.up <- tapply(user$UpVotes,user$country.code,mean)
user.sd.up <- tapply(user$UpVotes,user$country.code,sd)
user.qu.up<-  tapply(user$UpVotes,user$country.code,quantile)

user.mean.dn <-  tapply(user$DownVotes,user$country.code,mean)
user.sd.dn <- tapply(user$DownVotes,user$country.code,sd)
user.qu.dn<- tapply(user$DownVotes,user$country.code,quantile)


user.new <- data.frame(unlist(user.mean.rep),
                   unlist(user.sd.rep),
#                    unlist(user.qu.rep),
                   unlist(user.mean.up),
                   unlist(user.sd.up),
#                    unlist(user.qu.up),
                   unlist(user.mean.dn),
                   unlist(user.sd.dn)
#                    unlist(user.qu.dn),    
                  )

user.new$country.code <- rownames(user.new)
names(user.new) <- c("mean.rep","sd.rep","mean.up","sd.up","mean.dn","sd.dn","country.code")

#reshape
user.new <- melt(user.new,id.vars="country.code")

#ggplot
library(ggplot2)
library(hmisc)
  

plot.stat <- ggplot(user.new,
       aes(x=country.code,y=value,group=variable))
plot.stat+geom_point()+facet_grid(variable~.,scales="free")+
         scale_x_discrete("Country")+
         opts(strip.text.y=theme_text())
      
pdf("C:/Users/miaomiaocui/Documents/test/stackexchange/figures/user_stat_plot.pdf")


                                                       

