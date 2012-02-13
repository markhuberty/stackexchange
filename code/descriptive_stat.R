library(reshape)
#load user data
user <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/users_geocoded_final.csv",header=TRUE)

#take out user location
user$Location <-NULL

#calculate basic statistics
user.mean.rep <-data.frame(tapply(user$Reputation,user$country.code,mean))
user.sd.rep <-data.frame(tapply(user$Reputation,user$country.code,sd))
user.qu.rep<- data.frame(tapply(user$Reputation,user$country.code,quantile))

user.mean.up <-data.frame(tapply(user$UpVotes,user$country.code,mean))
user.sd.up <-data.frame(tapply(user$UpVotes,user$country.code,sd))
user.qu.up<- data.frame(tapply(user$UpVotes,user$country.code,quantile))

user.mean.dn <-data.frame(tapply(user$DownVotes,user$country.code,mean))
user.sd.dn <-data.frame(tapply(user$DownVotes,user$country.code,sd))
user.qu.dn<- data.frame(tapply(user$DownVotes,user$country.code,quantile))

#merge based on country code
user.new <- merge(user.mean.rep,user.sd.rep,by="row.names",all=FALSE)
user.new <- merge(user.new,user.qu.rep,by.x="Row.names",by.y="row.names",all=FALSE)
user.new <- merge(user.new,user.mean.up,by.x="Row.names",by.y="row.names",all=FALSE)
user.new <- merge(user.new,user.sd.up,by.x="Row.names",by.y="row.names",all=FALSE)
user.new <- merge(user.new,user.qu.up,by.x="Row.names",by.y="row.names",all=FALSE)
user.new <- merge(user.new,user.mean.dn,by.x="Row.names",by.y="row.names",all=FALSE)
user.new <- merge(user.new,user.sd.dn,by.x="Row.names",by.y="row.names",all=FALSE)
user.new <- merge(user.new,user.qu.dn,by.x="Row.names",by.y="row.names",all=FALSE)

#rename columns
names(user.new)<- c("country.code","mean.rep","sd.rep","qu.rep","mean.up","sd.up","qu.up",
                    "mean.dn","sd.dn","qu.dn"
                  )

#reshape
user.new <- melt(user.new,id="country.code")
                                                       

