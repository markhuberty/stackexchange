library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)


# #count users with rep<=1, the number is 31518
# user.sub.lowrep <-data.frame(user.sub$Reputation[user.sub$Reputation<=1])
# user.sub.lowrep.count <-sum(rep(1,nrow(user.sub.lowrep)))
# 
# #users that have reputation below 1 counts for 35% of total users
# lowrep.count.percentage <- user.sub.lowrep.count/dim(user.sub)[1]

#count user with rep <=1, by country

user.sub.lowrep <- user.sub[user.sub$Reputation<=1,]

user.sub.lowrep.count.country<- data.frame(tapply(rep(1,nrow(user.sub.lowrep)),
                                      user.sub.lowrep$country.code,
                                      sum))
user.sub.lowrep.count.country$country.code <-user.sub.lowrep.count.country$row.names
names(user.sub.lowrep.count.country) <- c("rep1.counts")

#total user by country
user.sub.total <-data.frame(tapply(rep(1,nrow(user.sub)),
                                        user.sub$country.code,
                                        sum))
names(user.sub.total) <- c("total.user")

#ratio of rep=1 by country
user.rep.pop <- merge(user.sub.lowrep.count.country,user.sub.total,by="row.names",all=FALSE)

rep1.ratio <- data.frame(user.rep.pop$rep1.counts/user.rep.pop$total.user)

names(rep1.ratio) <- c("rep1.ratio")

 


