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
user.sub.highrep<- user.sub[user.sub$Reputation>1,]

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


#plot CreatD, for US, DE, DK and CA
countries <- c("US", "DE", "DK", "CA")
total <- ggplot(drop.levels(user.sub[user.sub$country.code %in% countries,]),
            aes(x=CreatD,
                group=country.code,
                color=country.code
                )
            ) + 
              geom_density() 

low.rep<- ggplot(drop.levels(user.sub[user.sub$Reputation <=1 & user.sub$country.code %in% countries,]),
            aes(x=CreatD,
                group=country.code,
                color=country.code
                )
            ) + 
              geom_density()

high.rep<- ggplot(drop.levels(user.sub[user.sub$Reputation >1 & user.sub$country.code %in% countries,]),
                 aes(x=CreatD,
                     group=country.code,
                     color=country.code
                     )
                 ) + 
                   geom_density()

#plot histogram of duration

duration.all <-hist(user.sub$duration,
                    main="Histogram of all user duration") 

duration.low.rep <- hist(user.sub$duration[user.sub$Reputation<=1],
                         main="Histogram of user duration of reputation<=1")


plot.hist.low.duration  <- ggplot(user.sub.lowrep,
                     aes(x=duration)
                     ) + 
                       geom_histogram() + 
                       facet_wrap(~country.code, scales="free_y")+
                       opts(title="Time between account creation date and last access date 
                            for users with reputation <=1",axis.text.x=theme_text(
                         angle=90, hjust=1, size=6))
print(plot.hist.low.duration)

plot.hist.high.duration  <- ggplot(user.sub.highrep,
                     aes(x=duration)
                     ) + 
                       geom_histogram() + 
                       facet_wrap(~country.code, scales="free_y")+
                       opts(title="Time between account creation date and last access date 
                            for users with reputation >1",axis.text.x=theme_text(
                              angle=90, hjust=1, size=6))
print(plot.hist.high.duration)

plot.hist.low.create  <- ggplot(user.sub.lowrep,
                                  aes(x=as.integer(CreatD))
                                  ) + 
                                    geom_histogram() + 
                                    facet_wrap(~country.code, scales="free_y")+
                                    opts(title="Account creation date for reputation <=1",axis.text.x=theme_text(
                                      angle=90, hjust=1, size=6))+
                                        labs(x="creation date")
print(plot.hist.low.create)


plot.hist.high.create  <- ggplot(user.sub.highrep,
                                   aes(x=as.integer(CreatD))
                                   ) + 
                                     geom_histogram() + 
                                     facet_wrap(~country.code, scales="free_y")+
                                     opts(title="Account creation date for reputation >1",axis.text.x=theme_text(
                                       angle=90, hjust=1, size=6))+
                                         labs(x="creation date")
print(plot.hist.high.create)

plot.hist.low.lastacc  <- ggplot(user.sub.lowrep,
                                  aes(x=as.integer(LastAccD))
                                  ) + 
                                    geom_histogram() + 
                                    facet_wrap(~country.code, scales="free_y")+
                                    opts(title="Account last access date for reputation <=1",axis.text.x=theme_text(
                                      angle=90, hjust=1, size=6))+
                                        labs(x="last access date")
print(plot.hist.low.lastacc)

plot.hist.high.lastacc  <- ggplot(user.sub.highrep,
                                   aes(x=as.integer(LastAccD))
                                   ) + 
                                     geom_histogram() + 
                                     facet_wrap(~country.code, scales="free_y")+
                                     opts(title="Account last access date for reputation >1",axis.text.x=theme_text(
                                       angle=90, hjust=1, size=6))+
                                         labs(x="last access date")
print(plot.hist.high.lastacc)

