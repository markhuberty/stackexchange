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

# #cut duration into 10 breakpoints and categorize them
# user.sub$duration.decile <- cut(user.sub$duration,b=10)
# 
# #count rep=1 for each duration decile
# lowrep.count<-data.frame(tapply(rep(1,nrow(user.sub.lowrep)),
#                      user.sub.lowrep$duration.decile,
#                      sum))
# 
# names(lowrep.count)<-c("rep1.count")
# lowrep.count$duration.decile<-rownames(lowrep.count)
# 
# 
# #plot
# plot.lowrep.count <- ggplot(lowrep.count,aes(x=duration.decile,y=rep1.count))+
#                      geom_bar()+
#                      opts(title="rep1 count for duration decile")
# 
# print(plot.lowrep.count)
# 
# 
# #count rep>1 for each duration decile
# user.sub.highrep <- user.sub[user.sub$Reputation>1,]
# 
# highrep.count<-data.frame(tapply(rep(1,nrow(user.sub.highrep)),
#                                 user.sub.highrep$duration.decile,
#                                 sum))
# 
# names(highrep.count)<-c("high.rep.count")
# highrep.count$duration.decile<-rownames(highrep.count)
# 
# 
# #plot
# plot.highrep.count <- ggplot(highrep.count,aes(x=duration.decile,y=high.rep.count))+
#   geom_bar()+
#   opts(title="high rep count for duration decile")
#   
# 
# print(plot.highrep.count)
# 
# #plot by country
# 
# highrep.us <- user.sub.highrep[user.sub.highrep$country.code=="US",]
# highrep.count.us<-data.frame(tapply(rep(1,nrow(highrep.us)),
#                                  highrep.us$duration.decile,
#                                  sum))
# 
# names(highrep.count.us)<-c("high.rep.count.us")
# highrep.count.us$duration.decile<-rownames(highrep.count.us)



plot.hist.low.duration  <- ggplot(user.sub.lowrep,
                     aes(x=duration)
                     ) + 
                       geom_histogram() + 
                       facet_wrap(~country.code, scales="free_y")
print(plot.hist.low.duration)

plot.hist.high.duration  <- ggplot(user.sub.highrep,
                     aes(x=duration)
                     ) + 
                       geom_histogram() + 
                       facet_wrap(~country.code, scales="free_y")
print(plot.hist.high.duration)

plot.hist.low.create  <- ggplot(user.sub.lowrep,
                                  aes(x=as.integer(CreatD))
                                  ) + 
                                    geom_histogram() + 
                                    facet_wrap(~country.code, scales="free_y")
print(plot.hist.low.create)

plot.hist.high.create  <- ggplot(user.sub.highrep,
                                   aes(x=as.integer(CreatD)
                                   ) + 
                                     geom_histogram() + 
                                     facet_wrap(~country.code, scales="free_y")
print(plot.hist.high.create)

plot.hist.low.lastacc  <- ggplot(user.sub.lowrep,
                                  aes(x=as.integer(LastAccD))
                                  ) + 
                                    geom_histogram() + 
                                    facet_wrap(~country.code, scales="free_y")
print(plot.hist.low.lastacc)

plot.hist.high.lastacc  <- ggplot(user.sub.highrep,
                                   aes(x=as.integer(LastAccD))
                                   ) + 
                                     geom_histogram() + 
                                     facet_wrap(~country.code, scales="free_y")
print(plot.hist.high.lastacc)

#plot
plot.highrep.count.us <- ggplot(highrep.count.us,aes(x=duration.decile,y=high.rep.count.us))+
  geom_bar()+
  opts(title=" US high rep count for duration decile")

print(plot.highrep.count.us)


plot.country <- function(country){
  for (i in country){rep.du.country(i)}
}

rep.du.country <- function(cc){
  highrep.cc <- user.sub.highrep[user.sub.highrep$country.code==cc,]
  highrep.count.cc <-data.frame(tapply(rep(1,nrow(highrep.cc)),
                                       highrep.cc$duration.decile,
                                       sum))
  names(highrep.count.cc) <- c("high.rep.count.cc")
  highrep.count.cc$duration.decile <- rownames(highrep.count.cc)
  
  plot.highrep.count.cc <- ggplot(highrep.count.cc,aes(x=duration.decile,y=high.rep.count.cc))+
    geom_bar()
  out=print(plot.highrep.count.cc)
  return(out)
  
}


lowrep.count.country<-data.frame(tapply(rep(1,nrow(user.sub.lowrep)),
                                user.sub.lowrep$duration.decile & user.sub.lowrep$country.code,
                                sum))





