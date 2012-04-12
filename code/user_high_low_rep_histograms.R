#Subsetting users into two groups, one with reputation <=1 and the other with reputation >1
#Plotting histograms of account creation date, last access date and keep duration (difference of the two)
#for users of each subset. 
#Grouping both by country and regions (CME,LME,SCAND and others)
#Calculating percentage of low reputation users as total users for each country

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

#assign regions to users

lme <- c("US", "AU", "NZ", "IE", "GB", "CA")
cme <- c("DE", "AT", "JP", "BE", "FR", "IT", "NL", "CH", "JP")
scand <- c("DK", "SE", "NO", "FI")
others <- c("AT","BE","BG","CY","CZ","EE","ES","GR","HU","IL","LI","LU","LV",
            "MT","MX","PL","PT","RO","RU","SI","SK")

user.sub$region[user.sub$country.code
                                    %in% lme]<-"LME: US,CA,GB,AU,NZ,IE"
user.sub$region[user.sub$country.code
                                    %in% scand]<-"SCAND:SE,NO,DK,FI"
user.sub$region[user.sub$country.code
                                    %in% cme]<-"CME:DE,AU,JP,BE,FR,IT,NL,CH"
user.sub$region[user.sub$country.code
                %in% others] <- "Other countries"

#subset user into two groups, high rep (reputation >1) and low rep (reputation <=1)

user.sub.lowrep <- user.sub[user.sub$Reputation<=1,]
user.sub.highrep<- user.sub[user.sub$Reputation>1,]

#plot histogram of duration of low rep users by country

user.sub.lowrep.count.country<- data.frame(tapply(rep(1,nrow(user.sub.lowrep)),
                                      user.sub.lowrep$country.code,
                                      sum))
user.sub.lowrep.count.country$country.code <-user.sub.lowrep.count.country$row.names
names(user.sub.lowrep.count.country) <- c("rep1.counts")
user.sub.lowrep.count.country$country.code <- row.names(user.sub.lowrep.count.country)

#total user by country
user.sub.total <-data.frame(tapply(rep(1,nrow(user.sub)),
                                        user.sub$country.code,
                                        sum))
names(user.sub.total) <- c("total.user")
user.sub.total$country.code <- row.names(user.sub.total)

#ratio of low rep user by country
user.rep.pop <- merge(user.sub.lowrep.count.country,user.sub.total,by="country.code",all=FALSE)

low.rep.user.ratio <- data.frame(user.rep.pop$rep1.counts/user.rep.pop$total.user)

low.rep.user.ratio$country.code <- row.names(low.rep.user.ratio)

names(low.rep.user.ratio) <- c("low.rep.user.share","country.code")

#plot the proportion of low rep users by country

plot.low.rep.country <- ggplot(low.rep.user.ratio,
                       aes(x=country.code,
                           y=low.rep.user.share))+geom_point()+
                             opts(title="Percentage of users with reputation <=1, by country",
                                  axis.text.x=theme_text(size=6))+
                             labs(x="countries",y="percentage of low reputation users")
print(plot.low.rep.country)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_low_rep_share_country.pdf")


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

#plot low rep user histogram of duration by country and by region

duration.all <-hist(user.sub$duration,
                    main="Histogram of all user duration") 

duration.low.rep <- hist(user.sub$duration[user.sub$Reputation<=1],
                         main="Histogram of user duration of reputation<=1")

user.sub.lowrep$duration.decile <- cut(user.sub.lowrep$duration,10)

plot.user.lowrep.duration.country <- ggplot(user.sub.lowrep,
                                 aes(x=duration.decile,
                                     )
                                 )+
                                   geom_histogram()+
                                   facet_wrap(~country.code,scales="free_y")+
                                   opts(title="Histogram of account-keeping duration for users with reputation <=1
                                        by country",
                                        axis.text.x=theme_text(
                                          angle=90, hjust=1, size=6)
                                        )+
                                          labs(x="account-keeping duration")
print(plot.user.lowrep.duration.country)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_lowrep_duration_country.pdf")

plot.user.lowrep.duration.region <- ggplot(user.sub.lowrep,
                                            aes(x=duration.decile,
                                                )
                                            )+
                                              geom_histogram()+
                                              facet_wrap(~region,scales="free_y")+
                                              opts(title="Histogram of account-keeping duration for users with reputation <=1
                                                   by region",
                                                   axis.text.x=theme_text(
                                                     angle=90, hjust=1, size=6)
                                                   )+
                                                     labs(x="account-keeping duration")
print(plot.user.lowrep.duration.region)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_lowrep_duration_region.pdf")


#plot high rep user histogram of duration by country and by region

user.sub.highrep$duration.decile <- cut(user.sub.highrep$duration,10)

plot.user.highrep.duration.country <- ggplot(user.sub.highrep,
                                            aes(x=duration.decile,
                                                )
                                            )+
                                              geom_histogram()+
                                              facet_wrap(~country.code,scales="free_y")+
                                              opts(title="Histogram of account-keeping duration for users with reputation >1
                                        by country",
                                        axis.text.x=theme_text(
                                          angle=90, hjust=1, size=6)
                                                   )+
                                                     labs(x="account-keeping duration")
print(plot.user.highrep.duration.country)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_highrep_duration_country.pdf")

plot.user.highrep.duration.region <- ggplot(user.sub.highrep,
                                           aes(x=duration.decile,
                                               )
                                           )+
                                             geom_histogram()+
                                             facet_wrap(~region,scales="free_y")+
                                             opts(title="Histogram of account-keeping duration for users with reputation >1
                                                   by region",
                                                   axis.text.x=theme_text(
                                                     angle=90, hjust=1, size=6)
                                                  )+
                                                    labs(x="account-keeping duration")
print(plot.user.highrep.duration.region)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_highrep_duration_region.pdf")

#plot histogram of account creation date for low rep users by country and region
plot.hist.low.create  <- ggplot(user.sub.lowrep,
                                  aes(x=as.integer(CreatD))
                                  ) + 
                                    geom_histogram() + 
                                    facet_wrap(~country.code, scales="free_y")+
                                    opts(title="Histogram of account creation date for reputation <=1
                                         by country",axis.text.x=theme_text(
                                      angle=90, hjust=1, size=6))+
                                        labs(x="creation date")
print(plot.hist.low.create)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_lowrep_creatd_country")

plot.hist.low.create.region  <- ggplot(user.sub.lowrep,
                                aes(x=as.integer(CreatD))
                                ) + 
                                  geom_histogram() + 
                                  facet_wrap(~region, scales="free_y")+
                                  opts(title="Histogram of account creation date for reputation <=1
                                         by region",axis.text.x=theme_text(
                                      angle=90, hjust=1, size=6))+
                                        labs(x="creation date")
print(plot.hist.low.create.region)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_lowrep_creatd_region")

#plot histogram of account creation date for high rep users by country and region

plot.hist.high.create  <- ggplot(user.sub.highrep,
                                   aes(x=as.integer(CreatD))
                                   ) + 
                                     geom_histogram() + 
                                     facet_wrap(~country.code, scales="free_y")+
                                     opts(title="Histogram of account creation date for reputation >1
                                          by country",axis.text.x=theme_text(
                                       angle=90, hjust=1, size=6))+
                                         labs(x="creation date")
print(plot.hist.high.create)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_highrep_creatd_country")

plot.hist.high.create.region  <- ggplot(user.sub.highrep,
                                 aes(x=as.integer(CreatD))
                                 ) + 
                                   geom_histogram() + 
                                   facet_wrap(~region, scales="free_y")+
                                   opts(title="Histogram of account creation date for reputation >1
                                          by region",axis.text.x=theme_text(
                                       angle=90, hjust=1, size=6))+
                                         labs(x="creation date")
print(plot.hist.high.create.region)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_highrep_creatd_region")

#plot histogram of account last access date for low rep users by country and region

plot.hist.low.lastacc  <- ggplot(user.sub.lowrep,
                                  aes(x=as.integer(LastAccD))
                                  ) + 
                                    geom_histogram() + 
                                    facet_wrap(~country.code, scales="free_y")+
                                    opts(title="Histogram of account last access date for reputation <=1
                                         by country",axis.text.x=theme_text(
                                      angle=90, hjust=1, size=6))+
                                        labs(x="last access date")
print(plot.hist.low.lastacc)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_lowrep_lastacc_country")

plot.hist.low.lastacc.region  <- ggplot(user.sub.lowrep,
                                 aes(x=as.integer(LastAccD))
                                 ) + 
                                   geom_histogram() + 
                                   facet_wrap(~region, scales="free_y")+
                                   opts(title="Histogram of account last access date for reputation <=1
                                         by region",axis.text.x=theme_text(
                                      angle=90, hjust=1, size=6))+
                                        labs(x="last access date")
print(plot.hist.low.lastacc.region)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_lowrep_lastacc_region")

#plot histogram of account last access date for high rep users by country and region
plot.hist.high.lastacc  <- ggplot(user.sub.highrep,
                                   aes(x=as.integer(LastAccD))
                                   ) + 
                                     geom_histogram() + 
                                     facet_wrap(~country.code, scales="free_y")+
                                     opts(title="Histogram of account last access date for reputation >1
                                          by country",axis.text.x=theme_text(
                                       angle=90, hjust=1, size=6))+
                                         labs(x="last access date")
print(plot.hist.high.lastacc)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_highrep_lastacc_country")

plot.hist.high.lastacc.region <- ggplot(user.sub.highrep,
                                  aes(x=as.integer(LastAccD))
                                  ) + 
                                    geom_histogram() + 
                                    facet_wrap(~region, scales="free_y")+
                                    opts(title="Histogram of account last access date for reputation >1
                                          by region",axis.text.x=theme_text(
                                       angle=90, hjust=1, size=6))+
                                         labs(x="last access date")
print(plot.hist.high.lastacc.region)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_highrep_lastacc_country")
