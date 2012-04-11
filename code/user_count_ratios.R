#calculated SO user to patent, labor, capital, ICT (braod and narrow definition) ratios by country

library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)

#load the labor data
it.b <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/it_share_broad.csv",header=TRUE)
it.n <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/it_share_narrow.csv",header=TRUE)
total.labor <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/labor.csv",header=TRUE)

#merge it_b,it_n, and labor
it.b.df.new <-merge(it.b,it.n,by="country_code",all=FALSE)
new.labor <- merge(it.b.df.new,total.labor,by="country_code",all=FALSE)
names(new.labor) <- c("country.code","country","broad.it.share","country","narrow.it.share","country","total")
new.labor$country.code <-toupper(new.labor$country.code)
new.labor$country <-NULL
new.labor$country <-NULL
new.labor$country <-NULL

#multiply to obtain IT labor force
new.labor$broad.it.labor <- new.labor$broad.it.share*new.labor$total/100
new.labor$narrow.it.labor <- new.labor$narrow.it.share*new.labor$total/100

#load patent data
patent <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/patents_count.csv",header=TRUE)
names(patent) <- c("country.code","2008.patent.counts")
patent$country.code <- toupper(patent$country.code)


#load user data
user <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/users_geocoded_final.csv",header=TRUE)


#change the user file's country codes to the lower cases

# user$country.code <- tolower(user$country.code)

#count user

user.sub.counts <- tapply(rep(1, nrow(user.sub)),user.sub$country.code,sum)
user.sub.counts <-data.frame(names(user.sub.counts),user.sub.counts)
names(user.sub.counts) <- c("country.code","user.sub")



#merge labor and patent into user_count

user.sub.labor <- merge(user.sub.counts,new.labor,by="country.code", all=FALSE)
user.sub.patent <- merge(user.sub.counts,patent,by="country.code",all=FALSE)

user.sub.ratio <-merge(user.sub.labor,user.sub.patent,by="country.code",all=FALSE)

user.sub.ratio$broad.it.share <- NULL
user.sub.ratio$narrow.it.share <-NULL
user.sub.ratio$user.sub.y <-NULL

#clean up column names
names(user.sub.ratio) <- c("country.code","user.counts","total.labor","broad.it.labor","narrow.it.labor","patent.counts")

#calculate ratios
user.sub.ratio$user.to.total.labor <- user.sub.ratio$user.counts/user.sub.ratio$total.labor
user.sub.ratio$user.to.broad.labor <- user.sub.ratio$user.counts/user.sub.ratio$broad.it.labor
user.sub.ratio$user.to.narrow.labor <- user.sub.ratio$user.counts/user.sub.ratio$narrow.it.labor
user.sub.ratio$user.to.patent <- user.sub.ratio$user.counts/user.sub.ratio$patent.counts

#remove missing values
user.sub.ratio <- na.omit(user.sub.ratio)

#plot user to patent, ict labor, capita ratios

plot.user.patent<-ggplot(drop.levels(user.sub.ratio),
                         aes(x=country.code,
                             y=user.to.patent))+
                               geom_point()+
                               opts(title="User per triadic patent by country",
                                    axis.text.x=theme_text(size=6))+
                                      labs(x="countries",y="User/patent")
                               
                                
print(plot.user.patent)

plot.user.total <- ggplot(drop.levels(user.sub.ratio),
                          aes(x=country.code,
                              y=user.to.total.labor))+
                                geom_point()+
                                opts(title="User per capita by country",
                                     axis.text.x=theme_text(size=6))+
                                       labs(x="countries",y="User/capita")
print(plot.user.total)

plot.user.broad <- ggplot(drop.levels(user.sub.ratio),
                          aes(x=country.code,
                              y=user.to.broad.labor))+
                                geom_point()+
                                opts(title="User per thousand ICT labor (broad definition) by country",
                                     axis.text.x=theme_text(size=6))+
                                       labs(x="countries",y="User/broad ICT labor (000)")
print(plot.user.broad)

plot.user.narrow <- ggplot(user.sub.ratio,
                          aes(x=country.code,
                              y=user.to.narrow.labor))+
                                geom_point()+
                                opts(title="User per thousand ICT labor (narrow definition) by country",
                                     axis.text.x=theme_text(size=6))+
                                       labs(x="countries",y="User/narrow ICT labor (000)")
print(plot.user.narrow)

#check if narrow and broad ICT labor follow some linear relationship across countries
plot.narrow.broad <- ggplot(drop.levels(new.labor),
                            aes(x=country.code,
                                y=narrow.it.labor/broad.it.labor))+
                                  geom_point()+
                                  opts(title="Narrow def. ICT labor as a share of broad def. ICT labor",
                                       axis.text.x=theme_text(size=6))+
                                         labs(x="countries",y="Share")
print(plot.narrow.broad)

#export
write.csv(user.ratio,file="C:/Users/miaomiaocui/Documents/teSt/stackexchange/data/user_ratio.csv",row.names=FALSE)

#get the statistics
summary(user.ratio)



