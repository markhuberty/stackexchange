#Count users for each duration and creation date quantile, by country


library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)
library(catspec)

#load user data
user <-
  read.csv("C:/Users/miaomiaocui/stackexchange/data/user.csv",
           header=TRUE)

#reformat date

#take out CreatD and redo it because it's now a factor
user$CreatD <- NULL

#format date
format.date <- function(d){
  d<- str_split(d,"T")
  d<- sapply(d,function(x){x[[1]][1]})
  d<- as.Date(d)
  return(d)
}

user$CreatD <- format.date(user$CreationDate)

#add creation date
user$creat.date<-as.integer(user$CreatD)

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

user.sub <- drop.levels(user[user$country.code
                                               %in% country.sub,])

#count amount of users per quantile for duration and creation date
# count.creatd.duration.user <- ftable(cut(user.sub$creat.date,
#                                          breaks=seq(1,34260,68520,68530,102800,137100)),cut(user.sub$duration,10),
#                 dnn=c("creation.date","duration"))
# 
# write.table(count.creatd.duration.user,
#           file="C:/Users/miaomiaocui/Documents/test/stackexchange/data/count_creat_duration_user.csv",
#           sep=",",
#             row.names=TRUE,
#             col.names=NA,
#             qmethod="double")

count.creatd.duration.user.country <- ftable(cut(user.sub$creat.date,
                                            breaks=seq(14450,14660,14880)),
                                     cut(user.sub$duration,
                                         breaks=seq(61,246,538)),
                                     dnn=c("creation.date","duration","country.code"),
                                     group=user.sub$country.code)

cd <- data.frame(count.creatd.duration.user.country)

#plot
creatd.duration.plot <-ggplot(cd,
                              aes(x=log(Freq+1),
                                  y=duration,
                                  label=country.code)) +
                              geom_text(size=1,angle =330, hjust=0)+
                              facet_grid(creation.date ~ .)+
                              opts(title="User count (log) per duration and creation date quantile by country")
                              
                              
print(creatd.duration.plot)


