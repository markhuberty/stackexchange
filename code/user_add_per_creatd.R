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

#load country labor data
                            
user.ratio<-
  read.csv("C:/Users/miaomiaocui/stackexchange/data/user_ratio.csv")

user.ratio$country.code <- toupper(user.ratio$country.code)

labor <- data.frame(user.ratio$country.code,user.ratio$total.labor)
names(labor) <- c("country.code","total.labor")

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

#subcategorize by region

eng <- c("US",
         "CA",
         "GB",
         "AU",
         "NZ",
         "IE"
         )
scan <- c("SE",
          "NO",
          "DK",
          "FI",
          "IS"
          )
frde <- c("FR",
         "DE"
         )
others <- c("SK",
            "NL",
            "LV",
            "LI",
            "ES",
            "EE",
            "PL",
            "IT",
            "PT",
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

#count users by creation date by country

country.names <- levels(user.sub$country.code)

count.user<- function(user.sub){
  out<-NULL
  user.sub.per.creatd <- tapply(user.sub$creat.date,user.sub$country.code,table)
  
  for(i in 1:length(country.names))
  {count.user.sub.country<-data.frame(user.sub.per.creatd[country.names[i]])
   count.user.sub.country$country.code <- country.names[i]
   names(count.user.sub.country)<-c("creation.date","user.add","country.code")
   #normalize dates
   count.user.sub.country$min.date <- min(count.user.sub.country$creation.date)
   count.user.sub.country$normal.creation.date <- count.user.sub.country$creation.date -
   count.user.sub.country$min.date
   # second order difference
   count.user.sub.country$diff.add <- NULL
   count.user.sub.country$diff.rate <- NULL
   count.user.sub.country$diff.add[1]<- 0
   count.user.sub.country$diff.rate[1]<-0
   if (length(diff(count.user.sub.country$user.add))>0){
     for(j in 1:length(diff(count.user.sub.country$user.add))){
       count.user.sub.country$diff.add[j+1]<-
         diff(count.user.sub.country$user.add)[j]
       count.user.sub.country$diff.rate[j+1]<-
         count.user.sub.country$diff.add[j+1]/
         count.user.sub.country$user.add[j]
         }
      }
    
   out <- rbind(out,count.user.sub.country)
   }
   
   
      
  return(out)
}

count.user.sub.per.creatd <- count.user(user.sub)

#add regions to plot in one graph

count.user.sub.per.creatd$region[count.user.sub.per.creatd$country.code
                %in% eng]<-"eng:US,CA,GB,AU,NZ,IE"
count.user.sub.per.creatd$region[count.user.sub.per.creatd$country.code
                %in% scan]<-"scan:SE,NO,DK,FI"
count.user.sub.per.creatd$region[count.user.sub.per.creatd$country.code
                %in% frde]<-"frde:FR,DE"
count.user.sub.per.creatd$region[count.user.sub.per.creatd$country.code
                %in% others]<-"other countries"


#merge in total labor

count.user.sub.per.creatd<-merge(count.user.sub.per.creatd,labor,by="country.code",all=FALSE)

#calculate add per labor
count.user.sub.per.creatd$user.add.per.labor <-
  count.user.sub.per.creatd$user.add/count.user.sub.per.creatd$total.labor

#plot on the same plot
plot1 <- ggplot(count.user.sub.per.creatd,
               aes(x=normal.creation.date,
                   y=log10(user.add.per.labor),
                   group=country.code,
                   color=region))+
                     geom_line()+
                     opts(title="User add per labor (log) by region",
                          )

print(plot1)

plot2 <- ggplot(count.user.sub.per.creatd,
               aes(x=normal.creation.date,
                   y=log10(user.add.per.labor),
                   group=country.code,
                   color=region))+
                     geom_point()+
                     opts(title="Point Plot: User add per labor (log) by region")

print(plot2)



#Count users by month by country

country.names <- levels(user.sub$country.code)

count.user.month<- function(user.sub){
  out<-NULL
  #firstly, normalize date
  for (i in 1:length(country.names))
  {user.sub$min.date[user.sub$country.code==country.names[i]] <- min(user.sub$creat.date[user.sub$country.code==country.names[i]])
   
   user.sub$normal.date[user.sub$country.code==country.names[i]] <-
     user.sub$creat.date[user.sub$country.code==country.names[i]]-
     user.sub$min.date[user.sub$country.code==country.names[i]]}
  
  #secondly, cut dates
  user.sub$normal.date.mo <- cut(user.sub$normal.date,
                                30)
  
  #thirdly, sum added users per month, per country
  user.sub.per.creatd.mo <- tapply(user.sub$normal.date.mo,user.sub$country.code,table)
  
  #forthly, make a data frame
  for(j in 1:length(country.names))
  {count.user.sub.country.mo<-data.frame(user.sub.per.creatd.mo[country.names[j]])
   count.user.sub.country.mo$country.code <- country.names[j]
   names(count.user.sub.country.mo)<-c("normalized.creation.month","user.add.month","country.code")
   out <- rbind(out,count.user.sub.country.mo)
  }
  
  
  
  return(out)
}

count.user.sub.per.creatd.mo <- count.user.month(user.sub)

#add regions to plot in one graph

lme <- c("US", "AU", "NZ", "IE", "GB", "CA")
cme <- c("DE", "AT", "JP", "BE", "FR", "IT", "NL", "CH", "JP")
scand <- c("DK", "SE", "NO", "FI")

group <- c("US", "AU", "NZ", "IE", "GB", "CA",
           "DE", "AT", "JP", "BE", "FR", "IT", "NL",
           "CH","DK", "SE", "NO", "FI")

count.user.sub.per.creatd.mo$region[count.user.sub.per.creatd.mo$country.code
                                 %in% lme]<-"LME: US,CA,GB,AU,NZ,IE"
count.user.sub.per.creatd.mo$region[count.user.sub.per.creatd.mo$country.code
                                 %in% scand]<-"SCAND:SE,NO,DK,FI"
count.user.sub.per.creatd.mo$region[count.user.sub.per.creatd.mo$country.code
                                 %in% cme]<-"CME:DE,AU,JP,BE,FR,IT,NL,CH"
# count.user.sub.per.creatd.mo$region[count.user.sub.per.creatd.mo$country.code
#                                  %in% others]<-"other countries"


#drop countries that are not in the categories
count.user.sub.group <- drop.levels(count.user.sub.per.creatd.mo[count.user.sub.per.creatd.mo$country.code
                                         %in% group,])

#merge in total labor

count.user.sub.group<-merge(count.user.sub.group,labor,by="country.code",all=FALSE)

#calculate add per labor
count.user.sub.group$user.add.per.labor.month <-
  count.user.sub.group$user.add.month/count.user.sub.group$total.labor

#plot on the same plot
plot3 <- ggplot(count.user.sub.group,
               aes(x=normalized.creation.month,
                   y=log10(user.add.per.labor.month),
                   group=country.code,
                   color=region))+
                     geom_line()+
                     opts(title="Monthly added users per labor, by region",
                          axis.text.x=theme_text(
                            angle=90, hjust=1, size=6))+
                              labs(x="Normalized creation date divided into 30 bins",
                                   y="Log10 (Monthly added users per labor)")

print(plot3)



plot4 <- ggplot(count.user.sub.per.creatd,
                aes(x=normal.creation.date,
                    y=log10(user.add.per.labor),
                    group=country.code,
                    color=region))+
                      geom_point()+
                      opts(title="Point Plot: User add per labor (log), per month by region")

print(plot4)




