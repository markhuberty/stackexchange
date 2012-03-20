library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)
library(catspec)

#load user data

user <-
  read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/user.csv",
           header=TRUE)

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

#count users by creation date by country

country.names <- levels(user.sub$country.code)

count.user<- function(user.sub){
  out<-NULL
  user.sub.per.creatd <- tapply(user.sub$creat.date,user.sub$country.code,table)
  
  for(i in 1:length(country.names))
  {count.user.sub.country<-data.frame(user.sub.per.creatd[country.names[i]])
   count.user.sub.country$country.code <- country.names[i]
   names(count.user.sub.country)<-c("creation.date","user.count","country.code")
   count.user.sub.country$min.date <- min(as.numeric(
     levels(count.user.sub.country$creation.date[creation.date])))
   count.user.sub.country$normal.creation.date <- as.numeric(
     levels(count.user.sub.country$creation.date[creation.date]))-
   count.user.sub.country$min.date
   # first order difference
   count.user.sub.country$diff.count <- NULL
   count.user.sub.country$diff.count[1]<- 0
   if (length(diff(count.user.sub.country$user.count))>0){
     for(j in 1:length(diff(count.user.sub.country$user.count))){
       count.user.sub.country$diff.count[j+1]<-
         diff(count.user.sub.country$user.count)[j]
         }
      }
   out <- rbind(out,count.user.sub.country)
   }
   
   
      
  return(out)
}

count.user.sub.per.creatd <- count.user(user.sub)

#calculate first order difference

#plot
plot <- ggplot(count.user.sub.per.creatd,
               aes(x=normal.creation.date,
                   y=diff.count,
                   group=country.code))+
                     geom_line()+
                     facet_wrap(~ country.code)
print(plot)



  
