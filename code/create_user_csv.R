library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)


#load user data
user <-
  read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/users_geocoded_final.csv",header=TRUE)

#take out user location
user$Location <-NULL
user$Id <- NULL
user$DisplayName <- NULL

#format date
format.date <- function(d){
  d<- str_split(d,"T")
  d<- sapply(d,function(x){x[[1]][1]})
  d<- as.Date(d)
  return(d)
}

access.date <- format.date(user$LastAccessDate)
creation.date <- format.date(user$CreationDate)
user$LastAccD <- access.date
user$CreatD <- creation.date

user$duration <- as.integer(difftime(user$LastAccD,
                                         user$CreatD,
                                         units="days")
                                )
write.csv(user,file="C:/Users/miaomiaocui/stackexchange/data/user.csv",row.names=FALSE)