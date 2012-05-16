#Calculates the reponse time/wait time for questions to be answered
#Only look at the accepted answers
#Calculate both overall average time and average time by country
#Pairwise t-test and ks-test on the response/wait time

library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)

#load question_answer_time query data 

user.time<- read.csv("C:/Users/miaomiaocui/stackexchange/data/question_answer_time.csv",header=FALSE)

names(user.time) <- c("question.date","question.id","question.userid","answer.date",
                      "answer.id","answer.userid","vote.type","vote.count")

#load user with id and location
user.id<- read.csv("C:/Users/miaomiaocui/stackexchange/data/users_geocoded_final.csv",header=FALSE)
names(user.id)<-c("location","id","rep","creationdate","displayname",
                  "lastaccessdate","website","upvotes","downvotes",
                  "country.code")

user.id.country<-data.frame(user.id$id,user.id$country.code)
names(user.id.country)<- c("userid","country.code")

#merge country code into user.time so that we get both time and location

user.time <- merge(user.time,user.id.country,by.x="question.userid",by.y="userid",all=FALSE)
names(user.time)<- c("question.userid","question.date",
                     "question.id","answer.date","answer.id",
                     "answer.userid","vote.type","vote.count",
                     "question.country")

user.time <- merge(user.time,user.id.country,by.x="answer.userid",by.y="userid",all=FALSE)

names(user.time)<- c("answer.userid","question.userid",
                     "question.date","question.id","answer.date","answer.id",
                    "vote.type","vote.count",
                     "question.country","answer.country")

#taking out NA values

user.time <- user.time[complete.cases(user.time),]


#take out countries that we do not care about
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

#drop question countries that we are not interested in
user.time <- drop.levels(user.time[user.time$question.country
                                       %in% country.sub,])


#drop answer countries that we are not interested in
user.time <- drop.levels(user.time[user.time$answer.country
                                           %in% country.sub,])


#only look at votetype1 for selected answers
user.time<-drop.levels(user.time[user.time$vote.type==1,])

#format date and time

user.time$question.date <-
  as.POSIXct(as.character(user.time$question.date),
             format="%m/%d/%Y %H:%M")
user.time$answer.date <-
  as.POSIXct(as.character(user.time$answer.date),
             format="%m/%d/%Y %H:%M")

user.time$wait.minutes <-
  as.integer(difftime(user.time$answer.date,
                      user.time$question.date,
                      units="mins"
                      )
             )
                                   uas.integer(difftime(as.POSIXct(as.character(user.time$answer.date),
                                         format="%m/%d/%Y %H:%M"),
                              as.POSIXct(as.character(user.time$question.date),
                                         format="%m/%d/%Y %H:%M"),units="mins"))

#take out negative values, there are 68 "bad" values, with either NA in question or answer date
#or answer date earlier than question date
user.time<-user.time[user.time$wait.minutes>=0,]

user.time<-user.time <- user.time[complete.cases(user.time),]

write.csv(user.time,file="C:/Users/miaomiaocui/stackexchange/data/user_time.csv")

#average wait time: 87.64 hours

mean.wait.hours <- mean(user.time$wait.minutes)/60

#calculate average wait time among all country pairs
qc<-levels(user.time$question.country)
ac<-levels(user.time$answer.country)

avg<-NULL
for(i in 1:length(qc))
{wait<-user.time[user.time$question.country==qc[i],]
  for(j in 1:length(ac)){wait.pair<-wait[wait$answer.country==ac[j],]
                         mean<-mean(wait.pair$wait.minutes)
                         first<-quantile(wait.pair$wait.minutes,0.25)
                         second<-quantile(wait.pair$wait.minutes,0.50)
                         third<-quantile(wait.pair$wait.minutes,0.75)
                         max<-max(wait.pair$wait.minutes)
                         min<-min(wait.pair$wait.minutes)
                         rec<-data.frame(qc[i],ac[j],mean,first,second,third,max,min)
                         names(rec)<-c("question.country","answer.country",
                                       "mean.wait.minutes","first.quantile",
                                       "second.quantile","third.quantile",
                                       "min","max")
                         avg<-rbind(avg,rec)}
}

#take out NaN and NA
avg <- drop.levels(avg[avg$mean.wait.minutes!="NaN",])
avg$mean.wait.hours<-avg$mean.wait.minutes/60

write.csv(avg, file="C:/users/miaomiaocui/stackexchange/data/mean_wait_minutes.csv",
          col.names=TRUE,row.names=FALSE)


focus <- c("US","CA","GB","DE","FR","AU","NZ")
avg.focus<-drop.levels(avg[avg$question.country
                           %in% focus,])
avg.focus<-drop.levels(avg.focus[avg.focus$answer.country
                           %in% focus,])

avg.focus$mean.wait.hours<-avg.focus$mean.wait.minutes/60
write.csv(avg.focus, file="C:/users/miaomiaocui/stackexchange/data/mean_wait_minutes_focus.csv",
          col.names=TRUE,row.names=FALSE)

#time zone analysis----t and ks tests
#name country pairs (question, answer)
user.time$country.pair <- NULL

for(i in 1:dim(user.time)[1])
{user.time$country.pair[i]<-paste(as.character(user.time$question.country[i]),
                              as.character(user.time$answer.country[i]),sep=" ")}
 

#too many country pairs for tests so we only pick the ones in the focus group
user.time.sub<-drop.levels(user.time[user.time$question.country
                                    %in% focus,])
user.time.sub<-drop.levels(user.time.sub[user.time.sub$answer.country
                                    %in% focus,])

#pairwise t-test on the wait time
compute.pairwise.t <- function(variable,factor){
  unique.factor <- levels(factor)
  out <- c()
  i.vec <- c()
  j.vec <- c()
  for(i in 1:length(unique.factor))
  {
    for(j in 1:length(unique.factor))
    {ttest<-t.test(variable[factor==unique.factor[i]],
                   variable[factor==unique.factor[j]],
                   paired=FALSE)
     ttest.p <- ttest$p.value
     out <- append (out,ttest.p)
     i.vec <- append(i.vec,unique.factor[i])
     j.vec <- append(j.vec,unique.factor[j])
    }
  }
  
  dft.out <- data.frame(i.vec,j.vec,out)
  dft.out <- dft.out[dft.out$i.vec !=
    dft.out$j.vec,]
  dft.out$out <- round(dft.out$out,4)
  return(dft.out)
}


#pairwise test reputation
t.test.user.time.sub<- compute.pairwise.t(user.time.sub$wait.minutes,
                                          factor(user.time.sub$country.pair))
t.test.user.time.sub$sig.p <- t.test.user.time.sub$out<0.1


plot.pairwise.wait.t <- ggplot(t.test.user.time.sub,
                                   aes(x=i.vec,
                                       y=j.vec,
                                       fill=sig.p
                                       )
                                   )+
                                     geom_tile()+
                                     scale_x_discrete("Question country (top), Answer country (bottom)")+
                                     scale_y_discrete("Question country (left), Answer country (right)")+
                                     opts(title="Pairwise significance of difference in mean response time to the question
                                                                                                          t-test",
                                          axis.text.x=theme_text(angle=90, hjust=1,size=6),
                                          axis.text.y=theme_text(size=6))
print(plot.pairwise.wait.t)


#write pairwise ks-test function

compute.pairwise.ks <- function(variable,factor){
  unique.factor <- levels(factor)
  out <- c()
  i.vec <- c()
  j.vec <- c()
  for(i in 1:length(unique.factor))
  {
    for(j in 1:length(unique.factor))
    {kstest<-ks.test(variable[factor==unique.factor[i]],
                     variable[factor==unique.factor[j]],
                     paired=FALSE)
     kstest.p <- kstest$p.value
     out <- append (out,kstest.p)
     i.vec <- append(i.vec,unique.factor[i])
     j.vec <- append(j.vec,unique.factor[j])
    }
  }
  
  dft.out <- data.frame(i.vec,j.vec,out)
  dft.out <- dft.out[dft.out$i.vec !=
    dft.out$j.vec,]
  dft.out$out <- round(dft.out$out,4)
  return(dft.out)
}


#pairwise ks test

ks.test.user.time.sub<- compute.pairwise.ks(user.time.sub$wait.minutes,
                                          factor(user.time.sub$country.pair))
ks.test.user.time.sub$sig.p <- ks.test.user.time.sub$out<0.1


plot.pairwise.wait.ks <- ggplot(ks.test.user.time.sub,
                               aes(x=i.vec,
                                   y=j.vec,
                                   fill=sig.p
                                   )
                               )+
                                 geom_tile()+
                                 scale_x_discrete("Question country (top), Answer country (bottom)")+
                                 scale_y_discrete("Question country (left), Answer country (right)")+
                                 opts(title="Pairwise significance of distribution of response time to the question
                                                                                               ks-test",
                                          axis.text.x=theme_text(angle=90, hjust=1,size=6),
                                      axis.text.y=theme_text(size=6))
print(plot.pairwise.wait.ks)
<<<<<<< HEAD
=======

#plot histogram of user participating time


for(i in 1:dim(user.time)[1])
  {user.time$question.hour[i]<-str_split(user.time$question.date[i], " ")[[1]][2]
   user.time$question.hour[i]<-str_split(user.time$question.hour[i], ":")[[1]][1]
   }



#draw histogram


user.time$question.hour<- factor(user.time$question.hour,levels=c("0","1","2",
                                                                  "3","4","5",
                                                                  "6","7","8",
                                                                  "9","10","11","12",
                                                                  "13","14","15","16",
                                                                  "17","18","19","20",
                                                                  "21","22","23","24"
                                                       ))

user.time$question.hour.range <- cut(user.time$question.hour,breaks=8)

plot.user.time.country <- ggplot(user.time,
                           aes(x=question.hour)
                           )+
                             geom_histogram()+
                             facet_wrap(~question.country,scales="free_y")+
                             opts(title="Time for posting questions by country",
                                  axis.text.x=theme_text(
                                    hjust=1, size=6)
                                  )+labs(x="question posting time")
print(plot.user.time.country)

for(i in 1:dim(user.time)[1])
{user.time$answer.hour[i]<-str_split(user.time$answer.date[i], " ")[[1]][2]
 user.time$answer.hour[i]<-str_split(user.time$answer.hour[i], ":")[[1]][1]
}

user.time$answer.hour<- factor(user.time$answer.hour,levels=c("0","1","2",
                                                                  "3","4","5",
                                                                  "6","7","8",
                                                                  "9","10","11","12",
                                                                  "13","14","15","16",
                                                                  "17","18","19","20",
                                                                  "21","22","23","24"
                                                                  ))
>>>>>>> origin/master
