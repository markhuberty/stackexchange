library(reshape)
library(ggplot2)
library(Hmisc)
library(gdata)


#load question_answer_count data
q.a.count <- read.csv("/mnt/fwire_80/stackexchange/
                      question_answer_counts.csv",header=TRUE)
names(q.a.count) <- c("week","user","post.type","count")
q<-drop.levels(q.a.count[q.a.count$post.type==1,])
a<-drop.levels(q.a.count[q.a.count$post.type==2,])
qa.count<-merge(q,a,by=c("user","week"),all=TRUE)
names(qa.count)<-c("user","week","post.type.1",
                   "question.count","post.type.2","answer.count")

#change all NAs to 0
qa.count$question.count<-ifelse(is.na(qa.count$question.count),0,1)
qa.count$answer.count<-ifelse(is.na(qa.count$answer.count),0,1)

#proportion of question counts by user
qa.count$question.proportion<-qa.count$question.count/
  (qa.count$question.count+qa.count$answer.count)

#merge in country code
cc<-read.csv("/mnt/fwire_80/stackexchange/users_geocoded_final.csv",header=TRUE)

code<-data.frame(cc$Id,cc$country.code)

qa.count<-merge(qa.count,code,by.x="user",by.y="cc.Id",all=FALSE)

names(qa.count)<-c("user","week","post.type1",
                   "question.count","post.type2",
                   "answer.count","question.proportion","country.code")

#write out csv file
write.csv(qa.count,
          file="/mnt/fwire_80/stackexchange/question_proportion.csv",
          row.names=FALSE)

#sort weeks
out<-strsplit(as.character(qa.count$week),"/")
qa.count<-data.frame(qa.count,do.call(rbind,out))

#take inegers

qa.count$X1year<-as.numeric(as.character(qa.count$X1))
qa.count$X2week<-as.numeric(as.character(qa.count$X2))

#format the year and week into numbers like 200900025
qa.count$time<-qa.count$X1year*1e5+qa.count$X2week

#sort by user then by time
qa.count<-qa.count[order(qa.count$user,qa.count$time),]

#cumulative count
qa.count$cum.question.count<-unlist(tapply(
  qa.count$question.count,factor(qa.count$user),cumsum))

qa.count$cum.answer.count<-unlist(tapply(
  qa.count$answer.count,factor(qa.count$user),cumsum))

qa.count$cum.question.proportion<-qa.count$cum.question.count/
  (qa.count$cum.question.count+qa.count$cum.answer.count)





