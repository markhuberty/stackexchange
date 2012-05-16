library(reshape)
library(ggplot2)
library(Hmisc)
library(gdata)


#load question_answer_count data
q.a.count <- read.csv("C:/Users/miaomiaocui/stackexchange/data/question_answer_counts.csv",header=TRUE)
names(q.a.count) <- c("week","user","post.type","count")
q<-drop.levels(q.a.count[q.a.count$post.type==1,])
a<-drop.levels(q.a.count[q.a.count$post.type==2,])
qa.count<-merge(q,a,by="week",all=TRUE)