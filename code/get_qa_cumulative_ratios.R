library(reshape)
library(ggplot2)
library(Hmisc)
library(gdata)
library(foreach)
library(doMC)
setwd("~/")
#load question_answer_count data
q.a.count <- 
  read.csv("/mnt/fwire_80/stackexchange/question_answer_counts.csv",header=TRUE)
names(q.a.count) <- c("week","user","post.type","count")
q<-drop.levels(q.a.count[q.a.count$post.type==1,])
a<-drop.levels(q.a.count[q.a.count$post.type==2,])
qa.count<-NULL
qa.count<-merge(q,a,by=c("user","week"),all=TRUE)
names(qa.count)<-c("user",
                   "week",
                   "post.type.1",
                   "question.count",
                   "post.type.2",
                   "answer.count"
                   )

#change all NAs to 0
qa.count$question.count <- ifelse(is.na(qa.count$question.count),
                                  0,
                                  qa.count$question.count
                                  )
qa.count$answer.count <- ifelse(is.na(qa.count$answer.count),
                                0,
                                qa.count$answer.count
                                )

#sort weeks
out<-strsplit(as.character(qa.count$week),"/")
qa.count<-data.frame(qa.count,do.call(rbind,out))

#take inegers
qa.count$X1year<-as.numeric(as.character(qa.count$X1))
qa.count$X2week<-as.numeric(as.character(qa.count$X2))

#format the year and week into numbers like 200900025
qa.count$time<-qa.count$X1year*1e2+qa.count$X2week


## Faster version of the nested loop:
## 1. blow out the count vector to have one entry per timestamp
## 2. cumsum the resulting count vector to get one cumsum per time
##    stamp
## 3. return
cumsum.count <- function(user, user.times, user.count, unique.times){
  unique.times <- sort(unique.times)
  full.timevec <- sapply(unique.times, function(x){
    if(x %in% user.times)
      {
        idx <- which(user.times == x)
        return(user.count[idx])
      }else{
        return(0)
      }
  })
  full.timevec <- unlist(full.timevec)
  cum.counts <- cumsum(full.timevec)
  return(cum.counts)

}


#registerDoMC(3)
unique.times <- sort(unique(qa.count$time))
#writeLines("", "qa.log")
qa.cum <- foreach(x=unique(qa.count$user), .combine=rbind) %do% {
  ptm <- proc.time()
  user.idx <- qa.count$user == x
  user.times <- qa.count$time[user.idx]
  user.question.count <- qa.count$question.count[user.idx]
  user.answer.count <- qa.count$answer.count[user.idx]
  
  question.counts <-
    cumsum.count(x,
                 user.times,
                 user.question.count,
                 unique.times
                 )
  answer.counts <-
    cumsum.count(x,
                 user.times,
                 user.answer.count,
                 unique.times
                 )
  qa.ratio <- question.counts / (question.counts + answer.counts)
  out <- cbind(x, unique.times, question.counts, answer.counts,
               qa.ratio)
  colnames(out) <- c("user", "time", "q.count", "a.count", "qa.ratio")
  return(out)
  ptm <- ptm - proc.time()
  sink("qa.log", append=TRUE)
  cat(paste("Iteration time:", ptm, "\n"))
  sink()
}

## write.csv(qa.cum, file="/mnt/fwire_80/stackexchange/meh_qa_count.csv",
##           row.names=FALSE
##           )
## quit()
## ## Done
