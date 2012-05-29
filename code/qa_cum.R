library(reshape)
library(ggplot2)
library(Hmisc)
library(gdata)


#load question_answer_count data
q.a.count <- 
  read.csv("/mnt/fwire_80/stackexchange/question_answer_counts.csv",header=TRUE)
names(q.a.count) <- c("week","user","post.type","count")
q<-drop.levels(q.a.count[q.a.count$post.type==1,])
a<-drop.levels(q.a.count[q.a.count$post.type==2,])
qa.count<-NULL
qa.count<-merge(q,a,by=c("user","week"),all=TRUE)
names(qa.count)<-c("user","week","post.type.1",
                   "question.count","post.type.2","answer.count")

#change all NAs to 0
qa.count$question.count<-ifelse(is.na(qa.count$question.count),0,qa.count$question.count)
qa.count$answer.count<-ifelse(is.na(qa.count$answer.count),0,qa.count$answer.count)

#sort weeks
out<-strsplit(as.character(qa.count$week),"/")
qa.count<-data.frame(qa.count,do.call(rbind,out))

#take inegers

qa.count$X1year<-as.numeric(as.character(qa.count$X1))
qa.count$X2week<-as.numeric(as.character(qa.count$X2))

#format the year and week into numbers like 200900025
qa.count$time<-qa.count$X1year*1e2+qa.count$X2week

#sort by user then by time
qa.count<-qa.count[order(qa.count$user,qa.count$time),]

#calculate cumulative counts
qa.cum<- foreach(x=unique(qa.count$user), .combine="rbind") %:% 
  foreach(y=sort(unique(qa.count$time)), .combine="rbind") %do% {
    
    cum.question.count <- sum(qa.count$question.count[qa.count$user==x & qa.count$time <= y],
                              na.rm=TRUE)
    cum.answer.count <- sum(qa.count$answer.count[qa.count$user==x & qa.count$time <= y], 
                            na.rm=TRUE)
    
    out <- c(x, y, cum.question.count, cum.answer.count)
    
    return(out)
  }

qa.cum <- data.frame(qa.cum)

qa.cum$X5 <- qa.cum$X3/(qa.cum$X3+qa.cum$X4)

#merge in country code
cc<-read.csv("/mnt/fwire_80/stackexchange/users_geocoded_final.csv",header=TRUE)

code<-data.frame(cc$Id,cc$country.code)

qa.count<-merge(qa.cum,code,by.x="X1",by.y="cc.Id",all=FALSE)

names(qa.count)<-c("user","time","cum.question.count",
                   "cum.answer.count","cum.question.proportion","country.code")

#write out csv file
write.csv(qa.cum,
          file="/mnt/fwire_80/stackexchange/qa_cum.csv",
          row.names=FALSE)



#take out countries that we do not care about
country.sub <- c("AU","US","GB","DE","NO","SE","SK","NL","LV","LI","ES","EE",
                 "PL","IT","PT","FI","DK","FR","CA","IE","NZ","IL","LU","BE",
                 "SI","CH","BG","RO","HU","GR","AT","MT","CY","CZ","RU","MX",
                 "JP")
qa.cum.sub<-drop.levels(qa.cum[qa.cum$country.code %in% country.sub,])

#factorize time
qa.cum$week <- factor(qa.cum$time)

#using bootstrap to calculate mean,med and 95% CI

calc.boot <- function(value, 
                      time, 
                      n.boot=1000, 
                      probs=c(0.025, 0.975),
                      fun="mean"){
  val.time <- split(value,time)
  out <- lapply(val.time, function(x){
    boot.val(x, n.boot, probs, fun)
  }
                )
  mean.out = sapply(out, function(x) mean(x, na.rm=TRUE))
  quantile.out = sapply(out, function(x) quantile(x, probs=probs,na.rm=TRUE))
  out = data.frame(names(mean.out), round(mean.out, 4), t(round(quantile.out, 4)))
  names(out) <- c("week", "sum.stat", "ci.lower", "ci.upper")
  return(out)
}

boot.val <- function(val, n.boot, probs, fun="mean"){
  
  out <- sapply(1:n.boot, function(x){
    
    sample.vec <- sample(1:length(val), length(val), replace=TRUE)
    val.boot = val[sample.vec]
    
    this.fun <- match.fun(fun)
    this.fun(val.boot, na.rm=TRUE)
    
  })  
  
  return(out)
  
}


#calculate sample means

time.country <- function(value,data){
  out<-NULL
  country <- levels(data$country.code)
  for (i in 1:length(country)){
    country.data<-cbind(country[i],
                        calc.boot(value[data$country.code==country[i]], 
                                  data$time[data$country.code==country[i]], 
                                  n.boot=1000, probs=c(0.025, 0.975),
                                  fun="mean"))
    out<-rbind(out,country.data)}
  return(out)
}

mean.cum.q.p <- time.country(qa.cum.sub$cum.question.proportion,qa.cum.sub)
names(mean.cum.q.p)<-c("country.code","week","mean","ci.lower","ci.upper")

#plotting

pdf("/mnt/fwire_80/stackexchange/mean_cum_question_proportion_3.pdf")
plot1 <- ggplot(mean.cum.q.p,
                aes(x=week,
                    y=mean,
                    ymin=ci.lower,
                    ymax=ci.upper
                    )
                ) +
                  geom_pointrange() + 
                  facet_wrap(~country.code)+
                  opts(title="Mean cumulative proportion of questions by country, 95% CI",
                       axis.text.x=theme_text(hjust=1,angle=90,size=3))
print(plot1)
dev.off()

pdf("/mnt/fwire_80/stackexchange/mean_cum_question_proportion_4.pdf")
plot1 <- ggplot(mean.cum.q.p,
                aes(x=week,
                    y=mean
                    )) +
                  geom_line() + 
                  geom_ribbon(aes(ymin=ci.lower,ymax=ci.upper),alph=0.5)+
                  facet_wrap(~country.code)+
                  opts(title="Mean cumulative proportion of questions by country, 95% CI",
                       axis.text.x=theme_text(hjust=1,angle=90,size=3))
print(plot1)
dev.off()