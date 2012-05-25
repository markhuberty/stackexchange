library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)

#load sample

sample<- read.csv("C:/Users/miaomiaocui/stackexchange/data/qa_count_sample.csv",
                  header=TRUE)



#using bootstrap to calculate mean,med and 95% CI

calc.boot <- function(value, 
                      time, 
                      n.boot=10, 
                      probs=c(0.025, 0.975),
                      fun="mean"){
  
  val.time <- split(value,time)
  
  out <- lapply(val.time, function(x){
    
    boot.val(x, n.boot, probs, fun)
    
  })
  
  mean.out = sapply(out, function(x) mean(x, na.rm=TRUE))
  quantile.out = sapply(out, function(x) quantile(x, probs=probs,na.rm=TRUE))
  out = data.frame(names(mean.out), round(mean.out, 4), t(round(quantile.out, 4)))
  names(out) <- c("time", "sum.stat", "ci.lower", "ci.upper")
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
                         sample$week[data$country.code==country[i]], 
                         n.boot=10, probs=c(0.025, 0.975),
                         fun="mean"))
    out<-rbind(out,country.data)}
  return(out)
}
    
mean.cum.q.p <- time.country(sample$cum.question.proportion,sample)
names(mean.cum.q.p)<-c("country.code","week","mean","ci.lower","ci.upper")

#point-range plot
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
                     axis.text.x=theme_text(hjust=1,angle=90,size=6))
print(plot1)

#ribbon plot
plot2 <- ggplot(mean.cum.q.p,
                aes(x=week,y=mean)) +
              geom_ribbon(aes(ymin=ci.lower,
                               ymax=ci.upper)) + 
                  geom_line()+
                  facet_wrap(~country.code)+
                  opts(title="Mean cumulative proportion of questions by country, 95% CI",
                       axis.text.x=theme_text(hjust=1,angle=90,size=6))
print(plot2)   



                            
                           