
random.sample <- function(qa,total){
  #get sample size, with proportion of counts by country, share of total
  sample.proportion<-summary(qa$country.code)/sum(summary(qa$country.code))
  #get sample size
  sample.size <- round(sample.proportion * total)
  
  country.names<-names(sample.size)
  
  out <- NULL
  for(i in 1:length(country.names))
    {
    data=qa[qa$country.code==country.names[i],]
    unique.users=unique(data$user)
    user.to.keep=sample(unique.users, sample.size[[i]],replace=FALSE)
    data.to.keep=data[data$user %in% user.to.keep,]
    out <- rbind(out,data.to.keep)
    }
   return(out) 
}

