library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)

survey.ex <- table(survey$experience, survey$country.code)
survey.age <- table(survey$age, survey$country.code)
survey.ind <- table(survey$industry, survey$country.code)
survey.occ <- table(survey$occupation, survey$country.code)
survey.rep <- table(survey$rep, survey$country.code)

test.survey.ex <- chisq.test(survey.ex)
test.survey.age <- chisq.test(survey.age)
test.survey.ind <- chisq.test(survey.ind)
test.survey.occ <- chisq.test(survey.occ)
test.survey.rep <- chisq.test(survey.rep)

survey.chi.p <- data.frame(test.survey.ex$p.value,test.survey.age$p.value,
                      test.survey.ind$p.value,test.survey.occ$p.value,
                      test.survey.rep$p.value)

names(survey.chi.p) <- c("Experience", "Age", "Industry", "Occupation", "Reputation")

#Reduce country categories
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
assign.area.code <- function(variable){
  out <- c()
  i.vec <- c()
  for (i in 1:length(variable))
  {if variable[factor%in%europe]
   }
}