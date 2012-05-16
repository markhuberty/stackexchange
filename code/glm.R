# Regress user skill counts, mean skill level, against reputation, in a GLM structure

library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)
library(lme4)

# I. Process group-level data
#load epi data (EPI score is percentage of correct answers,so English speaking countries
#are assumed to have scored 100%, that is 100)
epi <- read.csv("C:/Users/miaomiaocui/stackexchange/data/english_proficiency.csv",
                header=TRUE)

epi$country.code <- toupper(epi$country.code)

#load country employment protection and education data
voc <- read.csv("C:/Users/miaomiaocui/stackexchange/data/voc_country_covariates.csv",
                header=TRUE)
#only take year 2000
voc <- drop.levels(voc[voc$Year==2000,])
voc$Year <- NULL
voc$Country <- NULL

#merge group level data
country.level <- merge(epi,voc,by="country.code",all=FALSE)

#get names of countries in country.level
countries <- country.level$country.code


# II .Process individual level data

#load user skill map
user.skill<- read.csv("C:/Users/miaomiaocui/stackexchange/data/users_skills_geo.csv",header=TRUE)

#only keep users with countries in the country.level data
user.skill.sub <- drop.levels(user.skill[user.skill$country.code
                                         %in% countries,])

#merge in reputation, accessdate, etc.
user <- read.csv("C:/Users/miaomiaocui/stackexchange/data/user.csv",header=TRUE)

#now we get individual level
individual.level <- merge(user.skill.sub,
                          user,
                          by.x="userid",
                          by.y="Id",
                          all=FALSE)

#take out repetative columns and rearrange names
individual.level$Reputation.x <- NULL
individual.level$CreationDate.x <- NULL
individual.level$LastAccessDate.x <- NULL
individual.level$Reputation.x <- NULL
individual.level$CreationDate.y <- NULL
individual.level$LastAccessDate.y <- NULL
individual.level$index <- NULL
individual.level$country.code.y <- NULL

names(individual.level) <- c("userid","skillid","skillvalue",
                             "country.code","reputation",
                             "upvotes","downvotes",
                             "LastAccd","CreatD","duration")

#count number of skillid for each user id
skill.counts <- tapply(rep(1,nrow(individual.level)),
                       individual.level$userid,
                       sum)
skill.counts <- data.frame(names(skill.counts),skill.counts)
names(skill.counts)<-c("userid","skill.counts")

#calculate mean user skill level
skill.mean.level <- tapply(individual.level$skillvalue,
                           individual.level$userid,
                           mean)
skill.mean.level <- data.frame(names(skill.mean.level),
                               skill.mean.level)
names(skill.mean.level) <- c("userid","skill.mean.level")

#construct individual level data frame

indi.level <- individual.level
indi.level$skillid<- NULL
indi.level$skillvalue <- NULL
indi.level <- unique(indi.level)
indi.level<-merge(indi.level,
                  skill.counts,
                  by="userid",
                  all=FALSE)
indi.level<-merge(indi.level,
                  skill.mean.level,
                  by="userid",
                  all=FALSE)

indi.level$skill.counts <- as.integer(indi.level$skill.counts)
indi.level$skill.mean.level <- as.numeric(indi.level$skill.mean.level)


# III. Merge country level and individual level to "data"
#      standardize reputation and add regions

data <- merge(indi.level, country.level,
              by="country.code",
              all=FALSE)

data.high.rep <- drop.levels(data[data$reputation>1,])
data.high.rep<-na.omit(data.high.rep)
data.high.rep$reputation <- as.numeric(data.high.rep$reputation)

#standardize reputation

data.high.rep$log.rep.std <- 
  (log(data.high.rep$reputation)-mean(log(data.high.rep$reputation)))/
  (2*sd(log(data.high.rep$reputation)))

#add user skill standard deviations
user.skill.sd.mean <- read.csv("C:/Users/miaomiaocui/stackexchange/data/user_skill_value_summary.csv",
         header=TRUE)

data.high.rep <- merge(data.high.rep,
                       user.skill.sd.mean,
                       by="userid",
                       all=FALSE)
#add regions
lme <- c("US", "AU", "NZ", "IE", "GB", "CA")
cme <- c("DE", "AT", "JP", "BE", "FR", "IT", "CH","NL")
scand <- c("DK", "SE", "NO", "FI")

data.high.rep$region[data.high.rep$country.code
                     %in% lme]<-"LME:US,CA,GB,AU,IE"
data.high.rep$region[data.high.rep$country.code
                     %in% scand]<-"SCAND:SE,NO,DK,FI"
data.high.rep$region[data.high.rep$country.code
                     %in% cme]<-"CME:DE,AU,JP,BE,FR,IT,NL,CH"

 
sc.reg <- glm(log(skill.counts) ~ log.rep.std + factor(country.code), 
              data=data.high.rep,
              family=gaussian)   
sk.reg <- glm(log(skill.mean.level + 5.0001) ~ log.rep.std + factor(country.code), 
              data=data.high.rep,
              family=gaussian)

data.high.rep<-na.omit(data.high.rep)

#test for heteroscedasticity useing the Breusch-Pagan test (hetero exists)
htest<- bptest(sd.skill.value  ~ log.rep.std + duration +
  factor(country.code) -1, data=data.high.rep)

#generalized linear regression
data.high.rep.high.sd <- drop.levels(data.high.rep[data.high.rep$sd.skill.value>0,])

sk.sd.reg <- glm(sd.skill.value ~ log.rep.std + duration +
  factor(country.code) -1,
                 data=data.high.rep.high.sd,
                 family=gaussian)

#test fit of sk.sd.reg
test.sk.sd.reg <- predict(sk.sd.reg)
plot(data.high.rep.high.sd$sd.skill.value ~ test.sk.sd.reg, 
     ylim=c(min(data.high.rep.high.sd$sd.skill.value),
            quantile(data.high.rep.high.sd$sd.skill.value,0.75))
     )
#plot country indicator coefficents
coef.sk.sd.reg <- data.frame(coef(summary(sk.sd.reg)))
#take out log.rep.std and duration
coef.sk.sd.reg$coef.name <- row.names(coef.sk.sd.reg)

coef.sk.sd.reg <- drop.levels(coef.sk.sd.reg
                              [coef.sk.sd.reg$coef.name!="log.rep.std",])
coef.sk.sd.reg <- drop.levels(coef.sk.sd.reg
                              [coef.sk.sd.reg$coef.name!="duration",])
coef.sk.sd.reg$country.code <- c("AT","AU","BE","CA","CH","DE","DK",
                                 "FI","FR","GB","IE","IT","JP","NL","NO",
                                 "SE","US")

levels(coef.sk.sd.reg$country.code)<-c("FI","BE","AU","IT","CA","AT","NL",
                                       "IE","NO","US","SE","CH","GB","FR",
                                       "DE","DK","JP")
#add error bar
limits <- aes(ymax=Estimate + Std..Error,ymin=Estimate - Std..Error,
              data=coef.sk.sd.reg)

ggplot(coef.sk.sd.reg,aes(x=country.code,y=Estimate))+
  geom_point()+
  geom_errorbar(limits,width=0.25)+
  opts(title="Country indicator coefficients for sd.skill.value (no log)
+/- 1 standard error bar",
       xlab="countries",
       ylab="county indicator coefficient estimate")+
  scale_x_discrete(limits=levels(coef.sk.sd.reg$country.code))


#plot heteroscedasticity
plot(sk.sd.reg$resid ~ data.high.rep.high.sd$log.rep.std,
     ylim=c(min(sk.sd.reg$resid),
            quantile(sk.sd.reg$resid,0.75)))
plot(sk.sd.reg$resid ~ data.high.rep.high.sd$duration,
     ylim=c(min(sk.sd.reg$resid),
            quantile(sk.sd.reg$resid,0.75)))

#robust linear model
library(MASS)
sk.sd.r.reg <- rlm(sd.skill.value ~ log.rep.std + duration +
  factor(country.code) -1,
                   data=data.high.rep)



a<-length(data.high.rep$sd.skill.value)-length(coef(sk.sd.reg))-1
b<-length(data.high.rep$sd.skill.value) - 1

r.adjusted.squared.sk.sd <- 1 - (b/a)*(sum((data.high.rep$sd.skill.value-test.sk.sd.reg)^2)/
  sum((data.high.rep$sd.skill.value-mean(data.high.rep$sd.skill.value))^2))



log.sk.sd.reg <- glm(log(sd.skill.value) ~ log.rep.std + duration +
  factor(country.code) -1, 
                     data=data.high.rep.high.sd,
                     family=gaussian)
#plot country indicator coefficents
coef.log.sk.sd.reg <- data.frame(coef(summary(log.sk.sd.reg)))
#take out log.rep.std and duration
coef.log.sk.sd.reg$coef.name <- row.names(coef.log.sk.sd.reg)

coef.log.sk.sd.reg <- drop.levels(coef.log.sk.sd.reg
                              [coef.log.sk.sd.reg$coef.name!="log.rep.std",])
coef.log.sk.sd.reg <- drop.levels(coef.log.sk.sd.reg
                              [coef.log.sk.sd.reg$coef.name!="duration",])
coef.log.sk.sd.reg$country.code <- c("AT","AU","BE","CA","CH","DE","DK",
                                 "FI","FR","GB","IE","IT","JP","NL","NO",
                                 "SE","US")

levels(coef.log.sk.sd.reg$country.code)<-c("IE","FI","GB","CA","IT","US","AU",
                                       "DK","BE","AT","SE","NL","DE","JP",
                                           "NO","FR","CH")
#add error bar
limits <- aes(ymax=Estimate + Std..Error,ymin=Estimate - Std..Error,
              data=coef.log.sk.sd.reg)

ggplot(coef.log.sk.sd.reg,aes(x=country.code,y=Estimate))+
  geom_point()+geom_errorbar(limits,width=0.25)+
  opts(title="country indicator coefficients for log(sd.skill.value)
       +/- 1 standard error bar")+
  scale_x_discrete(limits=levels(coef.log.sk.sd.reg$country.code))

sk.sd.r.reg <- rlm(log(sd.skill.value + 1e-05) ~ log.rep.std + duration +
  factor(country.code) -1,
                   data=data.high.rep)
ftest<-anova.glm(log.sk.sd.reg,test="F")

#test fit of sk.sd.reg
test.log.sk.sd.reg <- predict(log.sk.sd.reg)
plot(log(data.high.rep.high.sd$sd.skill.value) ~ test.log.sk.sd.reg, 
     ylim=c(min(log(data.high.rep.high.sd$sd.skill.value)),
            quantile(log(data.high.rep.high.sd$sd.skill.value),0.75)))

a<-length(data.high.rep$sd.skill.value)-length(coef(log.sk.sd.reg))-1
b<-length(data.high.rep$sd.skill.value) - 1


r.adjusted.squared.log.sk.sd <- 1 - 
  (b/a)*(sum((log(data.high.rep$sd.skill.value+1e-05)-test.log.sk.sd.reg)^2)/
  sum((log(data.high.rep$sd.skill.value+1e-05)-  
  mean(log(data.high.rep$sd.skill.value+1e-05)))^2))
