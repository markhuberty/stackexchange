# Regress user skill counts, mean skill level, against reputation, in a multilevel model structure

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

data.high.rep$rep.std <- 
  (data.high.rep$reputation-mean(data.high.rep$reputation))/
  (2*sd(data.high.rep$reputation))

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

#IV. Plot std.rep's correlation with skill counts and skill levels, by region
#    here only three regions, not enough for multi-level model

#skill count
plot0 <- xyplot(skill.counts~
  rep.std|region,
                data=data.high.rep,
                main="Standardized reputation and skill counts
varying intercepts, varying slopes",
                xlab="standardized reputation",
                ylab="skill counts",
                panel=function(x,y,subscripts){
                  panel.xyplot(x,y)
                  panel.lmline(x,y,tly=1,col=4)
                },
                strip=strip.custom(par.strip.text = list(cex=0.6))) 
print(plot0)

#skill level

plot1 <- xyplot(skill.mean.level~
  rep.std|region,
                data=data.high.rep,
                main="Standardized reputation and mean skill levels
varying intercepts, varying slopes",
                xlab="standardized reputation",
                ylab="mean skill levels",
                panel=function(x,y){
                  panel.xyplot(x,y)
                  panel.abline(lm(y~x),lty=1,col=4)
                },
                strip=strip.custom(par.strip.text = list(cex=0.6)))
print(plot1)


#V. Run multilevel regressions (vary intercepts and slopes) 
#   merging coefficients to "data.high.rep", with intercepts and slopes added to each row.
#   So each user's rep.std could be mapped to a + b*rep.std

#skill counts coefficients
scc <- data.high.rep
#coefficients including country-level variables
reg.scc.p.1<-glmer(formula=as.numeric(skill.counts) ~ 
  rep.std + epi + emplprot + medTenure + vocTrainSh + pcUnivEduc +
  (1|country.code), 
               family=poisson,
               data=scc,
               verbose=TRUE)
#coefficents that do not include country-level variables, poisson distribution
reg.scc.p<-glmer(formula=as.numeric(skill.counts) ~ 
  rep.std + 
  (1+rep.std|country.code), 
               family=poisson,
               data=scc)

coef.scc.p <- data.frame(coef(reg.scc.p)[[1]])

coef.scc.p$country.code <- row.names(coef.scc.p)

coef.scc.p <- data.frame(coef.scc.p$country.code,coef.scc.p$X.Intercept.,coef.scc.p$rep.std)
names(coef.scc.p)<-c("country.code","sc.intercept","sc.rep.std")

#coefficents that do not include country-level variables, gaussian distribution
reg.scc.g.1<-lmer(formula=as.numeric(skill.counts) ~ 
  rep.std + epi + emplprot + medTenure + vocTrainSh + pcUnivEduc + 
  (1|country.code), 
                 data=scc)

reg.scc.g<-lmer(formula=as.numeric(skill.counts) ~ 
  rep.std + 
  (1+rep.std|country.code), 
                data=scc,
                verbose=TRUE)
coef.scc.g <- data.frame(coef(reg.scc.g)[[1]])

coef.scc.g$country.code <- row.names(coef.scc.g)

coef.scc.g <- data.frame(coef.scc.g$country.code,coef.scc.g$X.Intercept.,coef.scc.g$rep.std)
names(coef.scc.g)<-c("country.code","sc.intercept.g","sc.rep.std.g")


#mean skill level coefficients, gaussian distribution
skc <- data.high.rep

#include country-level variables
reg.skc<-glmer(formula=skill.mean.level ~ 
  rep.std + epi + emplprot + medTenure + vocTrainSh + pcUnivEduc +
  (1+rep.std|country.code), 
               data=skc,
               verbose=TRUE)

#do not include country-level variables
reg.skc<-glmer(formula=skill.mean.level ~ 
  rep.std + (1+rep.std|country.code), 
                   data=skc,
                   verbose=TRUE)

coef.skc <- data.frame(coef(reg.skc)[[1]])

coef.skc$country.code <- row.names(coef.skc)

coef.skc <- data.frame(coef.skc$country.code,coef.skc$X.Intercept.,coef.skc$rep.std)
names(coef.skc)<-c("country.code","sk.intercept","sk.rep.std")

#merge coefficients into data.high.rep
data.high.rep.coef <- merge(data.high.rep,coef.scc.p,by="country.code",all=FALSE)
data.high.rep.coef <- merge(data.high.rep.coef,coef.skc,by="country.code",all=FALSE)
data.high.rep.coef <- merge(data.high.rep.coef,coef.scc.g,by="country.code",all=FALSE)



#VI. Plot complete pooling and multilevel variable for countries
plot.scc.p <- xyplot(skill.counts~
  rep.std|country.code, 
                data=data.high.rep.coef,
                main="Standardized reputation and skill counts
Poisson, varying intercepts, varying slopes, Poisson distribution
Blue line: complete pooling; Black line: mixed model",
                xlab="standardized reputation",
                ylab="skill counts",
                panel=function(x,y,subscripts){
                  panel.xyplot(x,y)
                  panel.abline(glm(y~x,family=poisson),lty=1,col=4)
                  panel.abline(data.high.rep.coef$sc.intercept[subscripts]+
                    data.high.rep.coef$sc.rep.std[subscripts]*
                    data.high.rep.coef$rep.std[subscripts],col="black")
                })

print(plot.scc.p)


plot.scc.g <- xyplot(skill.counts~
  rep.std|country.code, 
                data=data.high.rep.coef,
                main="Standardized reputation and skill counts
varying intercepts, varying slopes, Gaussian distribution
Blue line: complete pooling; Black line: mixed mode",
                xlab="standardized reputation",
                ylab="skill counts",
                panel=function(x,y,subscripts){
                  panel.xyplot(x,y)
                  panel.abline(glm(y~x),lty=1,col="blue")
                  panel.abline(data.high.rep.coef$sc.intercept.g[subscripts]+
                    data.high.rep.coef$sc.rep.std.g[subscripts]*
                    data.high.rep.coef$rep.std[subscripts],col="black")
                })
print(plot.scc.g)

#skill level plotting

plot.skc <- xyplot(skill.mean.level~
  rep.std|country.code,
                data=data.high.rep.coef,
                main="Standardized reputation and mean skill levels
varying intercepts, varying slopes
Blue line: complete pooling; Black line: mixed model",
                xlab="standardized reputation",
                ylab="mean skill levels",
                panel=function(x,y,subscripts){
                  panel.xyplot(x,y)
                  panel.abline(lm(y~x),lty=1,col=4)
                  panel.abline(data.high.rep.coef$sk.intercept[subscripts]+
                    data.high.rep.coef$sk.rep.std[subscripts]*
                    data.high.rep.coef$rep.std[subscripts],col="black")
                })
print(plot.skc)


#EVERYTHING LOG now

#skill counts coefficients
scc <- data.high.rep

scc$log.rep<-log(scc$reputation)
scc$log.rep.std <- (scc$log.rep-mean(scc$log.rep))/(2*sd(scc$log.rep))

#coefficents that do not include country-level variables, gaussian distribution
reg.scc.g.1<-lmer(formula=log(skill.counts) ~ 
  log.rep.std + epi + emplprot + medTenure + vocTrainSh + pcUnivEduc + 
  (1|country.code), 
                  data=scc)

reg.scc.g.2<-lmer(formula=log(skill.counts) ~ 
  log.rep.std + 
  (1|country.code), 
                data=scc)
coef.scc.g.1 <- data.frame(coef(reg.scc.g.1)[[1]])

coef.scc.g.1$country.code <- row.names(coef.scc.g.1)

coef.scc.g.1 <- data.frame(coef.scc.g.1$country.code,coef.scc.g.1$X.Intercept.,
                           coef.scc.g.1$log.rep.std)
names(coef.scc.g.1)<-c("country.code","sc.intercept.g1","sc.rep.std.g1")

coef.scc.g.2 <- data.frame(coef(reg.scc.g.2)[[1]])

coef.scc.g.2$country.code <- row.names(coef.scc.g.2)

coef.scc.g.2 <- data.frame(coef.scc.g.2$country.code,coef.scc.g.2$X.Intercept.,
                           coef.scc.g.2$log.rep.std)
names(coef.scc.g.2)<-c("country.code","sc.intercept.g2","sc.rep.std.g2")

#mean skill level coefficients, gaussian dis~tribution
skc <- data.high.rep

#include country-level variables
reg.skc<-glmer(formula=skill.mean.level ~ 
  rep.std + epi + emplprot + medTenure + vocTrainSh + pcUnivEduc +
  (1+rep.std|country.code), 
               data=skc,
               verbose=TRUE)

#do not include country-level variables
reg.skc<-glmer(formula=skill.mean.level ~ 
  rep.std + (1+rep.std|country.code), 
               data=skc,
               verbose=TRUE)

coef.skc <- data.frame(coef(reg.skc)[[1]])

coef.skc$country.code <- row.names(coef.skc)

coef.skc <- data.frame(coef.skc$country.code,coef.skc$X.Intercept.,coef.skc$rep.std)
names(coef.skc)<-c("country.code","sk.intercept","sk.rep.std")

#merge coefficients into data.high.rep

data.high.rep.coef <- merge(data.high.rep.coef,coef.skc,by="country.code",all=FALSE)
data.high.rep.coef <- merge(scc,coef.scc.g.1,by="country.code",all=FALSE)
data.high.rep.coef <- merge(data.high.rep.coef,coef.scc.g.2,by="country.code",all=FALSE)


#VI. Plot complete pooling and multilevel variable for countries
plot.scc.g12 <- xyplot(log(skill.counts)~
  log.rep.std|country.code, 
                     data=data.high.rep.coef,
                     main="Standardized reputation and skill counts
Gaussian, varying intercepts, varying slopes, Poisson distribution
                     Blue line: complete pooling; Black line: mixed model with country-level;
                       Green line: mixed model without country-level variables",
                xlab="standardized reputation",
                     ylab="skill counts",
                     panel=function(x,y,subscripts){
                       panel.xyplot(x,y)
                       panel.abline(glm(y~x),lty=1,col=4)
                       panel.abline(data.high.rep.coef$sc.intercept.g1[subscripts]+
                         data.high.rep.coef$sc.rep.std.g1[subscripts]*
                         data.high.rep.coef$log.rep.std[subscripts],col="black")
                       panel.abline(data.high.rep.coef$sc.intercept.g2[subscripts]+
                         data.high.rep.coef$sc.rep.std.g2[subscripts]*
                         data.high.rep.coef$log.rep.std[subscripts],col="green")
                     })

print(plot.scc.g12)


plot.scc.g <- xyplot(skill.counts~
  rep.std|country.code, 
                     data=data.high.rep.coef,
                     main="Standardized reputation and skill counts
varying intercepts, varying slopes, Gaussian distribution
                     Blue line: complete pooling; Black line: mixed mode",
                xlab="standardized reputation",
                     ylab="skill counts",
                     panel=function(x,y,subscripts){
                       panel.xyplot(x,y)
                       panel.abline(glm(y~x),lty=1,col="blue")
                       panel.abline(data.high.rep.coef$sc.intercept.g[subscripts]+
                         data.high.rep.coef$sc.rep.std.g[subscripts]*
                         data.high.rep.coef$rep.std[subscripts],col="black")
                     })
print(plot.scc.g)

#skill level plotting

plot.skc <- xyplot(skill.mean.level~
  rep.std|country.code,
                   data=data.high.rep.coef,
                   main="Standardized reputation and mean skill levels
varying intercepts, varying slopes
                   Blue line: complete pooling; Black line: mixed model",
                xlab="standardized reputation",
                   ylab="mean skill levels",
                   panel=function(x,y,subscripts){
                     panel.xyplot(x,y)
                     panel.abline(lm(y~x),lty=1,col=4)
                     panel.abline(data.high.rep.coef$sk.intercept[subscripts]+
                       data.high.rep.coef$sk.rep.std[subscripts]*
                       data.high.rep.coef$rep.std[subscripts],col="black")
                   })
print(plot.skc)

