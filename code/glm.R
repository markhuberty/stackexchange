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


sc.epi.reg <- glm(log(skill.counts) ~ log.rep.std + epi + factor(country.code) -1, 
              data=data.high.rep,
              family=gaussian)
sk.epi.reg <- glm(skill.mean.level ~ log.rep.std + epi + factor(country.code) -1, 
                  data=data.high.rep,
                  family=gaussian)
#impact of speaking english

data.high.rep$english[data.high.rep$country.code
                     %in% lme]<-1
data.high.rep$english[data.high.rep$country.code
                     %in% scand]<-0
data.high.rep$english[data.high.rep$country.code
                     %in% cme]<-0

sc.english.reg.only <- glm(log(skill.counts) ~  log.rep.std+
  english  -1, 
                      data=data.high.rep,
                      family=gaussian)

sk.english.reg.only <- glm(skill.mean.level ~ log.rep.std +
  english -1, data= data.high.rep,family=gaussian)


sc.english.reg <- glm(log(skill.counts) ~  log.rep.std+
  english + factor(country.code) -1, 
                  data=data.high.rep,
                  family=gaussian)

sk.english.reg <- glm(skill.mean.level ~  log.rep.std+
  english + factor(country.code) -1, 
                      data=data.high.rep,
                      family=gaussian)

plot(log(data.high.rep$skill.counts), data.high.rep$log.rep.std)
abline(sc.reg)