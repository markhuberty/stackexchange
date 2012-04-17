#Counts the frequency of country pairs of which user ask and answer questions

library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)

#load question_answer_flow query data to match countries of users that ask and answer questions

user.flow<- read.csv("C:/Users/miaomiaocui/stackexchange/data/question_answer_flow.csv",header=FALSE)

names(user.flow) <- c("q.user.location","a.user.location",
                      "question.user.rep","answer.user.rep","question.id","question.userid",
                      "answer.id","answer.userid","vote.type","vote.count")

#load user with id and location
user.id<- read.csv("C:/Users/miaomiaocui/stackexchange/data/users_geocoded_final.csv",header=FALSE)
names(user.id)<-c("location","id","rep","creationdate","displayname",
                 "lastaccessdate","website","upvotes","downvotes",
                 "country.code")

user.id.country<-data.frame(user.id$id,user.id$country.code)
names(user.id.country)<- c("userid","country.code")

#merge country code into user.flow, to replace the location in original query of country code. 
#checked that country code matched by user id and original location from query matched up.

user.flow <- merge(user.flow,user.id.country,by.x="question.userid",by.y="userid",all=FALSE)
names(user.flow)<- c("question.userid","q.user.location",
                     "a.user.location","question.user.rep","answer.user.rep",
                     "question.id","answer.id","answer.userid","vote.type","vote.count",
                     "question.country")

user.flow <- merge(user.flow,user.id.country,by.x="answer.userid",by.y="userid",all=FALSE)

names(user.flow)<- c("answer.userid","question.userid",
                     "q.user.location","a.user.location",
                     "question.user.rep","answer.user.rep",
                     "question.id","answer.id",
                     "vote.type","vote.count",
                     "question.country","answer.country")

#taking out NA values

user.flow <- na.omit(user.flow)

#take out countries that we do not care about
country.sub <- c("AU",
                 "US",
                 "GB",
                 "DE",
                 "NO",
                 "SE",
                 "SK",
                 "NL",
                 "LV",
                 "LI",
                 "ES",
                 "EE",
                 "PL",
                 "IT",
                 "PT",
                 "FI",
                 "DK",
                 "FR",
                 "CA",
                 "IE",
                 "NZ",
                 "IL",
                 "LU",
                 "BE",
                 "SI",
                 "CH",
                 "BG",
                 "RO",
                 "HU",
                 "GR",
                 "AT",
                 "MT",
                 "CY",
                 "CZ",
                 "RU",
                 "MX"
                 )

#drop question countries that we are not interested in
user.flow.sub <- drop.levels(user.flow[user.flow$question.country
                                               %in% country.sub,])


#drop answer countries that we are not interested in
user.flow.sub <- drop.levels(user.flow.sub[user.flow.sub$answer.country
                                       %in% country.sub,])

#drop votetype = 1 because it indicates that the answer is accepted by the person who asked.
user.flow.sub.total<-drop.levels(user.flow.sub[user.flow.sub$vote.type!=1,])

#count the frequency of country pairs by (question, ansewr)
user.flow.sub.count<-data.frame(ftable(user.flow.sub.total$question.country,user.flow.sub.total$answer.country))
names(user.flow.sub.count)<- c("question.country","answer.country","flow.frequency")

#break frequency by quantile
user.flow.sub.count$freq.quantiles <- cut(user.flow.sub.count$flow.frequency,
                                      breaks=c(-0.01,2,12,52,66170))
#plotting tile
q.a.flow <- ggplot(user.flow.sub.count,
               aes(x=question.country,
                   y=answer.country))+geom_tile(aes(fill=freq.quantiles))+
                     opts(title="Flow of knowledge, regardless of quality of answers",
                          axis.text.x=theme_text(size=6))
print(q.a.flow)

pdf(file="C:/Users/miaomiaocui/stackexchange/figures/q_a_unweighted_flow")

#weigh by votetype

#seperate by vote types
user.flow.sub.acc<-drop.levels(user.flow.sub[user.flow.sub$vote.type==1,])
user.flow.sub.up<-drop.levels(user.flow.sub[user.flow.sub$vote.type==2,])
user.flow.sub.dn<-drop.levels(user.flow.sub[user.flow.sub$vote.type==3,])

#count country pairs seperately for each vote type
user.flow.sub.acc.count<-data.frame(ftable(user.flow.sub.acc$question.country,
                                           user.flow.sub.acc$answer.country))
user.flow.sub.up.count<-data.frame(ftable(user.flow.sub.up$question.country,
                                          user.flow.sub.up$answer.country))
user.flow.sub.dn.count<-data.frame(ftable(user.flow.sub.dn$question.country,
                                          user.flow.sub.dn$answer.country))

#weighted frequency, for each country pair, 
#frequency = amount of upvoted answers-amount of downvoted answers
user.flow.sub.w.count <- data.frame(user.flow.sub.acc.count$Var1,
                                    user.flow.sub.acc.count$Var2)
user.flow.sub.w.count$freq <- user.flow.sub.up.count$Freq - user.flow.sub.dn.count$Freq

names(user.flow.sub.w.count) <- c("question.country","answer.country","weighted.flow.frequency")

#plot weighted frequency
user.flow.sub.w.count$freq.quantiles <- cut(user.flow.sub.w.count$weighted.flow.frequency,
                                          breaks=c(-1.1,2,9,40,51080))
#plotting tile
q.a.w.flow <- ggplot(user.flow.sub.w.count,
                   aes(x=question.country,
                       y=answer.country))+geom_tile(aes(fill=freq.quantiles))+
                         opts(title="Weighted flow of knowledge, upvoted answers-downvoted answers",
                              axis.text.x=theme_text(size=6))
print(q.a.w.flow)

pdf(file="C:/Users/miaomiaocui/stackexchange/figures/q_a_weighted_flow")