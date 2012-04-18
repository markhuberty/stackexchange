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

#only keep votetype = 1 because it indicates that the answer is accepted by the person who asked.
user.flow.sub.acc<-drop.levels(user.flow.sub[user.flow.sub$vote.type==1,])

#count the frequency of country pairs by (question, ansewr)
user.flow.sub.acc.count<-data.frame(ftable(user.flow.sub.acc$question.country,user.flow.sub.acc$answer.country))
names(user.flow.sub.acc.count)<- c("question.country","answer.country","flow.frequency")

#break frequency by quantile
user.flow.sub.acc.count$freq.quantiles <- cut(user.flow.sub.acc.count$flow.frequency,
                                      breaks=c(-0.01,1,4,15,59,22070))
#plotting tile
q.a.flow <- ggplot(user.flow.sub.acc.count,
               aes(x=question.country,
                   y=answer.country))+geom_tile(aes(fill=freq.quantiles))+
                     opts(title="Flow of knowledge, measured by the amount of accepted answers",
                          axis.text.x=theme_text(size=6))
print(q.a.flow)

pdf(file="C:/Users/miaomiaocui/stackexchange/figures/q_a_knowledge_flow")

#weigh counts by country
user.question.country <- aggregate(user.flow.sub.acc.count$flow.frequency,
                                   by=list(user.flow.sub.acc.count$question.country),"sum")

names(user.question.country)<- c("question.country","total.questions.asked.by.q.country")

#merge into the original count

user.flow.sub.acc.count <- merge(user.flow.sub.acc.count,
                                 user.question.country,
                                 by="question.country",
                                 all=FALSE)

#ratio of questions answered by countries
user.flow.sub.acc.count$weight.by.answer.country <- 
  user.flow.sub.acc.count$flow.frequency/user.flow.sub.acc.count$total.questions.asked.by.q.country

#weigh by overall flow
user.flow.sub.acc.count$total.answers <- sum(user.flow.sub.acc.count$flow.frequency)
user.flow.sub.acc.count$weight.by.total.answers <- 
  user.flow.sub.acc.count$flow.frequency/user.flow.sub.acc.count$total.answers

#take out zeros
user.flow.sub.acc.count <- drop.levels(
  user.flow.sub.acc.count[user.flow.sub.acc.count$weight.by.answer.country
                                                               !=0,])
user.flow.sub.acc.count <- drop.levels(
  user.flow.sub.acc.count[user.flow.sub.acc.count$weight.by.total.answers
                          !=0,])

#graphing nodes and vector edges
library(igraph)

user.flow.acc.count.weight.a <- data.frame(user.flow.sub.acc.count$question.country,
                                           user.flow.sub.acc.count$answer.country,
                                           1/user.flow.sub.acc.count$weight.by.answer.country)
names(user.flow.acc.count.weight.a) <- c("question.country","answer.country","inverse.weight.by.answer.country")

#generate edges (question flow) and vertices (countries)
acc.count.weight.a.flow <- graph.data.frame(user.flow.acc.count.weight.a,
                                            directed=TRUE,vertices=NULL)
E(acc.count.weight.a.flow)$weight <- 1/user.flow.sub.acc.count$weight.by.answer.country

#generate minimum spanning tree, and the weight is 1/weight.by.answer.country
test <- minimum.spanning.tree(acc.count.weight.a.flow)

test2 <- add.edges(test,
                   E(acc.count.weight.a.flow)[E(acc.count.weight.a.flow)$weight<10&&
                     E(acc.count.weight.a.flow)$weight>max(E(test)$weight)])

test2.flow<- plot.igraph(test2,layout=layout.fruchterman.reingold, vertex.color="gray60", 
     vertex.label= V(acc.count.weight.a.flow)$name,
     edge.arrow.size = 0.1, edge.color = "gray80")


#add back some vertices
add.vertices <- user.flow.acc.count.weight.a$question.country[user.flow.acc.count.weight.a$inverse.weight.by.answer.country<
  mean(user.flow.acc.count.weight.a$inverse.weight.by.answer.country)]



