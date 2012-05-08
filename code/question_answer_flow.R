# First Step:
#Obtain flow of answers and questions among users
#Only consider answers that are accepted by questioner (votetype=1)

# Second Step:the heatmap
#Counts the information exchange betwee country pairs 
#Assign quantiles to the counts and plot heatmap


# Third Step: the network graph
#Plot network graphs that contain the following information:
#nodes: countries that only include CME, LME and SCAND
#edges: directed, from countries that ask question to those that answer

#weight type 1 --- reflects bilateral relationship: 
#             edge weight between (A,B) = 
#             questions asked by country A that got answered by country B/total questions asked by country A

#weight type 2 --- reflects countries' overall influences:
#             edge weight between (A,B) =
#             questions asked by country A that got answered by country B/total questions asked by all countries

#graphs only display edges with weight above the 75-percentile

#THEN among the edges that ARE displayed, three colors are assigned to partition these edges.


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

#exploring whether tie zone could make any difference 

#load in time zone
time.zone <- read.csv("C:/users/miaomiaocui/stacKexchange/data/gmt.csv",header=FALSE)

names(time.zone) <- c("country.code","GMT")


user.flow.sub.acc.count<-merge(user.flow.sub.acc.count,time.zone,
                               by.x="question.country",
                               by.y="country.code",
                               all=FALSE)

names(user.flow.sub.acc.count)<-c("question.country","answer.country","flow.frequency","flow.quantiles",
                                  "total.questions.asked.by.q.country","weight.by.answer.country",
                                  "total.answers","weight.by.total.answers","q.GMT")

user.flow.sub.acc.count<-merge(user.flow.sub.acc.count,time.zone,
                               by.x="answer.country",
                               by.y="country.code",
                               all=FALSE)

names(user.flow.sub.acc.count)<-c("answer.country","question.country","flow.frequency",
                                  "flow.quantiles",
                                  "total.questions.asked.by.q.country",
                                  "weight.by.answer.country",
                                  "total.answers","weight.by.total.answers",
                                  "q.GMT","a.GMT")

user.flow.sub.acc.count$time.zone.diff <- abs(user.flow.sub.acc.count$q.GMT-user.flow.sub.acc.count$a.GMT)

#get country pairs
a<-user.flow.sub.acc.count

a$country.pair <- NULL

for(i in 1:dim(a)[1])
{a$country.pair[i]<-paste(as.character(a$question.country[i]),
                                  as.character(a$answer.country[i]),sep=" ")}
#if relative time diff>12, 24-12
user.flow.sub.acc.count$time.zone.diff.r<-sapply(user.flow.sub.acc.count$time.zone.diff,
                                                 function(x){if (x>12) x=24-x
                                                             else x
                                                             })
a<-user.flow.sub.acc.count

#plot the relatioship between time zone difference and bi-lateral rate
time <- ggplot(a,aes(x=time.zone.diff.r,
                     y=weight.by.answer.country))+
                       geom_point()+
                       geom_smooth()+
                       opts(title="Correlation between time-zone difference and information flow",
                            labs(x="time zone difference, hours",
                                 y="share of questions raised by country on the left,
                                 answered by country on the right"))
print(time)
                       






#graphing nodes and vector edges for weights by answering country
library(igraph)

user.flow.acc.count.weight.a <- data.frame(user.flow.sub.acc.count$question.country,
                                           user.flow.sub.acc.count$answer.country,
                                           user.flow.sub.acc.count$weight.by.answer.country)
names(user.flow.acc.count.weight.a) <- c("question.country","answer.country","weight.by.answer.country")



#to make the graph looks better, we only keep the question countries those that belong to cme, lme and scand

keep <- c("US", "AU", "NZ", "IE", "GB", "CA",
         "DE", "AT", "JP", "BE", "FR", "IT", "NL", "CH", "JP",
         "DK", "SE", "NO", "FI")

k<- drop.levels(user.flow.acc.count.weight.a[user.flow.acc.count.weight.a$question.country
                                             %in% keep,])
k<- drop.levels(k[k$answer.country
                                             %in% keep,])
                
high.share <- data.frame(k$question.country[k$weight.by.answer.country>
                            quantile(k$weight.by.answer.country,0.75)],
                         k$answer.country[k$weight.by.answer.country>
                           quantile(k$weight.by.answer.country,0.75)],
                         k$weight.by.answer.country[k$weight.by.answer.country>
                           quantile(k$weight.by.answer.country,0.75)])

names(high.share)<-c("question.country","answer.country","weight.by.answer.country")


test2 <- graph.data.frame(high.share,directed=TRUE,vertices=NULL)

E(test2)$weight <-high.share$weight.by.answer.country

E(test2)[weight <= quantile(E(test2)$weight,0.50)]$color <- "gray80"
E(test2)[weight > quantile(E(test2)$weight,0.50)]$color <- "green"
E(test2)[weight >= quantile(E(test2)$weight,0.75)]$color <- "red"


weight.a.ring<-plot.igraph(test2,layout=layout.fruchterman.reingold.grid, vertex.color="gray60", vertex.size=3,
            vertex.label= V(test2)$name,
            edge.arrow.size = 0.1, edge.color = E(test2)$color, main="Flow from questioner to answerer countries
                           weighted by answer countries", 
            sub="                   red: >=3rd quantile,
 green: median to 3rd quantile,
                           grey: <median")


weight.a.tree<-plot.igraph(test2,layout=layout.reingold.tilford, vertex.color="gray60", vertex.size=3,
                           vertex.label= V(test2)$name,
                           edge.arrow.size = 0.1, edge.color = E(test2)$color, main="Flow from questioner to answerer countries
                           weighted by answer countries", 
                           sub="                   red: >=3rd quantile,
 green: median to 3rd quantile,
                           grey: <median")







#graphing nodes and vector edges for weights by answering country


user.flow.acc.count.weight.t <- data.frame(user.flow.sub.acc.count$question.country,
                                           user.flow.sub.acc.count$answer.country,
                                           user.flow.sub.acc.count$weight.by.total.answers)
names(user.flow.acc.count.weight.t) <- c("question.country","answer.country","weight.by.total.answers")



#to make the graph looks better, we only keep the question countries those that belong to cme, lme and scand

keep <- c("US", "AU", "NZ", "IE", "GB", "CA",
          "DE", "AT", "JP", "BE", "FR", "IT", "NL", "CH", "JP",
          "DK", "SE", "NO", "FI")

d<- drop.levels(user.flow.acc.count.weight.t[user.flow.acc.count.weight.t$question.country
                                             %in% keep,])
d<- drop.levels(d[d$answer.country
                  %in% keep,])

high.share.d <- data.frame(d$question.country[d$weight.by.total.answers>
  quantile(d$weight.by.total.answers,0.75)],
                         d$answer.country[d$weight.by.total.answers>
                           quantile(d$weight.by.total.answers,0.75)],
                         d$weight.by.total.answers[d$weight.by.total.answers>
                           quantile(d$weight.by.total.answers,0.75)])

names(high.share.d)<-c("question.country","answer.country","weight.by.total.answers")


test3 <- graph.data.frame(high.share.d,directed=TRUE,vertices=NULL)

E(test3)$weight <-high.share.d$weight.by.total.answers

E(test3)[weight <= quantile(E(test3)$weight,0.50)]$color <- "gray80"
E(test3)[weight > quantile(E(test3)$weight,0.50)]$color <- "green"
E(test3)[weight >= quantile(E(test3)$weight,0.75)]$color <- "red"


weight.t.ring<-plot.igraph(test3,layout=layout.fruchterman.reingold.grid, vertex.color="gray60", vertex.size=3,
                           vertex.label= V(test3)$name,
                           edge.arrow.size = 0.1, edge.color = E(test3)$color, main="Flow from questioner to answerer countries
                           weighted by total answers", 
                           sub="                   red: >=3rd quantile,
 green: median to 3rd quantile,
                           grey: <median")


weight.t.tree<-plot.igraph(test3,layout=layout.reingold.tilford, vertex.color="gray60", vertex.size=3,
                           vertex.label= V(test3)$name,
                           edge.arrow.size = 0.1, edge.color = E(test3)$color, main="Flow from questioner to answerer countries
                           weighted by total answers", 
                           sub="                   red: >=3rd quantile,
 green: median to 3rd quantile,
                           grey: <median")




#ZOOM in for CA, US, DE, GB

only <- c("CA","US","DE","GB")
oa<- drop.levels(user.flow.acc.count.weight.a[user.flow.acc.count.weight.a$question.country
                                             %in% only,])
oa<- drop.levels(oa[oa$answer.country
                  %in% only,])

# high.share.d <- data.frame(d$question.country[d$weight.by.total.answers>
#   quantile(d$weight.by.total.answers,0.75)],
#                            d$answer.country[d$weight.by.total.answers>
#                              quantile(d$weight.by.total.answers,0.75)],
#                            d$weight.by.total.answers[d$weight.by.total.answers>
#                              quantile(d$weight.by.total.answers,0.75)])
# 
# names(high.share.d)<-c("question.country","answer.country","weight.by.total.answers")

o<-data.frame(oa$question.country,oa$answer.country,oa$weight.by.answer.country)
names(o)<-c("question.country","answer.country","weight.by.answer.country")

test4 <- graph.data.frame(o,directed=TRUE,vertices=NULL)

E(test4)$weight <-o$weight.by.answer.country

E(test4)[weight <= quantile(E(test2)$weight,0.50)]$color <- "gray80"
E(test4)[weight > quantile(E(test2)$weight,0.50)]$color <- "green"
E(test4)[weight >= quantile(E(test2)$weight,0.75)]$color <- "red"


weight.t.ring<-plot.igraph(test4,layout=layout.fruchterman.reingold.grid, vertex.color="gray60", vertex.size=3,
                           vertex.label= V(test4)$name,edge.curved=TRUE,
                           edge.arrow.size = 0.1, edge.color = E(test4)$color, main="(ZOOM) Flow from questioner to answerer countries
                           weighted by answer countries", 
                           sub="                   red: >=3rd quantile,
 green: median to 3rd quantile,
                           grey: <median")


weight.t.tree<-plot.igraph(test4,layout=layout.reingold.tilford, vertex.color="gray60", vertex.size=3,
                           vertex.label= V(test4)$name,
                           edge.arrow.size = 0.1, edge.color = E(test4)$color, main="(ZOOM) Flow from questioner to answerer countries
                           weighted by answer countries", 
                           sub="                   red: >=3rd quantile,
 green: median to 3rd quantile,
                           grey: <median")

