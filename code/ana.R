library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)


#load user survey data, use na.strings to fill blank cells to assign binary values
survey <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/survey.csv",header=TRUE,
                   na.strings=""
                   )

#add binary variables to programming language and tech device 
#(for example, if one user checked C, then that cell is 1)
survey$java <- ifelse(is.na(survey$lan.java),0,1)
survey$javascript <- ifelse(is.na(survey$lan.javascript),0,1)
survey$php <- ifelse(is.na(survey$lan.php),0,1)
survey$css <- ifelse(is.na(survey$lan.css),0,1)
survey$python <- ifelse(is.na(survey$lan.python),0,1)
survey$obC <- ifelse(is.na(survey$lan.obC),0,1)
survey$ruby <- ifelse(is.na(survey$lan.ruby),0,1)
survey$sql <- ifelse(is.na(survey$lan.sql),0,1)
survey$c <- ifelse(is.na(survey$lan.c.),0,1)
survey$C <- ifelse(is.na(survey$lan.c),0,1)
survey$c.. <- ifelse(is.na(survey$lan.c..),0,1)
survey$perl <- ifelse(is.na(survey$lan.perl),0,1)
survey$html5 <- ifelse(is.na(survey$lan.html5),0,1)

#[IMPORTANT]!!!other very specific tools, but is.null does not seem to work
survey$other <- ifelse(is.na(survey$lan.other),0,1)

#take out lan. columns to clean up the data so that we only have binary data

survey$lan.java <- NULL
survey$lan.javascript <- NULL
survey$lan.php <- NULL
survey$lan.css <- NULL
survey$lan.python <- NULL
survey$lan.obC <- NULL
survey$lan.ruby <- NULL
survey$lan.sql <- NULL
survey$lan.c <- NULL
survey$lan.c. <- NULL
survey$lan.c.. <- NULL
survey$lan.perl <- NULL
survey$lan.html5 <- NULL

#add user id
survey$user.id <- c(1:nrow(survey))

#plot experience distribution

plot.experience.country <- ggplot(survey,
                           aes(x=experience)
                           )+
                             geom_histogram()+
                             facet_wrap(~country.code,scales="free_y")+
                             opts(title="Survey user experience histogram by country",
                                  axis.text.x=theme_text(
                                    angle=90, hjust=1, size=6)
                                  )+
                            labs(x="experience")
print(plot.experience.country)

#plot age distribution

plot.age.country <- ggplot(survey,
                           aes(x=age)
                           )+
                             geom_histogram()+
                             facet_wrap(~country.code,scales="free_y")+
                             opts(title="Survey user age histogram by country",
                               axis.text.x=theme_text(
                                 angle=90, hjust=1, size=6)
                                  )+labs(x="age")
print(plot.age.country)

#plot industry distribution

plot.industry.country <- ggplot(survey,
                           aes(x=industry)
                           )+
                             geom_histogram()+
                             facet_wrap(~country.code,scales="free_y")+
                             opts(title="Survey user industry histogram by country and region",
                                  axis.text.x=theme_text(
                                    angle=90,hjust=1,size=6)
                                  )+labs(x="industry")
print(plot.industry.country)

#plot occupation distribution

plot.occupation.country <- ggplot(survey,
                                aes(x=occupation)
                                )+
                                  geom_histogram()+
                                  facet_wrap(~country.code,scales="free_y")+
                                  opts(title="Survey user occupation histogram by country and region",
                                       axis.text.x=theme_text(
                                         angle=90,hjust=1,size=6)
                                       )+labs(x="occupation")
print(plot.occupation.country)

#plot reputation histogram
plot.rep.country <- ggplot(survey,
                                  aes(x=rep)
                                  )+
                                    geom_histogram()+
                                    facet_wrap(~country.code,scales="free_y")+
                                    opts(title="Survey user reputation histogram by country and region",
                                         axis.text.x=theme_text(
                                           angle=90,hjust=1,size=6)
                                         )+labs(x="reputation")
print(plot.rep.country)

#create a data frame with langauges
survey.lan <- data.frame(
#                          survey$user.id,
                         survey$country.code,
                         survey$java,
                         survey$javascript,
                         survey$php,
                         survey$css,
                         survey$python,
                         survey$obC,
                         survey$ruby,
                         survey$sql,
                         survey$c,
                         survey$C,
                         survey$c..,
                         survey$perl,
                         survey$html5,
                         survey$other)

names(survey.lan) <- c("country.code",
                       "java",
                       "javascript",
                       "php",
                       "css",
                       "python",
                       "obC",
                       "ruby",
                       "sql",
                       "c",
                       "C",
                       "c..",
                       "$perl",
                       "html5",
                       "other")

survey.lan.melt <- melt(survey.lan,id.vars="country.code")

plot.language.country <- ggplot(drop.levels(survey.lan.melt[survey.lan.melt$value==1,]),
                                  aes(x=variable)
                                  )+
                                    geom_histogram()+
                                    facet_wrap( ~ country.code,scales="free_y")+
                                    opts(title="Survey user programming languages/tools histogram by country",
                                         axis.text.x=theme_text(
                                           angle=90, hjust=1, size=6)
                                         )+labs(x="programming langauges/tools")
print(plot.language.country)


#plot histograms


#calculate percentage
# survey.user.counts <- data.frame(tapply(rep(1, nrow(survey)),survey$country.code,sum))
# survey.user.counts$country.code <-rownames(survey.user.counts)
# names(survey.user.counts) <- c("total.user","country.code")
# 
# survey <- merge(survey,survey.user.counts,by="country.code",all=FALSE)
# 
# #plot histograms of percentage
# plot.age.percent.country <- ggplot(survey,
#                            aes(x=age/total.user)
#                            )+
#                              geom_histogram()+
#                              facet_wrap(~country.code,scales="free_y")+
#                              opts(title="survey user age as a share of total user histogram by country",
#                                   axis.text.x=theme_text(
#                                     angle=90, hjust=1, size=6)
#                                   )
# print(plot.age.percent.country)

