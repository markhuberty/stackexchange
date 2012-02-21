library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)


#load user survey data
survey <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/survey.csv",header=TRUE)

#add binary variables to programming language and tech device 
#(for example, if one user checked C, then that cell is 1)
survey$java <- ifelse(survey$lan.java=="Java",1,0)
survey$javascript <- ifelse(survey$lan.javascript=="JavaScript",1,0)
survey$php <- ifelse(survey$lan.php=="PHP",1,0)
survey$css <- ifelse(survey$lan.css=="CSS",1,0)
survey$python <- ifelse(survey$lan.python=="Python",1,0)
survey$obC <- ifelse(survey$lan.obC=="Objective-C",1,0)
survey$ruby <- ifelse(survey$lan.ruby=="Ruby",1,0)
survey$sql <- ifelse(survey$lan.sql=="SQL",1,0)
survey$c <- ifelse(survey$lan.c.=="C#",1,0)
survey$C <- ifelse(survey$lan.c=="C",1,0)
survey$c.. <- ifelse(survey$lan.c..=="C++",1,0)
survey$perl <- ifelse(survey$lan.perl=="Perl",1,0)
survey$html5 <- ifelse(survey$lan.html5=="HTML5",1,0)

#[IMPORTANT]!!!other very specific tools, but is.null does not seem to work
survey$other <- ifelse(is.null(survey$lan.other),0,1)

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


#plot age distribution

plot.age.country <- ggplot(survey,
                           aes(x=age)
                           )+
                             geom_histogram()+
                             facet_wrap(~country.code,scales="free_y")+
                             opts(title="survey user age histogram by country and region",
                                  axis.text.x=theme_text(size=10)
                                  )
print(plot.age.country)

#plot industry distribution

plot.industry.country <- ggplot(survey,
                           aes(x=industry)
                           )+
                             geom_histogram()+
                             facet_wrap(~country.code,scales="free_y")+
                             opts(title="survey user industry histogram by country and region",
                                  axis.text.x=theme_text(size=10)
                                  )
print(plot.industry.country)

#plot occupation distribution

plot.occupation.country <- ggplot(survey,
                                aes(x=occupation)
                                )+
                                  geom_histogram()+
                                  facet_wrap(~country.code,scales="free_y")+
                                  opts(title="survey user occupation histogram by country and region",
                                       axis.text.x=theme_text(size=10)
                                       )
print(plot.occupation.country)
