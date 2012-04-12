#Divide user account-keeping duration into 10 deciles
#For each decile, locate countries on (EPI, Reputation)


library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)

#load epi data
epi <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/english_proficiency.csv",
         header=TRUE)

epi$country.code <- toupper(epi$country.code)

#merge duration
#load user data
user <-
  read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/user.csv",
           header=TRUE)

user.epi <- merge(user,epi,
                  by="country.code",
                  all=FALSE)

user.epi$duration.quantile <- cut(user.epi$duration,10)

duration.epi.plot <-ggplot(user.epi,
                           aes(x=epi,
                               y=log(Reputation + 1),
                               group=country.code,
                               label=country.code)
                           ) +
                             stat_summary(fun.data="mean_cl_boot") +
                             opts(title="Countries' EPI (English Proficiency Index) and mean reputation (log)
                                  by decile of user account-keeping duration")+
                                    labs(x="EPI",y="log(reputation+1)")+
                             #geom_text(colour="red", size=2) + 
                            facet_grid(duration.quantile ~ .)
print(duration.epi.plot)
pdf(file="C:/Users/miaomiaocui/stackexchange/figures/new/user_epi_duration.pdf")                        



