#Subset user by regions and plot reputation histograms with bins that resemble the survey category

library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)


lme <- c("US", "AU", "NZ", "IE", "GB", "CA")
cme <- c("DE", "AT", "JP", "BE", "FR", "IT", "NL", "CH", "JP")
scand <- c("DK", "SE", "NO", "FI")
others <- c("AT","BE","BG","CY","CZ","EE","ES","GR","HU","IL","LI","LU","LV",
            "MT","MX","PL","PT","RO","RU","SI","SK")

user.sub$region[user.sub$country.code
                %in% lme]<-"LME:US,CA,GB,AU,NZ,IE"
user.sub$region[user.sub$country.code
                %in% scand]<-"SCAND:SE,NO,DK,FI"
user.sub$region[user.sub$country.code
                %in% cme]<-"CME:DE,AU,JP,BE,FR,IT,NL,CH"
user.sub$region[user.sub$country.code
                %in% others] <- "Other countries"

user.sub$reputation.bin <- cut(user.sub$Reputation,
                               breaks=c(0.9,1.1,50.1,100.1,200.1,500.1,
                                        1000.1,2000.1,3000.1,5000.1,
                                        10000.1,285700.1,max(user.sub$Reputation)+0.1
                                 ))

plot.user.reputation.region <- ggplot(user.sub,
                                            aes(x=reputation.bin,
                                                )
                                            )+
                                              geom_histogram()+
                                              facet_wrap(~region,scales="free_y")+
                                              opts(title="Histogram of user reputation by region",
                                        axis.text.x=theme_text(
                                          angle=90, hjust=1, size=6)
                                                   )+
                                                     labs(x="reputation")
print(plot.user.reputation.region)