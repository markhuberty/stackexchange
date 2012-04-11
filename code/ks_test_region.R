#pairwise ks test by region
library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)

#load user data

user <-
  read.csv("C:/Users/miaomiaocui/stackexchange/data/user.csv",
           header=TRUE)

#reformat date

#take out CreatD and redo it because it's now a factor
user$CreatD <- NULL

#format date
format.date <- function(d){
  d<- str_split(d,"T")
  d<- sapply(d,function(x){x[[1]][1]})
  d<- as.Date(d)
  return(d)
}

user$CreatD <- format.date(user$CreationDate)

#add creation date
user$creat.date<-as.integer(user$CreatD)

#drop levels to get user.sub
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

user.sub <- drop.levels(user[user$country.code
                             %in% country.sub,])

#subset into regions
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
user.sub$region <- factor(user.sub$region,levels=c("LME:US,CA,GB,AU,NZ,IE",
                                                   "SCAND:SE,NO,DK,FI",
                                                   "CME:DE,AU,JP,BE,FR,IT,NL,CH",
                                                   "Other countries"))
#pairwise ks-test

compute.pairwise.ks <- function(variable,factor){
  unique.factor <- levels(factor)
  out <- c()
  i.vec <- c()
  j.vec <- c()
  for(i in 1:length(unique.factor))
  {
    for(j in 1:length(unique.factor))
    {kstest<-ks.test(variable[factor==unique.factor[i]],
                     variable[factor==unique.factor[j]],
                     paired=FALSE)
     kstest.p <- kstest$p.value
     out <- append (out,kstest.p)
     i.vec <- append(i.vec,unique.factor[i])
     j.vec <- append(j.vec,unique.factor[j])
    }
  }
  
  dft.out <- data.frame(i.vec,j.vec,out)
  dft.out <- dft.out[dft.out$i.vec !=
    dft.out$j.vec,]
  dft.out$out <- round(dft.out$out,4)
  return(dft.out)
}


#pairwise ks test reputation
ks.test.user.sub.rep.region <- compute.pairwise.ks(log(user.sub$Reputation),
                                            user.sub$region)
ks.test.user.sub.rep.region$pvalue<- t.test.user.sub.rep.region$out<0.1

plot.pairwise.rep.mean.ks.region <- ggplot(ks.test.user.sub.rep.region,
                                    aes(x=i.vec,
                                        y=j.vec,
                                        fill=pvalue
                                        )
                                    )+
                                      geom_tile()+
                                      scale_x_discrete("Region")+
                                      scale_y_discrete("Region")+
                                      opts(title="Pairwise significance of difference (p<0.1) in user reputation (log) distribution
                                           by region",
                                           axis.text.x=theme_text(size=8),
                                           axis.text.y=theme_text(size=8))

print(plot.pairwise.rep.mean.ks.region)

#pairwise test upvote
ks.test.user.sub.up.region <- compute.pairwise.ks(log(user.sub$UpVotes),
                                           user.sub$region)
ks.test.user.sub.up.region$pvalue <- ks.test.user.sub.up.region$out<0.1

plot.pairwise.up.mean.ks.region <- ggplot(ks.test.user.sub.up.region,
                                   aes(x=i.vec,
                                       y=j.vec,
                                       fill=pvalue
                                       )
                                   )+
                                     geom_tile()+
                                     scale_x_discrete("Region")+
                                     scale_y_discrete("Region")+
                                     opts(title="Pairwise significance of difference (p<0.1) in user up votes (log) distribution
                                          by region",
                                          axis.text.x=theme_text(size=8),
                                          axis.text.y=theme_text(size=8))

print(plot.pairwise.up.mean.ks.region)

#pairwise test down vote
ks.test.user.sub.dn.region <- compute.pairwise.ks(log(user.sub$DownVotes),
                                           user.sub$region)
ks.test.user.sub.dn.region$pvalue <- ks.test.user.sub.dn.region$out<0.1

plot.pairwise.dn.mean.ks.region <- ggplot(ks.test.user.sub.dn.region,
                                   aes(x=i.vec,
                                       y=j.vec,
                                       fill=pvalue
                                       )
                                   )+
                                     geom_tile()+
                                     scale_x_discrete("Region")+
                                     scale_y_discrete("Region")+
                                     opts(title="Pairwise significance of difference (p<0.1) in user down votes (log) distribution
                                          by region",
                                          axis.text.x=theme_text(size=8),
                                          axis.text.y=theme_text(size=8))

print(plot.pairwise.dn.mean.ks.region)

#pairwise test duration

ks.test.user.sub.duration.region <- compute.pairwise.ks(user.sub$duration,
                                                 user.sub$region)
ks.test.user.sub.duration.region$pvalue <- ks.test.user.sub.duration.region$out<0.1

plot.pairwise.duration.mean.ks.region <- ggplot(ks.test.user.sub.duration.region,
                                         aes(x=i.vec,
                                             y=j.vec,
                                             fill=pvalue
                                             )
                                         )+
                                           geom_tile()+
                                           scale_x_discrete("Region")+
                                           scale_y_discrete("Region")+
                                           opts(title="Pairwise significance of difference (p<0.1) in distribution of account-keeping duration
                                                by region",
                                                axis.text.x=theme_text(size=8),
                                                axis.text.y=theme_text(size=8))

print(plot.pairwise.duration.mean.ks.region)

#creation date
ks.test.user.sub.creatd.r<- compute.pairwise.ks(as.integer(user.sub$CreatD),
                                               user.sub$region)
ks.test.user.sub.creatd.r$pvalue <- ks.test.user.sub.creatd.r$out<0.1

plot.pairwise.creatd.mean.ks.r <- ggplot(ks.test.user.sub.creatd.r,
                                       aes(x=i.vec,
                                           y=j.vec,
                                           fill=pvalue
                                           )
                                       )+
                                         geom_tile()+
                                         scale_x_discrete("Region")+
                                         scale_y_discrete("Region")+
                                         opts(title="Pairwise significance of difference (p<0.1) in distribution of user account creation date
                                              by region",
                                              axis.text.x=theme_text(size=8),
                                              axis.text.y=theme_text(size=8))

print(plot.pairwise.creatd.mean.ks.r)

#last access date
ks.test.user.sub.lastacc.r <- compute.pairwise.ks(as.integer(user.sub$LastAccD),
                                                user.sub$region)
ks.test.user.sub.lastacc.r$pvalue <- ks.test.user.sub.lastacc.r$out<0.1

plot.pairwise.lastacc.mean.ks.r <- ggplot(ks.test.user.sub.lastacc.r,
                                        aes(x=i.vec,
                                            y=j.vec,
                                            fill=pvalue
                                            )
                                        )+
                                          geom_tile()+
                                          scale_x_discrete("Region")+
                                          scale_y_discrete("Region")+
                                          opts(title="Pairwise significance of difference (p<0.1) in distribution of user account last access date
                                               by region",
                                               axis.text.x=theme_text(size=8),
                                               axis.text.y=theme_text(size=8))

print(plot.pairwise.lastacc.mean.ks.r)

