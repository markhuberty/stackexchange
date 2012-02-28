library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)

#pairwise t-test

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
ks.test.user.sub.rep <- compute.pairwise.ks(log(user.sub$Reputation),
                                          user.sub$country.code)
ks.test.user.sub.rep$sig.p <- t.test.user.sub.rep$out<0.1

plot.pairwise.rep.mean.ks <- ggplot(ks.test.user.sub.rep,
                                   aes(x=i.vec,
                                       y=j.vec,
                                       fill=sig.p
                                       )
                                   )+
                                     geom_tile()+
                                     scale_x_discrete("Country")+
                                     scale_y_discrete("Country")+
                                     opts(title="Pairwise significance of difference in user reputation (log) distribution",
                                          axis.text.x=theme_text(size=6),
                                          axis.text.y=theme_text(size=6))

print(plot.pairwise.rep.mean.ks)

#pairwise test upvote
ks.test.user.sub.up <- compute.pairwise.ks(log(user.sub$UpVotes),
                                         user.sub$country.code)
ks.test.user.sub.up$sig.p <- ks.test.user.sub.up$out<0.1

plot.pairwise.up.mean.ks <- ggplot(ks.test.user.sub.up,
                                  aes(x=i.vec,
                                      y=j.vec,
                                      fill=sig.p
                                      )
                                  )+
                                    geom_tile()+
                                    scale_x_discrete("Country")+
                                    scale_y_discrete("Country")+
                                    opts(title="Pairwise significance of difference in user up votes (log) distribution",
                                         axis.text.x=theme_text(size=6),
                                         axis.text.y=theme_text(size=6))

print(plot.pairwise.up.mean.ks)

#pairwise test down vote
ks.test.user.sub.dn <- compute.pairwise.ks(log(user.sub$DownVotes),
                                         user.sub$country.code)
ks.test.user.sub.dn$sig.p <- ks.test.user.sub.dn$out<0.1

plot.pairwise.dn.mean.ks <- ggplot(ks.test.user.sub.dn,
                                  aes(x=i.vec,
                                      y=j.vec,
                                      fill=sig.p
                                      )
                                  )+
                                    geom_tile()+
                                    scale_x_discrete("Country")+
                                    scale_y_discrete("Country")+
                                    opts(title="Pairwise significance of difference in user down votes (log) distribution",
                                         axis.text.x=theme_text(size=6),
                                         axis.text.y=theme_text(size=6))

print(plot.pairwise.dn.mean.ks)

#pairwise test duration

ks.test.user.sub.duration <- compute.pairwise.ks(user.sub$duration,
                                               user.sub$country.code)
ks.test.user.sub.duration$sig.p <- ks.test.user.sub.duration$out<0.1

plot.pairwise.duration.mean.ks <- ggplot(ks.test.user.sub.duration,
                                        aes(x=i.vec,
                                            y=j.vec,
                                            fill=sig.p
                                            )
                                        )+
                                          geom_tile()+
                                          scale_x_discrete("Country")+
                                          scale_y_discrete("Country")+
                                          opts(title="Pairwise significance of difference in distribution of user duration between account creation date and last access date",
                                               axis.text.x=theme_text(size=6),
                                               axis.text.y=theme_text(size=6))

print(plot.pairwise.duration.mean.ks)

#creation date
ks.test.user.sub.creatd <- compute.pairwise.ks(as.integer(user.sub$CreatD),
                                                 user.sub$country.code)
ks.test.user.sub.creatd$sig.p <- ks.test.user.sub.creatd$out<0.1

plot.pairwise.creatd.mean.ks <- ggplot(ks.test.user.sub.creatd,
                                         aes(x=i.vec,
                                             y=j.vec,
                                             fill=sig.p
                                             )
                                         )+
                                           geom_tile()+
                                           scale_x_discrete("Country")+
                                           scale_y_discrete("Country")+
                                           opts(title="Pairwise significance of difference in distribution of user account creation date",
                                                axis.text.x=theme_text(size=6),
                                                axis.text.y=theme_text(size=6))

print(plot.pairwise.creatd.mean.ks)

#last access date
ks.test.user.sub.lastacc <- compute.pairwise.ks(as.integer(user.sub$LastAccD),
                                                 user.sub$country.code)
ks.test.user.sub.lastacc$sig.p <- ks.test.user.sub.lastacc$out<0.1

plot.pairwise.lastacc.mean.ks <- ggplot(ks.test.user.sub.lastacc,
                                         aes(x=i.vec,
                                             y=j.vec,
                                             fill=sig.p
                                             )
                                         )+
                                           geom_tile()+
                                           scale_x_discrete("Country")+
                                           scale_y_discrete("Country")+
                                           opts(title="Pairwise significance of difference in distribution of user account last access date",
                                                axis.text.x=theme_text(size=6),
                                                axis.text.y=theme_text(size=6))

print(plot.pairwise.lastacc.mean.ks)

#overall ks-test
compute.overall.ks <- function(variable,factor){
  unique.factor<- levels(factor)
  out <- c()
  i.vec <- c()
  for (i in 1:length(unique.factor))
  {kstest <- ks.test(variable[factor == unique.factor[i]],
                   variable,
                   paired=FALSE)
   kstest.p <- kstest$p.value
   out<-append(out,kstest.p)
   i.vec <- append(i.vec,unique.factor[i])}
  dft.out <- data.frame(i.vec,out)
  dft.out$out<-round(dft.out$out,4)
  dft.out$i.vec <- factor(dft.out$i.vec,
                          levels=levels(dft.out$i.vec)[order(dft.out$out)])
  dft.out$sig.p <- dft.out$out < 0.1
  return(dft.out)
}


#test each reputation against population
ks.test.user.sub.all.rep <- compute.overall.ks(log(user.sub$Reputation),
                                             user.sub$country.code)
ks.test.user.sub.all.rep <- merge(ks.test.user.sub.all.rep,
                                 user.sub.counts,
                                 by.x="i.vec",
                                 by.y="country.code",
                                 all.x=TRUE,
                                 all.y=FALSE)


plot.ks.test.user.sub.all.rep <- ggplot(ks.test.user.sub.all.rep,
                                       aes(x=i.vec,
                                           y=out,
                                           size=log(user.sub)
                                           
                                           ))+
                                             geom_point()+
                                             geom_hline(aes(yintercept=0.1),
                                                        colour="red",
                                                        legend=FALSE)+
                                                          scale_x_discrete("Country")+
                                                          scale_y_continuous("P-values (10% level in red)")+
                                                          opts(title="KS-test P-values, per country user reputation (log) versus all countries",
                                                               axis.text.x=theme_text(size=6))+
                                                                 scale_size("Log user count")

print(plot.ks.test.user.sub.all.rep)


#test duration

ks.test.user.sub.all.duration <- compute.overall.ks(user.sub$duration,
                                                  user.sub$country.code)
ks.test.user.sub.all.duration <- merge(ks.test.user.sub.all.duration,
                                      user.sub.counts,
                                      by.x="i.vec",
                                      by.y="country.code",
                                      all.x=TRUE,
                                      all.y=FALSE)


plot.ks.test.user.sub.all.duration <- ggplot(ks.test.user.sub.all.duration,
                                            aes(x=i.vec,
                                                y=out,
                                                size=log(user.sub)
                                                ))+
                                                  geom_point()+
                                                  geom_hline(aes(yintercept=0.1),
                                                             colour="red",
                                                             legend=FALSE)+
                                                               scale_x_discrete("Country")+
                                                               scale_y_continuous("P-values (10% level in red)")+
                                                               opts(title="KS-test P-values, per country user duration versus all countries",
                                                                    axis.text.x=theme_text(size=6))+
                                                                      scale_size("Log user count")
print(plot.ks.test.user.sub.all.duration)

#plot creation

ks.test.user.sub.all.creatd <- compute.overall.ks(as.integer(user.sub$CreatD),
                                                    user.sub$country.code)
ks.test.user.sub.all.creatd <- merge(ks.test.user.sub.all.creatd,
                                       user.sub.counts,
                                       by.x="i.vec",
                                       by.y="country.code",
                                       all.x=TRUE,
                                       all.y=FALSE)


plot.ks.test.user.sub.all.creatd <- ggplot(ks.test.user.sub.all.creatd,
                                             aes(x=i.vec,
                                                 y=out,
                                                 size=log(user.sub)
                                                 ))+
                                                   geom_point()+
                                                   geom_hline(aes(yintercept=0.1),
                                                              colour="red",
                                                              legend=FALSE)+
                                                                scale_x_discrete("Country")+
                                                                scale_y_continuous("P-values (10% level in red)")+
                                                                opts(title="KS-test P-values, per country user account creation date versus all countries",
                                                                     axis.text.x=theme_text(size=6))+
                                                                       scale_size("Log user count")
print(plot.ks.test.user.sub.all.creatd)

#last access date

ks.test.user.sub.all.lastacc <- compute.overall.ks(as.integer(user.sub$LastAccD),
                                                  user.sub$country.code)
ks.test.user.sub.all.lastacc <- merge(ks.test.user.sub.all.lastacc,
                                     user.sub.counts,
                                     by.x="i.vec",
                                     by.y="country.code",
                                     all.x=TRUE,
                                     all.y=FALSE)


plot.ks.test.user.sub.all.lastacc <- ggplot(ks.test.user.sub.all.lastacc,
                                           aes(x=i.vec,
                                               y=out,
                                               size=log(user.sub)
                                               ))+
                                                 geom_point()+
                                                 geom_hline(aes(yintercept=0.1),
                                                            colour="red",
                                                            legend=FALSE)+
                                                              scale_x_discrete("Country")+
                                                              scale_y_continuous("P-values (10% level in red)")+
                                                              opts(title="KS-test P-values, per country user account last access date versus all countries",
                                                                   axis.text.x=theme_text(size=6))+
                                                                     scale_size("Log user count")
print(plot.ks.test.user.sub.all.lastacc)

#overall t test without US
compute.overall.ks.no.us <- function(variable,factor){
  unique.factor<- levels(factor)
  out <- c()
  i.vec <- c()
  for (i in 1:length(unique.factor))
  {kstest <- ks.test(variable[factor == unique.factor[i]],
                   variable[factor != "US"],
                   paired=FALSE)
   kstest.p <- kstest$p.value
   out<-append(out,kstest.p)
   i.vec <- append(i.vec,unique.factor[i])}
  dft.out <- data.frame(i.vec,out)
  dft.out$out<-round(dft.out$out,4)
  dft.out$i.vec <- factor(dft.out$i.vec,
                          levels=levels(dft.out$i.vec)[order(dft.out$out)])
  dft.out$sig.p <- dft.out$out < 0.1
  return(dft.out)
}

#test each reputation against population
ks.test.user.sub.no.us.rep <- compute.overall.ks.no.us(log(user.sub$Reputation),
                                                     user.sub$country.code)
ks.test.user.sub.no.us.rep <- merge(ks.test.user.sub.no.us.rep,
                                   user.sub.counts,
                                   by.x="i.vec",
                                   by.y="country.code",
                                   all.x=TRUE,
                                   all.y=FALSE)


plot.ks.test.user.sub.no.us.rep <- ggplot(ks.test.user.sub.no.us.rep,
                                         aes(x=i.vec,
                                             y=out,
                                             size=log(user.sub)
                                             
                                             ))+
                                               geom_point()+
                                               geom_hline(aes(yintercept=0.1),
                                                          colour="red",
                                                          legend=FALSE)+
                                                            scale_x_discrete("Country")+
                                                            scale_y_continuous("P-values (10% level in red)")+
                                                            opts(title="KS-test P-values, per country user reputation (log) versus all non-US countries",
                                                                 axis.text.x=theme_text(size=6))+
                                                                   scale_size("Log user count")

print(plot.ks.test.user.sub.no.us.rep)

#duration

ks.test.user.sub.no.us.duration <- compute.overall.ks.no.us(user.sub$duration,
                                                          user.sub$country.code)
ks.test.user.sub.no.us.duration <- merge(ks.test.user.sub.no.us.duration,
                                        user.sub.counts,
                                        by.x="i.vec",
                                        by.y="country.code",
                                        all.x=TRUE,
                                        all.y=FALSE)

plot.ks.test.user.sub.no.us.duration <- ggplot(ks.test.user.sub.no.us.duration,
                                              aes(x=i.vec,
                                                  y=out,
                                                  size=log(user.sub)
                                                  
                                                  ))+
                                                    geom_point()+
                                                    geom_hline(aes(yintercept=0.1),
                                                               colour="red",
                                                               legend=FALSE)+
                                                                 scale_x_discrete("Country")+
                                                                 scale_y_continuous("P-values (10% level in red)")+
                                                                 opts(title="KS-test P-values, per country user duration versus all non-US countries",
                                                                      axis.text.x=theme_text(size=6))+
                                                                        scale_size("Log user count")

print(plot.ks.test.user.sub.no.us.duration)

#plot creation

ks.test.user.sub.no.us.creatd <- compute.overall.ks.no.us(as.integer(user.sub$CreatD),
                                                  user.sub$country.code)
ks.test.user.sub.no.us.creatd <- merge(ks.test.user.sub.no.us.creatd,
                                     user.sub.counts,
                                     by.x="i.vec",
                                     by.y="country.code",
                                     all.x=TRUE,
                                     all.y=FALSE)


plot.ks.test.user.sub.no.us.creatd <- ggplot(ks.test.user.sub.no.us.creatd,
                                           aes(x=i.vec,
                                               y=out,
                                               size=log(user.sub)
                                               ))+
                                                 geom_point()+
                                                 geom_hline(aes(yintercept=0.1),
                                                            colour="red",
                                                            legend=FALSE)+
                                                              scale_x_discrete("Country")+
                                                              scale_y_continuous("P-values (10% level in red)")+
                                                              opts(title="KS-test P-values, per country user account creation date versus all non-US countries",
                                                                   axis.text.x=theme_text(size=6))+
                                                                     scale_size("Log user count")
print(plot.ks.test.user.sub.no.us.creatd)

#last access date

ks.test.user.sub.no.us.lastacc <- compute.overall.ks.no.us(as.integer(user.sub$LastAccD),
                                                   user.sub$country.code)
ks.test.user.sub.no.us.lastacc <- merge(ks.test.user.sub.no.us.lastacc,
                                      user.sub.counts,
                                      by.x="i.vec",
                                      by.y="country.code",
                                      all.x=TRUE,
                                      all.y=FALSE)


plot.ks.test.user.sub.no.us.lastacc <- ggplot(ks.test.user.sub.no.us.lastacc,
                                            aes(x=i.vec,
                                                y=out,
                                                size=log(user.sub)
                                                ))+
                                                  geom_point()+
                                                  geom_hline(aes(yintercept=0.1),
                                                             colour="red",
                                                             legend=FALSE)+
                                                               scale_x_discrete("Country")+
                                                               scale_y_continuous("P-values (10% level in red)")+
                                                               opts(title="KS-test P-values, per country user account last access date versus all non-US countries",
                                                                    axis.text.x=theme_text(size=6))+
                                                                      scale_size("Log user count")
print(plot.ks.test.user.sub.no.us.lastacc)
