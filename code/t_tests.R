#pairwise two-tailed t-tests with significance 0.1 by country

library(reshape)
library(foreach)
library(stringr)
library(ggplot2)
library(Hmisc)
library(gdata)

#pairwise t-test

compute.pairwise.t <- function(variable,factor){
  unique.factor <- levels(factor)
  out <- c()
  i.vec <- c()
  j.vec <- c()
  for(i in 1:length(unique.factor))
  {
    for(j in 1:length(unique.factor))
    {ttest<-t.test(variable[factor==unique.factor[i]],
                   variable[factor==unique.factor[j]],
                   paired=FALSE)
     ttest.p <- ttest$p.value
     out <- append (out,ttest.p)
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


#pairwise test reputation
t.test.user.sub.rep <- compute.pairwise.t(log(user.sub$Reputation),
                                          user.sub$country.code)
t.test.user.sub.rep$sig.p <- t.test.user.sub.rep$out<0.1

plot.pairwise.rep.mean.t <- ggplot(t.test.user.sub.rep,
                          aes(x=i.vec,
                              y=j.vec,
                              fill=sig.p
                              )
                          )+
                            geom_tile()+
                            scale_x_discrete("Country")+
                            scale_y_discrete("Country")+
                            opts(title="Pairwise significance of difference in user mean reputation (log)",
                                 axis.text.x=theme_text(size=6),
                                 axis.text.y=theme_text(size=6))

print(plot.pairwise.rep.mean.t)

#pairwise test upvote
t.test.user.sub.up <- compute.pairwise.t(log(user.sub$UpVotes),
                                          user.sub$country.code)
t.test.user.sub.up$sig.p <- t.test.user.sub.up$out<0.1

plot.pairwise.up.mean.t <- ggplot(t.test.user.sub.up,
                                   aes(x=i.vec,
                                       y=j.vec,
                                       fill=sig.p
                                       )
                                   )+
                                     geom_tile()+
                                     scale_x_discrete("Country")+
                                     scale_y_discrete("Country")+
                                     opts(title="Pairwise significance of difference in user mean up votes (log)",
                                          axis.text.x=theme_text(size=6),
                                          axis.text.y=theme_text(size=6))

print(plot.pairwise.up.mean.t)

#pairwise test down vote
t.test.user.sub.dn <- compute.pairwise.t(log(user.sub$DownVotes),
                                         user.sub$country.code)
t.test.user.sub.dn$sig.p <- t.test.user.sub.dn$out<0.1

plot.pairwise.dn.mean.t <- ggplot(t.test.user.sub.dn,
                                  aes(x=i.vec,
                                      y=j.vec,
                                      fill=sig.p
                                      )
                                  )+
                                    geom_tile()+
                                    scale_x_discrete("Country")+
                                    scale_y_discrete("Country")+
                                    opts(title="Pairwise significance of difference in user mean down votes (log)",
                                         axis.text.x=theme_text(size=6),
                                         axis.text.y=theme_text(size=6))

print(plot.pairwise.dn.mean.t)

#pairwise test duration

t.test.user.sub.duration <- compute.pairwise.t(user.sub$duration,
                                         user.sub$country.code)
t.test.user.sub.duration$sig.p <- t.test.user.sub.duration$out<0.1

plot.pairwise.duration.mean.t <- ggplot(t.test.user.sub.duration,
                                  aes(x=i.vec,
                                      y=j.vec,
                                      fill=sig.p
                                      )
                                  )+
                                    geom_tile()+
                                    scale_x_discrete("Country")+
                                    scale_y_discrete("Country")+
                                    opts(title="Pairwise significance of difference in user mean duration between account creation date and last access date",
                                         axis.text.x=theme_text(size=6),
                                         axis.text.y=theme_text(size=6))

print(plot.pairwise.duration.mean.t)


#overall t-test
compute.overall.t <- function(variable,factor){
  unique.factor<- levels(factor)
  out <- c()
  i.vec <- c()
  for (i in 1:length(unique.factor))
  {ttest <- t.test(variable[factor == unique.factor[i]],
                   variable,
                   paired=FALSE)
   ttest.p <- ttest$p.value
   out<-append(out,ttest.p)
   i.vec <- append(i.vec,unique.factor[i])}
  dft.out <- data.frame(i.vec,out)
  dft.out$out<-round(dft.out$out,4)
  dft.out$i.vec <- factor(dft.out$i.vec,
                          levels=levels(dft.out$i.vec)[order(dft.out$out)])
  dft.out$sig.p <- dft.out$out < 0.1
  return(dft.out)
}


#test each reputation against population
t.test.user.sub.all.rep <- compute.overall.t(log(user.sub$Reputation),
                                             user.sub$country.code)
t.test.user.sub.all.rep <- merge(t.test.user.sub.all.rep,
                                 user.sub.counts,
                                 by.x="i.vec",
                                 by.y="country.code",
                                 all.x=TRUE,
                                 all.y=FALSE)


plot.t.test.user.sub.all.rep <- ggplot(t.test.user.sub.all.rep,
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
                             opts(title="T-test P-values, per country user reputation (log) versus all countries",
                                  axis.text.x=theme_text(size=6))+
                                    scale_size("Log user count")
                                   
print(plot.t.test.user.sub.all.rep)
                                      

#test duration

t.test.user.sub.all.duration <- compute.overall.t(user.sub$duration,
                                             user.sub$country.code)
t.test.user.sub.all.duration <- merge(t.test.user.sub.all.duration,
                                 user.sub.counts,
                                 by.x="i.vec",
                                 by.y="country.code",
                                 all.x=TRUE,
                                 all.y=FALSE)


plot.t.test.user.sub.all.duration <- ggplot(t.test.user.sub.all.duration,
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
                                                          opts(title="T-test P-values, per country user duration versus all countries",
                                                               axis.text.x=theme_text(size=6))+
                                                                 scale_size("Log user count")
print(plot.t.test.user.sub.all.duration)

#overall t test without US
compute.overall.t.no.us <- function(variable,factor){
  unique.factor<- levels(factor)
  out <- c()
  i.vec <- c()
  for (i in 1:length(unique.factor))
  {ttest <- t.test(variable[factor == unique.factor[i]],
                   variable[factor != "US"],
                   paired=FALSE)
   ttest.p <- ttest$p.value
   out<-append(out,ttest.p)
   i.vec <- append(i.vec,unique.factor[i])}
  dft.out <- data.frame(i.vec,out)
  dft.out$out<-round(dft.out$out,4)
  dft.out$i.vec <- factor(dft.out$i.vec,
                          levels=levels(dft.out$i.vec)[order(dft.out$out)])
  dft.out$sig.p <- dft.out$out < 0.1
  return(dft.out)
}

#test each reputation against population
t.test.user.sub.no.us.rep <- compute.overall.t.no.us(log(user.sub$Reputation),
                                             user.sub$country.code)
t.test.user.sub.no.us.rep <- merge(t.test.user.sub.no.us.rep,
                                 user.sub.counts,
                                 by.x="i.vec",
                                 by.y="country.code",
                                 all.x=TRUE,
                                 all.y=FALSE)


plot.t.test.user.sub.no.us.rep <- ggplot(t.test.user.sub.no.us.rep,
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
                                                          opts(title="T-test P-values, per country user reputation (log) versus all non-US countries",
                                                               axis.text.x=theme_text(size=6))+
                                                                 scale_size("Log user count")

print(plot.t.test.user.sub.no.us.rep)

#duration

t.test.user.sub.no.us.duration <- compute.overall.t.no.us(user.sub$duration,
                                                     user.sub$country.code)
t.test.user.sub.no.us.duration <- merge(t.test.user.sub.no.us.duration,
                                   user.sub.counts,
                                   by.x="i.vec",
                                   by.y="country.code",
                                   all.x=TRUE,
                                   all.y=FALSE)

plot.t.test.user.sub.no.us.duration <- ggplot(t.test.user.sub.no.us.duration,
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
                                                            opts(title="T-test P-values, per country user duration versus all non-US countries",
                                                                 axis.text.x=theme_text(size=6))+
                                                                   scale_size("Log user count")

print(plot.t.test.user.sub.no.us.duration)
