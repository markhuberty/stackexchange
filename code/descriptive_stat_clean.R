2




par(mfrow=c(4,1))
plot.mean.rep <- ggplot(user.sub.mean.rep,
                        aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
                            colour=cut))+geom_pointrange()+opts(title="mean.rep")


plot.mean.up <- ggplot(user.sub.mean.up,
                        aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
                            colour=cut))+geom_pointrange()+opts(title="mean.up")

plot.mean.dn <- ggplot(user.sub.mean.dn,
                       aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
                           colour=cut))+geom_pointrange()+opts(title="mean.dn")

plot.mean.diffdate <- ggplot(user.sub.mean.diffdate,
                             aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
                                 colour=cut))+geom_pointrange()+opts(title="mean.diffdate")

par(mfrow=c(4,1))

plot.med.up <- ggplot(user.sub.med.up,
                        aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
                            colour=cut))+geom_pointrange()+opts(title="med.up")

plot.med.rep <- ggplot(user.sub.med.rep,
                       aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
                           colour=cut))+geom_pointrange()+opts(title="med.rep")



plot.med.dn <- ggplot(user.sub.med.dn,
                        aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
                            colour=cut))+geom_pointrange()+opts(title="med.dn")


plot.med.diffdate <- ggplot(user.sub.med.diffdate,
                        aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
                            colour=cut))+geom_pointrange()+opts(title="med.diffdate")
  

# test <- ggplot(user.sub.mean.rep,
#                         aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
#                             colour=cut))+geom_pointrange()+opts(title="mean.rep")
# 
# 
# test+ggplot(user.sub.mean.up,
#                        aes(x=V1,y=V2,ymin=X2.5.,ymax=X97.5.,
#                            colour=cut))+geom_pointrange()+opts(title="mean.up")
