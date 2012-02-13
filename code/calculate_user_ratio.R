#load the labor data
it.b <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/it_share_broad.csv",header=TRUE)
it.n <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/it_share_narrow.csv",header=TRUE)
total.labor <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/labor.csv",header=TRUE)

#merge it_b,it_n, and labor
it.b.df.new <-merge(it.b,it.n,by="country_code",all=FALSE)
new.labor <- merge(it.b.df.new,total.labor,by="country.code",all=FALSE)
names(new.labor) <- c("country.code","country","broad.it.share","country","narrow.it.share","country","total")
new.labor$country <-NULL
new.labor$country <-NULL
new.labor$country <-NULL

#multiply to obtain IT labor force
new.labor$broad.it.labor <- new.labor$broad.it.share*new.labor$total/100
new.labor$narrow.it.labor <- new.labor$narrow.it.share*new.labor$total/100

#load patent data
patent <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/patents_count.csv",header=TRUE)
names(patent) <- c("country_code","2008_patent_counts")


#load user data
user <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/users_geocoded_final.csv",header=TRUE)

#change the user file's country codes to the lower cases
user$country.code <- tolower(user$country.code)

#count user

user.counts <- tapply(rep(1, nrow(user)),user$country.code,sum)
user.counts <-data.frame(names(user.counts),user.counts)
names(user.counts) <- c("country.code","users")



#merge labor and patent into user_count

user.labor <- merge(user.counts,new.labor,by="country.code", all=FALSE)
user.patent <- merge(user.counts,patent,by="country.code",all=FALSE)

user.ratio <-merge(user.labor,user.patent,by="country.code",all=FALSE)

user.ratio$broad.it.share <- NULL
user.ratio$narrow.it.share <-NULL
user.ratio$users.y <-NULL

#clean up column names
names(user.ratio) <- c("country.code","user.counts","total.labor","broad.it.labor","narrow.it.labor","patent.counts")

#calculate ratios
user.ratio$user.to.total.labor <- user.ratio$user.counts/user.ratio$total.labor
user.ratio$user.to.broad.labor <- user.ratio$user.counts/user.ratio$broad.it.labor
user.ratio$user.to.narrow.labor <- user.ratio$user.counts/user.ratio$narrow.it.labor
user.ratio$user.to.patent <- user.ratio$user.counts/user.ratio$patent.counts

#export
write.csv(user.ratio,file="C:/Users/miaomiaocui/Documents/teSt/stackexchange/data/user_ratio.csv",row.names=FALSE)

#get the statistics
summary(user.ratio)



