#load the labor data
it_b <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/it_share_broad.csv",header=TRUE)
it_n <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/it_share_narrow.csv",header=TRUE)
total_labor <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/labor.csv",header=TRUE)

#merge it_b,it_n, and labor
it_b.df.new <-merge(it_b,it_n,by="country_code",all=FALSE)
new_labor <- merge(it_b.df.new,total_labor,by="country_code",all=FALSE)
names(new_labor) <- c("country_code","country","broad_it_share","country","narrow_it_share","country","total")
new_labor$country <-NULL
new_labor$country <-NULL
new_labor$country <-NULL

#multiply to obtain IT labor force
new_labor$broad_it_labor <- new_labor$broad_it_share*new_labor$total/100
new_labor$narrow_it_labor <- new_labor$narrow_it_share*new_labor$total/100

#load patent data
patent <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/patents_count.csv",header=TRUE)
names(patent) <- c("country_code","2008_patent_counts")


#load user data
user <- read.csv("C:/Users/miaomiaocui/Documents/test/stackexchange/data/users_geocoded_final.csv",header=TRUE)

#change the user file's country codes to the lower cases
user$country.code <- tolower(user$country.code)

#count user

user_counts <- tapply(rep(1, nrow(user)),user$country.code,sum)
user_counts <-data.frame(names(user_counts),user_counts)
names(user_counts) <- c("country_code","users")



#merge labor and patent into user_count

user_labor <- merge(user_counts,new_labor,by="country_code", all=FALSE)
user_patent <- merge(user_counts,patent,by="country_code",all=FALSE)

user_ratio <-merge(user_labor,user_patent,by="country_code",all=FALSE)

user_ratio$broad_it_share <- NULL
user_ratio$narrow_it_share <-NULL
user_ratio$users.y <-NULL

#clean up column names
names(user_ratio) <- c("country_code","user_counts","total_labor","broad_it_labor","narrow_it_labor","patent_counts")

#calculate ratios
user_ratio$user_to_total_labor <- user_ratio$user_counts/user_ratio$total_labor
user_ratio$user_to_broad_labor <- user_ratio$user_counts/user_ratio$broad_it_labor
user_ratio$user_to_narrow_labor <- user_ratio$user_counts/user_ratio$narrow_it_labor
user_ratio$user_to_patent <- user_ratio$user_counts/user_ratio$patent_counts

#export
write.csv(user_ratio,file="C:/Users/miaomiaocui/Documents/teSt/stackexchange/data/user_ratio.csv",row.names=FALSE)
