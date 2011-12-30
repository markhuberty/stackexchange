
if(grepl("mac.binary", .Platform$pkgType, fixed=TRUE))
  {
    setwd("~/Documents/Research/Papers/stackexchange")
  }else{
    setwd("~/Documents/stackexchange")
  }

library(ggplot2)
library(reshape)
library(foreach)
library(stringr)
library(Hmisc)
library(gdata)

geocoded.locations <- read.csv("./data/location_geocoded_final.csv",
                               header=TRUE
                               )
country.codes <- read.csv("./data/iso_country_codes.csv", header=TRUE)
country.codes$country <- tolower(country.codes$country)

## Process the CSV file
geocoded.locations$name <- as.character(geocoded.locations$name)

formatted.locations <- foreach(x=1:nrow(geocoded.locations),
                               .combine="rbind") %do% {

  if(geocoded.locations$type[x] != "Country" &
     geocoded.locations$type[x] != "Continent" &
     geocoded.locations$type[x] != "Supername" &
     geocoded.locations$type[x] != "Ocean")
    {

      country <- str_sub(geocoded.locations$name[x], start = -2)

      return(c(as.character(geocoded.locations$name[x]),
               as.character(geocoded.locations$location[x]),
               as.character(country),
               as.character(geocoded.locations$type[x])
               )
             )
      
    }else if(geocoded.locations$type[x] == "Country"){

      country <- tolower(geocoded.locations$name[x])
      idx <- which(country.codes$country == country)
      country <- country.codes$code[idx]

      return(c(as.character(geocoded.locations$name[x]),
               as.character(geocoded.locations$location[x]),
               as.character(country),
               as.character(geocoded.locations$type[x])
               )
             )
    }else{

      return(rep(NA, 4))
  
    }
}
  

formatted.locations <- as.data.frame(formatted.locations)
names(formatted.locations) <- c("name",
                                "orig.location",
                                "country.code",
                                "type"
                                )

to.merge <- formatted.locations[,c("orig.location", "country.code")]
to.merge <- unique(to.merge)

load("./data/users.RData")
users.df.location <- users.df[!is.na(users.df$Location),]
users.df.location$Location <- tolower(users.df.location$Location)
users.df.location$Location <- gsub(" ", "+", users.df.location$Location)
users.df.location$Location <- gsub(",", "", users.df.location$Location)

users.df.new <- merge(users.df.location,
                      to.merge,
                      by.x="Location",
                      by.y="orig.location",
                      all.x=TRUE,
                      all.y=FALSE
                      )

## Need to get the code / sums and then merge w/ total pop and divide out
iso.code.conversion <- read.csv("./data/iso_country_code_conversion.csv")
wb.total.pop <- read.csv("./data/wb_total_population.csv")

wb.total.pop <- merge(wb.total.pop,
                      iso.code.conversion,
                      by.x="Country.Code",
                      by.y="A3"
                      )
wb.total.pop <- wb.total.pop[,c("A2", "X2008")]

user.country.counts <- tapply(rep(1, nrow(users.df.new)),
                              users.df.new$country.code,
                              sum
                              )
user.country.mean.rep <- tapply(users.df.new$Reputation,
                           users.df.new$country.code,
                           mean
                           )
user.country.sd.rep <- tapply(users.df.new$Reputation,
                              users.df.new$country.code,
                              sd
                              )
user.country.counts <- data.frame(names(user.country.counts),
                                  user.country.counts,
                                  user.country.mean.rep,
                                  user.country.sd.rep
                                  )
names(user.country.counts) <- c("country.code",
                                "user.count",
                                "user.mean.rep",
                                "user.sd.rep"
                                )

user.country.counts <- merge(user.country.counts,
                             wb.total.pop,
                             by.x="country.code",
                             by.y="A2"
                             )
user.country.counts$user.pc <-
  user.country.counts$user.count / user.country.counts$X2008
user.country.counts$pc.quantile <- cut2(user.country.counts$user.pc,
                                        g=5
                                        )
user.country.counts$country.code <-
                             reorder(user.country.counts$country.code,
                                     user.country.counts$user.pc
                                     )
user.country.counts$log.user.pc <- log(user.country.counts$user.pc)


plot.user.freq <- ggplot(users.df.new,
                         aes(x=country.code)
                         ) +
  geom_bar()

pdf("./figures/plot_user_freq.pdf")
print(plot.user.freq)
dev.off()

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
## 27 eu members + us, ca, au, nz, ch, no, il = 34                 
                 

user.country.counts.sub <-
  drop.levels(user.country.counts[user.country.counts$country.code
  %in% country.sub,])

user.country.counts.sub$country.code <-
  factor(user.country.counts.sub$country.code,
         levels=user.country.counts.sub$country.code[order(user.country.counts.sub$log.user.pc)])

plot.user.freq.pc <-
                            ggplot(user.country.counts.sub,
                                   aes(x=country.code,
                                       y=log.user.pc
                                       )
                                   ) +
  geom_point() +
  scale_y_continuous() + 
  opts(axis.text.x=theme_text(size=4)) 

pdf("./figures/plot_user_freq_pc.pdf")
print(plot.user.freq.pc)
dev.off()


## Look at the distribution of user reputation in aggregate
## across the countries of interest

country.count.sub <- 
  user.country.counts.sub$country.code[user.country.counts.sub$user.count > 100]

users.df.new.sub <- drop.levels(users.df.new[users.df.new$country.code
                                             %in% country.count.sub,])


format.date <- function(d){

  d <- str_split(d, "T")
  d <- sapply(d, function(x){x[[1]][1]})
  d <- as.Date(d)
  return(d)

}

access.date <- format.date(users.df.new.sub$LastAccessDate)
creation.date <- format.date(users.df.new.sub$CreationDate)
users.df.new.sub$LastAccessDate <- access.date
users.df.new.sub$CreationDate <- creation.date

plot.user.access.date <- ggplot(users.df.new.sub,
                                aes(x=LastAccessDate)
                                ) +
  geom_density(aes(group=country.code)) +
  geom_density(colour="red")

pdf("./figures/plot_user_access_date_density.pdf")
print(plot.user.access.date)
dev.off()

plot.user.creation.date <- ggplot(users.df.new.sub,
                                aes(x=CreationDate)
                                ) +
  geom_density(aes(group=country.code)) +
  geom_density(colour="red")

pdf("./figures/plot_user_creation_date_density.pdf")
print(plot.user.creation.date)
dev.off()

plot.falloff.date <- ggplot(users.df.new.sub,
                            aes(x=CreationDate,
                                y=LastAccessDate,
                                colour=country.code
                                )
                            ) +
  geom_point(alpha=0.5, size=1)

pdf("./figures/plot_falloff_date.pdf")
print(plot.falloff.date)
dev.off()



plot.user.rep <- ggplot(users.df.new.sub,
                        aes(x=log(Reputation)
                            )
                        ) +
  geom_density(aes(group=country.code)) +
  geom_density(colour="red")

pdf("./figures/plot_user_rep.pdf")
print(plot.user.rep)
dev.off()

## Would want to do pairwise KS tests on the distributions
## to see if they are in fact different...but different
## from what? Undifferentiated density?

## Functions to compute pairwise and aggregate ks tests
## by country / other grouping for the reputation data
compute.pairwise.ks <- function(variable, factor){

  unique.factor <- levels(factor)
  out <- c()
  i.vec <- c()
  j.vec <- c()
  for(i in 1:length(unique.factor))
    {
      for(j in i:length(unique.factor))
        {

          ks <- ks.test(variable[factor == unique.factor[i]],
                        variable[factor == unique.factor[j]]
                        )
          ks.p <- ks$p.value
          out <- append(out, ks.p)
          i.vec <- append(i.vec, unique.factor[i])
          j.vec <- append(j.vec, unique.factor[j])
          
        }
    }

  df.out <- data.frame(i.vec, j.vec, out)

  return(df.out)

}

compute.overall.ks <- function(variable, factor){

  unique.factor <- levels(factor)
  out <- c()
  i.vec <- c()
  for(i in 1:length(unique.factor))
    {
      ## Here, drop the US from the reference case b/c the US accounts
      ## for ~ 50% of the overall population and might skew results.
      ks <- ks.test(variable[factor != "US"], variable[factor == unique.factor[i]])
      ks.p <- ks$p.value
      out <- append(out, ks.p)
      i.vec <- append(i.vec, unique.factor[i])

    }

  df.out <- data.frame(i.vec, out)
  return(df.out)
  

}

## Test each distribution against all others to determine
## exchangeability among pairwise countries
test <- compute.pairwise.ks(users.df.new.sub$Reputation,
                            users.df.new.sub$country.code
                            )
test <- test[test$i.vec != test$j.vec,]
test$out <- round(test$out, 4)
test$sig.p <- test$out < 0.1

plot.pairwise.ks <- ggplot(test,
                           aes(x=i.vec,
                               y=j.vec,
                               fill=sig.p
                               )
                           ) +
  geom_tile() +
  opts(axis.text.x=theme_text(size=6),
       axis.text.y=theme_text(size=6)
       )

pdf("./figures/plot_pairwise_ks_reputation_test.pdf")
print(plot.pairwise.ks)
dev.off()

## Test each distribution against the overall distribution
## to determine if they are exchangeable
test.all <- compute.overall.ks(users.df.new.sub$Reputation,
                               users.df.new.sub$country.code
                               )

## Reorder the countries by the p-values
test.all$i.vec <- factor(test.all$i.vec,
                         levels=levels(test.all$i.vec)[order(test.all$out)]
                         )

## Merge in the other by-country summary data
test.all <- merge(test.all,
                  user.country.counts.sub,
                  by.x="i.vec",
                  by.y="country.code",
                  all.x=TRUE,
                  all.y=FALSE
                  )

## Plot the p-values by country, showing user count
## and mean user repuration by country as well
plot.all.ks <- ggplot(test.all,
                      aes(x=i.vec,
                          y=out,
                          size=log10(user.count),
                          colour=user.mean.rep
                          )
                      ) +
  geom_point() +
  geom_hline(aes(yintercept=0.1), colour="red", legend=FALSE) +
  opts(axis.text.x=theme_text(size=6))

pdf("./figures/plot_all_ks_reputation_test.pdf")
print(plot.all.ks)
dev.off()


ks.test.creation.date <-
  compute.overall.ks(as.integer(users.df.new.sub$CreationDate),
                     users.df.new.sub$country.code
                     )
ks.test.creation.date <- merge(ks.test.creation.date,
                               user.country.counts.sub,
                               by.x="i.vec",
                               by.y="country.code",
                               all.x=TRUE,
                               all.y=FALSE
                               )

plot.all.ks <- ggplot(ks.test.creation.date,
                      aes(x=i.vec,
                          y=out,
                          size=log10(user.count),
                          colour=user.mean.rep
                          )
                      ) +
  geom_point() +
  geom_hline(aes(yintercept=0.1), colour="red", legend=FALSE) +
  opts(axis.text.x=theme_text(size=6))

pdf("./figures/plot_ks_creation_date_test.pdf")
print(plot.all.ks)
dev.off()


ks.test.access.date <-
  compute.overall.ks(as.integer(users.df.new.sub$LastAccessDate),
                     users.df.new.sub$country.code
                     )
ks.test.access.date <- merge(ks.test.access.date,
                               user.country.counts.sub,
                               by.x="i.vec",
                               by.y="country.code",
                               all.x=TRUE,
                               all.y=FALSE
                               )

plot.all.ks <- ggplot(ks.test.access.date,
                      aes(x=i.vec,
                          y=out,
                          size=log10(user.count),
                          colour=user.mean.rep
                          )
                      ) +
  geom_point() +
  geom_hline(aes(yintercept=0.1), colour="red", legend=FALSE) +
  opts(axis.text.x=theme_text(size=6))

pdf("./figures/plot_ks_access_date_test.pdf")
print(plot.all.ks)
dev.off()


l <- levels(users.df.new.sub$country.code)
m <- tapply(users.df.new.sub$Reputation,
            users.df.new.sub$country.code,
            mean
            )
users.df.new.sub$country.code <-
  factor(users.df.new.sub$country.code,
         levels=l[order(m)]
         )

plot.mean.reputation <- ggplot(users.df.new.sub,
                               aes(x=country.code,
                                   y=Reputation
                                   )
                               ) +
  stat_summary(fun.y="mean", geom="point", colour="black") + 
  stat_summary(fun.data="mean_cl_boot", geom="linerange", colour="red") +
  coord_cartesian(ylim=c(0,2000)) +
  scale_y_continuous() +
  opts(axis.text.x=theme_text(size=5))

pdf("./figures/plot_mean_reputation.pdf")
print(plot.mean.reputation)
dev.off()
