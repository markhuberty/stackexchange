
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

## Define some useful functions
title.wrapper <- wrapper <- function(x, ...){
  paste(strwrap(x, ...), collapse = "\n")
}

## Load in the geocoded locations from the yahoo
## geocode file
geocoded.locations <- read.csv("./data/location_geocoded_final.csv",
                               header=TRUE
                               )

## Load in the ISO country code file
country.codes <- read.csv("./data/iso_country_codes.csv", header=TRUE)
country.codes$country <- tolower(country.codes$country)

## Process the CSV file
geocoded.locations$name <- as.character(geocoded.locations$name)

## Dump locations that dont contain countries
## Then grab the 2-character country code from the
## end of the location name
formatted.locations <- foreach(x=1:nrow(geocoded.locations),
                               .combine="rbind") %do% {

  if(geocoded.locations$type[x] != "Country" &
     geocoded.locations$type[x] != "Continent" &
     geocoded.locations$type[x] != "Supername" &
     geocoded.locations$type[x] != "Ocean")
    {

      ## This is the country code grab
      country <- str_sub(geocoded.locations$name[x], start = -2)

      return(c(as.character(geocoded.locations$name[x]),
               as.character(geocoded.locations$location[x]),
               as.character(country),
               as.character(geocoded.locations$type[x])
               )
             )
      ## If only country is returned, crosswalk the country name
      ## to the country code
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
  
## With the location data, merge it into the big
## users file
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
## And write it out.
write.csv(users.df.new, file="./data/users_geocoded_final.csv",
          row.names=FALSE,
          )

## Look at users per-capita based on the world bank population data
## Need to get the code / sums and then merge w/ total pop and divide out
iso.code.conversion <- read.csv("./data/iso_country_code_conversion.csv",
                                header=TRUE
                                )
wb.total.pop <- read.csv("./data/wb_total_population.csv")

## add the iso codes to the world bank codes
wb.total.pop <- merge(wb.total.pop,
                      iso.code.conversion,
                      by.x="Country.Code",
                      by.y="A3"
                      )
wb.total.pop <- wb.total.pop[,c("A2", "X2008")]

## Count users by country
user.country.counts <- tapply(rep(1, nrow(users.df.new)),
                              users.df.new$country.code,
                              sum
                              )
## Average user rep by country
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
## Merge in the world bank data with the
## user data on the basis of the country code
user.country.counts <- merge(user.country.counts,
                             wb.total.pop,
                             by.x="country.code",
                             by.y="A2"
                             )
## Get users per capita; calculate log users per capita 
user.country.counts$user.pc <-
  user.country.counts$user.count / user.country.counts$X2008
user.country.counts$pc.quantile <- cut2(user.country.counts$user.pc,
                                        g=5
                                        )

## Trick: reorder the country code factor levels based on
## users per-capita; this comes in handy further down
user.country.counts$country.code <-
                             reorder(user.country.counts$country.code,
                                     user.country.counts$user.pc
                                     )
user.country.counts$log.user.pc <- log(user.country.counts$user.pc)

## Subset countries I care about
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
## 27 eu members + us, ca, au, nz, ch, no, il ru = 35                 
                 

user.country.counts.sub <-
  drop.levels(user.country.counts[user.country.counts$country.code
  %in% country.sub,])

user.country.counts.sub$country.code <-
  factor(user.country.counts.sub$country.code,
         levels=user.country.counts.sub$country.code[order(user.country.counts.sub$log.user.pc)])

## Plot the user per capita data
plot.user.freq.pc <-
  ggplot(user.country.counts.sub,
         aes(x=country.code,
             y=log.user.pc
             )
         ) +
  geom_point() +
  scale_y_continuous("Log users per capita") +
  scale_x_discrete("Country") + 
  opts(title=title.wrapper("Per-capita users by country", width=80),
       axis.text.x=theme_text(size=4)) 

pdf("./figures/plot_user_freq_pc.pdf")
print(plot.user.freq.pc)
dev.off()


## Look at the distribution of user reputation in aggregate
## across the countries of interest

country.count.sub <- 
  user.country.counts.sub$country.code[user.country.counts.sub$user.count > 100]

users.df.new.sub <- drop.levels(users.df.new[users.df.new$country.code
                                             %in% country.count.sub,])

## Stuff below looks at other covariates, not directly relevant.
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
  geom_density(colour="red") +
  scale_y_continuous("Density") +
  scale_x_date("User last access date") +
  opts(title=title.wrapper("PDF of user last access date for each country (black) and whole population (red)", width=80)
       )

plot.user.creation.date <- ggplot(users.df.new.sub,
                                  aes(x=CreationDate)
                                  ) +
  geom_density(aes(group=country.code)) +
  geom_density(colour="red") +
  scale_y_continuous("Density") +
  scale_x_date("User creation date") +
  opts(title=title.wrapper("PDF of user creation date for each country (black) and whole population (red)", width=80)
       )

pdf("./figures/plot_user_access_date_density.pdf")
print(plot.user.access.date)
dev.off()

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
  geom_density(colour="red") +
  scale_y_continuous("Density") +
  scale_x_continuous("log(Reputation)") +
  opts(title=title.wrapper("PDF of user reputation for each country (black) and whole population (red)", width=80)
       )

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
ks.test.user.rep <- compute.pairwise.ks(users.df.new.sub$Reputation,
                                        users.df.new.sub$country.code
                                        )
ks.test.user.rep <- ks.test.user.rep[ks.test.user.rep$i.vec !=
                                     ks.test.user.rep$j.vec,
                                     ]
ks.test.user.rep$out <- round(ks.test.user.rep$out, 4)
ks.test.user.rep$sig.p <- ks.test.user.rep$out < 0.1

plot.pairwise.ks <- ggplot(ks.test.user.rep,
                           aes(x=i.vec,
                               y=j.vec,
                               fill=sig.p
                               )
                           ) +
  geom_tile() +
  scale_x_discrete("Country") +
  scale_y_discrete("Country") +
  opts(title=title.wrapper("Pairwise significance of difference in user reputation distribution", width=80),
       axis.text.x=theme_text(size=6),
       axis.text.y=theme_text(size=6)
       )

pdf("./figures/plot_pairwise_ks_reputation_test.pdf")
print(plot.pairwise.ks)
dev.off()

## Test each distribution against the overall distribution
## to determine if they are exchangeable
ks.test.user.rep.overall <- compute.overall.ks(users.df.new.sub$Reputation,
                                               users.df.new.sub$country.code
                                               )

## Reorder the countries by the p-values
ks.test.user.rep.overall$i.vec <- factor(ks.test.user.rep.overall$i.vec,
                                         levels=levels(ks.test.user.rep.overall$i.vec)[order(ks.test.user.rep.overall$out)]
                                         )

## Merge in the other by-country summary data
ks.test.user.rep.overall <- merge(ks.test.user.rep.overall,
                                  user.country.counts.sub,
                                  by.x="i.vec",
                                  by.y="country.code",
                                  all.x=TRUE,
                                  all.y=FALSE
                                  )

## Plot the p-values by country, showing user count
## and mean user repuration by country as well
plot.all.ks <- ggplot(ks.test.user.rep.overall,
                      aes(x=i.vec,
                          y=out,
                          size=log10(user.count),
                          colour=user.mean.rep
                          )
                      ) +
  geom_point() +
  geom_hline(aes(yintercept=0.1), colour="red", legend=FALSE) +
  scale_x_discrete("Country") +
  scale_y_continuous("P-value (10% level in red)") + 
  opts(title=title.wrapper("KS-test p-values, country user reputation distribution versus all users", width=80),
       axis.text.x=theme_text(size=6)
       ) +
  scale_size("Log user count") +
  scale_colour_gradient("Mean user reputation")

pdf("./figures/plot_all_ks_reputation_test.pdf")
print(plot.all.ks)
dev.off()

## Test differences in the distribution of access and creation date
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

plot.ks.creation.date <- ggplot(ks.test.creation.date,
                                aes(x=i.vec,
                                    y=out,
                                    size=log10(user.count),
                                    colour=user.mean.rep
                                    )
                                ) +
  geom_point() +
  geom_hline(aes(yintercept=0.1), colour="red", legend=FALSE) +
  scale_x_discrete("Country") +
  scale_y_continuous("P-value (10% level in red)") +
  scale_size("Log user count") +
  scale_colour_gradient("Mean user reputation") + 
  opts(title=title.wrapper("KS-test p-values, country user creation date distribution versus all users", width=80),
       axis.text.x=theme_text(size=6)
       )

pdf("./figures/plot_ks_creation_date_test.pdf")
print(plot.ks.creation.date)
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
  scale_x_discrete("Country") +
  scale_y_continuous("P-value (10% level in red)") + 
  opts(title=title.wrapper("KS-test p-values, country user last access date distribution versus all users"),
       axis.text.x=theme_text(size=6))

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
  scale_x_discrete("Country") + 
  scale_y_continuous("User reputation") +
  coord_cartesian(ylim=c(0,2000)) +
  opts(title=title.wrapper("Mean user reputation and variance by country", width=80),
       axis.text.x=theme_text(size=5)
       )

pdf("./figures/plot_mean_reputation.pdf")
print(plot.mean.reputation)
dev.off()
