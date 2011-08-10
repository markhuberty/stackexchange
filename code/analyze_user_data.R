
if(grepl("mac.binary", .Platform$pkgType, fixed=TRUE))
  {
    setwd("~/Documents/Research/Papers/stackexchange")
  }else{
    setwd("~/Documents/stackexchange")
  }

library(ggplot2)
library(reshape)
library(foreach)

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

      return(rep(NA, 3))
  
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

plot.user.freq <- ggplot(users.df.new,
                         aes(x=country.code)
                         ) +
  geom_bar()

pdf("./figures/plot_user_freq.pdf")
print(plot.user.freq)
dev.off()
