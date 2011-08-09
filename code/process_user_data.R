
if(grepl("mac.binary", .Platform$pkgType, fixed=TRUE))
  {
    setwd("~/Documents/Research/Papers/stackexchange")
  }else{
    setwd("~/Documents/stackexchange")
  }
library(XML)
library(foreach)

states <- read.csv("./data/states.csv")

users.file <-
  "./data/stack-dump/stack_dump_apr2011/Content/stackoverflow/stackoverflow_dump_apr2011/users.xml"

users.xml <- xmlTreeParse(users.file, useInternalNodes=TRUE)

users.list <- xmlToList(users.xml)

fields.to.take <- c("Id",
                    "Reputation",
                    "CreationDate",
                    "DisplayName",
                    "LastAccessDate",
                    "WebsiteUrl",
                    "Location",
                    "UpVotes",
                    "DownVotes"
                    )

N <- length(users.list)
users.df <- foreach(i=1:N, .combine="rbind") %do% {

  out <- users.list[[i]][fields.to.take]
  
  return(out)


}

users.df <- as.data.frame(users.df
                          )

col.classes <- c("character",
                 "integer",
                 "character",
                 "character",
                 "character",
                 "character",
                 "character",
                 "integer",
                 "integer"
                 )

for(i in 1:ncol(users.df))
  {

    users.df[,i] <- as.character(users.df[,i])
    class(users.df[,i]) <- col.classes[i]

  }

save(users.df,
     file="./data/users.RData"
     )

users.df.location <- users.df[!is.na(users.df$Location),]

locations <- unique(users.df.location$Location)
locations <- tolower(locations)

## Encode correctly for the POST request
locations <- gsub(" ", "+", locations)
locations <- gsub(",", "", locations)

write.csv(locations, file="./data/unique.locations.csv")

count.location <- tapply(rep(1, nrow(users.df.location)),
                         tolower(users.df.location$Location),
                         sum
                         )
                             
