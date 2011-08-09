
if(grepl("mac.binary", .Platform$pkgType, fixed=TRUE))
  {
    setwd("~/Documents/Research/Papers/stackoverflow")
  }else{
    setwd("~/Documents/stackoverflow")
  }
library(XML)
library(foreach)

users.xml <- xmlTreeParse("users.xml", useInternalNodes=TRUE)

users.list <- xmlToList(users.xml)

users.df <- foreach(i=1:length(users.list), .combine="rbind") %do% {

  out <- c(users.list[[i]]$ID,
           users.list[[i]]$Reputation,
           users.list[[i]]$CreationDate,
           users.list[[i]]$DisplayName,
           users.list[[i]]$LastAccessDate,
           users.list[[i]]$WebsiteUrl,
           users.list[[i]]$Location,
           users.list[[i]]$UpVotes,
           users.list[[i]]$DownVotes
           )

  return(out)


}
