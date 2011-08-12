if(grepl("mac.binary", .Platform$pkgType, fixed=TRUE))
  {
    setwd("~/Documents/Research/Papers/stackexchange")
  }else{
    setwd("~/Documents/stackexchange")
  }

library(DBI)
library(RMySQL)
library(stringr)
library(foreach)
library(doMC)
registerDoMC(2)

username = "markhuberty"
password = "overflowpwd"
db = "stackoverflow"

parse.tag.list <- FALSE
if(parse.tag.list)
  {
    conn <- dbConnect("MySQL",
                      username = username,
                      password = password,
                      dbname = db
                      )

    tag.query <- dbSendQuery(conn, "SELECT TAGS FROM posts")
    tags <- fetch(tag.query, -1)

    dbDisconnect(conn)


    tags.vec <- as.character(tags[,1])
    rm(tags)
    gc()

    tags.vec <- gsub("^[<]{1}", "", tags.vec)
    tags.vec <- gsub("[>]{1}$", "", tags.vec)
    tags.list <- str_split(tags.vec, "><")

    save(tags.list, file="./data/tags.list.RData")
  }

rm(list=ls())
gc()
load("./data/tags.list.RData")

format.sparse.matrix <- function(list.in){

  unique.tags <- unique(unlist(list.in))
  
  mat.out <- foreach(i=1:length(list.in), .combine="rbind") %dopar% {

    if(length(list.in[[i]]) > 0)
      {
        if(list.in[[i]] != "")
          {

            idxs <- which(unique.tags %in% list.in[[i]])

            vec.out <- rep(NA, length(unique.tags))
            
            vec.out[idxs] <- 1
            
            return(vec.out)
            
          }else{

            return(rep(NA, length(unique.tags)))## end if list is full of nulls
          }
      }else{
        return(rep(NA, length(unique.tags)))## end of list is length 0
      }
  } ## id foreach
  
  return(mat.out)

}

tag.matrix <- format.sparse.matrix(tags.list)

save(tag.matrix,
     file="./data/stackoverflow.tag.sparse.matrix.RData"
     )



