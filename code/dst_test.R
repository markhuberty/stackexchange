## Test some dst stuff

library(RCurl)

baseurl <- "http://www.datasciencetoolkit.org/text2places/"

test.location <- "New York NY"

url.location <- gsub(" ", "+", test.location)

testurl <- URLencode(paste(baseurl,
                         url.location,
                         sep=""
                         )
                     )

test.get <- getURL(testurl)


my.url <-
  "http://www.datasciencetoolkit.org/text2places/%5b%22Cairo%2c+Egypt%22%5d"

getURL(my.url)
