library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(jsonlite)
library(tidyr)
library(XML)
library(RCurl)


url <- "kansas.xml"
doc <- xmlTreeParse(url, useInternal = TRUE)
str(doc)
top <- xmlRoot(doc)
str(top)
xmlName(top)
names(top[2][[1]])

art <- top[[2]]
xmlValue(art[[1]])

xmlSApply(art[[1]], xmlValue)

xmlValue(art)
xmlSApply(art, function(x) xmlSApply(x, xmlValue))

# XPath
names(top)
nodes <- getNodeSet(top, "//article")

xmlValue(doc[["//table"]])

###################
# test from html
theurl <- "http://www.politico.com/2016-election/results/map/president/kansas"
webpage <- getURL(theurl)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)

pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)

# Extract table header and contents
tablehead <- xpathSApply(pagetree, "//*/table[@class='results-table']/tbody/tr/th", xmlValue)
results <- xpathSApply(pagetree, "//*/table[@class='results-table']/tbody/tr/th/td/span/span", xmlValue)

tablehead <- xpathSApply(pagetree, "//*/table[@class='results-table']/tbody/tr/th", xmlValue)
candidates <- xpathSApply(pagetree, "//*/th[@class='results-name']", xmlValue)
PCresults <- xpathSApply(pagetree, "//*/td[@class='results-percentage']", xmlValue)
votes_results  <- xpathSApply(pagetree, "//*/td[@class='results-popular']", xmlValue)

# Convert character vector to dataframe
content <- as.data.frame(matrix(PCresults, ncol = 1, byrow = TRUE))
candidates_content <- as.data.frame(matrix(candidates, ncol = 1, byrow = TRUE))
kansas <- cbind(content, candidates_content)

tablehead
results


c <- xpathSApply(pagetree, "//*/article[@class='results-group']@id", xmlValue)
#ids <- as.data.frame(toString(pagetree["//*/article[@class='results-group']/@id"]))
ids <- pagetree["//*/article[@class='results-group']/@id"]

as.data.frame(ids)

data <- htmlTreeParse(theurl)
data


df <- data.frame(id = data["//*/article[@class='results-group']/@id"])


##############################################################
##############################################################
##############################################################
###########
# try again with downloaded xml file #
url <- "kansas.xml"
doc <- xmlTreeParse(url, useInternal = TRUE)

thisstate <- data.frame()
ids <- unlist(doc["//article/@id"], use.names = FALSE)
#fips <- unlist(doc["//article/@data-fips"], use.names = FALSE)

for (i in 1:length(ids)) {
  #i <- 2
  id <- unlist(doc[paste("//article[", i, "]/@id")], use.names = FALSE)
  fip <- unlist(doc[paste("//article[", i, "]/@data-fips")], use.names = FALSE)
  candidates <- xpathSApply(doc, paste("//article[", i, "]/div/div/div/table/tbody/tr/th"), xmlValue)
  pc <- xpathSApply(doc, paste("//article[", i, "]/div/div/div/table/tbody/tr/td[@class='results-percentage']"), xmlValue)
  votes <- xpathSApply(doc, paste("//article[", i, "]/div/div/div/table/tbody/tr/td[@class='results-popular']"), xmlValue)
  thisstate <- rbind(thisstate,(cbind(fip, id, candidates, pc, votes)))
}
##############################################################
##############################################################
##############################################################