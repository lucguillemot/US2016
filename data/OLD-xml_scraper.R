library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(jsonlite)
library(tidyr)
library(XML)
library(RCurl)
library(taRifx)

##############################################################

# Function to parse xml files downloaded from politico.com
parse_xml_states <- function(state) {
  
  url <- paste("map/", state, ".xml", sep = "")
  doc <- xmlTreeParse(url, useInternal = TRUE)
  
  thisstate <- data.frame()
  ids <- unlist(doc["//article/@id"], use.names = FALSE)
  #fips <- unlist(doc["//article/@data-fips"], use.names = FALSE) # unused
  
  for (i in 1:length(ids)) {
    #i <- 2
    id <- unlist(doc[paste("//article[", i, "]/@id")], use.names = FALSE)
    fip <- unlist(doc[paste("//article[", i, "]/@data-fips")], use.names = FALSE)
    candidates <- gsub("[[:space:]]", "", xpathSApply(doc, paste("//article[", i, "]/div/div/div/table/tbody/tr/th"), xmlValue))
    pc <- xpathSApply(doc, paste("//article[", i, "]/div/div/div/table/tbody/tr/td[@class='results-percentage']"), xmlValue)
    votes <- as.numeric( # converts to numbers
      gsub(",", "", # gsub used to remove commas
           xpathSApply(doc, 
                       paste("//article[", i, "]/div/div/div/table/tbody/tr/td[@class='results-popular']"), 
                       xmlValue)))
    thisstate <- rbind(thisstate,(cbind(fip, id, candidates, pc, votes)))
  }
  
  unique(thisstate$fip)
  thisstate$pc <- NULL
  thisstate$id <- NULL
  
  # remove "Uncommitted" and other 'candidate' values leading to erros (duplicate names)
  thisstate <- thisstate %>% 
    filter(candidates != "Uncommitted") %>% 
    filter(candidates != "NoPreference") %>%
    filter(candidates != "TotalWrite-ins")
  
  # From long to wide format
  thisstate.spreaded <- spread(thisstate, candidates, votes)
  
  # Select columns of still-running candidates + Rubio
  # and add State column
  thisstate.spreaded <- thisstate.spreaded %>% 
    select(fip, matches("H.Clinton"), matches("B.Sanders"), matches("D.Trump"), matches("T.Cruz"), matches("M.Rubio"), matches("J.Kasich")) %>% 
    mutate(state = state)

  thisstate.spreaded
}

# IOWA missing from Politico data
states <- c("NH", "SC", "NV", "AL", "AK", "AR", "GA", "MA", "MN", "OK", "TN", "TX", "VT", "VA", "KS", "KY", "LA", "ME", "HI", "ID", "MI", "MS", "DC", "FL", "IL", "MO", "NC", "OH", "AZ", "UT")

# Get States data (creates a dataframe for each States with data)
for (i in 1:length(states)) {
  this <- states[i]
  assign(this, parse_xml_states(states[i]))
  print(states[i]) # Check if States is ok
}

# Calculates totals // NOT WORKING !!
#st.sp.cv <- japply(thisstate.spreaded, which(sapply(thisstate.spreaded, class)=="character"), as.numeric )
#st <- thisstate.spreaded %>% mutate(sum = rowSums(.[2:12]))

# Bind States's data
# bind_rows (from dplyr) allows to rbind datafames with uneven number of columns
st <- bind_rows(NH, SC, NV, AL, AK, AR, GA, MA, MN, OK, TN, TX, VT, VA, KS, KY, LA, ME, HI, ID, MI, MS, DC, FL, IL, MO, NC, OH, AZ, UT)
#st.cl <- st %>% select(fip, as.numeric(H.Clinton))

# find the winner
## for each row, find the index of the column with max value
### for the dems
dems <- names(st[2:3])
reps <- names(st[4:7])
st$maxdems <- as.numeric(apply(st[2:3], 1, which.max))
st$maxreps <- as.numeric(apply(st[4:7], 1, which.max))

st <- st %>% mutate(indexdems = maxdems + 1, indexreps = maxreps + 3) # Column index of the label of the max value

for (j in 1:length(st$fip)) {
  # get the max value (the number of votes obtained by the winner)
  st$winnerdem_votes[j] <- as.numeric(st[j,st$indexdems[j]])
  st$winnerrep_votes[j] <- as.numeric(st[j,st$indexreps[j]])
  # get the name of the winner
  st$winnerdem <- as.character(dems[st$maxdems])
  st$winnerrep <- as.character(reps[st$maxreps])
}

# housekeeping (remove useless columns)
st <- st %>% select(-maxdems, -maxreps, -indexdems, -indexreps)

# Add 2015 population estimates and Counties labels
pop <- read.csv("pop/pop2015.csv")
pop$fip <- as.character(pop$fip)
st.pop <- inner_join(st, pop, by = "fip")



## CALCULATE RATIO VOTES/DELEGATES ## REPRESENTATIVITY ##
# Score for each State: Republicans
st.ratio.rep <- rep.cc %>% 
  select(State, pop_vote, del_sent) %>% 
  mutate(st_ratio_rep = del_sent/pop_vote) %>%
  select(state = State, st_ratio_rep)
# Score for each State: Democrats
st.ratio.dem <- dem.cc %>% 
  select(State, pop_vote, del_sent) %>% 
  mutate(st_ratio_dem = del_sent/pop_vote) %>%
  select(state = State, st_ratio_dem)




st.ratio <- merge(st.ratio.rep, st.ratio.dem, by = "state")
st.ratio.pop <- merge(st.pop, st.ratio, by = "state")

st.ratio.pop <- st.ratio.pop %>% mutate(ratio_county_rep = winnerrep_votes*st_ratio_rep,
                                ratio_county_dem = winnerdem_votes*st_ratio_dem,
                                ratio_county_rep_pop = respop72015*st_ratio_rep,
                                ratio_county_dem_pop = respop72015*st_ratio_dem)

#max(st.rep$ratio_county_pop)
# Export data to csv file
write.csv(st.ratio.pop, "map/counties.csv", row.names = FALSE)




# END ##############################################################