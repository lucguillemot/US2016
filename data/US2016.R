library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(jsonlite)
library(tidyr)
library(XML)
library(RCurl)
library(taRifx)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # DATA FOR BAR CHARTS AND STACKED BBAR CHARTS # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Republicans # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rep <- read.csv("republicans.csv")
popVote <- 20866873
delSent <- 1512
rep <- rep %>% mutate(pop_vote = Trump+Cruz+Rubio+Kasich, 
                      del_sent =Trump_del+Cruz_del+Rubio_del+Kasich_del) %>%
  mutate(PTrump = Trump/pop_vote, 
         PCruz = Cruz/pop_vote,
         PRubio = Rubio/pop_vote,
         PKasich = Kasich/pop_vote,
         PDTrump = Trump_del/del_sent, 
         PDCruz = Cruz_del/del_sent,
         PDRubio = Rubio_del/del_sent,
         PDKasich = Kasich_del/del_sent) %>%
  mutate(del_prop = (pop_vote/popVote)*delSent) %>%# Number of delegate for each State if
                                            # delegates were allocated proportionally 
                                            # the to number of votes
  
  mutate(Trump_del_if = del_prop*PTrump,
             Cruz_del_if = del_prop*PCruz,
             Rubio_del_if = del_prop*PRubio,
             Kasich_del_if = del_prop*PKasich)# Number of delegates each candidate
                                              # would get if delegates were allocated
                                              # proportionally to the number of votes


rep <- rep[-1,] # remove the row "total"

rep.cc <- rep[complete.cases(rep), ]
sum(rep.cc$del_sent)
# PLAY WITH R CHARTS TO PEEK INTO DATA #
# popVote <- sum(rep$pop_vote, na.rm = TRUE)
# delTot <- sum(rep[, 9:12], na.rm = TRUE)
# 
# plot(rep$Delegates, rep$pop_vote)
# 
# ggplot(rep.cc, aes(pop_vote, del_prop))+
#   geom_point(colour="red")+
#   geom_text(aes(label=State), hjust=-.2, vjust =-.2, size = 3)+
#   #theme(axis.text=element_text(size=8))+
#   geom_smooth(method='lm')
# 
# g <- ggplot(rep.cc)+
#   geom_point(aes(x = pop_vote, y = del_prop, color = "red"))+
#   geom_point(aes(x = pop_vote, y = del_sent, color = "darkblue"))+
#   geom_text(aes(x = pop_vote, y = del_prop, color = "red", label = State), hjust=-.2, vjust =-.2, size = 3)
#   
# #rep.melt <- melt(rep.cc, id.vars="State", value.name="value", variable.name="Year")
# 
# ggplot(rep, aes(State, del_sent)) + 
#   geom_line()#+
#   geom_line(aes(y = pop_vote, colour = "Share of votes"))
# 
# rep.tots <- rep %>% select(del_sent, pop_vote)
# #counts <- table(rep.cc$del_sent)
# barplot(rep.cc$pop_vote/rep.cc$del_sent, main="Share of votes and Share of delegates",
#         xlab="Share", col=c("darkblue"),
#         legend = rep$State) #, beside=TRUE
# 
# # EXPORT DATA FOR THE STATES MAP # NOT USED !
# rep.chart <- rep.cc %>% select(ansi_code, State, winner, PTrump, PCruz, PRubio, PKasich)
# write.csv(rep.cc, "rep.cc.csv", row.names = FALSE)

# EXPORT DATA FOR THE BERTIN MATRIX #
names <- c("state", "ansi_code", "votes", "delegates")

# Real number of delegates
chart.trump <- rep.cc %>% select(State, ansi_code, Trump, Trump_del)
names(chart.trump) <- names
write.csv(chart.trump, "charts/trump.csv", row.names = FALSE)

chart.cruz <- rep.cc %>% select(State, ansi_code, Cruz, Cruz_del)
names(chart.cruz) <- names
write.csv(chart.cruz, "charts/cruz.csv", row.names = FALSE)

chart.rubio <- rep.cc %>% select(State, ansi_code, Rubio, Rubio_del)
names(chart.rubio) <- names
write.csv(chart.rubio, "charts/rubio.csv", row.names = FALSE)

chart.kasich <- rep.cc %>% select(State, ansi_code, Kasich, Kasich_del)
names(chart.kasich) <- names
write.csv(chart.kasich, "charts/kasich.csv", row.names = FALSE)

# 'If' number of delegates ####
chart.trump.if <- rep.cc %>% select(State, ansi_code, Trump, Trump_del_if)
names(chart.trump.if) <- names
write.csv(chart.trump.if, "charts/trump_if.csv", row.names = FALSE)

chart.cruz.if <- rep.cc %>% select(State, ansi_code, Cruz, Cruz_del_if)
names(chart.cruz.if) <- names
write.csv(chart.cruz.if, "charts/cruz_if.csv", row.names = FALSE)

chart.rubio.if <- rep.cc %>% select(State, ansi_code, Rubio, Rubio_del_if)
names(chart.rubio.if) <- names
write.csv(chart.rubio.if, "charts/rubio_if.csv", row.names = FALSE)

chart.kasich.if <- rep.cc %>% select(State, ansi_code, Kasich, Kasich_del_if)
names(chart.kasich.if) <- names
write.csv(chart.kasich.if, "charts/kasich_if.csv", row.names = FALSE)

# Get States codes
#write.csv(rep.cc$ansi_code, "states_ansi.csv", row.names = FALSE)
states_ansi <- c("19","33","45","32","1","2","5","13","25","27","40","47","48","50","51","20","21","22","23","15","16","26","28","11","12","17","29","37","39","4","49")

states <- rep.cc %>% select(State)
write.csv(states, "states.csv", row.names = FALSE)
states <- c("IA", "NH", "SC", "NV", "AL", "AK", "AR", "GA", "MA", "MN", "OK", "TN", "TX", "VT", "VA", "KS", "KY", "LA", "ME", "HI", "ID", "MI", "MS", "DC", "FL", "IL", "MO", "NC", "OH", "AZ", "UT")

# EXPORT DATA FOR THE STACKED CHART (CUMULATIVE) #
Namen <- c("state", "x", "y")
li <- list()

# Real numbers
rep.del.count <- rep.cc %>% select(ansi_code, Trump_del, Cruz_del, Rubio_del, Kasich_del)
noms <- c("State", "Trump", "Cruz", "Rubio", "Kasich")
names(rep.del.count) <- noms

for (st in 1:length(states_ansi)) {
  del.st <- rep.del.count %>% filter(State == states_ansi[st])
  del.st.melted <- del.st %>% melt(id = c("State"))
  names(del.st.melted) <- Namen
  #del.st.melted$State <- NULL
  
  li[[st]] <- del.st.melted # Creates a list of dataframe, each will become a json array
  #forjson[st, 1] <- toJSON(del.st.melted)
}

# create a json file from the list of dataframes (good format for d3.layout().stack())
stack <- toJSON(li)
write(stack, "stacked/rep_stacked.json")

# What if numbers
rep.del.count <- rep.cc %>% select(ansi_code, Trump_del_if, Cruz_del_if, Rubio_del_if, Kasich_del_if)
noms <- c("State", "Trump", "Cruz", "Rubio", "Kasich")
names(rep.del.count) <- noms

for (st in 1:length(states_ansi)) {
  del.st <- rep.del.count %>% filter(State == states_ansi[st])
  del.st.melted <- del.st %>% melt(id = c("State"))
  names(del.st.melted) <- Namen
  #del.st.melted$State <- NULL
  
  li[[st]] <- del.st.melted # Creates a list of dataframe, each will become a json array
  #forjson[st, 1] <- toJSON(del.st.melted)
}

stack <- toJSON(li)
write(stack, "stacked/rep_stacked_if.json")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # DEMOCRATS # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

dem <- read.csv("democrats.csv")
popVote_dem <- 16297485
#delSent_dem <- 2324
delSent_dem <- 2824
dem <- dem %>% mutate(pop_vote = Clinton+Sanders, 
                      del_sent =Clinton_del+Sanders_del) %>%
  mutate(PClinton = Clinton/pop_vote, 
         PSanders = Sanders/pop_vote,         
         PDClinton = Clinton_del/del_sent, 
         PDSanders = Sanders_del/del_sent) %>%
  mutate(del_prop = (pop_vote/popVote_dem)*delSent_dem) %>% # Number of delegate for each State if
  # delegates were allocated proportionally 
  # the to number of votes
  
  mutate(Clinton_del_if = del_prop*PClinton,
         Sanders_del_if = del_prop*PSanders)# Number of delegates each candidate
# would get if delegates were allocated
# proportionally to the number of votes


dem <- dem[-1,] # remove the row "total"

dem.cc <- dem[complete.cases(dem), ]
sum(dem.cc$del_sent)

# EXPORT DATA FOR THE BERTIN MATRIX #
names <- c("state", "ansi_code", "votes", "delegates")

# Real number of delegates
chart.clinton <- dem.cc %>% select(State, ansi_code, Clinton, Clinton_del)
names(chart.clinton) <- names
write.csv(chart.clinton, "charts/clinton.csv", row.names = FALSE)

chart.sanders <- dem.cc %>% select(State, ansi_code, Sanders, Sanders_del)
names(chart.sanders) <- names
write.csv(chart.sanders, "charts/sanders.csv", row.names = FALSE)

# 'If' number of delegates ####
chart.clinton.if <- dem.cc %>% select(State, ansi_code, Clinton, Clinton_del_if)
names(chart.clinton.if) <- names
write.csv(chart.clinton.if, "charts/clinton_if.csv", row.names = FALSE)

chart.sanders.if <- dem.cc %>% select(State, ansi_code, Sanders, Sanders_del_if)
names(chart.sanders.if) <- names
write.csv(chart.sanders.if, "charts/sanders_if.csv", row.names = FALSE)


# To get States codes
#write.csv(rep.cc$ansi_code, "states_ansi.csv", row.names = FALSE)


states_dem <- dem.cc %>% select(ansi_code)
write.csv(states_dem, "states_dem.csv", row.names = FALSE)
#states <- c("IA", "NH", "SC", "NV", "AL", "AK", "AR", "GA", "MA", "MN", "OK", "TN", "TX", "VT", "VA", "KS", "KY", "LA", "ME", "HI", "ID", "MI", "MS", "DC", "FL", "IL", "MO", "NC", "OH", "AZ", "UT")
states_ansi <- c("33","45","1","5","8","13","25","27","40","47","48","50","51","20","22","31","26","28","12","17","29","37","39","4","16","49","15","55","56")

# EXPORT DATA FOR THE STACKED CHART (CUMULATIVE) #
Namen <- c("state", "x", "y")
li <- list()

# Real numbers
dem.del.count <- dem.cc %>% select(ansi_code, Clinton_del, Sanders_del)
noms <- c("State", "Clinton", "Sanders")
names(dem.del.count) <- noms

for (st in 1:length(states_ansi)) {
  del.st <- dem.del.count %>% filter(State == states_ansi[st])
  del.st.melted <- del.st %>% melt(id = c("State"))
  names(del.st.melted) <- Namen
  #del.st.melted$State <- NULL
  
  li[[st]] <- del.st.melted # Creates a list of dataframe, each will become a json array
  #forjson[st, 1] <- toJSON(del.st.melted)
}

# create a json file from the list of dataframes (good format for d3.layout().stack())
stack <- toJSON(li, pretty=TRUE) 
write(stack, "stacked/dem_stacked.json")

# reinitiate list
li <- list()

# What if numbers
dem.del.count <- dem.cc %>% select(ansi_code, Clinton_del_if, Sanders_del_if)
noms <- c("State", "Clinton", "Sanders")
names(dem.del.count) <- noms

for (st in 1:length(states_ansi)) {
  del.st <- dem.del.count %>% filter(State == states_ansi[st])
  del.st.melted <- del.st %>% melt(id = c("State"))
  names(del.st.melted) <- Namen
  #del.st.melted$State <- NULL
  
  li[[st]] <- del.st.melted # Creates a list of dataframe, each will become a json array
  #forjson[st, 1] <- toJSON(del.st.melted)
}

stack <- toJSON(li, pretty=TRUE)
write(stack, "stacked/dem_stacked_if.json")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # COUNTIES DATA FOR MAP # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


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