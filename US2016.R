library(dplyr)
library(ggplot2)
library(reshape2)
library(jsonlite)
library(tidyr)
rep <- read.csv("data/rep.csv")
popVote <- 19783685
rep <- rep %>% mutate(pop_vote = Trump+Cruz+Rubio+Kasich, 
                      del_sent =Trump_del+Cruz_del+Rubio_Del+Kasich_del) %>%
  mutate(PTrump = Trump/pop_vote, 
         PCruz = Cruz/pop_vote,
         PRubio = Rubio/pop_vote,
         PKasich = Kasich/pop_vote,
         PDTrump = Trump_del/del_sent, 
         PDCruz = Cruz_del/del_sent,
         PDRubio = Rubio_Del/del_sent,
         PDKasich = Kasich_del/del_sent) %>%
  mutate(del_prop = (pop_vote/popVote)*2472)

rep <- rep[-1,] # remove the row "total"

rep.cc <- rep[complete.cases(rep), ]

# PLAY WITH R CHARTS TO PEEK INTO DATA #
# popVote <- sum(rep$pop_vote, na.rm = TRUE)
delTot <- sum(rep[, 9:12], na.rm = TRUE)

plot(rep$Delegates, rep$pop_vote)

ggplot(rep.cc, aes(pop_vote, del_prop))+
  geom_point(colour="red")+
  geom_text(aes(label=State), hjust=-.2, vjust =-.2, size = 3)+
  #theme(axis.text=element_text(size=8))+
  geom_smooth(method='lm')

g <- ggplot(rep.cc)+
  geom_point(aes(x = pop_vote, y = del_prop, color = "red"))+
  geom_point(aes(x = pop_vote, y = del_sent, color = "darkblue"))+
  geom_text(aes(x = pop_vote, y = del_prop, color = "red", label = State), hjust=-.2, vjust =-.2, size = 3)
  
#rep.melt <- melt(rep.cc, id.vars="State", value.name="value", variable.name="Year")

ggplot(rep, aes(State, del_sent)) + 
  geom_line()#+
  geom_line(aes(y = pop_vote, colour = "Share of votes"))

rep.tots <- rep %>% select(del_sent, pop_vote)
#counts <- table(rep.cc$del_sent)
barplot(rep.cc$pop_vote/rep.cc$del_sent, main="Share of votes and Share of delegates",
        xlab="Share", col=c("darkblue"),
        legend = rep$State) #, beside=TRUE

# EXPORT DATA FOR THE BERTIN MATRIX #
rep.chart <- rep.cc %>% select(ansi_code, State, winner, PTrump, PCruz, PRubio, PKasich)
write.csv(rep.cc, "data/rep.cc.csv", row.names = FALSE)

which.max(rep.cc$Trump)
which.max(rep.cc$Trump_del)
names <- c("state", "ansi_code", "votes", "delegates")

chart.trump <- rep.cc %>% select(State, ansi_code, Trump, Trump_del)
names(chart.trump) <- names
write.csv(chart.trump, "data/trump.csv", row.names = FALSE)

chart.cruz <- rep.cc %>% select(State, ansi_code, Cruz, Cruz_del)
names(chart.cruz) <- names
write.csv(chart.cruz, "data/cruz.csv", row.names = FALSE)

chart.rubio <- rep.cc %>% select(State, ansi_code, Rubio, Rubio_Del)
names(chart.rubio) <- names
write.csv(chart.rubio, "data/rubio.csv", row.names = FALSE)

chart.kasich <- rep.cc %>% select(State, ansi_code, Kasich, Kasich_del)
names(chart.kasich) <- names
write.csv(chart.kasich, "data/kasich.csv", row.names = FALSE)

# To get States codes
write.csv(rep.cc$ansi_code, "data/states_ansi.csv", row.names = FALSE)
states_ansi <- c("19","33","45","32","1","2","5","13","25","27","40","47","48","50","51","20","21","22","23","15","16","26","28","11","12","17","29","37","39","4","49")

states <- rep.cc %>% select(State)
write.csv(states, "data/states.csv", row.names = FALSE)
states <- c("IA", "NH", "SC", "NV", "AL", "AK", "AR", "GA", "MA", "MN", "OK", "TN", "TX", "VT", "VA", "KS", "KY", "LA", "ME", "HI", "ID", "MI", "MS", "DC", "FL", "IL", "MO", "NC", "OH", "AZ", "UT")

# Export data for the stacked chart (cumulative) #
Namen <- c("state", "x", "y")
iterations <- 31
variables <- 1
forjson <- matrix(ncol=variables, nrow=iterations)
li <- list()

rep.del.count <- rep.cc %>% select(ansi_code, Trump_del, Cruz_del, Rubio_Del, Kasich_del)
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
write(stack, "data/rep_stacked.json")
