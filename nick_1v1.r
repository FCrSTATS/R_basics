## 1v1 session for Nick Ungson - @NickUngson
## K-Means for Player Profiles 

# load libraries 
require(StatsBombR)
require(tidyverse)

# load event data 
events <- StatsBombFreeEvents()

# get minutes played data 
minutes.played <- get.minutesplayed(events)

# sumarise as total minutes played 
minutes.played.per.player <- 
  minutes.played %>% 
  group_by(player.name) %>% 
  summarise(total.minutes.played = sum(minutes.played))

# add total minutes to events dataframe 
events <- merge(events, minutes.played.per.player, by = "player.name")

# seperate the location data 
events <- events %>% separate(location, c("extra.location","x","y")) %>%
          select(-extra.location) %>% 
          mutate(x = as.numeric(as.character(x)),
                 y = as.numeric(as.character(y)))


events <- events %>% separate(pass.end_location, c("extra.location","pass.end.x","pass.end.y")) %>%
          select(-extra.location) %>% mutate(pass.end.x = as.numeric(as.character(pass.end.x)),
                 pass.end.y = as.numeric(as.character(pass.end.y)))


## add pass in the box  

events <- 
  events %>% 
  mutate(type.name = ifelse(type.name == "Pass" & pass.length >= quantile(events$pass.length, probs = c(0.95), na.rm = T), "Long.Pass", type.name)) %>%
  mutate(type.name = ifelse(type.name == "Pass" & pass.end.x >= 102 & pass.end.y > 18 & pass.end.y < 62, "Pass.into.Box", type.name)) %>% 
  mutate(type.name = ifelse(type.name == "Shot" & x >= 102 & y > 18 & y < 62, "Shot.in.box", type.name)) %>% 
  mutate(type.name = ifelse(type.name == "Shot", "Shot.outside.box", type.name)) %>% 
  mutate(type.name = ifelse(type.name == "Pressure" & x >= 80, "High.Pressure", type.name))


# all in one 
player.summaries <- 
  events %>% 
  filter(total.minutes.played > 270 & position.name != "Goalkeeper" & competition_id == 43) %>% 
  group_by(player.name, type.name) %>%
  summarise(Total = (n() / max(total.minutes.played) * 90), 
            total.minutes.played = max(total.minutes.played)) %>% 
  group_by(player.name) %>% 
  spread(type.name, Total)

# replace NAs with zero
player.summaries[is.na(player.summaries)] <- 0

# see all varaibles  
allVars <- colnames(player.summaries)

# keep only columns we want 
data.to.cluster <- 
  player.summaries %>% 
  rename(Ball.Recovery = `Ball Recovery`) %>%
  select(player.name, total.minutes.played, Pass.into.Box,
         Ball.Recovery, Clearance, Dribble, Long.Pass, High.Pressure,
         Interception, Pass, Shot.outside.box, Shot.in.box, Pressure, 
         )

# normalise the data - define function and apply to all columns that need it
NCOL(data.to.cluster)
normalise <- function(x) {return ( (x - min(x)) / (max(x) - min(x))) }
normalised.data <- normalise(data.to.cluster[3:ncol(data.to.cluster)])

## ----- CLUSTERS ---------------------------------

# run k-means to show output
kmeans(normalised.data, 10, nstart=50,iter.max = 15)

# A key decision we need to make is how many clusters, there are a 
# few techniques for this. 1st the Elbow 

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(normalised.data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# 2nd is from a bayesian perspective. Bayesian Information Criterion for 
# expectation-maximization, initialized by hierarchical clustering for 
# parameterized Gaussian mixture models

#install.packages("mclust")
require(mclust)

d_clust <- Mclust(as.matrix(normalised.data), G=1:15, 
                  modelNames = mclust.options("emModelNames"))
d_clust$BIC
plot(d_clust)

## 3rds is using NbClust to calculate test 30 combinations of setups
# returning the majorties optimal number 

#install.packages("NbClust",dependencies = TRUE)
library(NbClust)
nb <- NbClust(normalised.data, diss=NULL, distance = "euclidean", 
              min.nc=2, max.nc=5, method = "kmeans", 
              index = "all", alphaBeale = 0.1)
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

## what should we choose!! 
no.clusters <- 7

## run the actual analysis to use 
cluster.results <- kmeans(normalised.data, no.clusters, nstart=50,iter.max = 15)
cluster.results$cluster

table(cluster.results$cluster)

cluster.results$centers

## bind the results 
data.to.cluster$Cluster <- cluster.results$cluster

## check out the results 
View(data.to.cluster %>% filter(Cluster == 5))

