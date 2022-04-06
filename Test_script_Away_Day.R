#load in packages needed - install if necessary 

lapply(c("ggplot2"),library, character.only = TRUE)

# Read in the data

parentaldata <- read.table(file = "speciesdata.txt", header = TRUE, sep = "\t")

#subset for only the data we need

parentaldata2 <- subset(parentaldata, 
                        (!is.na(parentaldata$FeedingOffspring)) & 
                        (!is.na(parentaldata$Incubation)) & 
                        (!is.na(parentaldata$Precocilaity)) & 
                        (!is.na(parentaldata$Courtshipfeeding)) &
                        (!is.na(parentaldata$Nestbuildin)) & 
                        (!is.na(parentaldata$wp_offspring)) &
                        (!is.na(parentaldata$ep_offspring)))


#create a new variable which is a binary for whether a species 

parentaldata2$IncubationYN <- ifelse(parentaldata2$Incubation > 0, 1, 0)


# Examine our data frame

str(parentaldata2)


#calculate the total number of offspring by adding within and extra-pair young

parentaldata2$totaloffspring <- parentaldata2$ep_offspring + parentaldata2$wp_offspring


#convert to have percentage of extra pair offspring

parentaldata2$EPPnew <- parentaldata2$ep_offspring/parentaldata2$totaloffspring


#scrutinize the data

hist(parentaldata2$EPPnew)
max(parentaldata2$EPPnew)
min(parentaldata2$EPPnew)


#scale the explanatory variables

parentaldata2$Incubationscaled <- scale(parentaldata2$Incubation)


#glm model

Parentalmodel <- glm(cbind(ep_offspring,wp_offspring) ~
                  Incubationscaled, family = binomial, data = parentaldata2)

summary(Parentalmodel)

save.image("Test_away_day.RData")
  
