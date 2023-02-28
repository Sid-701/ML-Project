#Association rule mining 
#install.packages("arules")
#detach("package:arulesViz", unload=TRUE)
#detach("package:arules", unload=TRUE)
library(arules)
library(arulesViz)

#install.packages("TSP")
#install.packages("data.table")
## NOTE: If you are asked if you want to INSTALL FROM SOURCE - click YES!
#install.packages("arulesViz", dependencies = TRUE)
## IMPORTANT ## arules ONLY grabs rules with ONE item on the right
#install.packages("sp")


#install.packages("dplyr", dependencies = TRUE)
#install.packages("purrr", dependencies = TRUE)
#install.packages("devtools", dependencies = TRUE)
#install.packages("tidyr")
#install.packages("htmltools")
library(viridis)

library(TSP)
library(data.table)
library(ggplot2)
library(Matrix)
library(tcltk)
library(dplyr)
library(devtools)
library(purrr)
library(tidyr)

data1<- read.csv('output.csv')
data1

# Removing disease column
data1$Accuracy_Passes <-cut(data1$Accuracy_Passes, breaks = c(0, 30, 60 ,90, Inf), labels = c("(0-30) AP", "(30-60) AP", "(60-90) AP","(90+) AP"))

data1$Goals_Total <- cut(data1$Goals_Total, breaks = c(0, 10, 25, Inf), labels = c("(0-10) Goals", "(10-25) Goals", "(25+) Goals"))
data1$Goals_Assist <- cut(data1$Goals_Assist, breaks = c(0, 7, Inf), labels = c("(0-7) Assists", "(7-13) Assists"))
data1$Weight_kg<-discretize(data1$Weight_kg, method="interval", breaks=3,  
                      labels = c("Light Wieght","Middle Weight","Heavy Weight"),    
                      ordered=FALSE, onlycuts=FALSE)
data1$Height_cm <- discretize(data1$Height_cm, method="interval", breaks=3,  
                              labels = c("Short","Medium Height","Tall"),    
                              ordered=FALSE, onlycuts=FALSE)
data1$Rating <- discretize(data1$Rating, method="interval", breaks=3,  
                              labels = c("Low Rating","Average Rating","High Rating"),    
                              ordered=FALSE, onlycuts=FALSE)
dataf <- data1[,c('Nationality','Team','Position','Accuracy_Passes','Goals_Total','Rating','Weight_kg','Height_cm')]
for (col in c("Nationality", "Position","Team")) {
  dataf[[col]] <- as.factor(dataf[[col]])
}
#Removing row names and column names 

rownames(dataf) <- NULL
colnames(dataf) <- NULL

#storing cleaned data into another csv file 

write.csv(dataf, "data1_ARM_final.csv", row.names = FALSE)

#converting the data type into transactions 

sx <- read.transactions("data1_ARM_final.csv",
                           rm.duplicates = FALSE, 
                           format = "basket",  ##if you use "single" also use cols=c(1,2)
                           sep=",",  ## csv file
                           cols=NULL) ## The dataset has no row numbers

#inspecting the transactions 

arules::inspect(sx[1:50])


######################## Apriori ##################################


Srules1 = arules::apriori(sx, parameter = list(support=.035, 
                                                 confidence=.35, minlen=2))
arules::inspect(Srules1[1:10])

#plotting all the rules
plot(Srules1, method = "scatterplot")

#plotting the frequent items in the transactions
itemFrequencyPlot(sx, topN=20, type="absolute")

#sorting rules wrto confidence
SortedRules1 <- sort(Srules1, by="lift", decreasing=TRUE)

#inspecting top 20 rules 
arules::inspect(SortedRules1[1:20])

#summary of the rules
(summary(SortedRules1))

#sorting rules with respect to lift 
subrules1 <- head(sort(SortedRules1, by="lift"),20)

#plotting 
plot(subrules1, method="graph", engine="htmlwidget")


########################## High Rating Rules ###################################

HighRatingRules <- apriori(data=sx,parameter = list(supp=.001, conf=.01, minlen=3),
                              appearance = list(default="lhs", rhs=c("High Rating")),
                              control=list(verbose=FALSE))

#sorting with respect to confidence 
HighRatingRules <- sort(HighRatingRules, decreasing=TRUE, by="confidence")

#inspecting top 20 rules 
arules::inspect(HighRatingRules[1:30])

#plotting
plot(HighRatingRules[1:30], method="graph", engine="htmlwidget")
plot(HighRatingRules[1:30], method = "graph", asEdges = TRUE, limit = 10) 

###########################High Goal rules #############################

HighGoalsRules <- apriori(data=sx,parameter = list(supp=.001, conf=.01, minlen=3),
                                appearance = list(default="lhs", rhs="(25+) Goals"),
                                control=list(verbose=FALSE))

#sorting with respect to confidence 
HighGoalsRules <- sort(HighGoalsRules, decreasing=TRUE, by="confidence")

#inspecting top 20 rules 
arules::inspect(HighGoalsRules[1:20])

#plotting
plot(HighGoalsRules[1:30], method="graph", engine="htmlwidget")
plot(HighGoalsRules[1:30], method = "graph", asEdges = TRUE, limit = 10) 

