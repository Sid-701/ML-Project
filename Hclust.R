library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)      # helps to tidy up model outputs
library(ggplot2)
#install.packages("proxy")
#install.packages("factoextra")
library(proxy)
library(factoextra)



# Load data 
df = read.csv("output.csv")
df = df[,c('Accuracy_Passes','Goals_Assist','Goals_Total','Height_cm')]
head(df)




dim(df)

df1 = df[sample(nrow(df), 60), ]

hc=hclust(dist(df1))
plot(hc)


#Using Cosine Similarity

cos_sim_matrix <- as.matrix(dist(df1, method = "cosine"))

hc <- hclust(as.dist(cos_sim_matrix), method = "ward.D2")

fviz_dend(hc, k = 2, cex = 0.8, k_colors = c("#00AFBB", "#E7B800"))