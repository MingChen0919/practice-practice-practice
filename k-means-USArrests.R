library(tidyverse)
library(cluster)
# install.packages('factoextra')
library(factoextra)

df = USArrests
dim(df)

# remove any missing value
df = na.omit(df)

# scale data
df = scale(df)
head(df)

distance = get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 = kmeans(df, centers = 2, nstart = 25)
str(k2)

k2
fviz_cluster(k2, data = df)
