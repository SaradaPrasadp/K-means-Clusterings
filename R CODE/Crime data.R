                                              #Crime_Data


cd <- read.csv(file.choose())


summary(cd)               # Summary of data set
#we know about skewness outliers all things from here.
sum(is.na(cd))           #No missing values are there

#First we have to see is there any outlier or not.
# we dont have outliers present here... 

#we need to standarize the value to get in a same scale.


std_cd <- scale(cd[ , 2:5] )
summary(std_cd)


# Elbow curve to decide the k value
twss <- NULL
for (i in 1:5) {
  twss <- c(twss, kmeans(std_cd, centers = i)$tot.withinss)
}
twss

# Look for an "elbow" in the scree plot
plot(1:5, twss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")


# 3 Cluster Solution
fit <- kmeans(std_cd, 3) 
str(fit)
fit$cluster
final <- data.frame(fit$cluster, cd) # Append cluster membership

aggregate(cd, by = list(fit$cluster), FUN = mean)
