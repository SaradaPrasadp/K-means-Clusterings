                                    #insurance


ir <- read.csv(file.choose())


summary(ir)               # Summary of data set
#we know about skewness outliers all things from here.
sum(is.na(ir))           #No missing values are there

#First we have to see is there any outlier or not.
# we dont have outliers present here... 

#we need to standarize the value to get in a same scale.


std_ir <- scale(ir )
summary(std_ir)


# Elbow curve to decide the k value
twss <- NULL
for (i in 1:5) {
  twss <- c(twss, kmeans(std_ir, centers = i)$tot.withinss)
}
twss

# Look for an "elbow" in the scree plot
plot(1:5, twss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")


# 3 Cluster Solution
fit <- kmeans(std_cd, 3) 
str(fit)
fit$cluster
final <- data.frame(fit$cluster, ir) # Append cluster membership

aggregate(ir, by = list(fit$cluster), FUN = mean)
