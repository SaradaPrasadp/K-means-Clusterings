library(readxl)
ew1 <- read_excel(file.choose(),sheet=2)
ew <- ew1[ , c(2:12)]

summary(ew)               # Summary of data set
#we know about skewness outliers all things from here.
sum(is.na(ew))           #No missing values are there

#First we have to see is there any outlier or not.
# we have outliers present here...for handle the outliers, we have to treat outliers.we need to normalize the value.

norm <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
} 
norm_ew <- norm(ew)
summary(norm_ew)


# Elbow curve to decide the k value
twss <- NULL
for (i in 1:11) {
  twss <- c(twss, kmeans(norm_ew, centers = i)$tot.withinss)
}
twss

# Look for an "elbow" in the scree plot
plot(1:11, twss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")


# 3 Cluster Solution
fit <- kmeans(norm_ew, 5) 
str(fit)
fit$cluster
final <- data.frame(fit$cluster, ew1) # Append cluster membership

aggregate(ew1, by = list(fit$cluster), FUN = mean)

