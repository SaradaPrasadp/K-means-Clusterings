                            #Auto insurance

at <- read.csv(file.choose())
at1 <- at[ , -c(1,2,3,4,7,9,11,12,20,23,24)]
attach(at1)
summary(at1)               # Summary of data set
#we know about skewness outliers all things from here.
sum(is.na(at))           #No missing values are there


#First we have to see is there any outlier or not.
# we dont have outliers present here... 

#we need to standarize the value to get in a same scale.

at_std0 <- scale(at1[ , 4:9 ])
at_std1 <- scale(at1[ , 13])

#combine these 2 
at_std <- cbind(at_std0,at_std1)

summary(at_std)


#now we have to create dummy variable for categorical data set.

library(CatEncoders)

#create for column :"coverage"
a <- LabelEncoder.fit(Coverage)
#now new values are transformed
coverage_new <- transform(a,Coverage)

#create for column :"Educatin"
b <- LabelEncoder.fit(Education)
#now new values are transformed
Education_new <- transform(b,Education)

#create for column :"Employment status"
c <- LabelEncoder.fit(EmploymentStatus)
#now new values are transformed
Employment_new <- transform(c,EmploymentStatus)

#create for column :"coverage"
d <- LabelEncoder.fit(Policy.Type)
#now new values are transformed
Policytype_new <- transform(d,Policy.Type)

#create for column :"coverage"
e <- LabelEncoder.fit(Policy)
#now new values are transformed
policy_new <- transform(e,Policy)

#create for column :"coverage"
f <- LabelEncoder.fit(Sales.Channel)
#now new values are transformed
sale_new <- transform(f,Sales.Channel)


#Now we'll combine this dummy data set with the standardized data set.
at_new <- cbind(at_std,coverage_new,Education_new,Employment_new,Policytype_new,policy_new,sale_new)
summary(at_new)

# Elbow curve to decide the k value
twss <- NULL
for (i in 1:11 ) {
  twss <- c(twss, kmeans(at_new, centers = i)$tot.withinss)
}
twss

# Look for an "elbow" in the scree plot
plot(1:11, twss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")


# 3 Cluster Solution
fit <- kmeans(at_new, 3) 
str(fit)
fit$cluster
final <- data.frame(fit$cluster, at) # Append cluster membership

aggregate(at, by = list(fit$cluster), FUN = mean)
