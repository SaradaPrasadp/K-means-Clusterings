                                    #Telco Customer

library(readxl)
tc <- read_excel(file.choose())
tc1 <- tc[ , -c(1,2,3,4,5,14,15,16,17,18,19,20,21)]
attach(tc1)
summary(tc1)               # Summary of data set
#we know about skewness outliers all things from here.
sum(is.na(tc1))           #No missing values are there


#First we have to see is there any outlier or not.
# we dont have outliers present here... 

#we need to standarize the value to get in a same scale.

tc_std0 <- scale(tc1[ , 1 ])
tc_std1 <- scale(tc1[ , 4])
tc_std2 <- scale(tc1[ , 8])
tc_std4 <- scale(tc1[ , 12:17])

#combine these 2 
tc_std <- cbind(tc_std0,tc_std1,tc_std2,tc_std4)

summary(tc_std)


#now we have to create dummy variable for categorical data set.

library(CatEncoders)

#create for column :"offer"
a <- LabelEncoder.fit(Offer)
#now new values are transformed
offer_new <- transform(a,Offer)

#create for column :"Phone Service"
b <- LabelEncoder.fit(`Phone Service`)
#now new values are transformed
phservice_new <- transform(b,`Phone Service`)

#create forcolumn :"Multiple lines"
c <- LabelEncoder.fit(`Multiple Lines`)
#now new values are transformed
multiple_new <- transform(c,`Multiple Lines`)

#create for column :"internet service"
d <- LabelEncoder.fit(`Internet Service`)
#now new values are transformed
netservice_new <- transform(d,`Internet Service`)

#create for column :"Internet type"
e <- LabelEncoder.fit(`Internet Type`)
#now new values are transformed
nettype_new <- transform(e,`Internet Type`)

#create for column :"contract"
f <- LabelEncoder.fit(Contract)
#now new values are transformed
contract_new <- transform(f,Contract)

#create for column :"Paperless billing"
g <- LabelEncoder.fit(`Paperless Billing`)
#now new values are transformed
paperlessbill_new <- transform(g,`Paperless Billing`)

#create for column :"Payment method"
h <- LabelEncoder.fit(`Payment Method`)
#now new values are transformed
payment_new <- transform(h,`Payment Method`)


#Now we'll combine this dummy data set with the standardized data set.
tc_new <- cbind(tc_std,offer_new,phservice_new,multiple_new,netservice_new,nettype_new,contract_new,paperlessbill_new,payment_new)
tc_new


#now we'll calcucalte gower's dist.
library(cluster)
gower_dist <- daisy(tc_new , metric="gower")

# Elbow curve to decide the k value
twss <- NULL
for (i in 1:17) {
  twss <- c(twss, kmeans(gower_dist, centers = i)$tot.withinss)
}
twss

# Look for an "elbow" in the scree plot
plot(1:17, twss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")


# 3 Cluster Solution
fit <- kmeans(gower_dist, 5) 
str(fit)
fit$cluster
final <- data.frame(fit$cluster, tc) # Append cluster membership

aggregate(tc, by = list(fit$cluster), FUN = mean)

