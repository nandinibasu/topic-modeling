library(maptpx)
library(glmnet)
library(rddtools)
library(dplyr)

# Part 1
# Q1:
class <- read.csv('class.csv')
scatter.smooth(class$class_size, class$mean_test_score, xlab = 'size', ylab ='score')

temp <- class %>%
  group_by(class_size) %>%
  summarise(mnsc = mean(mean_test_score))
plot(temp)

# Q3: 
clssz <- class$class_size
sc <- class$mean_test_score
data <- rdd_data(sc,clssz, cutpoint=29)

# Plot rdd result 
plot(data, col ='red', cex = 1, xlab ='size', ylab='score')

# Test treatment effect
rdd_mod <- rdd_reg_lm(rdd_object = data, slope = 'same')
summary(rdd_mod)

plot(rdd_mod, col ='red', cex = 1, xlab ='size', ylab='score')

## Try sample of +5/-5 unit of class size from the treatment
cls <- class[24<=class$class_size & class$class_size <=34,]
clssz1 <- cls$class_size
sc1 <- cls$mean_test_score
data1 <- rdd_data(sc1,clssz1, cutpoint=29)

# Plot rdd result 
plot(data1, col ='red',cex = 1, xlab ='size', ylab='score')

# Test treatment effect
rdd_mod1 <- rdd_reg_lm(rdd_object = data1, slope = 'same')
summary(rdd_mod1)

plot(rdd_mod1, col ='red', cex = 1, xlab ='size', ylab='score')

# Part 2
#1. Fit K-means to the speech text of the members, comprising 
#of the 1000 phrases, for K in 5,10,15,20,25
load("congress.RData")
congress = as.matrix(congress109Counts)
scale_congress <- scale(as.matrix(congress109Counts/rowSums(congress109Counts)))
kmscale_congress5 = kmeans(scale_congress,5)
print(apply(kmscale_congress$centers,1,function(c) colnames(scale_congress)[order(-c)[1:10]]))
kmscale_congress10 = kmeans(scale_congress,10)
kmscale_congress15 = kmeans(scale_congress,15)
kmscale_congress20 = kmeans(scale_congress,20)
kmscale_congress25 = kmeans(scale_congress,25)

# Use AICc to choose the K and interpret the selected model. 
# Also use the elbow curve method to identify the most optimal 
# value of K. Compare the two values of K that you obtained. 
#Are they equal?
AICc = function(kmfs) {
  df <- length(kmfs$centers)
  n <- sum(kmfs$size)
  D <- kmfs$tot.withinss
  AICc <- D + 2*df*n/(n-df-1)
  return(AICc)
}
# The different AICc for different ks
AICc(kmscale_congress5)
AICc(kmscale_congress10)
AICc(kmscale_congress15)
AICc(kmscale_congress20)
AICc(kmscale_congress25)
# k = 25 has the smallest AICc

Deviance = function(kmfs){
  D = kmfs$tot.withinss
  return(D)
}

kfit <- lapply(1:300, function(k) kmeans(scale_congress,k))
Deviance <- sapply(kfit, Deviance)
plot(Deviance)
abline(v=which.min(Deviance))
# both elbow method and AICc show that bigger K is better.

#3. Fit a topic model for the speech counts. 
#Use Bayes factors to choose the number of topics and interpret 
#your chosen model.
triplet_matrix_congress<- as.simple_triplet_matrix(congress109Counts)
tpc <- topics(triplet_matrix_congress,K=10) 
topics_congress <- topics(triplet_matrix_congress, K=5*(1:5), verb=10)
summary(topics_congress)
summary(topics_congress, n=10)

# According to Bayes factors, cluster into 10 topics is the best 

# 4. Connect the unsupervised clusters to partisanship. 
# Tabulate party membership by K-means cluster. 
# Are there any non-partisan topics? Fit topic regressions for 
# each of party and repshare. Compare to regression onto phrase percentages
party = congress109Ideology[,"party"]=="R"
repshare <- congress109Ideology[,'repshare']
tpcreg1 <- gamlr(topics_congress$omega, party)
tpcreg2 <- gamlr(topics_congress$omega, repshare)
drop(coef(tpcreg1))*0.1
drop(coef(tpcreg2))*0.1

# Using cv.glmnet for party memebership
bigram <- 100*congress109Counts/rowSums(congress109Counts)

regtopics.cv1 <- cv.glmnet(topics_congress$omega, party)
regwords.cv1 <- cv.glmnet(bigram,party)

par(mfrow=c(1,2))
plot(regtopics.cv1)
mtext("topic regression", font=2, line=2)
plot(regwords.cv1)
mtext("bigram regression", font=2, line=2)

# OOS for both 
max(1-regtopics.cv1$cvm/regtopics.cv1$cvm[1])
max(1-regwords.cv1$cvm/regwords.cv1$cvm[1])
# Topic regression did better

# Using cv.glmnet for repshare
regtopics.cv2 <- cv.glmnet(topics_congress$omega, repshare)
regwords.cv2 <- cv.glmnet(bigram,repshare)

par(mfrow=c(1,2))
plot(regtopics.cv2)
mtext("topic regression", font=2, line=2)
plot(regwords.cv2)
mtext("bigram regression", font=2, line=2)

# OOS for both 
max(1-regtopics.cv2$cvm/regtopics.cv2$cvm[1])
max(1-regwords.cv2$cvm/regwords.cv2$cvm[1])
# Topic regression still did better

# Tabulating party membership by K-means
km <- kmeans(scale_congress,10) 
tapply(congress109Ideology$party, km$cluster, table)
