# data taken from kaggle to to explore and drive the factors impacting the status of placement of the students

data <- read.csv(file.choose(),header = T)
data
str(data)
summary(data)

library(dplyr)
library(ggplot2)
data_placed <- data %>%
  filter(status == 'Placed')
data_not_placed <- data %>%
  filter(status == 'Not Placed')

summary(data_placed)  #tells you everything about the people who got placed
summary(data_not_placed)  # tells you everything about the students who did not get placed
#comparison of these two helps us understand the difference between students who got placed and those who didnt

par(mfrow = c(2,8))
ggplot(data,aes(x = degree_t ,fill = status))+
  geom_bar()

ggplot(data,aes(x = gender ,fill = status))+
  geom_bar()

ggplot(data,aes(x = ssc_b ,fill = status))+
  geom_bar()

ggplot(data,aes(x = hsc_b ,fill = status))+
  geom_bar()

ggplot(data,aes(x = hsc_s ,fill = status))+
  geom_bar()

ggplot(data,aes(x = workex ,fill = status))+
  geom_bar()

ggplot(data,aes(x = specialisation ,fill = status))+
  geom_bar()


library(forecast)
library(readxl)
library(tidyverse)

class(data$status)
class(data)
par(mfrow = c(3,2))
hist(data$ssc_p,col = 'darkorchid')
hist(data$hsc_p,col = 'darkorchid')
hist(data$degree_p,col = 'darkorchid')
hist(data$etest_p,col = 'darkorchid')
hist(data$mba_p,col = 'darkorchid')
hist(data$salary,col = 'darkorchid')

par(mfrow = c(2,3))
plot(status ~ degree_p, data = data)
plot(status ~ degree_t, data = data)
plot(status ~ workex, data = data)
plot(status ~ etest_p, data = data)
plot(status ~ specialisation, data =  data)
plot(status ~ mba_p, data = data)

par(mfrow = c(1,3))
boxplot(degree_p ~ status,ylab = 'degree percent',xlab = 'status',col = 'light blue' , data = data)
boxplot(etest_p ~ status,ylab = 'employability test score',xlab = 'status',col = 'light blue', data = data)
boxplot(mba_p ~ status,ylab = 'mba percentage',xlab = 'status',col = 'light blue', data = data)

data1 <- data[which(data$status == 'placed')]
data2 <- data[which(data$status == 'not placed')]
set.seed(123)

trainingset1 <- sample(1:nrow(data1), 0.75*nrow(data1))
trainingset2 <- sample(1:nrow(data2),0.75*nrow(data2))

trainingvalue1 <- data[trainingset1, ]
trainingvalue2 <- data[trainingset2, ]
trainingdata <- rbind(trainingvalue1,trainingvalue2)

testvalue1 <- data[-trainingset1, ]
testvalue2 <- data[-trainingset2, ]
testdata <- rbind(testvalue1,testvalue2)

library(randomForest)
output.forest1 <- randomForest(status ~ gender + ssc_p + ssc_b + hsc_p + hsc_b + hsc_s + degree_p + degree_t + workex + etest_p + specialisation + mba_p , data = data)
randomForest::importance(output.forest1)
#                     MeanDecreaseGini
#ssc_p                 26.386679
#ssc_b                  1.361196
#hsc_p                 18.546714
#hsc_b                  1.128759
#hsc_s                  1.610912
#degree_p              16.151898
#degree_t               1.749336
#workex                 2.962960
#etest_p                6.479031
#specialisation         2.556498
#mba_p                 10.452922


# This tells us that factors such as ssc_p,hsc_p,degree_p,mba_p,etest_p,workex,etest_p play a important role in placement of an mba student

