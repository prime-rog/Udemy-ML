getwd()

loans <- read.csv("loan_data.csv")
attach(loans)
summary(loans)

head(loans)

str(loans)


loans$inq.last.6mths <- as.factor(inq.last.6mths)
loans$delinq.2yrs <- as.factor(delinq.2yrs)
loans$pub.rec <- as.factor(pub.rec)

loans$not.fully.paid <- as.factor(not.fully.paid)
loans$credit.policy <- as.factor(credit.policy)

library(ggplot2)
library(dplyr)

loans %>% ggplot(aes(fico)) + geom_histogram(aes(fill = not.fully.paid))

loans %>% ggplot (aes(x=purpose , fill = not.fully.paid)) + geom_bar(position="dodge")

loans %>% ggplot(aes(x = int.rate , y = fico , col = not.fully.paid)) + geom_point(alpha=0.5)

library(caTools)

sample <- sample.split(loans , SplitRatio = 0.7)                                                                     

train <- subset(loans , sample= T)
test  <- subset(loans , sample=F)

library(e1071)
help("svm")
model <- svm(not.fully.paid ~ .  , data = train)

summary(model)
names(loans)
predictvalue <- predict(model , test[1:13])

table(predictvalue , test$not.fully.paid)

tune.results <- tune(svm,train.x=not.fully.paid~., data=train,kernel='radial',
                     ranges=list(cost=c(1,10), gamma=c(0.1,1)))
model <- svm(not.fully.paid ~ .,data=train,cost=10,gamma = 0.1)
predicted.values <- predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)