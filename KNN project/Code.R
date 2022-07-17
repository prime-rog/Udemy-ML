library(ISLR)
head(iris)
str(iris)
stand.features <- scale(iris[1:4])
var(stand.features[,1])
final.data <- cbind(stand.features,iris[5])
head(final.data)
set.seed(101)

library(caTools)

sample <- sample.split(final.data$Species, SplitRatio = .70)
train <- subset(final.data, sample == TRUE)
test <- subset(final.data, sample == FALSE)

predicted.species <- knn(train[1:4],test[1:4],train$Species,k=1)
predicted.species
mean(test$Species != predicted.species)


predicted.species <- NULL
error.rate <- NULL

for(i in 1:10){
  set.seed(101)
  predicted.species <- knn(train[1:4],test[1:4],train$Species,k=i)
  error.rate[i] <- mean(test$Species != predicted.species)
}
library(ggplot2)
k.values <- 1:10
error.df <- data.frame(error.rate,k.values)
pl <- ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()
pl + geom_line(lty="dotted",color='red')


mtcars
plot(mtcars)
plot(mtcars$gear , mtcars$am)
library(ggplot2)
ggplot(mtcars , aes( x = gear , y = am )) + geom_point() + geom_smooth()
ggpairs(mtcars)
library(ISLR)
ggpairs(iris)
