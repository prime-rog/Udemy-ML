library(ISLR)
data()
head(College)
library(dplyr)
View(College)
 df <- College

 library(ggplot2) 

 df %>% ggplot(aes(Room.Board , Grad.Rate , col= Private )) +geom_point()
 
 df$Private <- relevel(df$Private, "Yes")
 df %>% ggplot(aes(F.Undergrad , col = Private , fill=Private)) + geom_histogram(bins=50)
 
 df %>% ggplot(aes(Grad.Rate  , fill=Private)) + geom_histogram()

 rownames(df)[df$Grad.Rate>100] 
 
 df['Cazenovia College','Grad.Rate'] <- 100

 
 library(caTools) 

 sample <- sample.split(df , SplitRatio = 0.7) 

 train = subset(df , sample=T) 

 test = subset(df , sample = F) 
library(rpart)
 model <- rpart(Private ~ ., method = 'class' , data = train ) 

 predict(model , data=test) ->p 

 p 

 head(p) 
 
 p <- as.data.frame(p)

 j <- function(x)
 {
   if ( x >0.5)
     return('Yes')
   else
     return('No')
 }
 
 p$Private <- sapply(p$Yes , j)

 head(p) 

 table(p$Private , test$Private) 
 install.packages("rpart.plot")
 library(rpart.plot)
prp(model) 
install.packages("randomForest")
library(randomForest)
rf.model <- randomForest(Private ~ . , data = train , importance = T )

rf.model$confusion

table(df$Private)

rf.model$importance

p1<- predict(rf.model , test)

table(p1 , test$Private)
