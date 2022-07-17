getwd()

df <- read.csv("bikeshare.csv")

str(df)

head(df)
library(ggplot2)
ggplot(df , aes(temp  , count))+geom_point(alpha=0.5 , aes(color = temp))

df$datetime <-as.POSIXct(df$datetime)

ggplot(df , aes( datetime , count  , col= temp)) + geom_point(alpha=0.4)+ scale_color_gradient(low = "blue" , high ="red")


cor(df[ , c ("temp" , "count")])

ggplot(df , aes(x = factor(season) , count)) + geom_boxplot(aes(color = factor(season)))

df$hour <- sapply(df$datetime , function(x){format(x , "%H")})

head(df)

mg <- subset(df , workingday==1 , )

mg$workingday

ggplot( mg , aes(hour , count)) + geom_point(aes(color = temp) , position = position_jitter(w=1 , h=0) , alpha = 0.3) + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))

mgn = subset(df , workingday == 0 , )


ggplot( mgn , aes(hour , count)) + geom_point(aes(color = temp) , position = position_jitter(w=1 , h=0) , alpha = 0.3) + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))

temp.model <- lm (count ~ temp , df)

summary(temp.model)


9.1705 * 25 + 6.0462

temp.test <- data.frame(temp = c(25))
predict(temp.model , temp.test)

df$hour <- sapply(df$hour , as.numeric)

model <- lm( count ~ . -casual -registered -datetime -atemp , df)
summary(model)

##### how to plot a regression line given scatter plot


c1 <- c(68.95,80.23,69.45,74.15,50.0,55.5,80,70.5)
c2 <- c(0,0,0,0,1,1,0,0)
length(c2)
plot ( c1 , c2) + abline(lm(c2~c1 , mc))

mc <- data.frame(c1 , c2)
print(mc)

ggplot(mc , aes(c1,c2)) +geom_point() +stat_smooth()
 
