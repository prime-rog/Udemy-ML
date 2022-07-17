getwd()

adult <- read.csv("adult_sal.csv")
head(adult)
View(adult)
library(dplyr)
adult <- select(adult , -X)
head (adult)
str(adult)
table(adult$type_employer)
library(ggplot2)
adult %>% ggplot(aes(type_employer)) + geom_bar()
sum(is.na(adult$type_employer))
unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}
adult$type_employer <- sapply(adult$type_employer,unemp)
table(adult$type_employer)

group_emo <- function(job){
  if (job == 'Local-gov' | job =='State-gov')
    return('SL-gov')
  else if (job == 'Self-emp-inc' | job == 'Self-emp-not-inc')
    return('self-emp')
  else 
    return(job)
}

adult$type_employer <- sapply(adult$type_employer , group_emo)

table(adult$type_employer)

table (adult$marital)

sp <- function(m)
{
  if (m == 'Divorced' | m== 'Widowed' | m=='Separated' )
    return("Not-Married")
  else if (m == 'Never-married')
    return(m)
  
  else 
    return("Married")
}
adult$marital <- sapply(adult$marital , sp) 

table(adult$marital)


names(adult)

table(adult$country)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}
adult$country <- sapply(adult$country,group_country)

table(adult$country)

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$income <- sapply(adult$income , factor)
str(adult)

library(Amelia)

adult[adult == '?'] <- NA

table(adult$type_employer)

missmap(adult)

missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

adult <- na.omit(adult)

missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

adult %>% ggplot(aes(x = age , fill = income)) + geom_histogram()

adult %>% ggplot(aes(hr_per_week)) + geom_histogram()

names(adult)[names(adult)=='country'] <- 'region'

str(adult)

adult %>% ggplot(aes(region , fill = income)) + geom_bar() 
 

head(adult)

library(caTools)

set.seed(101)

sample <- sample.split(adult$income , SplitRatio = 0.7)

train = subset(adult , sample==T)
test = subset(adult , sample==F)

help(glm)

model = glm(income ~ . , family = binomial(logit) , data = train)

summary(model)

new.step.model <- step(model)

summary(new.step.model)

test$predicted.income = predict (model , test , type = 'response')

table(test$income , test$predicted.income >0.5)


ggplot(adult , aes(y = income , x = race)) + geom_point()+stat_smooth()
