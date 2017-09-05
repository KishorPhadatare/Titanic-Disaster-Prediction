df.train= read.csv("train.csv")
print(head(df.train))
str(df.train)
install.packages('Amelia')
library(Amelia)
missmap(df.train, main='Missing Data', col = c("yellow","black"), legend = FALSE)
install.packages("ggplot2")
library(ggplot2)
ggplot(df.train, aes(Survived))+ geom_bar()
ggplot(df.train, aes(Pclass))+ geom_bar(aes(fill=factor(Pclass)))
ggplot(df.train, aes(Sex))+ geom_bar(aes(fill=factor(Sex)))
ggplot(df.train, aes(Age))+ geom_histogram(bins = 20, alpha=0.5, fill='blue')
ggplot(df.train, aes(SibSp))+ geom_bar()
ggplot(df.train, aes(Fare))+geom_histogram(alpha=0.5, fill= 'blue', color='black')
pl=ggplot(df.train, aes(Pclass, Age))
pl=pl+geom_boxplot(aes(group=Pclass, fill=factor(Pclass), alpha=0.4))
pl+scale_y_continuous(breaks= seq(min(0), max(80),by=2))+ theme_bw()
impute_age <- function(age, class){
 out<- age
  for(i in 1: length(age)){
    if(is.na(age[i])){
      if (class[i]==1){
        out[i]=37
      }
      else if (class[i]==2){
        out[i]=29
      }
      else{
        out[i]=24
      }
    }else {
      out[i]=age[i]
    }
  }
 return(out)
}
fixed.ages=impute_age(df.train $Age, df.train$Pclass)    
df.train$Age=fixed.ages
missmap(df.train, main='Imputation Check', col = c("yellow","black"), legend = FALSE)

str(df.train)
install.packages('dplyr')
library('dplyr')
df.train= select(df.train,-PassengerId, -Ticket, -Cabin, -Name)
str(df.train)
df.train$Survived=factor(df.train$Survived)
df.train$Pclass=factor(df.train$Pclass)
df.train$Sex=factor(df.train$Sex)
df.train$Parch=factor(df.train$Parch)
df.train$SibSp=factor(df.train$SibSp)
str(df.train)
log.model= glm(Survived ~ ., family = binomial(link='logit'), data = df.train)
summary(log.model)
install.packages('caTools')
library('caTools')
set.seed(101)
split= sample.split(df.train$Survived, SplitRatio = 0.7)
train=subset(df.train, split==TRUE)
test=subset(df.train, split==FALSE)
final.log.model= glm(Survived ~ ., family = binomial(link = 'logit'), data=train)
str(final.log.model)
summary(final.log.model)
fitted.test.probability=predict(final.log.model, test, type = 'response')
fitted.test.results= ifelse( fitted.test.probability>0.5 ,1, 0)
misCalError= mean(fitted.test.results!= test$Survived)
print(1-misCalError)
table(test$Survived, fitted.test.probability>0.5)
fitted.train.probability=predict(final.log.model, train, type = 'response')
fitted.train.results= ifelse( fitted.train.probability>0.5 ,1, 0)
train.misCalError= mean(fitted.train.results!= train$Survived)
print(1-train.misCalError)
table(train$Survived, fitted.train.probability>0.5)
ggplot(train, aes(Survived))+ geom_bar()

