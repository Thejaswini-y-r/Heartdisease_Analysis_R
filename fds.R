#View the imported dataset
view(heart)

#Dimension of dataset
dim(heart)

#Details of Dataset
summary(heart)


#install.packages
library(caTools)
library(rpart)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(caTools)
library(class)
library(gmodels)
library(caret)
library(fastDummies)
library(tidyverse)


list.files(path = "../input")

# Structure of data
str(heart)

summary(heart)  # No null values
library(caTools)

# Split the dataset into test and training dataset
set.seed(99)
split = sample.split(heart$target, SplitRatio = 0.7)
train = subset(heart, split==TRUE)
test = subset(heart, split==FALSE)

# logistic regression

LogReg1 = glm(target~., data=train, family='binomial')

summary(LogReg1)
# Checking Multicolliniarity
cor(train)
abs(cor(train))>0.7
# No columns are highly correlated with each other. Let us view the above matrix as a heatmap.

library(reshape2)
melted_cormat = melt(abs(cor(train))>0.7)
head(melted_cormat)

ggplot(melted_cormat, aes(x = Var1, y=Var2, fill=as.numeric(value))) + geom_tile() +
  geom_text(aes(Var1, Var2, label=as.numeric(value)),color='black',size=2)+
  scale_color_gradient(low='blue',high='red') +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
# The plot clearly shows there are no highly correlated variables (positive or negative).

# Now since there is no multicollinearity we will removing Variables based on Significance Level
LogReg2 = glm(target~age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal,
              data=train, family='binomial')
summary(LogReg2)

LogReg2 = glm(target~age+sex+cp+trestbps+chol+restecg+thalach+exang+oldpeak+slope+ca+thal,
              data=train, family='binomial')
summary(LogReg2)

LogReg2 = glm(target~age+sex+cp+trestbps+chol+restecg+exang+oldpeak+slope+ca+thal,
              data=train, family='binomial')
summary(LogReg2)

LogReg2 = glm(target~age+sex+cp+trestbps+restecg+exang+oldpeak+slope+ca+thal,
              data=train, family='binomial')
summary(LogReg2)


LogReg2 = glm(target~sex+cp+trestbps+restecg+exang+oldpeak+slope+ca+thal,
              data=train, family='binomial')
summary(LogReg2)

# Making predictions
predictTrain = predict(LogReg2, type='response')

#Confusion matrix using threshold of 0.5
table(train$target, predictTrain>0.5)

#Accuracy on training set
(80+104)/nrow(train)

#Predictions on Test set
predictTest = predict(LogReg2, newdata=test, type='response')

#Confusion matrix using threshold of 0.5
table(test$target, predictTest>0.5)

#Accuracy
(28+45)/(nrow(test))


# Tree

library(rpart)
library(rpart.plot)
tree = rpart(target~., data=train, method='class')

prp(tree)

predictTree = predict(tree, newdata=test, type='class')

table(test$target, predictTree)

#Accuracy
(27+41)/nrow(test)



#logistic regression(Exception)

#heart$target[heart$target > 0] <- 1
#par(mfrow = c(1, 1))
#count(heart,14)
#barplot(table(heart$target),main="Fate", col="blue")

#heart$target<-as.factor(heart$target)
#heart.8<-heart
#heart.8[,2] <- factor(heart.8[,2])
#heart.8[,3] <- factor(heart.8[,3])
#heart.8[,6] <- factor(heart.8[,6])
#heart.8[,7] <- factor(heart.8[,7])
#heart.8[,9] <- factor(heart.8[,9])
#heart.8[,11] <- factor(heart.8[,11])
#heart.8[,12] <- factor(heart.8[,12])
#heart.8[,13] <- factor(heart.8[,13])

#logreg.fit <- glm(target~-age,data=heart.8,family=binomial)
#summary(logreg.fit)

#logreg.fit <- glm(target~-age-trestbps,data=heart.8,family=binomial)
#summary(logreg.fit)

#logreg.fit <- glm(target~-age-trestbps-thalach,data=heart.8,family=binomial)
#summary(logreg.fit)

#logreg.fit <- glm(target~-age-trestbps-thalach-chol,data=heart.8,family=binomial)
#summary(logreg.fit)

#logreg.fit <- glm(target~-age-trestbps-thalach-chol-oldpeak,data=heart.8,family=binomial)
#summary(logreg.fit)

#heart.1<-heart.8
#set.seed(1)

#Making training and testing Dataset
#train=sample(1:nrow(heart.1), nrow(heart.1)/2) 
#test=(-train)

#heart.1.train=heart.1[train,]
#heart.1.test=heart.1[test,]
#heart.1.test.output=heart.1$target[test]

#logreg.fit <- glm(target~-age-trestbps-thalach-chol-oldpeak,data=heart.1.train,family=binomial)
#summary(logreg.fit)

#logreg.fit.prob=predict(logreg.fit,heart.1.test,type="response")
#head(logreg.fit.prob)
#nrow(heart.1)

#logreg.fit.pred=rep(0,nrow(heart.1))
#logreg.fit.pred[logreg.fit.prob>.5]=1
#mean(logreg.fit.pred==heart.1.test.output)



