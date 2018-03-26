install.packages("gvlma")
install.packages("ggplot2")
install.packages("lattice")
install.packages("caret")
library(ggplot2)
library(lattice)
library(caret)
library(gvlma)
setwd("D:/download/MGS_ML/assignment1")
train.data=read.csv("blogData_train.csv", header =  FALSE)
table(train.data$V281)

#=================================================================
# Experiment 1
#=================================================================


#Experiment 1 dataframe
features1=c(51:60,281)
train.data$V281=ifelse(train.data$V281 == 0, 0, 1)
train1.data=train.data[,features1]
test.data= read.csv("blogData_test.csv", header =  FALSE)
test.data$V281=ifelse(test.data$V281 == 0, 0, 1)
test1.data=test.data[,features1]

#experiment 1 , linear model

lm.fit1 = lm( formula = V281 ~., data =train1.data )
summary(lm.fit1)

pred.lm1 = predict(lm.fit1, test1.data, se.fit = TRUE)
mse1=(sum((test1.data$V281 - pred.lm1$fit)^2))/7624
mse1
#0.1883511 

#without v5, v58,v59,v60
train1.data.modified = train1.data[,-c(5,8,9,10)]
lm.fit1.2 = lm( formula = V281 ~., data =train1.data.modified )
summary(lm.fit1.2)
test1.data.modified = test1.data[,-c(5,8,9,10)]
pred.lm1.2 = predict(lm.fit1.2, test1.data.modified, se.fit = TRUE)
pred.lm1.2 
mse2=sum((test1.data.modified$V281 - pred.lm1.2$fit)^2)/7624
mse2
#0.1883078
plot((test1.data$V281 - pred.lm1.2$fit))



#experiment 1 , logistic regression model

#with all 11 features
logit1 = glm( V281 ~., train1.data, family = "binomial")
summary(logit1)
pred.logit1 = predict(logit1,test1.data,TYPE = "response" ) 
predicted_value1 = ifelse(pred.logit1>0.5,1,0)
table(Actualvalue=test1.data.modified.lg$V281, predicted_value1)

#without v55,v60
train1.data.modified.lg = train1.data[,-c(5,10)]
test1.data.modified.lg = test1.data[,-c(5,10)]
logit1.2 = glm( V281 ~.,  train1.data.modified.lg , family = "binomial")
summary(logit1.2)
pred.logit1.2 = predict(logit1.2,test1.data.modified.lg,TYPE = "response" ) 
predicted_value2 = ifelse(pred.logit1.2>0.5,1,0)
table(Actualvalue=test1.data.modified.lg$V281, predicted_value2)




#=================================================================
# Experiment 2
#=================================================================


#Experiment 2 dataframe
features2=c(63:262,281)
train2.data=train.data[,features2]
test2.data=test.data[,features2]

#experiment 2 , linear model

summary(train2.data)
lm.fit2 = lm( formula = V281 ~., data =train2.data )
summary(lm.fit2)
#remove na features
numofvariables=length(lm.fit2$coefficients)-1
sum(is.na(lm.fit2$coefficients))
for (i in 1:numofvariables){
  if(is.na(lm.fit2$coefficients[i])){
                                   nalist = c(nalist,i)
  }
}

train2.data = train2.data[,-nalist]

lm.fit2 = lm( formula = V281 ~., data =train2.data )
summary(lm.fit2.1)
pred.lm2 = predict(lm.fit2, test2.data, se.fit = TRUE)
mse2.1=sum((test2.data$V281 - pred.lm2$fit)^2)/7624
mse2.1
#0.2060684
plot((test2.data$V281 - pred.lm2$fit))

#experiment 2 , logistic regression model
#covert binary features to factors

logit2 = glm( V281 ~., train2.data, family = "binomial")
summary(logit2)
pred.logit2 = predict(logit2,test2.data,TYPE = "response" ) 
#confusionMatrix2
predicted_value2 = ifelse(pred.logit2>0.5,1,0)
table(Actualvalue=test1.data.modified.lg$V281, predicted_value2)
