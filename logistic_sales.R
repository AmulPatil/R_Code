
d_train<- read.csv(file="HR-Attrition-Data.csv", header=TRUE)

library(caTools)

library(funModeling)
profiling_num(d_train)


d_train$WorkLifeBalance<-ifelse(d_train$WorkLifeBalance==1,"poor",ifelse(d_train$WorkLifeBalance==2,"average",ifelse(d_train$WorkLifeBalance==3,"good","very good")))
d_train$JobSatisfaction<-ifelse(d_train$JobSatisfaction==1,"poor",ifelse(d_train$JobSatisfaction==2,"average",ifelse(d_train$JobSatisfaction==3,"good","very good")))

library(dplyr)
sales <- filter(d_train,Department == "Sales")

library(devtools)
library(woe)
IV<-iv.mult(sales, y="Attrition", summary = TRUE)

# selecting variables with 0.1<IV<0.5
var<-IV[which(IV$InformationValue>0.1),]
var1<-var[which(var$InformationValue<0.8),]
final_var<-var1$Variable
x_train<-sales[final_var]
Attrition<-sales$Attrition
sales<-cbind(Attrition,x_train)
levels(sales$Attrition)<-0:1



set.seed(1545)


sample_0 <- sample.split(sales, SplitRatio = 0.8)

d_train_0 <- subset(sales, sample_0 == TRUE)

d_test_0 <- subset(sales, sample_0 == FALSE)

#------------------

logit_model<-step(glm(Attrition ~., family = binomial(link=logit),data = d_train_0))
summary(logit_model)

#----------

ccc<-d_train[,c('Attrition','MonthlyIncome','StockOptionLevel', 
                  'OverTime','JobRole','YearsWithCurrManager','DailyRate','JobInvolvement', 
                  'DistanceFromHome','BusinessTravel')]
IV<-iv.mult(ccc, y="Attrition", summary = TRUE)

#'MonthlyIncome','StockOptionLevel','JobInvolvement','BusinessTravel' 
#'OverTime','JobRole','YearsWithCurrManager'strong features which represents attrition rate

#beta co-efficients of our model
exp(cbind(OR = coef(logit_model), confint(logit_model)))

#### Prediction on test set #####

d_test_00<- d_test_0[-1]
pred_prob<-predict(logit_model, newdata=d_test_00, type="response")

# logit_model accuracy measures
library(ROCR)
pred <- prediction(pred_prob,d_test_0$Attrition)
# Area under the curve
performance(pred,'auc')
# creating ROC curve
roc<-performance(pred,"tpr","fpr")
plot(roc)
title("ROC Curve")

# For plotting accuracy measures
acc<-performance(pred,"tpr","tnr")
TR<- as.data.frame(cbind(acc@alpha.values[[1]],acc@x.values[[1]],acc@y.values[[1]]))
colnames(TR)<-c("Probability","TNR","TPR")
# removing infinity value from data frame
TR<-TR[-1,]
# reshape the data frame
library(reshape)
TR2<- melt(TR, measure.vars = c("TNR", "TPR"))
# plotting TNR, TPR on y axis and cutoff probability on x axis
library(ggplot2)
ggplot(TR2, aes(Probability, value, colour = variable)) + 
  geom_line()+ theme_bw()

# check file for choosing the cutoff value
write.csv(TR,"cutoff_check23.csv",row.names=F)

# logit_model accuracy - Confusion Matrix
library(SDMTools)
confusion.matrix (d_test_0$Attrition, pred_prob, threshold = 0.60)

#model accuracy came upto 88%

#inferences 

par(mfrow=c(2,2))
plot(sales$Attrition,sales$YearsAtCompany,main='Sales_Yrs@company')
plot(sales$Attrition,sales$ï..Age,main='Sales_age')
plot(sales$Attrition,sales$YearsInCurrentRole,main="Sales_yrs@current_role")
#EnvironmentSatisfaction plays crucial role as per plot
plot(sales$Attrition,sales$EnvironmentSatisfaction,main='Sales_Env_satisfaction')


c<-ggplot(sales, aes(OverTime, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("Sales_Overtime")
cc<-ggplot(sales, aes(MaritalStatus, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("Sales_Marital_status")
ccc<-ggplot(sales, aes(WorkLifeBalance, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("Sales_WorkLife_balance")

multiplot(ccc,c,cc, cols=1)
