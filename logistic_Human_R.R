
d_train<- read.csv(file="HR-Attrition-Data.csv", header=TRUE)

library(caTools)

library(funModeling)
profiling_num(d_train)

#---

d_train$WorkLifeBalance<-ifelse(d_train$WorkLifeBalance==1,"poor",ifelse(d_train$WorkLifeBalance==2,"average",ifelse(d_train$WorkLifeBalance==3,"good","very good")))
d_train$JobSatisfaction<-ifelse(d_train$JobSatisfaction==1,"poor",ifelse(d_train$JobSatisfaction==2,"average",ifelse(d_train$JobSatisfaction==3,"good","very good")))

library(dplyr)
Human_R <- filter(d_train, Department == "Human Resources")

library(devtools)
library(woe)
IV<-iv.mult(Human_R, y="Attrition", summary = TRUE)

# selecting variables with 0.1<IV<0.5
var<-IV[which(IV$InformationValue>0.1),]
var1<-var[which(var$InformationValue<0.5),]
final_var<-var1$Variable
x_train<-Human_R[final_var]
Attrition<-Human_R$Attrition
Human_R<-cbind(Attrition,x_train)
levels(Human_R$Attrition)<-0:1


set.seed(1545)
sample_0 <- sample.split(Human_R, SplitRatio = 0.9)
d_train_0 <- subset(Human_R, sample_0 == TRUE)
d_test_0 <- subset(Human_R, sample_0 == FALSE)

library(MASS)
lo<-rlm(Attrition~.,family=)
#------------------
logit_model<-step(glm(Attrition ~., family = binomial(link=logit),data = d_train_0))
summary(logit_model)

#-Find out which feature's best represent the outcome

ccc<-Human_R[,c('Attrition','YearsInCurrentRole','OverTime','JobLevel')]
IV<-iv.mult(ccc, y="Attrition", summary = TRUE)

##Dailyrate,Yearincurrentrole,maritalstatus,trainingtimeslastyear,jobinvolvement are features which strongly represents the outcome

#Beta coeffcients of the model equation
exp(cbind(OR = coef(logit_model), confint(logit_model)))

d_test_00<- d_test_0[-1]

#### Prediction on test set #####
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
write.csv(TR,"cutoff_check22.csv",row.names=F)

# logit_model accuracy - Confusion Matrix
library(SDMTools)
confusion.matrix (d_test_0$Attrition, pred_prob, threshold = 0.8)

#accuracy




#inferences 
par(mfrow=c(2,2))
plot(Human_R$Attrition,Human_R$JobLevel,main='HR_Joblevel')
plot(Human_R$Attrition,Human_R$YearsInCurrentRole,main='HR_Years_in_Current_role')
ggplot(Human_R, aes(OverTime, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("HR_Overtime")
