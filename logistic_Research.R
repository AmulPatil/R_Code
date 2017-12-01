
d_train<- read.csv(file="HR-Attrition-Data.csv", header=TRUE)
levels(d_train$Attrition)<-0:1

library(caTools)

library(funModeling)
profiling_num(d_train)

#---

d_train$RelationshipSatisfaction<- as.factor(d_train$RelationshipSatisfaction)
d_train$WorkLifeBalance<-ifelse(d_train$WorkLifeBalance==1,"poor",ifelse(d_train$WorkLifeBalance==2,"average",ifelse(d_train$WorkLifeBalance==3,"good","very good")))
d_train$JobSatisfaction<-ifelse(d_train$JobSatisfaction==1,"poor",ifelse(d_train$JobSatisfaction==2,"average",ifelse(d_train$JobSatisfaction==3,"good","very good")))

library(dplyr)
Research_D <- filter(d_train,Department == "Research & Development")

library(devtools)
library(woe)
IV<-iv.mult(Research_D, y="Attrition", summary = TRUE)

# selecting variables with 0.1<IV<0.5
var<-IV[which(IV$InformationValue>0.1),]
var1<-var[which(var$InformationValue<0.7),]
final_var<-var1$Variable
x_train<-Research_D[final_var]
Attrition<-Research_D$Attrition
Research_D<-cbind(Attrition,x_train)
levels(Research_D$Attrition)<-0:1

set.seed(1545)


sample_0 <- sample.split(Research_D, SplitRatio = 0.8)

d_train_0 <- subset(Research_D, sample_0 == TRUE)

d_test_0 <- subset(Research_D, sample_0 == FALSE)

#------------------
logit_model<-step(glm(Attrition ~ ., family = binomial(link=logit),data = d_train_0))
summary(logit_model)


#----------
ccc<-Research_D[,c( 'Attrition','OverTime','YearsAtCompany','ï..Age','YearsInCurrentRole',
                    'MaritalStatus','EnvironmentSatisfaction','WorkLifeBalance','JobInvolvement')]
IV<-iv.mult(ccc, y="Attrition", summary = TRUE)


logit_model<-step(glm(Attrition ~ OverTime+YearsAtCompany+ï..Age+YearsInCurrentRole
                    +MaritalStatus+EnvironmentSatisfaction+WorkLifeBalance+JobInvolvement,family = binomial(link=logit),data = d_train_0))
summary(logit_model)

#'JobRole','OverTime','YearsAtCompany','TotalWorkingYears'
#YearsInCurrentRole','StockOptionLevel feature's best represent the outcome of attrition. 

#Beta coefficients
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
write.csv(TR,"cutoff_check23.csv",row.names=F)

# logit_model accuracy - Confusion Matrix
library(SDMTools)

confusion.matrix (d_test_0$Attrition, pred_prob, threshold = 0.550)

#got 87% accuracy

#inferences 
par(mfrow=c(2,2))
plot(Research_D$Attrition,Research_D$YearsInCurrentRole,main='Research_YearsInCurrentRole')
plot(Research_D$Attrition,Research_D$StockOptionLevel,main='Research_StockOptionLevel')
plot(Research_D$Attrition,Research_D$YearsAtCompany,main='Research_YearsAtCompany')


c<-ggplot(Research_D, aes(MaritalStatus, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("Research_Marital_status")
cc<-ggplot(Research_D, aes(OverTime, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("Research_Overtime")
ccc<-ggplot(Research_D, aes(JobRole, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("Research_Job_role")
multiplot(ccc,c,cc, cols=1)
