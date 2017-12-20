
# Loading libraries -------------------------------------------------------


library(easypackages)
libraries("grid","caTools","woe","devtools","ggplot2","caret","gridExtra","C50","randomForest","corrplot","infotheo",
          "funModeling", "vcd", "dplyr","ROSE","rpart")
options(scipen=999)

#reading the data into R
d_full <- read.csv(file="HR-Attrition-Data.csv", header=TRUE,
                   stringsAsFactors = T)



#changing the independent varibles into numeric
levels(d_full$Attrition) <- 0:1
d_full <- cbind(d_full$Attrition,d_full)
d_full$Attrition <- NULL
names(d_full)[names(d_full) == "d_full$Attrition"] <- "Attrition"
profiling_num(d_full)
df_status(d_full)
#removing columns which doesn't effect dependent variable. 

d_full$EmployeeNumber <- NULL
d_full$Over18 <- NULL
names(d_full)[names(d_full) == "ï..Age"] <- "Age"

#Conversion into factors some of the varibles

fac_c <- c(7,10,13,14,16,23,24,26,29)

for (i in fac_c){
  d_full[,i]<-as.factor(d_full[,i])
}

# feature_enginerring -----------------------------------------------------

d_full$TenurePerJob <- ifelse(d_full$NumCompaniesWorked!=0, 
                              d_full$TotalWorkingYears/d_full$NumCompaniesWorked,0)
d_full$YearWithoutChange <- d_full$TenurePerJob- d_full$YearsSinceLastPromotion+as.integer(d_full$JobSatisfaction)+as.integer(d_full$EnvironmentSatisfaction)

# plots~EDA ---------------------------------------------------------------

#overall distribution of attrition
ggplot(d_full,aes(Attrition,fill=Attrition))+geom_bar()

#those who travel rarely has high attrition although there is 
#proportion is high among frequen traveller
f<- ggplot(d_full, aes(BusinessTravel, ..count..))+
  geom_bar(aes(fill = Attrition), position = "dodge")+
  ggtitle("Overall_organisation_behaviour")
prop.table(table(d_full$Attrition,d_full$BusinessTravel))

#high attrition when rating was low
f1<- ggplot(d_full, aes(PerformanceRating, ..count..))+
  geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("Overall_organisation_behaviour")
prop.table(table(d_full$Attrition,d_full$PerformanceRating))


#high attrition when jobsatisfaction is 1&3
f2<-ggplot(d_full, aes(JobSatisfaction, ..count..))+
  geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("Overall_organisation_behaviour")
prop.table(table(d_full$Attrition,d_full$JobSatisfaction))

#high attrition among single candidates
f3<- ggplot(d_full, aes(MaritalStatus, ..count..))+
  geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("Overall_organisation_behaviour")
prop.table(table(d_full$Attrition,d_full$MaritalStatus))


#attrition among MonthlyIncome employee's
f4<- ggplot(d_full,aes(MonthlyIncome,fill=Attrition))+geom_density(alpha=.5)+ggtitle("Overall_organisation_behaviour")

#attrition among 30~35 is high in the age distribution
f5<-ggplot(d_full,aes(Age,fill=Attrition))+geom_density(alpha=.5)+ggtitle("Overall_organisation_behaviour")

#low salary  since the mean is less compared to who are staying.
f6<-ggplot(d_full,aes(MonthlyIncome,fill=Attrition))+geom_density(alpha=.5)+ggtitle("Overall_organisation_behaviour")

#high attrtiion among freshers compared to expereienced
f7<-ggplot(d_full,aes(TotalWorkingYears,fill=Attrition))+geom_density(alpha=.5)+ggtitle("Overall_organisation_behaviour")



#overall distribution of attrition
f8<-ggplot(d_full,aes(PercentSalaryHike,fill=Attrition))+geom_bar()+ggtitle("Overall_organisation_behaviour")
prop.table(table(d_full$Attrition,d_full$PercentSalaryHike))
#1.2~2.7% proportion of attrition happens when salary is hiked in the range of 11~15%

# building corelation matrix
d_full_0<-d_full[,c("PercentSalaryHike","MonthlyIncome",'TotalWorkingYears',"YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager",
                    "NumCompaniesWorked","YearsAtCompany","Age","MonthlyRate","HourlyRate")]
heart_disease_4_disc = discretize(d_full_0)

# information
heart_info = mutinformation(heart_disease_4_disc, method = "emp")

diag(heart_info) = 0

f9<-corrplot(heart_info, method = "color", type = "lower", 
         number.cex = 0.6, addCoef.col = "black", tl.col = "red", 
         tl.srt = 90, tl.cex = 0.9, diag = FALSE, is.corr = F)


f10 <- ggplot(d_full,aes(TenurePerJob,fill=Attrition))+geom_density(alpha=.5)+
  ggtitle("Overall_dept_attrition_behaviour")

f11 <- ggplot(d_full,aes(YearWithoutChange,fill=Attrition))+geom_density(alpha=.5)+
  ggtitle("Overall_dept_attrition_behaviour")

# binning -----------------------------------------------------------------

#monthlyincome was skewed to left ,capped at 96%
qn <- quantile(d_full$MonthlyIncome,0.93, na.rm = TRUE)
d_full <- within(d_full, {MonthlyIncome <- ifelse(MonthlyIncome < qn, MonthlyIncome,qn )})

#creating different dataset department wise which is further used in multiple models.
Research_D <- filter(d_full,Department == "Research & Development")
Human_R <- filter(d_full, Department == "Human Resources")
sales <- filter(d_full,Department == "Sales")

#To find out which are the strong variables which best predict the output
IV <- iv.mult(d_full, y="Attrition", summary = TRUE)

# selecting variables with 0.1<IV<0.5
var <- IV[which(IV$InformationValue>0.1),]
var1 <- var[which(var$InformationValue<0.5),]
final_var <- var1$Variable
x_train <- d_full[final_var]
Attrition <- d_full$Attrition
d_full <- cbind(Attrition,x_train)




# split_data for whole organisation--------------------------------------------------------------


#Splitting data into train and test dataset
set.seed(1545)
sample_0 <- sample.split(d_full, SplitRatio = 0.7)
d_train <- subset(d_full, sample_0 == TRUE)
d_test <- subset(d_full, sample_0 == FALSE)


#creation of model using stepwise logistic regression--------------
logit_model <- step(glm(Attrition ~.,family = binomial(link=logit),data = d_train))


# Prediction on test set 
pred_prob <- predict(logit_model, newdata=d_test[-1], type="response")

# logit_model accuracy - Confusion Matrix
confusionMatrix(ifelse(pred_prob > .307,1,0),d_test$Attrition)

coefficients <- coef(summary(logit_model))
maxPColumn <- rownames(coefficients)[2:nrow(coefficients)]
maxPColumn


# decision tree -----------------------------------------------------------

#training the model:
decision_model <- C5.0(d_train[-1], d_train$Attrition)
#Prediction on test set
decision_pred <- predict(decision_model,d_test)
#result of the decision_tree_model:
confusionMatrix(d_test$Attrition, decision_pred)



# random forest -----------------------------------------------------------
#Training data using random forest
rand_model <- randomForest(Attrition ~ ., data = d_train)
# Prediction on test set 
prediction <- predict(rand_model, d_test[-1])
#result of the random_forest_model
confusionMatrix(d_test$Attrition, prediction)




# research_department -----------------------------------------------------

#eda

#overall distribution of attrition
ggplot(Research_D,aes(Attrition,fill=Attrition))+geom_bar()

#those who travel rarely has high attrition although there is 
#proportion is high among frequen traveller
r<-ggplot(Research_D, aes(BusinessTravel, ..count..))+
  geom_bar(aes(fill = Attrition), position = "dodge")+
  ggtitle("research_dept_attrition_behaviour")
prop.table(table(Research_D$Attrition,Research_D$BusinessTravel))

#high attrition when rating was low
r1<-ggplot(Research_D, aes(PerformanceRating, ..count..))+
  geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("research_dept_attrition_behaviour")
prop.table(table(Research_D$Attrition,Research_D$PerformanceRating))


#high attrition when jobsatisfaction is 1&3
r2<-ggplot(Research_D, aes(JobSatisfaction, ..count..))+
  geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("research_dept_attrition_behaviour")
prop.table(table(Research_D$Attrition,Research_D$JobSatisfaction))

#high attrition among single candidates
r3<-ggplot(Research_D, aes(MaritalStatus, ..count..))+
  geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("research_dept_attrition_behaviour")
prop.table(table(Research_D$Attrition,Research_D$MaritalStatus))


#attrition among MonthlyIncome employee's
r4<-ggplot(Research_D,aes(MonthlyIncome,fill=Attrition))+geom_density(alpha=.5)+ggtitle("research_dept_attrition_behaviour")

#attrition among 30~35 is high in the age distribution
r5<-ggplot(Research_D,aes(Age,fill=Attrition))+geom_density(alpha=.5)+ggtitle("research_dept_attrition_behaviour")

#low salary  since the mean is less compared to who are staying.
r6<-ggplot(Research_D,aes(MonthlyIncome,fill=Attrition))+geom_density(alpha=.5)+ggtitle("research_dept_attrition_behaviour")

#high attrtiion among freshers compared to expereienced
r7<-ggplot(Research_D,aes(TotalWorkingYears,fill=Attrition))+geom_density(alpha=.5)+ggtitle("research_dept_attrition_behaviour")



#overall distribution of attrition
r8<-ggplot(Research_D,aes(PercentSalaryHike,fill=Attrition))+geom_bar()+ggtitle("research_dept_attrition_behaviour")
prop.table(table(Research_D$Attrition,Research_D$PercentSalaryHike))
#1.2~2.7% proportion of attrition happens when salary is hiked in the range of 11~15%

# building corelation matrix
Research_D_0<-Research_D[,c("PercentSalaryHike","MonthlyIncome",'TotalWorkingYears',"YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager",
                            "NumCompaniesWorked","YearsAtCompany","Age","MonthlyRate","HourlyRate")]
heart_disease_4_disc = discretize(Research_D_0)

# information
heart_info = mutinformation(heart_disease_4_disc, method = "emp")

diag(heart_info) = 0

r9<-corrplot(heart_info, method = "color", type = "lower", 
         number.cex = 0.6, addCoef.col = "black", tl.col = "red", 
         tl.srt = 90, tl.cex = 0.9, diag = FALSE, is.corr = F)

r10 <- ggplot(Research_D,aes(TenurePerJob,fill=Attrition))+geom_density(alpha=.5)+
  ggtitle("sales_dept_attrition_behaviour")

r11 <- ggplot(Research_D,aes(YearWithoutChange,fill=Attrition))+geom_density(alpha=.5)+
  ggtitle("sales_dept_attrition_behaviour")


# split data for research -------------------------------------------------

Research_D$Department <- NULL

#To find out which are the strong variables which best predict the output
IV <- iv.mult(Research_D, y="Attrition", summary = TRUE)

# selecting variables with 0.1<IV<0.5
var <- IV[which(IV$InformationValue>0.1),]
var1 <- var[which(var$InformationValue<0.5),]
final_var <- var1$Variable
x_train <- Research_D[final_var]
Attrition <- Research_D$Attrition
Research_D <- cbind(Attrition,x_train)




#Splitting data into train and test dataset
set.seed(1545)
sample_0 <- sample.split(Research_D, SplitRatio = 0.7)
dr_train <- subset(Research_D, sample_0 == TRUE)
dr_test <- subset(Research_D, sample_0 == FALSE)


#creation of model using stepwise logistic regression--------------
logit_modelr <- step(glm(Attrition ~.,family = binomial(link=logit),data = dr_train))

# Prediction on test set 
pred_probr <- predict(logit_modelr, newdata=dr_test[-1], type="response")

# logit_model accuracy - Confusion Matrix
confusionMatrix(ifelse(pred_probr > .307,1,0),dr_test$Attrition)

coefficientsr <- coef(summary(logit_modelr))
maxPColumnr <- rownames(coefficientsr)[2:nrow(coefficientsr)]
maxPColumnr


# decision tree -----------------------------------------------------------

#training the model:
decision_modelr <- C5.0(dr_train[-1], dr_train$Attrition)
#Prediction on test set
decision_predr <- predict(decision_modelr,dr_test)
#result of the decision_tree_model:
confusionMatrix(dr_test$Attrition, decision_predr)



# random forest -----------------------------------------------------------
#Training data using random forest
rand_modelr <- randomForest(Attrition ~ ., data = dr_train)
# Prediction on test set 
predictionr <- predict(rand_modelr, dr_test[-1])
#result of the random_forest_model
confusionMatrix(dr_test$Attrition, predictionr)






# sales_department_EDA --------------------------------------------------------


# plots~EDA ---------------------------------------------------------------

#overall distribution of attrition
ggplot(sales,aes(Attrition,fill=Attrition))+geom_bar()

#those who travel rarely has high attrition although there is 
#proportion is high among frequen traveller
s<-ggplot(sales, aes(BusinessTravel, ..count..))+
  geom_bar(aes(fill = Attrition), position = "dodge")+
  ggtitle("sales_dept_attrition_behaviour")
prop.table(table(sales$Attrition,sales$BusinessTravel))

#high attrition when rating was low
s1<-ggplot(sales, aes(PerformanceRating, ..count..))+
  geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("sales_dept_attrition_behaviour")
prop.table(table(sales$Attrition,sales$PerformanceRating))


#high attrition when jobsatisfaction is 1&3
s2<-ggplot(sales, aes(JobSatisfaction, ..count..))+
  geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("sales_dept_attrition_behaviour")
prop.table(table(sales$Attrition,sales$JobSatisfaction))

#high attrition among single candidates
s3<-ggplot(sales, aes(MaritalStatus, ..count..))+
  geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("sales_dept_attrition_behaviour")
prop.table(table(sales$Attrition,sales$MaritalStatus))


#attrition among MonthlyIncome employee's
s4<-ggplot(sales,aes(MonthlyIncome,fill=Attrition))+geom_density(alpha=.5)+ggtitle("sales_dept_attrition_behaviour")

#attrition among 30~35 is high in the age distribution
s5<-ggplot(sales,aes(Age,fill=Attrition))+geom_density(alpha=.5)+ggtitle("sales_dept_attrition_behaviour")

#low salary  since the mean is less compared to who are staying.
s6<-ggplot(sales,aes(MonthlyIncome,fill=Attrition))+geom_density(alpha=.5)+ggtitle("sales_dept_attrition_behaviour")

#high attrtiion among freshers compared to expereienced
s7<-ggplot(sales,aes(TotalWorkingYears,fill=Attrition))+geom_density(alpha=.5)+ggtitle("sales_dept_attrition_behaviour")



#overall distribution of attrition
s8<-ggplot(sales,aes(PercentSalaryHike,fill=Attrition))+geom_bar()+ggtitle("sales_dept_attrition_behaviour")
prop.table(table(sales$Attrition,sales$PercentSalaryHike))
#1.2~2.7% proportion of attrition happens when salary is hiked in the range of 11~15%

# building corelation matrix
sales_0<-sales[,c("PercentSalaryHike","MonthlyIncome",'TotalWorkingYears',"YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager",
                  "NumCompaniesWorked","YearsAtCompany","Age","MonthlyRate","HourlyRate")]
heart_disease_4_disc = discretize(sales_0)

# information
heart_info = mutinformation(heart_disease_4_disc, method = "emp")

diag(heart_info) = 0

s9<-corrplot(heart_info, method = "color", type = "lower", 
         number.cex = 0.6, addCoef.col = "black", tl.col = "red", 
         tl.srt = 90, tl.cex = 0.9, diag = FALSE, is.corr = F)


s10 <- ggplot(sales,aes(TenurePerJob,fill=Attrition))+geom_density(alpha=.5)+
  ggtitle("sales_dept_attrition_behaviour")

s11 <- ggplot(sales,aes(YearWithoutChange,fill=Attrition))+geom_density(alpha=.5)+
  ggtitle("sales_dept_attrition_behaviour")


# split_for_sales ---------------------------------------------------------
sales$Department <- NULL
#To find out which are the strong variables which best predict the output
IV <- iv.mult(sales, y="Attrition", summary = TRUE)

# selecting variables with 0.1<IV<0.5
var <- IV[which(IV$InformationValue>0.1),]
var1 <- var[which(var$InformationValue<0.55),]
final_var <- var1$Variable
x_train <- sales[final_var]
Attrition <- sales$Attrition
sales <- cbind(Attrition,x_train)

#Splitting data into train and test dataset
set.seed(1545)
sample_0 <- sample.split(sales, SplitRatio = 0.7)
ds_train <- subset(sales, sample_0 == TRUE)
ds_test <- subset(sales, sample_0 == FALSE)


#creation of model using stepwise logistic regression--------------
logit_models <- step(glm(Attrition ~.,family = binomial(link=logit),data = ds_train))
coefficientss <- coef(summary(logit_models))
maxPColumns <- rownames(coefficientss)[2:nrow(coefficientss)]
maxPColumns
# Prediction on test set 
pred_probs <- predict(logit_models, newdata=ds_test[-1], type="response")

# logit_model accuracy - Confusion Matrix
confusionMatrix(ifelse(pred_probs > .307,1,0),ds_test$Attrition)

# decision tree -----------------------------------------------------------

#training the model:
decision_models <- C5.0(ds_train[-1], ds_train$Attrition)
#Prediction on test set
decision_preds <- predict(decision_models,ds_test)
#result of the decision_tree_model:
confusionMatrix(ds_test$Attrition, decision_preds)



# random forest -----------------------------------------------------------
#Training data using random forest
rand_models <- randomForest(Attrition ~ ., data = ds_train)
# Prediction on test set 
predictions <- predict(rand_models, ds_test[-1])
#result of the random_forest_model
confusionMatrix(ds_test$Attrition, predictions)












# Human_resource_dept -----------------------------------------------------


#overall distribution of attrition
ggplot(Human_R,aes(Attrition,fill=Attrition))+geom_bar()

#those who travel rarely has high attrition although there is 
#proportion is high among frequen traveller
h<-ggplot(Human_R, aes(BusinessTravel, ..count..))+
  geom_bar(aes(fill = Attrition), position = "dodge")+
  ggtitle("human_resource_dept_attrition_behaviour")
prop.table(table(Human_R$Attrition,Human_R$BusinessTravel))

#high attrition when rating was low
h1<-ggplot(Human_R, aes(PerformanceRating, ..count..))+
  geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("human_resource_dept_attrition_behaviour")
prop.table(table(Human_R$Attrition,Human_R$PerformanceRating))


#high attrition when jobsatisfaction is 1&3
h2<-ggplot(Human_R, aes(JobSatisfaction, ..count..))+
  geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("human_resource_dept_attrition_behaviour")
prop.table(table(Human_R$Attrition,Human_R$JobSatisfaction))

#high attrition among single candidates
h3<-ggplot(Human_R, aes(MaritalStatus, ..count..))+
  geom_bar(aes(fill = Attrition), position = "dodge")+ggtitle("human_resource_dept_attrition_behaviour")
prop.table(table(Human_R$Attrition,Human_R$MaritalStatus))


#attrition among MonthlyIncome employee's
h4<-ggplot(Human_R,aes(MonthlyIncome,fill=Attrition))+geom_density(alpha=.5)+ggtitle("human_resource_dept_attrition_behaviour")

#attrition among 30~35 is high in the age distribution
h5<-ggplot(Human_R,aes(Age,fill=Attrition))+geom_density(alpha=.5)+ggtitle("human_resource_dept_attrition_behaviour")

#low salary  since the mean is less compared to who are staying.
h6<-ggplot(Human_R,aes(MonthlyIncome,fill=Attrition))+geom_density(alpha=.5)+ggtitle("human_resource_dept_attrition_behaviour")

#high attrtiion among freshers compared to expereienced
h7<-ggplot(Human_R,aes(TotalWorkingYears,fill=Attrition))+geom_density(alpha=.5)+ggtitle("human_resource_dept_attrition_behaviour")



#overall distribution of attrition
h8<-ggplot(Human_R,aes(PercentSalaryHike,fill=Attrition))+geom_bar()+ggtitle("human_resource_dept_attrition_behaviour")
prop.table(table(Human_R$Attrition,Human_R$PercentSalaryHike))
#1.2~2.7% proportion of attrition happens when salary is hiked in the range of 11~15%

# building corelation matrix
Human_R_0<-Human_R[,c("PercentSalaryHike","MonthlyIncome",'TotalWorkingYears',"YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager",
                      "NumCompaniesWorked","YearsAtCompany","Age","MonthlyRate","HourlyRate")]
heart_disease_4_disc = discretize(Human_R_0)

# information
heart_info = mutinformation(heart_disease_4_disc, method = "emp")

diag(heart_info) = 0

h9<-corrplot(heart_info, method = "color", type = "lower", 
         number.cex = 0.6, addCoef.col = "black", tl.col = "red", 
         tl.srt = 90, tl.cex = 0.9, diag = FALSE, is.corr = F)

h10 <- ggplot(Human_R,aes(TenurePerJob,fill=Attrition))+geom_density(alpha=.5)+
  ggtitle("human_resource_dept_attrition_behaviour")

h11 <- ggplot(Human_R,aes(YearWithoutChange,fill=Attrition))+geom_density(alpha=.5)+
  ggtitle("human_resource_dept_attrition_behaviour")

#Splitting data into train and test dataset-----------
Human_R$Department <- NULL
Human_R <- filter(d_full, Department == "Human Resources")
Human_R <- ovun.sample(Attrition ~ ., data = Human_R, method = "over",N = 102)$data
#To find out which are the strong variables which best predict the output
IV <- iv.mult(Human_R, y="Attrition", summary = TRUE)

# selecting variables with 0.1<IV<0.5
var <- IV[which(IV$InformationValue>0.1),]
var1 <- var[which(var$InformationValue<0.5),]
final_var <- var1$Variable
x_train <- Human_R[final_var]
Attrition <- Human_R$Attrition
Human_R <- cbind(Attrition,x_train)


set.seed(1545)
sample_0 <- sample.split(Human_R, SplitRatio = 0.7)
dh_train <- subset(Human_R, sample_0 == TRUE)
dh_test <- subset(Human_R, sample_0 == FALSE)



#creation of model using stepwise logistic regression--------------
logit_modelh <- step(glm(Attrition ~.,family = binomial(link=logit),data = dh_train))

# Prediction on test set 
pred_probh <- predict(logit_modelh, newdata=dh_test[-1], type="response")

# logit_model accuracy - Confusion Matrix
confusionMatrix(ifelse(pred_probh > .507,1,0),dh_test$Attrition)
coefficientsh <- coef(summary(logit_modelh))
maxPColumnh <- rownames(coefficientsh)[2:nrow(coefficientsh)]
maxPColumnh


# decision tree -----------------------------------------------------------

#training the model:
decision_modelh <- C5.0(dh_train[-1], dh_train$Attrition)
#Prediction on test set
decision_predh <- predict(decision_modelh,dh_test)
#result of the decision_tree_model:
confusionMatrix(dh_test$Attrition, decision_predh)




# random forest -----------------------------------------------------------
#Training data using random forest
rand_modelh <- randomForest(Attrition ~ ., data = dh_train)
# Prediction on test set 
prediction <- predict(rand_modelh, dh_test[-1])
#result of the random_forest_model
confusionMatrix(dh_test$Attrition, prediction)





# overall_plot ------------------------------------------------------------

grid.arrange(f, r, h,s, ncol=2)
grid.arrange(f1, r1, h1,s1, ncol=2)
grid.arrange(f2, r2, h2,s2, ncol=2)#not required
grid.arrange(f3, r3, h3,s3, ncol=2)
grid.arrange(f4, r4, h4,s4, ncol=2)
grid.arrange(f5, r5, h5,s5, ncol=2)
grid.arrange(f6, r6, h6,s6, ncol=2)
grid.arrange(f7, r7, h7,s7, ncol=2)
grid.arrange(f8, r8, h8,s8, ncol=2)
grid.arrange(f10, r10, h10,s10, ncol=2)
grid.arrange(f11, r11, h11,s11, ncol=2)

