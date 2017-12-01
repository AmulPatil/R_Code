
#reading the data into R
d_full<- read.csv(file="HR-Attrition-Data.csv", header=TRUE,stringsAsFactors = T)

#changing the independent varibles into numeric
levels(d_full$Attrition)<-0:1

#removing columns which doesn't effect dependent variable. 
d_full$EmployeeNumber<- NULL

#get to know the dataset to find out any outlier's are available and finding NA value's.

library(funModeling)
profiling_num(d_full)
df_status(d_full)

#--
#departmental wise & educational field background related employee attrition mapping in organisation

d_sts<- d_full[,c(2,5,8),]
library(vcd)
structable(d_sts)

mapping<-structable(Department+Attrition~EducationField,d_sts)
mapping



#--------
#Information related to mapping
#Sales Dept:
  #29%attrition from technical graduates.
  #22% attrtiion from marketing domain graduates.
  #19% attrition from lifescience graduates.
  #16% attrition from medical graduates.
  #27%attrition from other discipline.
  ## overall attrition rate in  sales 20.52%
#Research Dept:
  #13%attrtion from life science.
  #13% attrition from medical background.
  #21% attrtiion from tech graduates..
  #11% attrtiion from other discipline.
  ## overall attrition rate in Research  Dept  13.65%  
#Human dept:
  #26% attrtiion rate who have done  hr as specialisation
  #15% for medical & 50% for tech graduates. 
  #6.2% from life science graduates.
  ## overall attrition rate in HR 19.92%
#---


library(ggplot2)
d_te<-d_full[,c(2,5,8,32),]
structable(Department+Attrition~EducationField,d_te)
structable(Attrition~YearsInCurrentRole,d_te)
plot(structable(Attrition~YearsInCurrentRole,d_te))
#attrition level is huge among freshers and
        #those who have 2 years of experence

plot(d_full$Attrition,d_full$YearsInCurrentRole)

#Converting numeric to categorical because these were the feature's which chi~square test showed significant.
d_full$WorkLifeBalance<-ifelse(d_full$WorkLifeBalance==1,"poor",ifelse(d_full$WorkLifeBalance==2,"average",ifelse(d_full$WorkLifeBalance==3,"good","very good")))
d_full$JobSatisfaction<-ifelse(d_full$JobSatisfaction==1,"poor",ifelse(d_full$JobSatisfaction==2,"average",ifelse(d_full$JobSatisfaction==3,"good","very good")))


#creating different dataset department wise which is further used in multiple models.
library(dplyr)
Research_D <- filter(d_full,Department == "Research & Development")
Human_R <- filter(d_full, Department == "Human Resources")
sales <- filter(d_full,Department == "Sales")

#To find out which are the strong variables which best predict the output
library(devtools)
library(woe)
IV<-iv.mult(d_full, y="Attrition", summary = TRUE)

# selecting variables with 0.1<IV<0.5
var<-IV[which(IV$InformationValue>0.1),]
var1<-var[which(var$InformationValue<0.5),]
final_var<-var1$Variable
x_train<-d_full[final_var]
Attrition<-d_full$Attrition
d_full<-cbind(Attrition,x_train)

#Splitting data into train and test dataset
library(caTools)
set.seed(1545)
sample_0 <- sample.split(d_full, SplitRatio = 0.8)
d_train <- subset(d_full, sample_0 == TRUE)
d_test <- subset(d_full, sample_0 == FALSE)

#creation of model using stepwise logistic regression
logit_model<-step(glm(Attrition ~., 
                      family = binomial(link=logit),data = d_train))

summary(logit_model)


#----------
#beta coefficients of model with confidence interval
exp(cbind(OR = coef(logit_model), confint(logit_model)))

#### Prediction on test set #####
d_test_0<- d_test[-1]
pred_prob<-predict(logit_model, newdata=d_test_0, type="response")

# logit_model accuracy measures
library(ROCR)
pred <- prediction(pred_prob,d_test$Attrition)
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
write.csv(TR,"cutoff_check21.csv",row.names=F)

# logit_model accuracy - Confusion Matrix
library(SDMTools)
confusion.matrix (d_test$Attrition, pred_prob, threshold = 0.55)

#--------------------
#got 90% accuracy for the above model.




# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}