# importing train and test data
f <- file.choose()
ff<- file.choose()
train<- read.csv(f)
test<- read.csv(ff)

table(train$Dependent)

train$Category<-c("Train")
test$Category<-c("Test")

# row binding imported train and test data
d<-rbind(train,test)
# recoding ordinal variables

library(plyr)
d$employee_count_code<-
  as.numeric(revalue(d$Founders_previous_company_employee_count,
         c("Small"=1, "Medium"=2, "Large"=3)))
# removing original variable from data frame
d$Founders_previous_company_employee_count = NULL




# partitioning of test and train set for own evaluation of models # seprating out 0 and 1 level
train_0 <- train[train$Dependent==0,]
train_1 <- train[train$Dependent==1,]
train_0$Category<-NULL
train_1$Category<-NULL


library(caTools)
sample_0 = sample.split(train_0, SplitRatio = .9) 
train_0_new = subset(train_0, sample_0 == TRUE)
test_0_new = subset(train_0, sample_0 == FALSE)


sample_1 = sample.split(train_1, SplitRatio = .9) 
train_1_new = subset(train_1, sample_1 == TRUE) 
test_1_new = subset(train_1, sample_1 == FALSE)


# final new train and test set
train_new<- rbind(train_1_new,train_0_new) 
test_new<- rbind(test_1_new,test_0_new)



# install developer tool and then woe package from gitub
# refer http://www.r-bloggers.com/r-credit-scoring-woe-information-value-in-woe-
#package/
install.packages("devtools")
library(devtools)
install_github("tomasgreif/woe")
library(woe)


#okay, sike
# only dependent and independent variable of training set # should be there in data frame
train_new$CAX_ID<-NULL
# calculation of information value
IV<-iv.mult(train,y="Dependent",TRUE)


# selecting variables with 0.1<IV<0.5
var<-IV[which(IV$InformationValue>0.1),]
var1<-var[which(var$InformationValue<0.5),]
final_var<-var1$Variable
x_train<-train_new[final_var]
Dependent<-train_new$Dependent 




train_final<-cbind(Dependent,x_train)



# fitting stepwise binary logistic regression with logit link function
mod<-step(glm(Dependent~., family = binomial(link=logit),data = train_final))
# model summary
summary(mod)

# final logistic regression model
model<-glm(formula = Dependent ~ Company_competitor_count + 
Company_1st_investment_time + Founders_Data_Science_skills_score +
Company_big_data + Founders_publications + Founders_global_exposure,
family = binomial(link = logit), data = train_final)
# model summary
summary(model)


# odds ratios and 95% CI
exp(cbind(OR = coef(model), confint(model)))



# model fit (Hosmer and Lemeshow goodness of fit (GOF) test)
hoslem.test(train_new$Dependent,model$fitted.values, g=10)





#part 6

# Prediction on test set
pred_prob<-predict (model, newdata=test_new, type="response")
# model accuracy measures
install.packages("ROCR",dep=T)
library(ROCR)
pred <- prediction (pred_prob, test_new$Dependent) 
# Area under the curve
performance(pred, 'auc')
# creating ROC curve
roc <- performance(pred,"tpr","fpr")
plot(roc)

# create data frame of values
perf <-as.data.frame(cbind(roc@alpha.values[[1]], roc@x.values[[1]],
roc@y.values[[1]]))
colnames(perf) <-c("Probability","FPR","TPR")

perf <-perf[-1,]

install.packages("reshape")
library(reshape)
perf2<- melt(perf, measure.vars = c("FPR", "TPR"))

library(ggplot2)
ggplot(perf2, aes(Probability, value, colour = variable)) +
  geom_line()+ theme_bw()

# model accuracy - Confusion Matrix
install.packages("SDMTools",dep=T)
library(SDMTools)
confusion.matrix (test_new$Dependent, pred_prob, threshold = 0.42)


# Prediction on test set of CAX
pred_CAX<- predict(model, newdata=test, type="response") 
submit_CAX<- cbind(test$CAX_ID,pred_CAX) 
colnames(submit_CAX)<- c("CAX_ID", "Dependent")
write.csv(submit_CAX,"Predictions.csv",row.names=F)
