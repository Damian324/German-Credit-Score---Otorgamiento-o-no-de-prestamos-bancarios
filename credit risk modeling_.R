rm(list=ls())

setwd('C:/Users/Usuario/Desktop/kaggle/German Credit Score')

getwd()

library(readxl)
library(gmodels)
library(ggplot2)
library(dplyr)
options(scipen=999)

credit_raw <- read.csv('german_credit_data.csv')

######VISUALIZING DATA AND DATA PREP

#understanding variables
credit_raw <- credit_raw[-1]

#Job (numeric: 0 - unskilled and non-resident, 1 - unskilled and resident, 2 - skilled, 3 - highly skilled)
#Duration (numeric, in month)

length(unique(credit_raw$Duration))
table(credit_raw$Duration)

#TREATING JOB AS FACTORS+++++++++++
credit_raw$Job <- as.factor(credit_raw$Job)


###NA treatment

sapply(credit_raw,function(x) sum(is.na(x)))

savings_na <- credit_raw[is.na(credit_raw$Saving.accounts),]

table(credit_raw$Saving.accounts,credit_raw$Housing)##Housing suggests 'little' savings account
table(savings_na$Housing)

table(credit_raw$Saving.accounts,credit_raw$Job)
table(savings_na$Job)##Jobs suggests 'little' savings account


##Analyze saving vs housing,job,sex,age

sav_2 <- ggplot(credit_raw,aes(x = Housing))+
  geom_bar(aes(fill = Saving.accounts), position = 'dodge')

sav_2 <- ggplot(credit_raw,aes(x = Job))+
  geom_bar(aes(fill = Saving.accounts), position = 'dodge')

sav_3 <- ggplot(credit_raw,aes(x = Sex))+
  geom_bar(aes(fill = Saving.accounts), position = 'dodge')

sav_4 <- ggplot(credit_raw,aes(x = Age))+
  geom_bar(aes(fill = Saving.accounts), position = 'dodge')


gridExtra::grid.arrange(sav_1, sav_2, sav_3, sav_4,
             ncol = 2, nrow = 2)

##FOR SAVINGS, I WILL REPLACE WITH MODAL VALUES++++++

credit_raw[is.na(credit_raw$Saving.accounts),'Saving.accounts'] <- 'little'

nlevels(credit_raw$Saving.accounts)
levels(credit_raw$Saving.accounts)

unique(credit_raw$Checking.account)

che_1 <- ggplot(credit_raw,aes(x = Age))+
  geom_bar(aes(fill = Checking.account), position = 'dodge')

che_2 <- ggplot(credit_raw,aes(x = Housing))+
  geom_bar(aes(fill = Checking.account), position = 'dodge')

che_3 <- ggplot(credit_raw,aes(x = Job))+
  geom_bar(aes(fill = Checking.account), position = 'dodge')

che_4 <- ggplot(credit_raw,aes(x = Sex))+
  geom_bar(aes(fill = Checking.account), position = 'dodge')

gridExtra::grid.arrange(che_1, che_2, che_3, che_4,
                        ncol = 2, nrow = 2)

credit_raw %>%
  group_by(Sex,Job,Housing,Checking.account)%>%
  summarize(check_count = n())%>%
  arrange(desc(check_count))


##FEATURE ENGINEERING



#FOR CHECKING, I WILL ADD A NEW COLUMN FOR MISSNING OR NOT MISSING VALUE+++++++

credit_raw$missing_check <- 0

credit_raw[is.na(credit_raw$Checking.account),'missing_check'] <- 1

levels(credit_raw$Checking.account) <- c(levels(credit_raw$Checking.account),'missing')

credit_raw[is.na(credit_raw$Checking.account), 'Checking.account'] <- 'missing'

table(credit_raw$Checking.account)

credit_raw$missing_check <- as.factor(credit_raw$missing_check)

credit_raw <- credit_raw[c(1,2,3,4,5,6,11,7,8,9,10)]

sapply(credit_raw,function(x) sum(is.na(x)))


#######SEMESTERS TO RETURN
max(credit_raw$Duration)

#Rounded semesters to repay to bank
round(sort(unique(credit_raw$Duration))/6)

credit_raw$sem_to_repay <- as.factor(round(credit_raw$Duration/6))

credit_raw <- credit_raw[c(1,2,3,4,5,6,7,8,9,12,10,11)]

##Credit amount

hist(credit_raw$Credit.amount, breaks = sqrt(nrow(credit_raw)))

ggplot(credit_raw,aes(x=credit_raw$Credit.amount))+
  geom_histogram(bins = sqrt(nrow(credit_raw)),col = 'red')+
  scale_x_continuous(breaks = seq(0,max(credit_raw$Credit.amount),by = 1000))+
  theme(axis.text.x = element_text(angle = 75, hjust = 1))

credit_raw$credit.segment <- 0

max(credit_raw$Credit.amount)

cred_small <- credit_raw[1:100,]

for(i in 1:nrow(credit_raw)){
  
  
  if(credit_raw$Credit.amount[i] > 0 & credit_raw$Credit.amount[i] <= 2000 ){
  
  seg <- '0-2000'
  }else if(credit_raw$Credit.amount[i] > 2000 & credit_raw$Credit.amount[i] <= 4000){
  
  seg <- '2000-4000'
  }else if(credit_raw$Credit.amount[i] > 4000 & credit_raw$Credit.amount[i] <= 6000){
  
  seg <- '4000-6000'
  }else if(credit_raw$Credit.amount[i] > 6000 & credit_raw$Credit.amount[i] <= 8000){
    
    seg <- '6000-8000'
  }else if(credit_raw$Credit.amount[i] > 8000 & credit_raw$Credit.amount[i] <= 10000){
    
    seg <- '8000-10000'
  }else if(credit_raw$Credit.amount[i] > 10000 & credit_raw$Credit.amount[i] <= 12000){
    
    seg <- '10000-12000'
  }else if(credit_raw$Credit.amount[i] > 12000 & credit_raw$Credit.amount[i] <= 14000){
    
    seg <- '12000-14000'
  }else if(credit_raw$Credit.amount[i] > 14000 & credit_raw$Credit.amount[i] <= 16000){
    
    seg <- '14000-16000'
  }else if(credit_raw$Credit.amount[i] > 16000 & credit_raw$Credit.amount[i] <= 18000){
    
    seg <- '16000-18000'
  }else{
    
    seg <- '>18000'
  }

  credit_raw$credit.segment[i] <- seg

}

credit_raw[credit_raw$credit.segment == '>18000',]

credit_raw$credit.segment <- as.factor(credit_raw$credit.segment)

credit_raw <- credit_raw[c(1,2,3,4,5,6,7,8,13,9,10,11,12)]

table(credit_raw$Risk)

sort(unique(credit_raw$Risk))
as.numeric(sort(unique(credit_raw$Risk)))



###TRAIN/TEST SPLIT
set.seed(999)
split_index <- sample(1:nrow(credit_raw),nrow(credit_raw)*0.75)

train_credit <- credit_raw[split_index,]
test_credit <- credit_raw[-split_index,]

library(caret)

control <- trainControl(method='cv',number = 10)

formula <- as.formula(Risk ~ .)

##LOGISTIC REGRESSION
logit_model <- train(formula,data=train_credit,trControl = control,method='glm', family='binomial')

summary(logit_model)
logit_model

modelLookup("glm")#Available parameters using CARET

glm_pred <- predict(logit_model,test_credit)

logit_acc <- 1-mean(glm_pred != test_credit$Risk)

##RANDOM FOREST

mtry <- sqrt(ncol(test_credit))
tunegrid_rf <- floor(expand.grid(.mtry=mtry))

rf_model <- train(formula,data=train_credit,trControl = control,method='rf',tuneGrid=tunegrid)

rf_model

modelLookup('rf')

##Random search tuning-----

control_rf_ran <- trainControl(method="repeatedcv", number=5, repeats=3, search="random")

set.seed(999)


rfRandom <- train(formula, data=train_credit, method="rf",tuneLength=15,
                  trControl=control_rf)

rfRandom

##Grid Search
control_rf_grid <- trainControl(method="cv", number=10, search="grid")
set.seed(999)
tunegrid <- expand.grid(.mtry=c(1:15))
rfGrid <- train(formula, data=train_credit, method="rf",tuneGrid=tunegrid,
                trControl=control_rf_grid)
rfGrid

##Best fit RF----

rf_model

rf_model_pred <- predict(rf_model,newdata=test_credit)

rf_model_acc <- 1-mean(rf_model_pred != test_credit$Risk)




#######SVM

svm_model <- train(Risk ~ .,data=train_credit,trControl = control,method='svmLinear')

svm_model

svm_pred <- predict(svm_model,newdata=test_credit)

svm_acc <- 1-mean(svm_pred != test_credit$Risk)


#######KNN

knn_model <- train(formula,data=train_credit,trControl=control,method='knn')

knn_model

knn_pred <- predict(knn_model,newdata=test_credit)

1-mean(knn_pred != test_credit$Risk)


##TUNING KNN

knn_control <- trainControl(method='repeatedcv',number=5,repeats=3, search='grid')

tunegrid_knn <- expand.grid(.k=c(1:15))

knnGrid <- train(formula, data=train_credit, method="knn",tuneGrid=tunegrid_knn,
                trControl=control)

knnGrid

k_13_grid <- expand.grid(.k=13)

knn_model <- train(formula,data=train_credit,trControl=trainControl(method='repeatedcv', number=5,repeats=3),method='knn')

knn_pred <- predict(knn_model,newdata=test_credit)

knn_acc <- 1-mean(knn_pred != test_credit$Risk)


##Accuracy comparisons

logit_acc
knn_acc
rf_model_acc##best Acc
svm_acc

#rf_model_acc %79.6











