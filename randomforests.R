install.packages("randomForest")
library("randomForest")

#Set the working diretory
setwd("C:/Users/hp/Downloads/")
application <- read.csv("application_credit.csv", header = TRUE, sep = ",")

data = application
str(data)
str(data$STATUS)

data$STATUS= ifelse(data$STATUS=='C',0,1)

#Removing the not-so-significant variables
col_names = c("CODE_GENDER","FLAG_OWN_CAR","CNT_CHILDREN","AMT_INCOME_TOTAL","FLAG_WORK_PHONE","FLAG_PHONE",
              "FLAG_EMAIL","CNT_FAM_MEMBERS","NAME_INCOME_TYPE","NAME_EDUCATION_TYPE","NAME_FAMILY_STATUS",
              "DAYS_EMPLOYED","MONTHS_BALANCE","STATUS")
data = data[col_names]
str(data)

colnames2 = c("AMT_INCOME_TOTAL","FLAG_PHONE","FLAG_EMAIL","MONTHS_BALANCE","STATUS")
student = ifelse(data$NAME_INCOME_TYPE=="Student",1,0)
HigherEducation = ifelse(data$NAME_EDUCATION_TYPE=="Higher education",1,0)
IncompleteEducation = ifelse(data$NAME_EDUCATION_TYPE=="Incomplete higher",1,0)
SecondaryEducation = ifelse(data$NAME_EDUCATION_TYPE=="Secondary / secondary special",1,0)
widow = ifelse(data$NAME_FAMILY_STATUS=="Widow",1,0)

dataNew = data[colnames2]
dataNew = cbind(dataNew,student,HigherEducation,IncompleteEducation,SecondaryEducation,widow)

index_train = sample(1:nrow(dataNew),2 / 3 * nrow(dataNew))
# Create training set: training_set
train.data <- dataNew[index_train, ]
# Create test set: test_set
test.data <- dataNew[-index_train, ]
# Using Random Forest in Training Data
outputforest <- randomForest(as.factor(STATUS) ~ .,data = train.data[1:30000,],importance=TRUE)
print(outputforest)
# Predicting the test data 
y_pred =predict(outputforest,newdata = test.data)
y_pred
#Confusion Matrix
cm=table(test.data[,3],y_pred)
cm
#Plotting Random Forest
plot(outputforest)
summary(outputforest)

