data  = application_credit
str(data)
str(data$STATUS)

#Converting the Dependent column to 0 and 1

data$STATUS= ifelse(data$STATUS=='C',0,1)

#Removing the not-so-significant variables
col_names = c("CODE_GENDER","FLAG_OWN_CAR","CNT_CHILDREN","AMT_INCOME_TOTAL","FLAG_WORK_PHONE","FLAG_PHONE",
              "FLAG_EMAIL","CNT_FAM_MEMBERS","NAME_INCOME_TYPE","NAME_EDUCATION_TYPE","NAME_FAMILY_STATUS",
              "DAYS_EMPLOYED","MONTHS_BALANCE","STATUS")
data = data[col_names]
str(data)

#This is a general GLM
Model1 <- glm(STATUS ~.,
              data = data, family = binomial)
summary(Model1)

#This uses feature selection
Model2 = step(Model1, direction = "both")
summary(Model2)

#This uses exhaustive search to pin down our algo
Model3 = leaps::regsubsets(STATUS~.,data=data, nbest=1,nvmax=ncol(data),method="exhaustive")
res=summary(Model3)
names(res)
res$which
plot(1:14,res$adjr2,type="b")
plot(1:14,res$bic,typ="b",col="red")
Model4 = leaps::regsubsets(STATUS~.,data=data, nbest=1,nvmax=9,method="exhaustive")
summary(Model4)

# new columns as per model
#AMT_INCOME_TOTAL
#FLAG_PHONE
#FLAG_EMAIL
#NAME_INCOME_TYPEStudent
#NAME_EDUCATION_TYPEHigher
#NAME_EDUCATION_TYPEIncomplete
#NAME_EDUCATION_TYPESecondary
#NAME_FAMILY_STATUSWidow
#MONTHS_BALANCE

colnames2 = c("AMT_INCOME_TOTAL","FLAG_PHONE","FLAG_EMAIL","MONTHS_BALANCE","STATUS")
student = ifelse(data$NAME_INCOME_TYPE=="Student",1,0)
HigherEducation = ifelse(data$NAME_EDUCATION_TYPE=="Higher education",1,0)
IncompleteEducation = ifelse(data$NAME_EDUCATION_TYPE=="Incomplete higher",1,0)
SecondaryEducation = ifelse(data$NAME_EDUCATION_TYPE=="Secondary / secondary special",1,0)
widow = ifelse(data$NAME_FAMILY_STATUS=="Widow",1,0)
#The new data with the replaced columns
dataNew = data[colnames2]
dataNew = cbind(dataNew,student,HigherEducation,IncompleteEducation,SecondaryEducation,widow)

index_train = sample(1:nrow(dataNew),2 / 3 * nrow(dataNew))
# Create training set: training_set
train.data <- dataNew[index_train, ]
# Create test set: test_set
test.data <- dataNew[-index_train, ]

#using classification Tree
library(rpart)
library(rpart.plot)
fit <- rpart(STATUS~., data = train.data, method = 'class')
rpart.plot(fit, extra = 106)
predict_unseen <-predict(fit, test.data, type = 'class')
table_mat <- table(test.data$STATUS, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, test.data, type = 'class')
  table_mat <- table(test.data$STATUS, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

control <- rpart.control(minsplit = 20,
                         minbucket = round(20 / 3),
                         maxdepth = 30,
                         cp = 0)
tune_fit <- rpart(STATUS~., data = train.data, method = 'class', control = control)
accuracy_tune(tune_fit)


