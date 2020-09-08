#Set the working diretory
setwd("C:/Users/Samuel/Documents/ITM 883")

#Load the datasets
application <- read.csv("application_record.csv", header = TRUE, sep = ",")
credit <- read.csv("credit_record.csv", header = TRUE, sep = ",")

View(application)
View(credit)

#merge the two datasets
total_data <- merge(application, credit, by = "ID")
View(total_data)

#what has been tested
#CODE_GENDER
#FLAG_OWN_CAR
#FLAW_OWN_REALTY
#CNT_CHILDREN
#AMT_INCOME_TOTAL
#FLAG_MOBIL
#FLAG_WORK_PHONE
#FLAG_PHONE
#FLAG_EMAIL
#CNT_FAM_MEMBERS

#what needs to be tested
#NAME_INCOME_TYPE
#NAME_EDUCATION_TYPE
#NAME_FAMILY_STATUS
#NAME_HOUSING_TYPE
#DAYS_BIRTH
#DAYS_EMPLOYED
#OCCUPATION_TYPE
#MONTHS_BALANCE

library(sqldf)

#Drop records that have X in Status
new_data <- total_data[ !(total_data$STATUS %in% 'X'), ]
View(new_data)

#Write csv file
#write.csv(new_data, "C:/Users/samuel/Documents/ITM 883/application_credit.csv", row.names = FALSE)

#Convert Status to a dummy variable
new_data$STATUS <- ifelse(new_data$STATUS == "C", 0 , 1)

#Hypothesis Testing with glm()
ModelA <- glm(STATUS ~ as.factor(CODE_GENDER), data = new_data, family = binomial)
summary(ModelA)
#Compares against Female
#Significant

ModelB <- glm(STATUS ~ as.factor(FLAG_OWN_CAR), data = new_data, family = binomial)
summary(ModelB)
#Significant

ModelC <- glm(STATUS ~ as.factor(FLAG_OWN_REALTY), data = new_data, family = binomial)
summary(ModelC)
#Not significant

ModelD <- glm(STATUS ~ CNT_CHILDREN, data = new_data, family = binomial)
summary(ModelD)
#Significant

ModelE <- glm(STATUS ~ AMT_INCOME_TOTAL, data = new_data, family = binomial)
summary(ModelE)
#Significant

ModelF <- glm(STATUS ~ FLAG_MOBIL, data = new_data, family = binomial)
summary(ModelF)
#Not significant

ModelG <- glm(STATUS ~ FLAG_WORK_PHONE, data = new_data, family = binomial)
summary(ModelG)
#Significant

ModelH <- glm(STATUS ~ FLAG_PHONE, data = new_data, family = binomial)
summary(ModelH)
#Significant

ModelI <- glm(STATUS ~ FLAG_EMAIL, data = new_data, family = binomial)
summary(ModelI)
#Significant

ModelJ <- glm(STATUS ~ CNT_FAM_MEMBERS, data = new_data, family = binomial)
summary(ModelJ)
#Significant

#Hypothesis Testing with glm() continued
ModelK <- glm(STATUS ~ as.factor(NAME_INCOME_TYPE), data = new_data, family = binomial)
summary(ModelI)
#Makes a comparison against Commercial associate
#Significant relationships

ModelL <- glm(STATUS ~ as.factor(NAME_EDUCATION_TYPE), data = new_data, family = binomial)
summary(ModelJ)
sqldf("SELECT DISTINCT(NAME_EDUCATION_TYPE)
      FROM new_data")
#Makes a comparison against Academic degree
#Significant relationships

ModelM <- glm(STATUS ~ as.factor(NAME_FAMILY_STATUS), data = new_data, family = binomial)
summary(ModelK)
sqldf("SELECT DISTINCT(NAME_FAMILY_STATUS)
      FROM new_data")
#Makes a comparison against civil marriage
#Significant relationships


ModelN <- glm(STATUS ~ as.factor(NAME_HOUSING_TYPE), data = new_data, family = binomial)
summary(ModelL)
sqldf("SELECT DISTINCT(NAME_HOUSING_TYPE)
      FROM new_data")
#Compares against Co-op apartments
#No significant relationships

ModelO <- glm(STATUS ~ DAYS_BIRTH, data = new_data, family = binomial)
summary(ModelE)
#Not significant

ModelP <- glm(STATUS ~ DAYS_EMPLOYED, data = new_data, family = binomial)
summary(ModelF)
#Significant

ModelQ <- glm(STATUS ~ as.factor(OCCUPATION_TYPE), data = new_data, family = binomial)
summary(ModelG)
query1 <- sqldf("SELECT DISTINCT (OCCUPATION_TYPE)
      FROM new_data")
#Consult the group about this variable!!!!!!

ModelR <- glm(STATUS ~ MONTHS_BALANCE, data = new_data, family = binomial)
summary(ModelH)
#Significant


#Run Hypothesis test with the variables that have significant relationships
ModelS <- glm(STATUS ~ as.factor(CODE_GENDER) + as.factor(FLAG_OWN_CAR) + CNT_CHILDREN + AMT_INCOME_TOTAL 
              + FLAG_WORK_PHONE + FLAG_PHONE + FLAG_EMAIL + CNT_FAM_MEMBERS + as.factor(NAME_INCOME_TYPE)
              + as.factor(NAME_EDUCATION_TYPE) + as.factor(NAME_FAMILY_STATUS) + DAYS_EMPLOYED + MONTHS_BALANCE,
              data = new_data, family = binomial)
summary(ModelS)



















