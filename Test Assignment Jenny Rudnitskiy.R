
#title: "Titanic Test Assignment"
#author: "Jenny_Rudnitskiy"
#date: "21/01/2020"
#output: html_document

#Load the libraries
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
library(ggplot2)
library(MASS)
library(boot)
library(caret)
library(tree)
library(e1071)
library(stats)
library(tidyverse)



setwd('C:\\Users\\Jenny\\Desktop')
titanic_train <- read.csv("train.csv")
titanic_test<- read.csv("test.csv")

#Merging two datasets to clean all the data in both train and test datasets at the same time
titanic_complete <- bind_rows(titanic_train,titanic_test)

# Checking the structure
str(titanic_complete)

#Checking dimensions of the dataset

attach(titanic_complete)
dim(titanic_complete)

#Making it presentable in a table format

formattable(titanic_complete[1:10,])


#to check variables type
str(titanic_complete)

#stat summary of dataset
summary(titanic_complete)

#number of N/A values
colSums(is.na(titanic_complete))

colSums(titanic_complete=="")

#Embarked - Very small number of missing values, will replace with majority class

xtabs(~Embarked, data = titanic_complete)

titanic_complete$Embarked[titanic_complete$Embarked==""] <- 'S'
xtabs(~Embarked, data = titanic_complete)


missing.fare.pclass <- titanic_complete$Pclass[is.na(titanic_complete$Fare)]
median.fare <- median(x = titanic_complete$Fare[titanic_complete$Pclass == missing.fare.pclass], na.rm = T)
titanic_complete$Fare[is.na(titanic_complete$Fare)] <- median.fare

summary(titanic_complete$Fare)
 


#For Age data cleaning we will look into other data that are available: Name, PClass, Sex
titanic_complete%>%
  filter(is.na(Age))%>%
  dplyr::select(Age, Name, Pclass, Sex)


#Split Name into Title and Name
#splitting every element of Name looking into one example
strsplit(titanic_complete$Name[1], split='[,.]')[[1]]
titanic_complete$Title <- sapply(titanic_complete$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]}) #applying split and creating a new feature Title
titanic_complete$Title <- sub(' ', '', titanic_complete$Title)
table(titanic_complete$Title)

#Verifying the split Title against Name 
titanic_complete%>%
  filter(is.na(Age))%>%
  dplyr::select(Age, Title, Name, Pclass, Sex)

titanic_complete$Title[titanic_complete$Title %in% c('Capt', 'Col', 'Major')] <- 'Officer'
titanic_complete$Title[titanic_complete$Title %in% c('Jonkheer', 'Don', 'Sir', 'the Countess','Dona', 'Lady')] <- 'Royalty'

titanic_complete$Title[titanic_complete$Title %in% c('Dr', 'Rev', 'Mr')] <- 'Mr'
titanic_complete$Title[titanic_complete$Title %in% c('Mme', 'Ms', 'Mrs')] <- 'Mrs'
titanic_complete$Title[titanic_complete$Title %in% c('Mlle', 'Miss')] <- 'Miss'
titanic_complete$Title[titanic_complete$Title %in% c('Master')]<- 'Master'

table(titanic_complete$Title)    
count(titanic_complete,Title)

titanic_complete%>%
  filter(is.na(Age))%>%
  dplyr::group_by(Title)%>% 
  group_by(median(Age))
  
titanic_complete %>% 
  group_by(Title) %>% 
  summarise(median = median(Age, na.rm = TRUE))

titanic_complete <- titanic_complete %>%
  group_by(Title) %>%
  mutate(Age = ifelse(is.na(Age), round(median(Age, na.rm = TRUE), 1), Age)) 

summary(titanic_complete$Age)

#Has Cabin Flag

titanic_complete$Cabin[titanic_complete$Cabin==""] <- NA

titanic_complete<-titanic_complete %>% 
  mutate(Cabin_f = ifelse(!is.na(Cabin),1,0))



###############Visualisations#####################

#Looking into Age density 
dens_age_surv <- density(titanic_complete$Age[titanic_train$Survived==1], na.rm = TRUE)
plot(dens_age_surv)
dens_age_died <- density(titanic_complete$Age[titanic_train$Survived==0], na.rm = TRUE)
plot(dens_age_died)


plot(dens_age_surv, 
     ylim = c(0, 0.050), 
     col = "green", 
     lwd = 1.5, 
     main = "Density Estimates of \'Age\'",
     xlab = "Age")

lines(dens_age_died, col = "red", lwd = 1.5)

legend(legend = c("Survived", "Died"), x ="topright", 
       col = c("green", "red"), 
       lwd = 1.5)


titanic_complete%>%
  filter(!is.na(Survived))%>%
  ggplot(aes(Sex, fill = Survived), position = "fill")+
  geom_bar(stat = "count")


ggplot(titanic_complete[1:891,], aes(Sex, fill=Survived)) +
  geom_bar(position = "fill") +
  ylab("Survival Rate") +
  ggtitle("Survival Rate by Gender")

ggplot(titanic_complete[1:891,], aes(Cabin_f, fill=Survived)) +
  geom_bar(position = "fill") +
  ylab("Survival Rate") +
  ggtitle("Survival Rate by Cabin Flag")

g <- ggplot(titanic_complete, aes(Age))
g + geom_histogram()
g + geom_histogram(aes(fill=Survived))
g + geom_density()
g + geom_density(aes(fill=Sex))
g + geom_density(aes(fill=Sex), alpha=0.3)


ggplot(titanic_train, aes(Sex, Age)) + geom_boxplot()
ggplot(titanic_train, aes(Pclass))+geom_bar()
ggplot(titanic_complete[1:891,], aes(Pclass, fill=Survived))+geom_bar()


barchart <- ggplot(titanic_complete[1:891,], aes(Pclass, fill=Survived))+geom_bar()
barchart+xlab("Passenger Class")+ylab("Number of Passengers")+ggtitle("Survival by Passenger Class")+
  scale_fill_discrete(name = "", labels = c("Died", "Survived"))



#################################################################

library(dplyr)
titanic_complete <- titanic_complete %>%
  dplyr::select(-c(Cabin, PassengerId, Ticket, Name, Title))


####################### Factor ##################################
factor <- data.frame(select_if(titanic_complete, is.factor))
ncol(factor)

titanic_complete$Survived<- as.factor(titanic_complete$Survived)
titanic_complete$Pclass<-as.factor(titanic_complete$Pclass)
titanic_complete$Embarked<-as.factor(titanic_complete$Embarked)
titanic_complete$Cabin_f<-as.factor(titanic_complete$Cabin_f)

titanic_complete$Sex<-as.numeric(titanic_complete$Sex)
titanic_complete$Embarked<-as.numeric(titanic_complete$Embarked)
##titanic_complete$Title<-as.numeric(titanic_complete$Title)


################## with labels #####################################
titanic_complete$Survived<- factor(titanic_complete$Survived, levels = c(0,1), labels = c('No','Yes'))
titanic_complete$Pclass<-factor(titanic_complete$Pclass, levels = c(1,2,3),
                                   labels = c("1st", "2nd", "3rd"))
titanic_complete$Embarked<-as.factor(titanic_complete$Embarked)
titanic_complete$Cabin_f<-as.factor(titanic_complete$Cabin_f)


###################################################################


#Modeling

#Split again into train and test and fit the logistic regression model


titanic_train <- titanic_complete[1:891,]
titanic_test <- titanic_complete[892:1309,]

##############Logistic Regression ###############################

glm_model = glm(Survived~.,data= titanic_train, family = 'binomial')
summary(glm_model)


## Using anova() to analyze the table of devaiance
anova(glm_model, test="Chisq")


final_model = glm(Survived~Sex + Pclass + Age + SibSp + Cabin_f, data = titanic_train, family = 'binomial')
summary(final_model)

str(titanic_test$Survived)
varImp(glm_model)

titanic_test


titanic_train$Survived<-as.factor(titanic_train$Survived)
titanic_test$Survived<-as.factor(titanic_test$Survived)
contrasts(Survived)

glm.prob<-predict(final_model, type='response')
glm.pred<-rep("Died",891)
glm.pred[glm.prob >0.5]= "Survived"

confusion_matrix_f<-table(glm.pred, titanic_train$Survived)
confusion_matrix_f

Misclassification_Rate<-(74+81)/(475+81+74+261)
Misclassification_Rate

Accuracy_Test_f<- sum(diag(confusion_matrix_f)) / sum(confusion_matrix_f)
Accuracy_Test_f

library(ROCR)

ROCRpred <- prediction(glm.prob, titanic_train$Survived)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")

plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))

auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

glm.prob1<-predict(final_model, titanic_test, type='response')
glm.pred1<-rep("Died",891)
glm.pred1[glm.prob1 >0.5]= "Survived"

confusion_matrix_f<-table(glm.pred1, titanic_train$Survived)
confusion_matrix_f

Accuracy_Test_f<- sum(diag(confusion_matrix_f)) / sum(confusion_matrix_f)
Accuracy_Test_f

set.seed(1)

cv_error_10= rep (0,10)

for (i in 1:10) {
  glm_model_10=glm(Survived~Sex + Pclass + Age + SibSp + Cabin_f, data = titanic_train, family = 'binomial')
  cv_error_10[i]=cv.glm(titanic_train, glm_model_10, K=10) $delta [1]
}
plot(cv_error_10, type = "b", xlab = "Order of Polynomials", ylab = "Error Rate")


library(cvTools)
## set up folds for cross-validation
folds <- cvFolds(nrow(titanic_train), K = 10, R = 50)
## compare LS, MM and LTS regression
# perform cross-validation for an LS regression model
fitglm <- glm(Survived~Sex + Pclass + Age + SibSp + Cabin_f, data = titanic_train)
cvFitglm <- cvglm(fitglm, cost = rtmspe,
                folds = folds, trim = 0.1)

...

# plot results for the MM regression model
bwplot(cvFitglmrob)
# plot combined results
bwplot(cvFits)


set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(Survived~Sex + Pclass + Age + SibSp + Cabin_f, data = titanic_train, method = "glm",
               trControl = train.control)
# Summarize the results
print(model)


