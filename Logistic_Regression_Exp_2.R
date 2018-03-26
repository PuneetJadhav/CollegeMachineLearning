setwd('/Users/gopisaran/Downloads/BlogFeedback')
#load data
blog_train <- read.csv('blogData_train.csv')
train <- blog_train[,c(63:262,281)]
colnames(train) <- c(63:262,"Dependent")
#print max value of dependent variable
print(max(train[,201]))# 1424
print(min(train[201]))# 0
#Covertic numeric target variable to multi-class categorical target variable so that 
#logictic regression can be performed
CatDependent <- cut(train[,201], breaks=c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500)
                    , labels=c("0-100","101-200","201-300","301-400","401-500","501-600","601-700",
                               "701-800","801-900","901-1000","1001-1100","1101-1200","1201-1300","1301-1400",
                               "1401-1500"),right=FALSE)
CatDependent
newtrain <- cbind(train,CatDependent)
#create training and validation data from given data

library(caTools)
set.seed(88)
split <- sample.split(newtrain$CatDependent, SplitRatio = 0.75)
#get training and test data
finaltrain <- subset(newtrain, split == TRUE)
finaltest <- subset(newtrain, split == FALSE)
#logistic regression model
#install.packages('nnet')
library(multinom)
glm.fit=multinom(CatDependent ~ .-Dependent, data=finaltrain , MaxNWts= 3000)
predicted=predict(glm.fit, finaltest)
cm = as.matrix(table(Actual = finaltest[,12], Predicted = predicted))
cm
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
accuracy = sum(diag) / n 
accuracy#0.9714

# Prediction for blogData_test-2012.02.01.00_00 
blog_test <- read.csv('blogData_test-2012.02.01.00_00.csv')
test <- blog_test[,c(63:262,281)]
colnames(test) <- c(63:262,"Dependent")
test
CatDependent <- cut(test[,201], breaks=c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500)
                    , labels=c("0-100","101-200","201-300","301-400","401-500","501-600","601-700",
                               "701-800","801-900","901-1000","1001-1100","1101-1200","1201-1300","1301-1400",
                               "1401-1500"),right=FALSE)
CatDependent
newtest <- cbind(test,CatDependent)
predicted=predict(glm.fit, newtest)
cm = as.matrix(table(Actual = newtest[,12], Predicted = predicted))
cm
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
accuracy = sum(diag) / n 
accuracy#1

# Prediction for blogData_test-2012.02.02.00_00 
blog_test <- read.csv('blogData_test-2012.02.02.00_00.csv')
test <- blog_test[,c(63:262,281)]
colnames(test) <- c(63:262,"Dependent")
test
CatDependent <- cut(test[,201], breaks=c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500)
                    , labels=c("0-100","101-200","201-300","301-400","401-500","501-600","601-700",
                               "701-800","801-900","901-1000","1001-1100","1101-1200","1201-1300","1301-1400",
                               "1401-1500"),right=FALSE)
CatDependent
newtest <- cbind(test,CatDependent)
predicted=predict(glm.fit, newtest)
cm = as.matrix(table(Actual = newtest[,12], Predicted = predicted))
cm
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
accuracy = sum(diag) / n 
accuracy#0.9924

# Prediction for blogData_test-2012.03.15.00_00 
blog_test <- read.csv('blogData_test-2012.03.15.00_00.csv')
test <- blog_test[,c(63:262,281)]
colnames(test) <- c(63:262,"Dependent")
test
CatDependent <- cut(test[,201], breaks=c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500)
                    , labels=c("0-100","101-200","201-300","301-400","401-500","501-600","601-700",
                               "701-800","801-900","901-1000","1001-1100","1101-1200","1201-1300","1301-1400",
                               "1401-1500"),right=FALSE)
CatDependent
newtest <- cbind(test,CatDependent)
predicted=predict(glm.fit, newtest)
cm = as.matrix(table(Actual = newtest[,12], Predicted = predicted))
cm
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
accuracy = sum(diag) / n 
accuracy#1

# Prediction for blogData_test-2012.03.16.00_00 
blog_test <- read.csv('blogData_test-2012.03.16.00_00.csv')
test <- blog_test[,c(63:262,281)]
colnames(test) <- c(63:262,"Dependent")
test
CatDependent <- cut(test[,201], breaks=c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500)
                    , labels=c("0-100","101-200","201-300","301-400","401-500","501-600","601-700",
                               "701-800","801-900","901-1000","1001-1100","1101-1200","1201-1300","1301-1400",
                               "1401-1500"),right=FALSE)
CatDependent
newtest <- cbind(test,CatDependent)
predicted=predict(glm.fit, newtest)
cm = as.matrix(table(Actual = newtest[,12], Predicted = predicted))
cm
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
accuracy = sum(diag) / n 
accuracy#1
















