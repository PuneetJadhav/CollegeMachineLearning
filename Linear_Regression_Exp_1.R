# Set Working directory for R
setwd("C:\\College\\GitHub\\College Project")
blog_train <- read.csv(file.choose())
### check the data
head(blog_train)
tail(blog_train)
## check no of rows and columns
nrow(blog_train) ## 52396 rows
ncol(blog_train) ## 281 columns

## Getting relevant data only
blog_train <- blog_train[,c(51:60,281)]
colnames(blog_train) <- c("First","Second","Third","Fourth","Fifth","Sixth","Seventh","Eight","Ninth","Tenth","Dependent")
## install packages
#install.packages('caTools')
library(caTools)
mse_total=0
  linear_regressor =lm(formula = Dependent ~ .,data = blog_train)
  ### Load test set 
  test_set_1 <- read.csv(file.choose())
  test_set_2 <- read.csv(file.choose())
  test_set_1 <- test_set_1[,c(51:60,281)]
  colnames(test_set_1) <- c("First","Second","Third","Fourth","Fifth","Sixth","Seventh","Eight","Ninth","Tenth","Dependent")
  test_set_2 <- test_set_2[,c(51:60,281)]
  colnames(test_set_2) <- c("First","Second","Third","Fourth","Fifth","Sixth","Seventh","Eight","Ninth","Tenth","Dependent")
  mse_test_value <- mean((test_set$Dependent - predict(linear_regressor,newdata = test_set_1))^2)
  mse_total=mse_total+mse_test_value
  mse_test_value <- mean((test_set$Dependent - predict(linear_regressor,newdata = test_set_1))^2)
  mse_total=mse_total+mse_test_value
print(paste("Average mean square error is ", mse_total/2))

