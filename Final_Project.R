### RMOSEK
install.packages("Rmosek", type="source", INSTALL_opts="--no-multiarch",repos="http://download.mosek.com/R/8", 
                 configure.vars="PKG_MOSEKHOME=C:/Program Files/Mosek/8/tools/platform/win64x86 PKG_MOSEKLIB=mosek64")
library(Rmosek)


# Set Working directory for R
setwd("C:\\College\\GitHub\\R")
blog <- read.csv(file.choose())
blog <- blog[1:6000,c(51:60,281)]
### check the data
head(blog)
tail(blog)
## check no of rows and columns
nrow(blog) ## 6000 rows
ncol(blog) ## 11 columns
### 6000 records with 10 independent columns and 1 dependent variable
train_sample <- sample(seq_len(nrow(blog)),size=(floor(0.75*nrow(blog))))
blog_train <- blog[train_sample,]
blog_test <- blog [-train_sample,]
nrow(blog_train)

#### Creating matrix for Training Set
blog_train_X=blog_train[,1:ncol(blog_train)-1]
blog_train_Y=blog_train[,ncol(blog_train)]
blog_train_Y=as.matrix(blog_train_Y)
blog_train_X=as.matrix(blog_train_X)

######3 Creating matrix for Test Set
blog_test_X=blog_test[,1:ncol(blog_test)-1]
blog_test_Y=blog_test[,ncol(blog_test)]
blog_test_Y=as.matrix(blog_test_Y)
blog_test_X=as.matrix(blog_test_X)


######################## OLS Function ####################

solve.ols<-function(X,y, verb=1){
  p<-dim(X)[2]  # number of parameters of interest
  
  #correspondence between OLS and the Quadratic Program
  xx<-crossprod(X) # X'X=Q variable in the Quadratic program
  c<--crossprod(X,y) # X'y=c variable in the Quadratic program
  xx2<-xx
  xx2[upper.tri(xx)]<-0 #mosek needs Q to be  triangular
  idx <- which(xx2 != 0, arr.ind=TRUE) #index of the nonzero elements of Q
  
  #problem definition in Mosek
  qo1<-list() #empty list that contains the QP problem
  qo1$sense<-"min" #problem sense
  qo1$c<-as.vector(c) #objective coefficients
  qo1$qobj<-list(i = idx[,1],
                 j = idx[,2],
                 v = xx2[idx] ) #the Q matrix is imputed by the row indexes i, the col indexes j and the values v that define the Q matrix
  qo1$A<-Matrix(rep(0,p), nrow=1,byrow=TRUE,sparse=TRUE) #constrain matrix A is a null matrix in this case
  qo1$bc<-rbind(blc=-Inf, buc= Inf) #constraint bounds
  qo1$bx<-rbind(blx=rep(-Inf,p), bux = rep(Inf,p)) #parameter bounds 
  r<-mosek(qo1, opts = list(verbose = verb)) #call mosek solver
  return(r)
}
### Linear Regression
colnames(blog_train) <- c("First","Second","Third","Fourth","Fifth","Sixth","Seventh","Eight","Ninth","Tenth","Dependent")
colnames(blog_test) <- c("First","Second","Third","Fourth","Fifth","Sixth","Seventh","Eight","Ninth","Tenth","Dependent")



library(caTools)
mse_total=0
linear_regressor =lm(formula = Dependent ~ .,data = blog_train)
mse_test_value <- mean((blog_test$Dependent - predict(linear_regressor,newdata = blog_test))^2)
mse_total=mse_total+mse_test_value
print(paste("Average mean square error is ", mse_total))
summary(linear_regressor)
mse_total

#### Mosek Calling
rf=solve.ols(blog_train_X, blog_train_Y)
rf


#########################3


yHat=blog_test_X[,1]*0.08984273 + blog_test_X[,2]*0.22632640 +
  blog_test_X[,3]*0.00277114+ blog_test_X[,4]*(-0.09152098)  + 
  blog_test_X[,5]*0.00000000 +blog_test_X[,6]*(2.16773849) +
  blog_test_X[,7]*(-0.63562090)+  blog_test_X[,8]*(-0.26849565) +
  blog_test_X[,9]*(-2.00226379)+  blog_test_X[,10]*0.00000000
YMinusYHat = blog_test_Y-yHat
mse <- function(error)
{
  
  mean(error^2)
  
}
MSE_Mosek=mse(YMinusYHat)
MSE_Mosek
YMinusYHat
RSS_Mosek=sum(YMinusYHat^2)
RSS_Mosek




