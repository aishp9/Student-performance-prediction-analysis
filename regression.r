##setwd(apfiles)
###########################  Multi Linear regression #########################
## Finding the best model with least cross validation error using the Best subset method
#constants 
PREDICTORS = 30   # i.e for G1 with all-G2-G3
SELECTION_METHOD = "backward"
predict_regsubsets = function(object, newdata, id,...)
{
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata )
  coefi = coef(object ,id=id)
  xvars = names(coefi )
  mat[,xvars ] %*% coefi
}
library(leaps)
source("st-utils.R")
#read the data
#maths = readStudentdata()
maths<-read.csv("stud_math.csv")
maths = factorizeStudentdata(maths)
findMissing(maths)  

attach(maths)
str(maths)
summary(maths)
pairs(maths[, c("age","failures","G1","G2","G3")])
maths = subset(maths, select = -c(G2,G3) )

# Best Subset Selection
#  Choosing among models  
set.seed(123)
#Using the Validation set approach:
Trainsamp = sample(c(TRUE ,FALSE), nrow(maths),rep=TRUE)
Testsamp = (! Trainsamp )
# choose among the models of diff. sizes using cross validation approach
# initializing the 10xPREDICTORS matrix for cv_errors
k = 10
folds = sample(1:k, nrow(maths), replace = TRUE)
cv_errors = matrix(NA,k,PREDICTORS, dimnames=list(NULL, paste(1:PREDICTORS)))
#cross validation
for ( j in 1:k )
{
  best_fwd = regsubsets(G1 ~ . , maths[folds!=j,], nvmax = PREDICTORS ,method = SELECTION_METHOD)
  best_fwd
  for (i in 1:PREDICTORS)
  {
    pred = predict_regsubsets( best_fwd, maths[folds==j,] , id = i)
    cv_errors[j,i]= mean((maths$G1[ folds==j ]-pred)^2)
  }
}

mean_cv_errors = apply(cv_errors ,2, mean)
mean_cv_errors
which.min(mean_cv_errors)

par( mfrow = c(1,1))
plot(mean_cv_errors , type='b')
# bestsubset selection on fulldataset
best_fwd = regsubsets(G1 ~ . , data = maths, nvmax = PREDICTORS ,method = SELECTION_METHOD)
coef(best_fwd ,which.min(mean_cv_errors) )
################# Multiple Logistic Regression on Student data ###############
library(broom)
source("student-utils.R")
#read the data
#maths = readStudentdata()
maths<-read.csv("stud_math.csv")
maths = factorizeStudentdata(maths)
findMissing(maths)
attach(maths)
str(maths)
#Distribution of school support = yes or no % in the student data
maths = binarySchoolsupport(maths)
prop.table(table(schoolsup))
# remove variables not considered/needed for this logistic regression
dropvar = c("G1","G2","G3")
maths = maths[, !(names(maths) %in% dropvar) ]
# Train and test data
set.seed(123)
train = sample(1: nrow(maths), size = nrow(maths)*2/3)
mtrain = maths[train, ]
mtest = maths[-train, ]
#logistic regression model
model = glm(schoolsup ~ . , data = mtrain, family = binomial)
summary(model)

predicted_val_ontrain = predict(model, data = mtrain, type = "response")
accuracy = table(mtrain$schoolsup, predicted_val_ontrain > 0.5)
sum(diag(accuracy))/sum(accuracy)

predicted_val_ontest = predict(model, newdata = mtest, type = "response")
accuracy = table(mtest$schoolsup, predicted_val_ontest > 0.5)
sum(diag(accuracy))/sum(accuracy)
