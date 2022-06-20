library(leaps)

# The regsubsets() function (part of the leaps library) performs best subset selection 
# by identifying the best model that contains a given number of predictors.
# Best is quantified using RSS.

npredictors = 33
regfit.full=regsubsets(poly3_model, mov, nvmax = npredictors)
reg.summary=summary(regfit.full)

reg.summary$rsq # R2 statistic increases monotonically as more variables are included. From 36% up to 50%.
which.min(reg.summary$rss) # identify the location of the minimum in the model with 33 predictors. 
which.max(reg.summary$adjr2) # Adjusted R2 statistic is max in the model with 17 predictors.
which.min(reg.summary$bic) # BIC is min in the model with 6 predictors
which.min(reg.summary$cp) # Cp is min in the model with 13 predictors

# The best results is the model with 8 predictors
cat("\nLocation of RSS min:",which.min(reg.summary$rss),"\n")
cat("Location of adj-RSq max:",which.max(reg.summary$adjr2),"\n ")
cat("Location of BIC min:",which.min(reg.summary$bic),"\n ")
cat("Location of Cp min:",which.min(reg.summary$cp),"\n ")

# Plot RSS, adjusted R2, Cp, and BIC for all of the models at once
dev.new()
par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS", type="l")
points(which.min(reg.summary$rss),min(reg.summary$rss), col="red",cex=2,pch=20)
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
points(which.max(reg.summary$adjr2),max(reg.summary$adjr2), col="red",cex=2,pch=20)
plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type="l")
points(which.min(reg.summary$cp ),min(reg.summary$cp),col="red",cex=2,pch=20)
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type="l")
points(which.min(reg.summary$bic),min(reg.summary$bic),col="red",cex=2,pch=20)

# Another way to see the best model according the graphs
dev.new()
par(mfrow=c(2,2))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp") # best model with "Cp" min
plot(regfit.full,scale="bic") # best model with smaller "bic"
# The BIC is a variant of the ACI with higher penalty terms. (It is closely related to the ACI)

# Coefficienti associati a un modello migliore, es quello a minimo BIC ovvero a 6 variabili
coef(regfit.full ,which.min(reg.summary$bic)) 



##### Forward and Backward Stepwise Selection #####
# Using the argument method="forward" or method="backward".

## Forward
regfit.fwd=regsubsets(poly3_model,data=mov, nvmax=npredictors, method ="forward")
summary(regfit.fwd)
# We see that using forward stepwise selection, the best one-variable is recommended.

## Backward
regfit.bwd=regsubsets(poly3_model,data=mov,nvmax=npredictors, method ="backward")
summary(regfit.bwd)

# The models found by best subset, forward and backward selection are equal.
j=npredictors; summary(regfit.full)$outmat[j,]==summary(regfit.fwd)$outmat[j,]

coef(regfit.full,npredictors)
coef(regfit.fwd,npredictors)
coef(regfit.bwd,npredictors)

# Same results with cleaner output 
round(coef(regfit.full,npredictors),3)
round(coef(regfit.fwd,npredictors),3)
round(coef(regfit.bwd,npredictors),3)



## Choosing Among Models Using the Validation Set Approach 
##### Validation Set Approach #####
set.seed (555)

# Sampling with replacement 
train=sample(c(TRUE,FALSE), nrow(mov),rep=TRUE)
sum(train) # --> 241
test=(!train)
sum(test) # --> 243

# Apply best subset selection to the training set
regfit.best=regsubsets(poly3_model,data=mov[train,], nvmax = npredictors)

# Make a model matrix from the test data.
# The model.matrix() function is used in many regression packages for 
# building an "X" matrix from data.
# Using a error matrix to calculate the test error
test.mat=model.matrix(poly3_model,data=mov[test,])

# Facciamo un ciclo in cui, per ogni i, estraiamo i coefficienti dal modello migliore,
# moltiplicandoli per l'appropriata colonna della model matrix per formare le predizioni e calcolare il testMSE

# Compute the MSE test for best model from 1 to npredictors regressors
val.errors=rep(NA,npredictors)
for(i in 1:npredictors){
  coefi = coef(regfit.best,id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((mov$Budget[test]-pred)^2)
}
# The best model is the one that contains which.min(val.errors)

val.errors
indexmin = which.min(val.errors); val.errors[indexmin] # min MSE = 8717899

coef(regfit.best,which.min(val.errors)) # This is based on training data

