library(pls)
attach(mov)



##### PCR #####

set.seed (5)

# Use the pcr() function:
# Setting scale=TRUE has the effect of standardizing each predictor (scale projection)
# Setting validation = 'CV' has the effect to use cross validation to rate M parameter
pcr.fit=pcr(poly2_model, data=mov, scale=TRUE, validation ="CV")

#Data: 	X dimension: [22] 484  
#       Y dimension: [1]  484
# By default, "pcr" gives us the RMSE (Root MSE) in terms of prediction with cross validation approach
# For each component, the result gives us the RMSE, as the number of components changes, and the variance explained.

# First line: variance explained as the number of regressors changes.
# Second line: variance explained as a function of Budget variance.
# (Non tutta la varianza dei regressori mi serve per spiegare la varianza di y)

# Variance:
#   - explained: 100% when there are all regressors
#   - Budget: 42.86%

summary(pcr.fit)
# Note that pcr() reports the root mean squared error; 
# It also provides the percentage of variance explained in the predictors and in the response using different numbers of components. 

# Plot the cross-validation scores
# Using val.type="MSEP" will cause the cross-validation MSE to be plotted.

dev.new(); validationplot(pcr.fit,val.type="MSEP",legendpos = "topright")
which.min(MSEP(pcr.fit)$val[1,,][-1])
# We see that the smallest CV error occurs when M = 15
# This suggests that there is a benefit in terms of reduction of dimensionality (M = 15)

# Now perform PCR on the training data and evaluate its test set performance:
set.seed (5)
x = model.matrix(poly2_model, data = mov)[,-1]
y = mov$Budget

train=sample(1:nrow(x), nrow(x)/2) # another typical approach to sample
test=(-train)
y.test=y[test]

pcr.fit=pcr(poly2_model,data = mov ,subset=train,scale=TRUE, validation ="CV")

# Plot MSE and RMSE 
dev.new(); validationplot(pcr.fit,val.type="MSEP",legendpos = "topright") 
minPCR <- which.min(MSEP(pcr.fit)$val[1,,][-1]); minPCR # M=16 shows the lowest CV error

dev.new(); plot(RMSEP(pcr.fit),legendpos = "topright")

# We compute the test MSE as follows:
pcr.pred=predict(pcr.fit,x[test,], ncomp=minPCR)
mean((pcr.pred-y.test)^2) # --> 8989853
# This test set MSE is competitive with the results obtained using ridge and the lasso

# Finally, we fit PCR on the full data set, using M = 16
pcr.fit=pcr(y~x, scale=TRUE, minPCR)
summary(pcr.fit) # 16 comp -> 41.70

dev.new(); validationplot(pcr.fit,val.type="MSEP",legendpos = "topright")

dev.new(); plot(pcr.fit, ncomp = minPCR, asp = 1, line = TRUE)
coef(pcr.fit) ## get the coefficients



##### PLS ##### 
set.seed (5)

# Using the plsr() function:
# Pls is similar to pcr, but is used to manage the variance of y 
# With pls, we don't concentrate our attention only on the regressions, but also on the y variable
# Setting scale=TRUE has the effect of standardizing each predictor (scale projection).
# Setting validation = 'CV' has the effect to use cross validation to rate M parameter
pls.fit=plsr(poly2_model, data=mov, scale=TRUE, validation ="CV")
summary(pls.fit)

dev.new(); validationplot(pls.fit,val.type="MSEP")
minPLS = which.min(MSEP(pls.fit)$val[1,,][-1]) # M = 5

# Now perform Pls on the training data and evaluate its test set performance:
set.seed (5)
pls.fit=plsr(poly2_model,data = mov, subset=train, scale=TRUE, validation ="CV")
dev.new(); validationplot(pls.fit,val.type="MSEP"); 

ncomp = which.min(MSEP(pls.fit)$val[1,,][-1]); # M = 3
ncomp

pls.pred=predict(pls.fit,x[test,],ncomp+1)
mean((pls.pred-y.test)^2) # --> 9059731

pls.pred=predict(pls.fit,x[test,],ncomp)
mean((pls.pred-y.test)^2) # --> 8960114

pls.pred=predict(pls.fit,x[test,],ncomp-1)
mean((pls.pred-y.test)^2) # --> 9223445
# The test MSE is comparable to (slightly higher) the test MSE obtained using ridge regression, the lasso, and PCR.

# Finally, we perform PLS using the full data set, using M = 3, 
pls.fit=plsr(poly2_model,data = mov ,scale=TRUE,ncomp)
summary(pls.fit) # 3 comp -> 40.35

# The percentage of variance in Movie that the 3-component PLS fit explains, 40.35%,
# is almost as much as that explained using the final 16-component model PCR fit, 41.70%.
# This is because PCR only attempts to maximize the amount of variance explained in the predictors, 
# while PLS searches for directions taht explain variance in both the predictors and the response.