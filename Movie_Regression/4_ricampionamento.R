mov <- read.csv(file='movie_final.csv', sep=',')
attach(mov)
n = nrow(mov)



##### Validation Set Approch ##### 

set.seed(7)
train=sample(1:n,n/2)
lm.fit=lm(linear_model, data = mov, subset = train)

# the estimated test MSE for the linear regression fit is 8718798 (seed=5)
x = mean(((Budget-predict(lm.fit,mov))[-train])^2)

# creo un vettore vuoto
y <- c()
# inserisco il valore dell' MSE relativo al modello lineare nel vettore y
y = append(y,x)

# use the poly() function to estimate the test error for the polynomials-2 transformation.
lm.fit2=lm(poly2_model, data = mov, subset=train)

# the estimated test MSE for the linear regression fit is 8898213 (seed=5)
x = mean(((Budget-predict(lm.fit2,mov))[-train])^2)
# inserisco il valore dell' MSE relativo al modello polinomiale di 2^ grado nel vettore y
y = append(y,x)

# use the poly() function to estimate the test error for the polynomials-3 transformation.
lm.fit3=lm(poly3_model, data = mov, subset=train) 

# the estimated test MSE for the linear regression fit is 8226787 (seed=5)
x = mean(((Budget-predict(lm.fit3,mov))[-train])^2)
# inserisco il valore dell' MSE relativo al modello polinomiale di 3^ grado nel vettore y
y = append(y,x)

# use the poly() function to estimate the test error for the polynomials-4 transformation.
lm.fit4=lm(poly4_model, data = mov, subset=train) 

# the estimated test MSE for the linear regression fit is 9096448 (seed=5)
x = mean(((Budget-predict(lm.fit4,mov))[-train])^2)
# inserisco il valore dell' MSE relativo al modello polinomiale di 4^ grado nel vettore y
y = append(y,x)


### seed=1,       y =  11693737 12500249 11222362 11227372
### seed=5,       y =  8718798  8898213  8226787  9096448
### seed=7,       y =  9986868  10118900 11052963 83488661
### seed=741,     y =  10172968 9967006  11231949 15118202
### seed=96,      y =  10787861 10620638 9911083  13752321
### seed=951357,  y =  11880105 11462737 10849294 13286827
### seed=111,     y =  8969305  9008374  9980730  337281041


## !!Changing the seed, the results remain consistent with our previous findings



##### K-Fold Cross Validation ##### 
library(boot)

glm.fit=glm(poly4_model ,data=mov)

cv.err=cv.glm(mov,glm.fit, K = 10)
cv.err$delta[1] 

set.seed(2505)
# K-Fold Cross validation for polynomial regressions with orders i=1,2,...,4.

cv.error=rep(0,4)
for (i in 1:4){
  glm.fit=glm(Budget~poly(Marketing.expense,i)+poly(Production.expense,i)+poly(Lead_.Actor_Rating,i)
              +poly(Lead_Actress_rating,i)+poly(Director_rating,i)+poly(Producer_rating,i)+poly(Critic_rating,i)
              +poly(Trailer_views,i)+poly(Time_taken,i)+poly(Twitter_hastags,i)+poly(Avg_age_actors,i), data = mov)
  cv.error[i]=cv.glm(mov,glm.fit, K=10)$delta[1]
}
cv.error

# !!We still see little evidence that using cubic or higher-order polynomial terms leads to lower test error than simply



##### Bootstrap ##### 

# The boot.fn() function can also be used in order to create bootstrap estimates 
# for the intercept and slope terms by randomly sampling from among the observations with replacement
# We will compare the estimates obtained using the bootstrap to those obtained using the previous models

library(stringr)


## Modello Lineare
boot.fn=function(data,index){
  return(coef(lm(linear_model, data = data,subset=index)))
}

index = 1:n
boot.fn(mov, index)

set.seed(2022)
# Boot estimate is not deterministic
boot.fn(mov,sample(1:n, 484,replace=T))
# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.
b = boot(mov, boot.fn, 1000)

s = summary(lm(linear_model, data = mov))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the linear model
c = s$coefficients[ ,2]
# c = as.numeric(c)

cat("\nDifference between no-Transformation Std.errors:\n",c - se,"\n")



## Modello Polinomiale di 2^ ordine
set.seed(2022)

boot.fn=function(data,index){
  return(coef(lm(poly2_model, data = data,subset=index)))
}

boot.fn(mov, index)

boot.fn(mov,sample(1:n, 484,replace=T))

# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.

b = boot(mov ,boot.fn ,1000)

s = summary(lm(poly2_model,data = mov))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the poly-2 transformation
c = s$coefficients[ ,2]
#c = as.numeric(c)

cat("\nDifference between poly-2 transformation Std.errors:\n",c - se,"\n")



## Modello Polinomiale di 3^ ordine
set.seed(2022)

boot.fn=function(data,index){
  return(coef(lm(poly3_model, data = data, subset=index)))
}

boot.fn(mov, index)
# Boot estimate is not deterministic, so we set the seed to get always the same results
boot.fn(mov,sample(1:n, 484,replace=T))
# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.
b = boot(mov ,boot.fn ,1000)

s = summary(lm(poly3_model, data = mov))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the poly-3 transformation
c = s$coefficients[ ,2]
#c = as.numeric(c)

cat("\nDifference between poly-3 transformation Std.errors:\n",c - se,"\n")



## Modello Polinomiale di 4^ ordine
set.seed(2022)
boot.fn=function(data,index){
  return(coef(lm(poly4_model, data = data,subset=index)))
}

boot.fn(mov, 1:n)

set.seed(2022)
# Boot estimate is not deterministic, so we set the seed to get always the same results
boot.fn(mov,sample(1:n, 484,replace=T))
# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.
b = boot(mov ,boot.fn ,1000)

s = summary(lm(poly4_model, data = mov))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the poly-4 transformation
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between poly-4 transformation Std.errors:\n",c - se,"\n")

##### Plot  #####

## Plot linear model ------- Production.expense
dev.new()
plot(as.factor(Production.expense),Budget,main='Linear Model', xlab="Production expense", ylab="Budget")
xx=seq(min(Production.expense),max(Production.expense),along.with = Production.expense)
ci_lin <- predict(lm(Budget~Production.expense,data=mov),newdata=data.frame(Production.expense=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)

## Plot linear model with polinomials-2 transformation
dev.new()
plot(as.factor(Production.expense),Budget,main='Polinomial-2 Model', xlab="Production expense", ylab="Budget")
xx=seq(min(Production.expense),max(Production.expense),along.with = Production.expense)
ci_lin <- predict(lm(Budget~I(Production.expense^2),data=mov),newdata=data.frame(Production.expense=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)

## Plot linear model with polinomials-3 transformation
dev.new()
plot(as.factor(Production.expense),Budget,main='Polinomial-3 Model', xlab="Production expense", ylab="Budget")
xx=seq(min(Production.expense),max(Production.expense),along.with = Production.expense)
ci_lin <- predict(lm(Budget~I(Production.expense^3),data=mov),newdata=data.frame(Production.expense=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)

## Plot linear model with polinomials-4 transformation
dev.new()
plot(as.factor(Production.expense),Budget,main='Polinomial-4 Model', xlab="Production expense", ylab="Budget")
xx=seq(min(Production.expense),max(Production.expense),along.with = Production.expense)
ci_lin <- predict(lm(Budget~I(Production.expense^4),data=mov),newdata=data.frame(Production.expense=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)




## Plot linear model ------- Twitter_hastags
dev.new()
plot(as.factor(Twitter_hastags),Budget,main='Linear Model', xlab="Twitter hastags", ylab="Budget")
xx=seq(min(Twitter_hastags),max(Twitter_hastags),along.with = Twitter_hastags)
ci_lin <- predict(lm(Budget~Twitter_hastags,data=mov),newdata=data.frame(Twitter_hastags=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)

## Plot linear model with polinomials-2 transformation
dev.new()
plot(as.factor(Twitter_hastags),Budget,main='Polinomial-2 Model', xlab="Twitter hastags", ylab="Budget")
xx=seq(min(Twitter_hastags),max(Twitter_hastags),along.with = Twitter_hastags)
ci_lin <- predict(lm(Budget~I(Twitter_hastags^2),data=mov),newdata=data.frame(Twitter_hastags=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)

## Plot linear model with polinomials-3 transformation
dev.new()
plot(as.factor(Twitter_hastags),Budget,main='Polinomial-3 Model', xlab="Twitter hastags", ylab="Budget")
xx=seq(min(Twitter_hastags),max(Twitter_hastags),along.with = Twitter_hastags)
ci_lin <- predict(lm(Budget~I(Twitter_hastags^3),data=mov),newdata=data.frame(Twitter_hastags=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)

## Plot linear model with polinomials-4 transformation
dev.new()
plot(as.factor(Twitter_hastags),Budget,main='Polinomial-4 Model', xlab="Twitter hastags", ylab="Budget")
xx=seq(min(Twitter_hastags),max(Twitter_hastags),along.with = Twitter_hastags)
ci_lin <- predict(lm(Budget~I(Twitter_hastags^4),data=mov),newdata=data.frame(Twitter_hastags=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)
