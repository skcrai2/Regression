#Check where the files are pulling from using getwd()
setwd()
getwd()
#Import the data and inspect the header
ins<-read.csv("insurance.csv")
head(ins)
#Let's start by creating a full linear model (charges is the dependent variable)
out<-lm(charges~.,data=ins)

#Now, we need to check the assumptions for linear regression by running some diagnostics
#Linearity and additivity of the relationship between dependent and independent variables
#Plot of observed versus predicted values (The points should be symmetrically distributed around a diagonal line, with a roughly constant variance)
plot(ins$charges~out$fitted.values)
abline(0,1)
#Plot of residuals versus predicted values (The points should be symmetrically distributed around a diagonal line, with a roughly constant variance)
plot(out$residuals~out$fitted.values)
abline(h=0)
#Plots of the residuals versus individual independent variables:
windows()
par(mfrow=c(2,3))
plot(out$residuals~ins$age)
plot(out$residuals~ins$sex)
plot(out$residuals~ins$bmi)
plot(out$residuals~ins$bmi)
plot(out$residuals~ins$children)
plot(out$residuals~ins$smoker)
plot(out$residuals~ins$region)
#Plots of the linear model
windows()
par(mfrow=c(2,2))
plot(out)
#You can see from the top-left plot, the red line is slightly curved and the residuals seem to increase as the fitted Y values increase 
#So, the inference is that heteroscedasticity exists.  
#It would appear that the assumptions of constant variance are not met based on all of the plots

#let’s run some statistical tests to verify our graphical assumptions
#The Breush-Pagan test and the NCV test:
lmtest::bptest(out)
car::ncvTest(out)
bartlett.test(ins$charges,ins$age,ins$sex,ins$bmi,ins$children,ins$smoker,ins$region)
#All of these tests have a p-value less that a significance level of 0.05, 
#therefore we can reject the null hypothesis that the variance of the residuals is constant and 
#infer that heteroscedasticity is indeed present, confirming our graphical inferences above

#Fix this issue either by rebuilding the model with new predictors or perform some kind of variable transformation, such a Box-Cox.
#Statistical independence of the errors (in particular, no correlation between consecutive errors in the case of time series data):
#Plot of residuals versus time series/row number:
plot(out$residuals)
#Plot estimates of the autocorrelation function:
acf(out$residuals)
#The X axis corresponds to the lags of the residual, increasing in steps of 1. 
#The very first line (to the left) shows the correlation of residual with itself (Lag0), therefore, it will always be equal to 1.  
#If the residuals were not autocorrelated, the correlation (Y-axis) from the immediate next line onwards 
#will drop to a near zero value below the dashed blue line (significance level).  
#So, it appears graphically that the residuals are not autocorrelated

#let’s run a statistical test to verify this inference:
durbinWatsonTest(out)
#The p-value is close, but still above the significance level of .05, so we will assume no serial correlation in our data.

#Normality of the error distribution(Violations of normality become an issue when trying to assess whether 
#model coefficients are significantly different from zero and for calculating confidence intervals)
#Start with a histogram:
hist(out$residuals)
#It appears as though the residuals are skewed to the right

#Normal probability plot or normal quantile plot of the residuals:
qqnorm(out$residuals)
qqline(out$residuals)
#These are plots of the error distribution versus a normal distribution with the same mean and variance  
#It would appear that our data are not normal based on the above graphs  
#We might have some issues with kurtosis based on the shape of the pattern of deviations

#Let’s check for normality with some statistical tests to verify what we saw in the plots:
shapiro.test(out$residuals)
#Since the p value is less than .05 we can conclude that the data are not normal

#Now, let’s look for leverages that might be affecting our normality:
hatvals<-hatvalues(out)
sum(hatvals)
head(ins)
windows()
par(mfrow=c(1,3))
hist(hatvals)
row<-seq(1:nrow(ins))
ins<-cbind(ins,row)
head(ins)
library(faraway)
halfnorm(hatvals,labs=row,ylab="Leverages")
qqnorm(rstandard(out))
abline(0,1)
#There might be a couple of leverage values in rows 439 and 1086.  

#Let us remove them and redo the regression for the full model:
rm4361086<-ins[-c(436,1086),]
rm4361086lm<-lm(charges~.,data=rm4361086)
summary(rm4361086lm)
#The regression coefficients haven’t changed much, and the standard errors haven’t changed much either  
#Also the R2 and R2 adjusted values do not seem to have changed significantly 
#Perhaps the potential leverage points aren’t having much of an influence on the regression line

#Now, let’s look for outliers that might be affecting our normality:
stud<-rstudent(out)
stud[which.max(abs(stud))]
#The studentized residual with the largest magnitude is 5.009599.  
#Comparing that to the critical value for a two-tailed family error rate of 5%:
qt(.05/nrow(ins)/2,nrow(ins)-9)
#It seems the largest studentized residual is large in magnitude, and so there probably are outliers. 
out<-lm(charges~.,data=ins)
cooksd<-cooks.distance(out)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd, na.rm=T), col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")

#function to look at outliers and decide whether to remove or not
install.packages("outliers")
library(outliers)
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}
outlierKD(ins,charges)
outlierKD(ins,bmi)
outlierKD(ins,children)
#It looks like most of the outliers are coming from the response variable, so I am hesitant to remove them at the moment.  
#I may try some transformations (Poisson? Log? Etc.) before removing any outliers/influential observations

#We will have to rectify these issues either by removing any outliers, 
#rebuilding the model with new predictors or perform some kind of variable transformation, such a Box-Cox.
install.packages("caret")
library(caret)
install.packages("rlang")
library(rlang)
distBCMod <- caret::BoxCoxTrans(ins$charges)
print(distBCMod)
insurance <- cbind(ins, dist_new=predict(distBCMod, ins$charges))
head(insurance)
insurance <- cbind(ins, charges_new=predict(distBCMod, ins$charges))
head(insurance)
lmMod_bc <- lm(charges_new ~., data=insurance)
bptest(lmMod_bc)
lmtest::bptest(lmMod_bc)
hist(insurance$charges_new)
#Let’s plot the model to see if we have any issues with heteroskedascity or normality:
par(mfrow=c(2,2))
plot(lmMod_bc)
##BoxCox did not rectify the non-constant variance issue
#It looks like the model is not a good fit.  
#I have attempted many transformations (square root, log, sin, reciprocal, etc.) 
#since this one, and still issues with non-constant variance and normality, 
#so let’s go back to the original model and try some additional investigative tests:
model<-lm(charges~age+sex+bmi+children+smoker+region, data=ins)
plot(fitted.values(model),rstudent(model),main="Plot of Studentized Residuals vs Fitted Values")
abline(h=0,lty=2)
spread.level.plot(model)
#The spread.level.plot command in returns a suggested transformation of for Y of λ=.2331668 (1-the slope of the spread level plot)
ncv.test(model)
#The result shows that the non-constant error variance is statistically significant, so 
#Y should be transformed before proceeding to the final regression analysis—recall that the Spread Level Plot suggested a transformation of p ∼ ¼
#Alternatively, weighted least-squares could be used instead of OLS

#I am going to try robust standard error since I am not sure of the pattern of non-constant error variance 
#and require less assumptions about the model than weighted least squares, with an unknown pattern
#We have to find the HCCM (heteroskedasticity consistent covariance matrix) Matrix to give us the robust standard errors first:
require(car)
sqrt(diag(hccm(model)))
#Then we create a function to get the White Standard Errors:
robust.se<-function(model){
  s<-summary(model)
  wse<-sqrt(diag(hccm(model)))
  t<-model$coefficients/wse
  p<-2*pnrom(-abs(t))
  results<-cbind(model$coefficients,wse,t,p)
  dimnames(results)<-dimnames(s$coefficients)
  results
}
model<-lm(charges~age+sex+bmi+children+smoker+region, data=ins)
robust.se(model)
#This allows us to obtain unbiased standard errors (which in turn give us unbiased t statistics and p-values) of OLS coefficients under heteroscedasticity.  
#All variables in the model are significant, and the R squared value is ~.75, which is fairly high.  
#So I think we have our model and we will use the robust standard errors

#let’s run a stepwise regression to verify:
null=lm(charge~1,data=ins)
full=lm(charges~age+sex+bmi+children+smoker+region, data=ins)
step(null,scope=list(lower=null,upper=full),direction="forward")
#Our model has the lowest AIC, so let’s stick with that

#There has been some research showing that although outcome transformations bias our estimates, 
#violations of the normality assumption in linear regression analyses do not. 
#The normality assumption is important to unbiasedly estimate standard errors, and 
#hence confidence intervals and p values, but this can be remedied by using robust standard errors. 
#Additionally, in larger sample sizes where the number of observations per variable is >10 (which we have in our data), 
#violations of this normality assumption don’t often truly impact the results.  
#However, the assumptions on the parametric model of homoscedasticity, removal of extreme observations, 
#and independency of the errors, tend to remain influential to the model (and thus need to be dealt with) even in large sample sizes.

#Since each coefficient is influenced by other variables in the model, 
#we can assume that the predictor variables are nearly always associated in some way, 
#and that two or more variables may explain some of the same variation in our response variable.  
#Each coefficient represents the additional effect of adding that variable to the model, 
#so the effects of all other variables in the model are already accounted for. 
#This means that each coefficient would change when other variables are either included or removed from the model.

#So, our final model would suggest that age, bmi, number of children, and positive smoking status negatively influence healthcare costs 
#(i.e. increase healthcare costs) while sex, being a male, and region other than northeast tends to lower healthcare costs.

#Our final model then would be:
#Charges= -11,938.54 + 256.86*age -131.31*sexmale + 339.19*bmi + 475.50*children + 23,848.53*smokeryes – 352.96*regionnorthwest – 1035.02*regionsoutheast – 960.05*regionsouthwest

#For the categorical variables, the coefficients are interpreted as the difference in the 
#predicted value in Y for each one-unit difference in X2, 
#if X1 remains constant, with a one unit difference representing a switch from one category to the other.