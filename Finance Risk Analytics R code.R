
library(Amelia)
library(caret)
library(DMwR)
library(tidyverse)
library(gridExtra)
library(car)
library(lmtest)
library(caret)
library(SDMTools)
library(pROC)
library(Hmisc)
library(pscl)
library(ModelMetrics)
setwd("C:/Users/DELL/Desktop/Akshay/Group Assignments/Group Assignment FRA")
getwd()

raw_data <- read.csv("C:/Users/DELL/Desktop/Akshay/Group Assignments/Group Assignment FRA/raw-data CSV.csv")
str(raw_data)
summary(raw_data)

#Data Preparation and Data cleaning

#number of NA's in all parameters.
missing_values = lapply(raw_data,function(x)sum(is.na(x)))
sum(is.na(raw_data))
missing_values

missmap(raw_data, main = "Missing values vs observed")
# 10 percent values are Missing values if we look from all columns in total


#NA values have been Changed with median value for that column as outlier are too broad.
raw_data_prepared <- data.frame(
  sapply(
    raw_data,
    function(x) ifelse(is.na(x),
                       median(x, na.rm = TRUE),
                       x)))
summary(raw_data_prepared)
str(raw_data_prepared)

missmap(raw_data_prepared, main = "Missing values vs observed")
# 2 percent values are Missing values if we look from all columns in total after missing data treatment


#removing the only column which has all values as NA, ie Deposits..accepted.by.commercial.banks.
raw_data_prepared<- raw_data_prepared[,-22]
colnames(raw_data_prepared)
dim(raw_data_prepared)


#Default Rate for the data set
default_rate = (sum(raw_data_prepared$Default)/(nrow(raw_data_prepared)))*100
paste("Default Rate for the dataset is ", default_rate, "%")

# Convert "Default" variable as categorical variable
raw_data_prepared$Default <- as.factor(raw_data_prepared$Default)
str(raw_data_prepared)

#EDA## Basic data summary, Univariate, Bivariate analysis

bp <- barplot(table(raw_data_prepared$Default),main = "Default rate",col="Blue",border="Black",
              density=100,ylim = c(0,3000),xlab="1 = Default & 0 = No Default")
#we can see mostly companies are not defaulters 

barplot(raw_data_prepared$Total.income,raw_data_prepared$Sales, xlab = "Total.income", ylab = "Sales", axisnames = TRUE,
        names.arg = raw_data_prepared$Num,col.axis = "Red")
# we can see companies have more sales have high total income , Company number 3135 has both parameters high , could be one of the outlier.


 p1 <-plot(raw_data_prepared$Default,raw_data_prepared$Sales, xlab = "Default", ylab = "Sales", 
        col.axis = "Red",col = 'orange', ylim = c(0,10000), main = "Box Plot for Default Vs. Sales")
#we can see companies which default have sales lesser than companies which don't Default
 
 p2 <-plot(raw_data_prepared$Default,raw_data_prepared$Total.expenses, xlab = "Default", ylab = "Expenses", 
              col.axis = "Red", col = 'orange', ylim = c(0,10000), main = "Box Plot for Default Vs. Total.expenses")
 
grid.arrange(p1,p2,nrow=1)

plot(raw_data_prepared$Total.income,raw_data_prepared$Default)# we can see companies who will defaul

ggplot2::aes(raw_data_prepared$Total.expenses,raw_data_prepared$Cash.profit)

boxplot(raw_data_prepared$Total.liabilities,raw_data_prepared$Total.assets,raw_data_prepared$Net.worth,raw_data_prepared$Total.income, col = "red")

# Check for Multicollinearity - Plot the graph based on Multicollinearity
library(corrplot)
library(Hmisc)
numericdata<-raw_data_prepared[,c(-1,-2)]
print(cor(numericdata),digits = 3)

cp = cor(numericdata) #create an object of the features
cp
corrplot.mixed(cp)
cordata <-cor(numericdata)
cordata.rcorr <-rcorr(as.matrix(cordata))
cordata.rcorr
cordara.coeff=cordata.rcorr$default

#OutlierTreatment Using capping for 95% and 5%
capOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

raw_data_prepared$Total.income=capOutlier(raw_data_prepared$Total.income)
raw_data_prepared$Total.assets=capOutlier(raw_data_prepared$Total.assets)
raw_data_prepared$Total.expenses=capOutlier(raw_data_prepared$Total.expenses)
raw_data_prepared$Net.worth=capOutlier(raw_data_prepared$Net.worth)
raw_data_prepared$Change.in.stock=capOutlier(raw_data_prepared$Change.in.stock)
raw_data_prepared$Profit.after.tax=capOutlier(raw_data_prepared$Profit.after.tax)
raw_data_prepared$PBDITA=capOutlier(raw_data_prepared$PBDITA)
raw_data_prepared$PBT=capOutlier(raw_data_prepared$PBT)
raw_data_prepared$Cash.profit=capOutlier(raw_data_prepared$Cash.profit)
raw_data_prepared$PBT.as...of.total.income=capOutlier(raw_data_prepared$Cash.profit)
raw_data_prepared$PBDITA.as...of.total.income=capOutlier(raw_data_prepared$PBDITA.as...of.total.income)
raw_data_prepared$PAT.as...of.total.income=capOutlier(raw_data_prepared$PAT.as...of.total.income)
raw_data_prepared$Cash.profit.as...of.total.income=capOutlier(raw_data_prepared$Cash.profit.as...of.total.income)
raw_data_prepared$PAT.as...of.net.worth=capOutlier(raw_data_prepared$PAT.as...of.net.worth)
raw_data_prepared$Sales=capOutlier(raw_data_prepared$Sales)
raw_data_prepared$Income.from.financial.services=capOutlier(raw_data_prepared$Income.from.financial.services)
raw_data_prepared$Other.income=capOutlier(raw_data_prepared$Other.income)
raw_data_prepared$Total.capital=capOutlier(raw_data_prepared$Total.capital)
raw_data_prepared$Reserves.and.funds=capOutlier(raw_data_prepared$Reserves.and.funds)
raw_data_prepared$Borrowings=capOutlier(raw_data_prepared$Borrowings)
raw_data_prepared$Current.liabilities...provisions=capOutlier(raw_data_prepared$Current.liabilities...provisions)
raw_data_prepared$Deferred.tax.liability=capOutlier(raw_data_prepared$Deferred.tax.liability)
raw_data_prepared$Shareholders.funds=capOutlier(raw_data_prepared$Shareholders.funds)
raw_data_prepared$Cumulative.retained.profits=capOutlier(raw_data_prepared$Cumulative.retained.profits)
raw_data_prepared$Capital.employed=capOutlier(raw_data_prepared$Capital.employed)
raw_data_prepared$TOL.TNW=capOutlier(raw_data_prepared$TOL.TNW)
raw_data_prepared$Total.term.liabilities...tangible.net.worth=capOutlier(raw_data_prepared$Total.term.liabilities...tangible.net.worth)
raw_data_prepared$Contingent.liabilities...Net.worth....=capOutlier(raw_data_prepared$Contingent.liabilities...Net.worth....)
raw_data_prepared$Contingent.liabilities=capOutlier(raw_data_prepared$Contingent.liabilities)
raw_data_prepared$Net.fixed.assets=capOutlier(raw_data_prepared$Net.fixed.assets)
raw_data_prepared$Investments=capOutlier(raw_data_prepared$Investments)
raw_data_prepared$Current.assets=capOutlier(raw_data_prepared$Current.assets)
raw_data_prepared$Net.working.capital=capOutlier(raw_data_prepared$Net.working.capital)
#raw_data_prepared$Quick.ratio..times.=capOutlier(raw_data_prepared$Quick.rao..times.)
#raw_data_prepared$Current.ratio..times.=capOutlier(raw_data_prepared$Current.rtiatio..times.)
raw_data_prepared$Debt.to.equity.ratio..times.=capOutlier(raw_data_prepared$Debt.to.equity.ratio..times.)
raw_data_prepared$Cash.to.average.cost.of.sales.per.day=capOutlier(raw_data_prepared$Cash.to.average.cost.of.sales.per.day)
raw_data_prepared$Cash.to.current.liabilities..times.=capOutlier(raw_data_prepared$Cash.to.current.liabilities..times.)
raw_data_prepared$Creditors.turnover=capOutlier(raw_data_prepared$Creditors.turnover)
raw_data_prepared$Debtors.turnover=capOutlier(raw_data_prepared$Debtors.turnover)
raw_data_prepared$Finished.goods.turnover=capOutlier(raw_data_prepared$Finished.goods.turnover)
raw_data_prepared$WIP.turnover=capOutlier(raw_data_prepared$WIP.turnover)
raw_data_prepared$Raw.material.turnover=capOutlier(raw_data_prepared$Raw.material.turnover)
raw_data_prepared$Shares.outstanding=capOutlier(raw_data_prepared$Shares.outstanding)
raw_data_prepared$Equity.face.value=capOutlier(raw_data_prepared$Equity.face.value)
raw_data_prepared$EPS=capOutlier(raw_data_prepared$EPS)
raw_data_prepared$Adjusted.EPS=capOutlier(raw_data_prepared$Adjusted.EPS)
raw_data_prepared$Total.liabilities=capOutlier(raw_data_prepared$Total.liabilities)
raw_data_prepared$PE.on.BSE=capOutlier(raw_data_prepared$PE.on.BSE)

boxplot(raw_data_prepared$Total.liabilities,raw_data_prepared$Total.assets,raw_data_prepared$Net.worth,raw_data_prepared$Total.income, col = "red",
        main = "Boxplot for Total.liabilities Vs. Total.assets")


#write.csv(raw_data_prepared,"raw_data_prepared_updated.csv")

#New variables created on some critical variables as per business knowledge and after seeing Outlier behaviour, they have been created as ratios to scale down the number data for companies.

#Net worth/ Total assets = NW_new,	
#Total income / Total assets= TI_new, 
#Total expenses  / Total assets = TE_new, 
#Profit after tax / Total assets = PAT_new, 
#PBT / Total assets = PBT_new, 
#Sales / Total assets = Sales_new.
#Current liabilities & provisions / Total assets= CLP_new,
#Capital employed / Total assets = CE_new,
#Net fixed assets / Total assets = NFA_new,
#Investments / Total assets = Investments_new, 
#Total liabilities / Total assets = TL_new
#Shares.outstanding / Total assets = SO_new

#Calculations has been done in Excel and then data is read again with new variables

raw_data_prepared_updated_data<-read.csv("raw_data_prepared_updated.csv")

raw_data_prepared_updated_data$Default <- as.factor(raw_data_prepared_updated_data$Default)


# a) Applying Logistic Regression and Interpret the Regression model output
# build the initial Logistic Regression Model taking all independent variables and new variables into consideration.

logit <- glm(raw_data_prepared_updated_data$Default~ .,family = 'binomial',data = raw_data_prepared_updated_data)
logit

summary(logit)

#Above Run will helps us to reduce the variables to be considered significant for model building and thus help in variable reduction.

logit2<-glm(raw_data_prepared_updated_data$Default~ Total.income + Change.in.stock +Total.expenses +PAT.as...of.net.worth
            +Sales +Total.capital+Borrowings+Cumulative.retained.profits +Capital.employed+Debt.to.equity.ratio..times.
            +Cash.to.current.liabilities..times.+PE.on.BSE
            ,family = 'binomial',data = raw_data_prepared_updated_data)

logit2
summary(logit2)

logit3<-glm(raw_data_prepared_updated_data$Default~ PAT.as...of.net.worth
            +Total.capital+Cumulative.retained.profits +Debt.to.equity.ratio..times.
            +Cash.to.current.liabilities..times.+PE.on.BSE
            ,family = 'binomial',data = raw_data_prepared_updated_data)
logit3
summary(logit3)

#FRom Below output we can see variables for our model :
#PAT.as...of.net.worth 
#Total.capital
#Cumulative.retained.profits and others
#Debt.to.equity.ratio..times.
#Cash.to.current.liabilities..times.
#PE.on.BSE
#TE_new
#NFA_new
#SO_new
#NW_new
#Sales_new


# Checking for Variation Inflation Factor
x<-vif(logit3)
x
#. Likelihood Ratio Test (LR test)
lrtest(logit3)
#. Looking for McFadden value
pR2(logit3)
odds<-exp(coef(logit3))

#3] Model Performance Measures ( Train)
#a) Confusion matrix interpretation for all models
#. Checking the prediction accuracy on the train data
predict.logit<-predict.glm(logit3, newdata= raw_data_prepared_updated_data, type="response")
summary(predict.logit)

#. ROC curve for train data
accuracy.logit<-
  roc.logit<-roc(raw_data_prepared_updated_data$Default,predict.logit)
roc.logit
plot(roc.logit,main="ROC Curve")

#. KS score
library(ROCR)
ks1<-prediction(predict.logit,raw_data_prepared_updated_data$Default)
perf_m3_train<-performance(ks1,"tpr","fpr")
ks_stats<-round(max(attr(perf_m3_train,'y.values')[[1]]-attr(perf_m3_train,'x.values')[[1]]),4)*100
ks_stats  

#. Gini values for logistics regression
install.packages("ineq")
library(ineq)
gini_stats=round(ineq(predict.logit,type="Gini"),4)*100
gini_stats

#Applying Model on Test Data
validation_data <- read.csv("C:/Users/DELL/Desktop/Akshay/Group Assignments/Group Assignment FRA/validation_data_updated.csv", header=FALSE)
str(validation_data)
summary(validation_data)

for (i in which(sapply(validation_data, is.numeric))) {
  validation_data[is.na(validation_data[, i]), i] <- median(validation_data[, i],  na.rm = TRUE)
}

validation_data<- validation_data[,-22]

#write.csv(validation_data,"validation_data_updated.csv")


validation_data_updated<-read.csv("validation_data_updated.csv")

validation_data_updated$Default...1 <- as.factor(validation_data_updated$Default...1)


#3] Model Performance Measures (Test )
#a) Confusion matrix interpretation for all models
#. Checking the prediction accuracy on the test data
predict.logit2<-predict.glm(logit3, newdata= validation_data_updated, type="response")
summary(predict.logit2)

#. ROC curve for test data
accuracy.logit2<-
  roc.logit2<-roc(validation_data_updated$Default...1,predict.logit2)
roc.logit2
plot(roc.logit2,main="ROC Curve")

#. KS score
library(ROCR)
ks1<-prediction(predict.logit,validation_data_updated$Default...1)
perf_m3_train<-performance(ks1,"tpr","fpr")
ks_stats<-round(max(attr(perf_m3_train,'y.values')[[1]]-attr(perf_m3_train,'x.values')[[1]]),4)*100
ks_stats  

#. Gini values for logistics regression
library(ineq)
gini_stats=round(ineq(predict.logit2,type="Gini"),4)*100
gini_stats

