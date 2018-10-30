# !diagnostics off
rm(list = ls())
#set working directory
setwd("E:/DS/Project/Emeployee Absenteism")

#loading Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "e1071",
      "DataCombine", "pROC")
#Used to read excel files
library(readxl)
#tibble is a modern dataframe
library(tibble)

#install.packages if not
#lapply(x, install.packages)

#load Packages
lapply(x, require, character.only = TRUE)

# Reading data
absenteeism_data = read_excel("Absenteeism.xls")
#Understanding the dimensions and type of data
str(absenteeism_data)

#Remove observations which are having 0 absent

#Renaming columns
colnames(absenteeism_data) = c("ID","Reason.for.absence","Month.of.absence","Day.of.the.week","Seasons","Transportation.expense"
                               ,"Distance.from.Residence.to.Work","Service.time","Age","Work.load.Average.day","Hit.target","Disciplinary.failure"
                               ,"Education","Son","Social.drinker","Social.smoker","Pet","Weight","Height","Body.mass.index","Absenteeism.time.in.hours")

######################################### Missing Values Analysis##################################3
missing_value = (as.data.frame(colSums(is.na(absenteeism_data)))*100/nrow(absenteeism_data))
colnames(missing_value) <- c("Missng Value Percentage")


#plotting boxplot of Reason.for.absence Vs. Absenteeism.time.in.hours
ggplot(absenteeism_data,aes_string(y=absenteeism_data$Absenteeism.time.in.hours,x=as.factor(absenteeism_data$Reason.for.absence)))+geom_boxplot()+xlab('Reason.for.absence')+ylab('Absenteeism.time.in.hours')

### Imputing values in the dataset ### 
# Category 27 of Reason.for.absence is taking < 10 hrs of Absenteeism.time.in.hours. So, null values of Reason.for.absence are put equal to 27 since Absenteeism.time.in.hours for the observations having null values is < 10 hrs.
absenteeism_data$Reason.for.absence[is.na(absenteeism_data$Reason.for.absence)] = 27
#Zero category of 'Reason for absence' column has been put equal to category 26(i.e. unjustified absence).
absenteeism_data$Reason.for.absence[absenteeism_data$Reason.for.absence==0] = 26
#Putting Month.of.absence null value equal to 10.
absenteeism_data$Month.of.absence[is.na(absenteeism_data$Month.of.absence)] = 10
#Finding ID column values for which there are missing values in Transportation.expense
absenteeism_data$ID[is.na(absenteeism_data$Transportation.expense)]
# [1] 10  3  1 15 20 22 20
for (i in c(1,3,10,15,20,22)){
  absenteeism_data$Transportation.expense[is.na(absenteeism_data$Transportation.expense) & absenteeism_data$ID==i] = mean(absenteeism_data$Transportation.expense[absenteeism_data$ID==i],na.rm = T)
}
#Finding ID column values for which there are missing values in Distance.from.Residence.to.Work 
absenteeism_data$ID[is.na(absenteeism_data$Distance.from.Residence.to.Work)]
# [1] 34 22 28
for (i in c(34,22,28)){
  absenteeism_data$Distance.from.Residence.to.Work[is.na(absenteeism_data$Distance.from.Residence.to.Work) & absenteeism_data$ID==i] = mean(absenteeism_data$Distance.from.Residence.to.Work[absenteeism_data$ID==i],na.rm = T)
}
#Finding ID column values for which there are missing values in Service.time
absenteeism_data$ID[is.na(absenteeism_data$Service.time)]
# [1] 28 34 34
for (i in c(34,28)){
  absenteeism_data$Service.time[is.na(absenteeism_data$Service.time) & absenteeism_data$ID==i] = mean(absenteeism_data$Service.time[absenteeism_data$ID==i],na.rm = T)
}
#Finding ID column values for which there are missing values in Age
absenteeism_data$ID[is.na(absenteeism_data$Age)]
# [1] 28 24 24
for (i in c(24,28)){
  absenteeism_data$Age[is.na(absenteeism_data$Age) & absenteeism_data$ID==i] = mean(absenteeism_data$Age[absenteeism_data$ID==i],na.rm = T)
}

#Converting the variable into numeric
absenteeism_data$Work.load.Average.day = as.numeric(absenteeism_data$Work.load.Average.day)
#Work.load.Average.day missing values are imputed using Month.of.absence and Hit.target
absenteeism_data$Month.of.absence[is.na(absenteeism_data$Work.load.Average.day)]
# [1]  9 10 11 11 12 12  1  1  1  5
absenteeism_data$Hit.target[is.na(absenteeism_data$Work.load.Average.day)]
# [1] 92 93 93 93 97 97 95 95 95 92
#making a dataframe having above two series
df = data.frame(m=c(9,10,11,11,12,12,1,1,1,5),h=c(92,93,93,93,97,97,95,95,95,92))
for (i in 1:10){
  absenteeism_data$Work.load.Average.day[(is.na(absenteeism_data$Work.load.Average.day) & 
  absenteeism_data$Month.of.absence==df[i,1]) & absenteeism_data$Hit.target==df[i,2]] = 
  mean(absenteeism_data$Work.load.Average.day[absenteeism_data$Month.of.absence==df[i,1] & absenteeism_data$Hit.target==df[i,2]],na.rm = T)
}
#Hit.target missing values are imputed using Month.of.absence and Work.load.Average.day
absenteeism_data$Month.of.absence[is.na(absenteeism_data$Hit.target)]
# [1] 11 11 12  1  1  1
absenteeism_data$Work.load.Average.day[is.na(absenteeism_data$Hit.target)]
# [1] 306345 306345 261306 308593 308593 308593

df1 = data.frame(m1=c(11,12,1),w1=c(306345,261306,308593))
for (i in 1:3){
  absenteeism_data$Hit.target[(is.na(absenteeism_data$Hit.target) & absenteeism_data$Month.of.absence==df1[i,1]) & absenteeism_data$Work.load.Average.day==df1[i,2]] = mean(absenteeism_data$Hit.target[absenteeism_data$Month.of.absence==df1[i,1] & absenteeism_data$Work.load.Average.day==df1[i,2]],na.rm = T)
}

#Disciplinary.failure missing values put to 0 because mode() of absenteeism_data$Disciplinary.failure =0
absenteeism_data$Disciplinary.failure[is.na(absenteeism_data$Disciplinary.failure)] = 0

#ID column is again used to impute missing value for Education

absenteeism_data$ID[is.na(absenteeism_data$Education)]
# [1] 11 10 34 34 14 34 34 34 10 24
for (i in c(10,11,14,24,34)){
  absenteeism_data$Education[is.na(absenteeism_data$Education) & absenteeism_data$ID==i] = mean(absenteeism_data$Education[absenteeism_data$ID==i],na.rm=T)
 }
#ID column used to impute missing value for Son
absenteeism_data$ID[is.na(absenteeism_data$Son)]
# [1] 20 14 34 34 27  1
for (i in c(1,14,20,27,34)){
  absenteeism_data$Son[is.na(absenteeism_data$Son) & absenteeism_data$ID==i] = mean(absenteeism_data$Son[absenteeism_data$ID==i],na.rm=T)
}
#ID column has been used to impute missing value for Social.drinker
absenteeism_data$ID[is.na(absenteeism_data$Social.drinker)]
# [1] 10 14 17
for (i in c(10,14,17)){
  absenteeism_data$Social.drinker[is.na(absenteeism_data$Social.drinker) & absenteeism_data$ID==i] = mean(absenteeism_data$Social.drinker[absenteeism_data$ID==i],na.rm=T)
}

#ID column has been used to impute missing value for Social.smoker
absenteeism_data$ID[is.na(absenteeism_data$Social.smoker)]
# [1] 34  1 11 15
for (i in c(34,1,11,15)){
  absenteeism_data$Social.smoker[is.na(absenteeism_data$Social.smoker) & absenteeism_data$ID==i] = mean(absenteeism_data$Social.smoker[absenteeism_data$ID==i],na.rm=T)
}

#ID column has been used to impute missing value for Pet
absenteeism_data$ID[is.na(absenteeism_data$Pet)]
# [1] 1 13
for (i in c(1,13)){
  absenteeism_data$Pet[is.na(absenteeism_data$Pet) & absenteeism_data$ID==i] = mean(absenteeism_data$Pet[absenteeism_data$ID==i],na.rm=T)
}
#ID column has been used to impute missing value for Weight
absenteeism_data$ID[is.na(absenteeism_data$Weight)]
# [1] 27

for (i in c(27)){
  absenteeism_data$Weight[is.na(absenteeism_data$Weight) & absenteeism_data$ID==i] = mean(absenteeism_data$Weight[absenteeism_data$ID==i],na.rm=T)
}
#ID column has been used to impute missing value for Height
absenteeism_data$ID[is.na(absenteeism_data$Height)]
# [1] 20 10 28 34 34 27 10 11  5 22 13 24 32 28
for (i in c(20,10,28,34,27,11,5,22,13,24,32)){
  absenteeism_data$Height[is.na(absenteeism_data$Height) & absenteeism_data$ID==i] = mean(absenteeism_data$Height[absenteeism_data$ID==i],na.rm=T)
}
#ID column has been used to impute missing value for Body.mass.index 
absenteeism_data$ID[is.na(absenteeism_data$Body.mass.index)]
# [1]  3 24 11 30  2 19 34  3 28 34 28  3 13 36 11 14 34 36 28 20 28 28 18 28 17 15 20 22 24 11  5
for (i in c(3,24,11,30,2,19,34,28,13,36,14,20,18,17,15,22,5)){
  absenteeism_data$Body.mass.index[is.na(absenteeism_data$Body.mass.index) & absenteeism_data$ID==i] = mean(absenteeism_data$Body.mass.index[absenteeism_data$ID==i],na.rm=T)
}
#Reason.for.absence column has been used to impute missing value for Absenteeism.time.in.hours 
absenteeism_data$Reason.for.absence[is.na(absenteeism_data$Absenteeism.time.in.hours)]
# [1]  23 14 10 22 26 26 23 26 26 22 10  6 28 26 11 22 22 26 26 23 26 13
for (i in c(23,14,10,22,26,6,28,11,13)){
  absenteeism_data$Absenteeism.time.in.hours[is.na(absenteeism_data$Absenteeism.time.in.hours) & absenteeism_data$Reason.for.absence==i] = mean(absenteeism_data$Absenteeism.time.in.hours[absenteeism_data$Reason.for.absence==i],na.rm=T)
}
### All missing values have been imputed ####

# Continuous Variables Distributions
#Transportation.expense 
hist(absenteeism_data$Transportation.expense,prob = TRUE,xlab = 'Transportation.expense')
lines(density(absenteeism_data$Transportation.expense))
#Distance.from.Residence.to.Work
hist(absenteeism_data$Distance.from.Residence.to.Work,prob = TRUE,xlab = 'Distance.from.Residence.to.Work')
lines(density(absenteeism_data$Distance.from.Residence.to.Work))
#Service.time
hist(absenteeism_data$Service.time,prob = TRUE,xlab = 'Service.time')
lines(density(absenteeism_data$Service.time))
#Age
hist(absenteeism_data$Age,prob = TRUE,xlab = 'Age')
lines(density(absenteeism_data$Age))
#Work.load.Average.day
hist(absenteeism_data$Work.load.Average.day,prob = TRUE,xlab = 'Work.load.Average.day')
lines(density(absenteeism_data$Work.load.Average.day))
#Hit.target
hist(absenteeism_data$Hit.target,prob = TRUE,xlab = 'Hit.target')
lines(density(absenteeism_data$Hit.target))
#Weight
hist(absenteeism_data$Weight,prob = TRUE,xlab = 'Weight')
lines(density(absenteeism_data$Weight))
#Height
hist(absenteeism_data$Height,prob = TRUE,xlab = 'Height')
lines(density(absenteeism_data$Height))
#Body.mass.index
hist(absenteeism_data$Body.mass.index,prob = TRUE,xlab = 'Body.mass.index')
lines(density(absenteeism_data$Body.mass.index))
###    All continuous variables have skewed distribution.

##########################Outlier Analysis##########################
num_col =c('Weight', 'Height', 'Body.mass.index','Absenteeism.time.in.hours','Transportation.expense',
           'Distance.from.Residence.to.Work', 'Service.time', 'Age','Hit.target','Work.load.Average.day')
cat_col = c('')
for (i in 1:length(num_col))
{
  assign(paste0("gn",i),ggplot(aes_string(y = (num_col[i]), x = 'Absenteeism.time.in.hours'),data = absenteeism_data) +
           stat_boxplot(geom = "errorbar", width = 0.5) +geom_boxplot(outlier.colour="blue", fill = "skyblue",
          outlier.shape=18,outlier.size=1, notch=FALSE) +labs(y=num_col[i],x="Absenteeism in Hours")+
           ggtitle(paste("Box plot of responded for",num_col[i])))
}

# Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)

#Capping outliers
#converting it into a dataframe which wasa a tipple earlier
absenteeism_data = as.data.frame(absenteeism_data)
#Not using loop because it is causing error Can't use matrix or array for column indexing
for (i in c('Transportation.expense','Service.time','Age','Work.load.Average.day','Hit.target','Height','Absenteeism.time.in.hours')){
  qnt = quantile(absenteeism_data[,i], probs=c(.25, .75), na.rm = T)
  iqr1 = qnt[2]-qnt[1]
  min1 = qnt[1]-1.5*iqr1
  max1 = qnt[2]+1.5*iqr1
  absenteeism_data[,i][absenteeism_data[,i]<min1] = min1
  absenteeism_data[,i][absenteeism_data[,i]>max1] = max1
}
#Correlation Analysis
#Converting catcols to factor as they are categorical
catcols = c('Reason.for.absence','Month.of.absence','Day.of.the.week','Seasons','Disciplinary.failure','Education','Son','Social.drinker','Social.smoker','Pet')

for (i in catcols){
  absenteeism_data[,i] = as.factor(absenteeism_data[,i])
}

str(absenteeism_data)
# Chi-square test for correlation between factors
pval = c()

#Calculating & storing p-values in vector pval from chisquare test

for(i in catcols){
  for(j in catcols){
    chi2 = chisq.test(absenteeism_data[,i],absenteeism_data[,j])
    pval = c(pval,chi2$p.value)
  }
}

length(pval)#100

#converting pval to matrix m1

m1 = matrix(pval,ncol=10)
m1

#Converting m1 to dataframe chi_df
chi_df = data.frame(m1)
#Setting row names to catcols
row.names(chi_df) = catcols
#Setting column names to catcols
colnames(chi_df) = catcols
chi_df

#p-values are <0.05 for chi-square test of all categorical variables with Reason.for.absence except Day.of.the.week.
#This means that categorical variables having p-values<0.05 have dependence on Reason.for.absence.
#So, all categorical variables except Reason.for.absence and Day.of.the.week will be dropped.

absenteeism_data[,c('Month.of.absence','Seasons','Disciplinary.failure','Education','Son','Social.drinker','Social.smoker','Pet')] = list(NULL)

#Correlation between continuous independent variables
cor(absenteeism_data[,4:13])

# Correlation amongst continuous independent variables < 0.95
# Correlation between every independent variable & dependent variable < 0.2
# This means that there is no relationship between any independent variable and dependent variable.
# Relationship between Reason.for.absence and Absenteeism.time.in.hours

# Aggregating Absenteeism.time.in.hours by Reason.for.absence

Reasons = aggregate(absenteeism_data$Absenteeism.time.in.hours, by=list(Category=absenteeism_data$Reason.for.absence), FUN=sum)
Reasons

#Calculating absenteeism_dataeeism time by category as percent of total time in column Absence
Reasons$Absence = (Reasons$x/sum(absenteeism_data$Absenteeism.time.in.hours))*100
Reasons = Reasons[order(Reasons$Absence),]
Reasons

barplot(Reasons$Absence,names.arg=Reasons$Category,xlab="Reason.for.absence",ylab="Absence",col="blue")

#Top 3 categories in terms of Absence time are:
# 1. Category-13:Diseases of the musculoskeletal system and connective tissue - 12.79 % of total time
# 2. Category-23:medical consultation - 11.22 % of total time
# 3. Category-19:Injury, poisoning and certain other consequences of external causes - 10.63 % of total time
# 4. Category 28:dental consultation - 8.54 % 0f total time
# 5. Category 26:unjustified absence - 7.66 % of total time

#Forecasting absenteeism_dataeeism time in hours per month for 2011
#Reading original data again
absenteeism_data1 = read_excel('Absenteeism.xls')
#Renaming the columns
colnames(absenteeism_data1) = c("ID","Reason.for.absence","Month.of.absence","Day.of.the.week","Seasons","Transportation.expense"
                               ,"Distance.from.Residence.to.Work","Service.time","Age","Work.load.Average.day","Hit.target","Disciplinary.failure"
                               ,"Education","Son","Social.drinker","Social.smoker","Pet","Weight","Height","Body.mass.index","Absenteeism.time.in.hours")


#Imputing missing values for Month.of.absence,Reason.for.absence,Absenteeism.time.in.hours
absenteeism_data1$Month.of.absence[is.na(absenteeism_data1$Month.of.absence)] = 10
absenteeism_data1$Reason.for.absence[is.na(absenteeism_data1$Reason.for.absence)] = 27
absenteeism_data1$Reason.for.absence[absenteeism_data1$Reason.for.absence==0] = 26

for (i in c(23,14,10,22,26,6,28,11,13)){
  absenteeism_data1$Absenteeism.time.in.hours[is.na(absenteeism_data1$Absenteeism.time.in.hours) & absenteeism_data1$Reason.for.absence==i] = median(absenteeism_data1$Absenteeism.time.in.hours[absenteeism_data1$Reason.for.absence==i],na.rm=T)
}

#Converting Month.of.absence to factor
absenteeism_data1$Month.of.absence = as.factor(absenteeism_data1$Month.of.absence)
#Making a timeseries aggregating Absenteeism.time.in.hours by Month.of.absence
monthly_absence = aggregate(absenteeism_data1$Absenteeism.time.in.hours,by=list(Category=absenteeism_data1$Month.of.absence),FUN=sum)
monthly_absence = monthly_absence[2:13,]
monthly_absence

#Calculating absenteeism_dataeeism time as percent of total time in column absenteeism_datahours
monthly_absence$absenteeism_datahours = monthly_absence$x/3
row.names(monthly_absence) = monthly_absence$Category
monthly_absence

# Modelling time series using arima
tsdata = ts(monthly_absence$absenteeism_datahours)
class(tsdata)
tsdata

#Visualizing timeseries
plot(tsdata)

#Checking stationarity - Augmented Dickey-Fuller Test
library(tseries)
adf.test(tsdata, alternative="stationary", k=0)

# Augmented Dickey-Fuller Test
# data:  tsdata
# Dickey-Fuller = -3.3984, Lag order = 0, p-value = 0.078
# alternative hypothesis: stationary

#time series tsdata is not stationary as determined by p-value of 0.078(>0.05).

#We will be subtracting shifted(single lag) time series from original time series.

tsdata2 = tsdata - stats::lag((tsdata),1)
plot(tsdata2)
#Doing Augmented Dickey-Fuller Test again
adf.test(tsdata2, alternative="stationary", k=0)
#ACF plot
acf(tsdata2)
#value of p should be 0.
#PACF plot
pacf(tsdata2)
#value of q should be 0.
library(forecast)
model = arima(tsdata2,c(4,0,9))
fit1 = fitted(model)
residuals1 = tsdata2 - fit1
sum(residuals1**2)
#RSS(residual sum of squares) for order=(1,0,0):61679.74
#RSS(residual sum of squares) for order=(2,0,0):47290.44
#RSS(residual sum of squares) for order=(3,0,0):20928.83
#RSS(residual sum of squares) for order=(4,0,0):20649.88
#RSS(residual sum of squares) for order=(4,0,1):20653.29
#RSS(residual sum of squares) for order=(4,0,2):16526.79
#RSS(residual sum of squares) for order=(4,0,3):10442.2
#RSS(residual sum of squares) for order=(4,0,4):8476.191
#RSS(residual sum of squares) for order=(4,0,5):8104.803
#RSS(residual sum of squares) for order=(4,0,7):6743.328
#RSS(residual sum of squares) for order=(4,0,9):2222.32

#Arima with order=(4,0,9) gives us lowest RSS of 2222.32 so we will order=(4,0,9)

plot(tsdata2)
lines(fit1)
absenteeism_data2011 = predict(model,n.ahead = 12)

#Scaling absenteeism_data2011 back to original
absence_2011 = cumsum(absenteeism_data2011$pred)
absence_2011_2 = absence_2011 + rep(tsdata[4],12)
as.data.frame(absence_2011_2)
ts_2011 = ts(absence_2011_2)
df1 = as.data.frame(absence_2011_2)
row.names(df1) = c(13:24)
ts_2011 = ts(df1$absence_2011_2,start=13)

#Plotting original timeseries & forecast values

plot(tsdata,xlim=c(1,24))
lines(ts_2011)
