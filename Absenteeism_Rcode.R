#Remove all object to clear the environment
rm(list=ls(all=T))
#set the working directory
setwd("C:/Users/vrush_000/Desktop/Data science/Project/Main Project/Absenteeism")
#Check the working directory
getwd()
#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "dummies","Information",
      "MASS", "rpart", "gbm", "ROSE", "sampling", "DataCombine", "inTrees","psych","gridExtra","xlsx")

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

## Read the data
absent_train = read.xlsx("Absenteeism_at_work_Project.xls",header = T,sheetIndex = 1)
##Explore the data
#dimension of data
dim(absent_train)
#structure of data
str(absent_train)
#unique value in each column
data.frame(apply(absent_train, 2, function(x) length(unique(x))))
#conversion of datatype of variable 
absent_train[1:5] = lapply(absent_train[1:5],as.factor)
absent_train[12:13] = lapply(absent_train[12:13],as.factor)
absent_train[15:16] = lapply(absent_train[15:16],as.factor)
#Let's have a look at summary of each variable 
summary(absent_train)

#*********************Missing Values Analysis**************************************#
#checking for missing values
sum(is.na(absent_train))
#Missing value in each column
missing_val = data.frame(apply(absent_train,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(absent_train)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
#store missing value file to system
write.csv(missing_val, "Missing_perc.csv", row.names = F)
ggplot(data = missing_val[,], aes(x=reorder(Columns, Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "DarkslateBlue") +coord_flip()+xlab("Parameter")+
  ggtitle("Missing data percentage") + theme_bw()
##Imputing missing values with reference to ID's
#For Reason.for.absence
absent_train$ID[which(is.na(absent_train$Reason.for.absence))]
table(absent_train$ID,absent_train$Reason.for.absence)
#3 = 27 and 6 = 23 and 20 = 28 
absent_train$Reason.for.absence[is.na(absent_train$Reason.for.absence) & absent_train$ID == 3] = 27
absent_train$Reason.for.absence[is.na(absent_train$Reason.for.absence) & absent_train$ID == 6] = 23
absent_train$Reason.for.absence[is.na(absent_train$Reason.for.absence) & absent_train$ID == 20] = 28
absent_train$Reason.for.absence[absent_train$Reason.for.absence == 0] = 26

#For Month.of.absence
absent_train$Month.of.absence[is.na(absent_train$Month.of.absence)] = 10
absent_train$Month.of.absence[absent_train$Month.of.absence == 0 & absent_train$Seasons== 1] = 7
absent_train$Month.of.absence[absent_train$Month.of.absence == 0 & absent_train$Seasons== 2] = 2
absent_train$Month.of.absence[absent_train$Month.of.absence == 0 & absent_train$Seasons== 3] = 5

#For Transportation.expense
#imputing with mean vaue of transportation expense for a paticular ID
table(absent_train$ID,absent_train$Transportation.expense)
absent_train$ID[is.na(absent_train$Transportation.expense)]
for (i in c(1,3,10,15,20,22)){
  print(i)
  absent_train$Transportation.expense[is.na(absent_train$Transportation.expense) & absent_train$ID == i] = mean(absent_train$Transportation.expense[absent_train$ID == i],na.rm = T)
} 

#For Distance.from.Residense.to.work
#Imputing with mean value with refernce to the ID
table(absent_train$ID,absent_train$Distance.from.Residence.to.Work)
absent_train$ID[is.na(absent_train$Distance.from.Residence.to.Work)]
for (i in c(22,28,34)){
  print(i)
  absent_train$Distance.from.Residence.to.Work[is.na(absent_train$Distance.from.Residence.to.Work) & absent_train$ID == i] = mean(absent_train$Distance.from.Residence.to.Work[absent_train$ID == i],na.rm = T)
}

#For Service.time
#Imputing with mean value with refernce to the ID
table(absent_train$ID,absent_train$Service.time)
absent_train$ID[is.na(absent_train$Service.time)]
for (i in c(28,34)){
  print(i)
  absent_train$Service.time[is.na(absent_train$Service.time) & absent_train$ID == i] = mean(absent_train$Service.time[absent_train$ID == i],na.rm = T)
}
#For Age
#Imputing with mean value with refernce to the ID
table(absent_train$ID,absent_train$Age)
absent_train$ID[is.na(absent_train$Age)]
for (i in c(24,28)){
  print(i)
  absent_train$Age[is.na(absent_train$Age) & absent_train$ID == i] = round(mean(absent_train$Age[absent_train$ID == i],na.rm = T))
}

#For Height
#Imputing with mean value with refernce to the ID
table(absent_train$ID,absent_train$Height)
H_id = absent_train$ID[is.na(absent_train$Height)]
for (i in H_id){
  print(i)
  absent_train$Height[is.na(absent_train$Height) & absent_train$ID == i] = round(mean(absent_train$Height[absent_train$ID == i],na.rm = T))
} 

#For work.load.average.day
#Imputing with mean value with refernce to the ID
table(absent_train$ID,absent_train$Work.load.Average.day.)
W_id = absent_train$ID[is.na(absent_train$Work.load.Average.day.)]
for (i in W_id){
  print(i)
  absent_train$Work.load.Average.day.[is.na(absent_train$Work.load.Average.day. & absent_train$ID == i)] = mean(absent_train$Work.load.Average.day.[absent_train$ID == i],na.rm = T)
}
#For Hit.target
#Imputing with mean value with refernce to the ID
table(absent_train$ID,absent_train$Hit.target)
T_id = absent_train$ID[is.na(absent_train$Hit.target)]
for (i in T_id){
  print(i)
  absent_train$Hit.target[is.na(absent_train$Hit.target) & absent_train$ID == i] = round(mean(absent_train$Hit.target[absent_train$ID == i],na.rm = T))
}
#For Disciplinary.failure
table(absent_train$ID,absent_train$Disciplinary.failure)
absent_train$ID[is.na(absent_train$Disciplinary.failure)]
for (i in c(10,20,22,34)){
  print(i)
  absent_train$Disciplinary.failure[is.na(absent_train$Disciplinary.failure) & absent_train$ID == i] = 0
}
#For Education
table(absent_train$ID,absent_train$Education)
E_id = absent_train$ID[is.na(absent_train$Education)]

for(i in E_id){
  print(i)
  absent_train$Education[is.na(absent_train$Education) & absent_train$ID == i] = 1
}
#For Son
absent_train$ID[is.na(absent_train$Son)]
table(absent_train$ID,absent_train$Son)
#1 = 1,14 = 2,20 = 4,27 = 0,34 = 0
absent_train$Son[is.na(absent_train$Son) & absent_train$ID == 1] = 1
absent_train$Son[is.na(absent_train$Son) & absent_train$ID == 14] = 2
absent_train$Son[is.na(absent_train$Son) & absent_train$ID == 20] = 4
absent_train$Son[is.na(absent_train$Son) & absent_train$ID == 27] = 0
absent_train$Son[is.na(absent_train$Son) & absent_train$ID == 34] = 0
#FOr Social,drinker
table(absent_train$ID,absent_train$Social.drinker)
absent_train$ID[is.na(absent_train$Social.drinker)]
absent_train$Social.drinker[is.na(absent_train$Social.drinker) & absent_train$ID == 10] = 1
absent_train$Social.drinker[is.na(absent_train$Social.drinker) & absent_train$ID == 14] = 1
absent_train$Social.drinker[is.na(absent_train$Social.drinker) & absent_train$ID == 17] = 0
#For social.smoker
table(absent_train$ID,absent_train$Social.smoker)
absent_train$ID[is.na(absent_train$Social.smoker)]
for (i in c(1,11,15,34)){
  print(i)
  absent_train$Social.smoker[is.na(absent_train$Social.smoker) & absent_train$ID == i] = 0
}
#For Pet
table(absent_train$ID,absent_train$Pet)
absent_train$ID[is.na(absent_train$Pet)]
absent_train$Pet[is.na(absent_train$Pet) & absent_train$ID == 1] = 1
absent_train$Pet[is.na(absent_train$Pet) & absent_train$ID == 13] = 0
#For Weight
table(absent_train$ID,absent_train$Weight)
absent_train$ID[is.na(absent_train$Weight)]
absent_train$Weight[is.na(absent_train$Weight) & absent_train$ID == 27] = 58
#For BMI
##We will use the formula of BMI (BMI = weight in kg/(Height in m^2)) to impute missing value in BMI column
absent_train$bmi = NA
absent_train$bmi = (absent_train$Weight)/((absent_train$Height/100)^2)
absent_train$Body.mass.index = ifelse(is.na(absent_train$Body.mass.index),absent_train$bmi,absent_train$Body.mass.index)
absent_train$bmi = NULL
#For Absenteeism.time.in.hours
#we will compare mean, mediam and knn and will select most suited method
#absent_train[3,21]
#absent_train[3,21] = NA
#Actual value = 2
#Mean = 3
#Median = 2
#knn = 4
#Mean method
table(absent_train$Reason.for.absence,absent_train$Absenteeism.time.in.hours)
T_reason = absent_train$Reason.for.absence[is.na(absent_train$Absenteeism.time.in.hours)]
#for (i in T_reason){
#  print(i)
#  absent_train$Absenteeism.time.in.hours[is.na(absent_train$Absenteeism.time.in.hours) & absent_train$Reason.for.absence == i] = round(mean(absent_train$Absenteeism.time.in.hours[absent_train$Reason.for.absence == i],na.rm = T))
#}
#Median Method
for (i in T_reason){
  print(i)
  absent_train$Absenteeism.time.in.hours[is.na(absent_train$Absenteeism.time.in.hours) & absent_train$Reason.for.absence == i] = round(median(absent_train$Absenteeism.time.in.hours[absent_train$Reason.for.absence == i],na.rm = T))
}
anyNA(absent_train)
# kNN Imputation
#absent_train = knnImputation(absent_train, k = 3)
#we have selected mediam method as it gives closer value to actual value
#*********************Data Visualisation********************************************#
#Histogram of numeric variables
#checking normality and skewness
numeric_index = sapply(absent_train,is.numeric) #selecting only numeric
numeric_data = absent_train[,numeric_index]
cnames = colnames(numeric_data)
multi.hist(numeric_data,nrow = 4,ncol = 3,bcol="linen", dcol=c("blue","red"),dlty=c("solid","solid"),main=NULL)
#Box plot distribution of numeric variables
par(mfrow = c(3,4))
for (i in 1:length(cnames)){
  boxplot(numeric_data[,i],xlab = cnames[i],outcol = "red",boxcol = "brown",medcol = "green")
}
par(mfrow = c(1,1))
#Categorical variable univariate analyis
factor_index = sapply(absent_train,is.factor)
factor_data = absent_train[,factor_index]
cat_names = colnames(factor_data)
#Define function to draw barplots
count.show = function(x){
  ggplot(absent_train,aes(x = absent_train[,x])) + 
    geom_bar(fill = "darkslateblue") + 
    theme_bw() + xlab(x) + ylab("Count") 
}
for (i in 1:length(cat_names)){
  assign(paste0("plt",i),count.show(cat_names[i]))
}
grid.arrange(plt1,plt2,nrow =2)
grid.arrange(plt3,plt4,plt5,plt6,ncol = 2,nrow = 2)
grid.arrange(plt7,plt8,plt9,ncol = 2,nrow = 2)

#Compairing categories with mean of target variable(Bivariate analysis)
#Define function to draw bar plots
cat_show = function(x,y){
m = aggregate(absent_train[,x],by = list(category = absent_train[,y]),FUN = mean)
ggplot(m,aes(x = m$category,y = m$x)) + geom_bar(stat = "identity",fill = "DarkslateBlue") + xlab(y) + 
  ylab("Mean of Absenteeism.time.in.hours") + 
  theme_bw()
}
cat_show(21,"ID")
cat_show(21,"Reason.for.absence")
cat_show(21,"Month.of.absence")
cat_show(21,"Day.of.the.week")
cat_show(21,"Seasons")
cat_show(21,"Disciplinary.failure")
cat_show(21,"Education")
cat_show(21,"Social.drinker")
cat_show(21,"Social.smoker")
#***********************Outlier Analysis********************************************#
#BoxPlots - Distribution and Outlier Check

numeric_index = sapply(absent_train,is.numeric) #selecting only numeric
numeric_data = absent_train[,numeric_index]
cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i])), data = subset(absent_train))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot of time for",cnames[i])))
}


# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)
gridExtra::grid.arrange(gn10,gn11,gn12,ncol=3)
#Outlier values in each column
for (i in cnames){
  print(i)
  print(unique(boxplot.stats(absent_train[,i])$out))
} 
#Replace outlier with NA's 
for(i in cnames){
  val = absent_train[,i][absent_train[,i] %in% boxplot.stats(absent_train[,i])$out]
  print(length(val))
  absent_train[,i][absent_train[,i] %in% val] = NA
}
#Imputing missing values resulting from outlier
#absent_train$Transportation.expense[2] = 118
#Actual value = 118
#Mean = 220.79
#median = 225
#knn = 137
#Mean Method
#absent_train$Transportation.expense[is.na(absent_train$Transportation.expense)] = mean(absent_train$Transportation.expense,na.rm = T)
#Median method
#absent_train$Transportation.expense[is.na(absent_train$Transportation.expense)] = median(absent_train$Transportation.expense,na.rm = T)
#KNN Imputation
absent_train = knnImputation(absent_train, k = 3)
#we select knn as it best fits our data
#store clean file into the system
write.csv(absent_train, "clean_file_R.csv", row.names = F)
#*******************************Feature Selection************************************#

#Correlation Plot 

corrgram(absent_train[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#As weight and BMI are correlated, we will eliminate BMI as weight is the basic mesure 
## Dimension Reduction
absent_train = subset(absent_train, select = -Body.mass.index)

#***********************************Feature scaling**********************************#
#Normality check
par(mfrow = c(2,2))
qqnorm(absent_train$Transportation.expense,main = 'Transportation.expense')
qqnorm(absent_train$Distance.from.Residence.to.Work,main = 'Distance.from.Residence.to.work')
qqnorm(absent_train$Service.time,main = 'Service.time')
qqnorm(absent_train$Absenteeism.time.in.hours,main = 'Absenteeism.time.in.hours')
par(mfrow = c(1,1))
# As our variables are not normally distributed, we will use normalization approach
#store cotinous variables name
numeric_index = sapply(absent_train,is.numeric)
numeric_data = absent_train[,numeric_index]
cnames = colnames(numeric_data)
#drop target variable
cnames = cnames[-11]
#Performing feature scaling with normalization
#Normalization formula
for(i in cnames){
  print(i)
  absent_train[,i] = (absent_train[,i] - min(absent_train[,i]))/
    (max(absent_train[,i] - min(absent_train[,i])))
}

#If the distribution is normal , we can use below formula for feature scaling
# #Standardisation
#for(i in cnames){
#print(i)
#absent_train[,i] = (absent_train[,i] - mean(absent_train[,i]))/
# sd(absent_train[,i])
#}

#Remove the unnecessary object
rmExcept("absent_train")
#Creating dummies for categorical variable
library(dummies)
#create list of categorical variables
Factor_index = sapply(absent_train,is.factor)
Factor_data = absent_train[,Factor_index]
cnames = colnames(Factor_data)
#apply dummies package for one hot encoding store the result in data object
data = dummy.data.frame(absent_train, names = cnames)
#*************************************Sampling******************************************#
#Divide the data into train and test in ratio of 80:20 using simple random sampling
train_index = sample(1:nrow(data), 0.8 * nrow(data))
train = data[train_index,]
test = data[-train_index,]
#As PCA is unsupervised technique, we will remove target variable from both train
#and test and will save them in different object and later will readd them after 
#performing PCA
#getting target variable from both train and test dataset
T_train = subset(train,select = Absenteeism.time.in.hours)
T_test = subset(test,select = Absenteeism.time.in.hours)
#drop target variable from both train and test
train = subset(train,select = -Absenteeism.time.in.hours)
test = subset(test,select = -Absenteeism.time.in.hours)
#****************************Principal component analysis*********************************#
prin_comp = prcomp(train)
names(prin_comp)
#outputs the mean of variables
prin_comp$center
#Output rotation of variable
prin_comp$rotation
prin_comp$rotation[1:5,1:4]
#Dimension of data
dim(prin_comp$x)
#compute standard deviation of each principal component
std_dev = prin_comp$sdev
#compute variance
pr_var = std_dev^2
#check variance of first 5 components
pr_var[1:5]
#proportion of variance explained
prop_varex = (pr_var/sum(pr_var))
prop_varex[1:10]
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
#Add a training set with principal components
train = data.frame(prin_comp$x)
#we are interested in first 45 PCAs as 95 % of variation is explained by first 45 PC's
train = train[,1:45]
#Add target variable back to train data
train = cbind(train,T_train)
#transform test into PCA
test  =  predict(prin_comp, newdata = test)
test = as.data.frame(test)
#select the first 45 components
test = test[,1:45]
#Add target variable back to test data
test = cbind(test,T_test)
#*****************************Model Building*****************************************#
set.seed(1234)
#********************Decision tree (rpart) for regression********************************#
# building Decision tree
fit = rpart(Absenteeism.time.in.hours ~ ., data = train, method = "anova")
#plot decision tree
plot(fit)
text(fit,pretty = 0)
#Predict for test cases
predictions_DT = predict(fit, test[,-46])
#Evaluate performance of model with RMSE and R-squared value
print(postResample(pred = predictions_DT, obs = test[,46]))
#RMSE = 3.42
#Rsquared = 10.18
#***************************Random Forest*************************************************#
#Building random forest
fit_RF = randomForest(Absenteeism.time.in.hours ~., data = train)
fit_RF
#Predict for test cases
predictions_RF = predict(fit_RF,test[,1:45])
# Evaluate performance of model with RMSE and R-squared value(Result)
print(postResample(pred = predictions_RF, obs = test$Absenteeism.time.in.hours))
#RMSE = 2.67
#Rsquared = 35.35
#**************************Linear regression Model***************************************#
#Building Linear Regression Model
lm_model = lm(Absenteeism.time.in.hours~.,data = train)
summary(lm_model)
#Plot linear regression model
par(mfrow = c(2,2))
plot(lm_model) 

##Building second linear model with square root transformation of dependent variable to improve it's normality 
lm_model2 = lm(sqrt(Absenteeism.time.in.hours)~.,data = train)
summary(lm_model2)
#Plot linear regression model
par(mfrow = c(2,2))
plot(lm_model2) 
#Predict for test cases as well as squaring prediction as we have taken square root of target variable in train data
predictions_lm = (predict(lm_model2,test[,1:45]))^2
#Evaluate performance of model with RMSE and R-squared value
print(postResample(pred = predictions_lm, obs = test[,46]))
#RMSE = 2.67
#RSquared = 36.07
#Based on the values of RMSE and Rsquared, Linear Regression gives least value of RMSE and Highest value of Rsquared, Hence we are selecting Linear Regression second model to predict for test cases.
#*******************************Monthly work loss***************************************#
#Get the clean file
loss = read.csv("clean_file_R.csv", header = T)
#check for any missing value
anyNA(loss)
#selct the column to calculate monthly work loss
Loss_df = loss[,c("Month.of.absence","Service.time","Work.load.Average.day.","Absenteeism.time.in.hours")]
#Formula to calculate monthly work loss 
#work loss = (work.load.average.day * Absenteeism.time.in.hours)/(Service.time)
#create a new column with monthly_loss
Loss_df$Monthly.loss = (Loss_df$Work.load.Average.day. * Loss_df$Absenteeism.time.in.hours)/(Loss_df$Service.time)
#Total work loss per month
Loss_2011 = aggregate(Loss_df$Monthly.loss, by = list(Month.of.absence = Loss_df$Month.of.absence),FUN = sum)
#Rename variable name
names(Loss_2011)[2]  ="Monthly.loss"
#Visualise monthly work loss
ggplot(Loss_2011,aes(x = Loss_2011$Month.of.absence,y = Loss_2011$Monthly.loss)) + 
  geom_bar(stat = "identity",fill = 'orange') + xlab("Month.of.absence") + ylab("Monthly_loss") + 
  theme_bw() + ggtitle(" Possible Monthly work loss in 2011 ")

