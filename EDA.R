###################################################
#                       EDA                       #
###################################################

options(scipen = 999)

#Packs
library(ggplot2)
library(reshape2) #to change the data from wide to long
library(corrplot)
library(moments)
library(gmodels)

#Import Data
Data = read.csv("heart.csv",sep=",",header=T)


str(Data)


#Data Manipulations

#################Factor##########################
#to change all categorical to factors
Data$target = factor(Data$target)
levels(Data$target)= c("0","1")

Data$sex =factor(Data$sex)
levels(Data$sex)= c ("0","1")

Data$cp =factor(Data$cp)
levels(Data$cp)= c ("0","1","2","3")

Data$fbs =factor(Data$fbs)
levels(Data$fbs)= c ("0","1")

Data$restecg =factor(Data$restecg)
levels(Data$restecg)= c ("0","1","2")

Data$exang =factor(Data$exang)
levels(Data$exang)= c ("0","1")

Data$slope =factor(Data$slope)
levels(Data$slope)= c ("0","1","2")

Data$NumberMV =factor(Data$NumberMV)
levels(Data$NumberMV)= c ("0","1","2","3")

Data$thal=factor(Data$thal)
levels(Data$thal)= c ("0","1","2")
############################################

# sapply : Function to return true of each column if its numeric else return false
numeric.vars = sapply(Data, is.numeric)
#Data.numeric: only numeric Col and row
Data.numeric=Data[,numeric.vars] 

Data.long = melt(Data.numeric)
#melt : to make the data Vertical every column with her data and after her the other column...


#descriptive statistics

#Frequency Table#

#class
ft=data.frame(table(Data$target))
colnames(ft) = c("Heart Disease" , "Freq")

#add Freq Column 
ft$Rel.Freq = round((ft$Freq/ sum(ft$freq))*100,2)

#Bar chart for the class
ggplot(Data,aes(x=target,fill=target,color=target)) + geom_bar(color="black")


#sex
ftSex=data.frame(table(Data$sex))
colnames(ftSex) = c("Sex" , "Freq")

ggplot(Data,aes(x=sex,fill=target,color=target)) + geom_bar(color="black")


#chest
ftchist=data.frame(table(Data$cp))
colnames(ftchist) = c("Chest" , "Freq")

ggplot(Data,aes(x=cp,fill=target,color=target)) + geom_bar(color="black")
 

#fasting
ftFasting=data.frame(table(Data$fbs))
colnames(ftFasting) = c("Fasting" , "Freq")

ggplot(Data,aes(x=fbs,fill=target,color=target)) + geom_bar(color="black")


#resting
ftRest=data.frame(table(Data$restecg))
colnames(ftRest) = c("Resting" , "Freq")

ggplot(Data,aes(x=restecg,fill=target,color=targete)) + geom_bar(color="black")


#exercise
ftexe=data.frame(table(Data$exang))
colnames(ftexe) = c("Exercise" , "Freq")

ggplot(Data,aes(x=exang,fill=target,color=target)) + geom_bar(color="black")


#slope
ftslope=data.frame(table(Data$slope))
colnames(ftslope) = c("Slope" , "Freq")

ggplot(Data,aes(x=slope,fill=target,color=target)) + geom_bar(color="black")


#major
ftmv=data.frame(table(Data$NumberMV))
colnames(ftmv) = c("Major Vessels" , "Freq")

ggplot(Data,aes(x=NumberMV,fill=target,color=target)) + geom_bar(color="black")


#thal
ftthal=data.frame(table(Data$thal))
colnames(ftthal) = c("Thal" , "Freq")

ggplot(Data,aes(x=thal,fill=target,color=target)) + geom_bar(color="black")



#################### Descriptive Statistic Table #######################


#age
Measure = c("Valid N", "Mean", "SD", "Minimum", "Median", "Maximum")
Value = round(c(sum(!is.na(Data$age)),mean(Data$age),sd(Data$age), min(Data$age), median(Data$age), max(Data$age)),2) # if missing values exist, use na.rm = T
Age.DS = data.frame(Measure, Value)
ggplot(Data,aes(x=age,fill=target,color=target)) + geom_bar(color="black")


#resting blood 
Measure2 = c("Valid N", "Mean", "SD", "Minimum", "Median", "Maximum")
Value2 = round(c(sum(!is.na(Data$trestbps)),mean(Data$trestbps),sd(Data$trestbps), min(Data$trestbps), median(Data$trestbps), max(Data$trestbps)),2) # if missing values exist, use na.rm = T
Resting.DS = data.frame(Measure2, Value2)
ggplot(Data,aes(x=trestbps,fill=target,color=target)) + geom_bar(color="black")

#serum
Measure3 = c("Valid N", "Mean", "SD", "Minimum", "Median", "Maximum")
Value3 = round(c(sum(!is.na(Data$chol)),mean(Data$chol),sd(Data$chol), min(Data$chol), median(Data$chol), max(Data$chol)),2) # if missing values exist, use na.rm = T
serum.DS = data.frame(Measure3, Value3)
ggplot(Data,aes(x=chol,fill=target,color=target)) + geom_bar(color= "black")

#maximum
Measure4 = c("Valid N", "Mean", "SD", "Minimum", "Median", "Maximum")
Value4 = round(c(sum(!is.na(Data$thalach)),mean(Data$thalach),sd(Data$thalach), min(Data$thalach), median(Data$thalach), max(Data$thalach)),2) # if missing values exist, use na.rm = T
maximum.DS = data.frame(Measure4, Value4)
ggplot(Data,aes(x=thalach,fill=target,color=target)) + geom_bar(color="black")

#oldpeak
Measure5 = c("Valid N", "Mean", "SD", "Minimum", "Median", "Maximum")
Value5 = round(c(sum(!is.na(Data$oldpeak)),mean(Data$oldpeak),sd(Data$oldpeak), min(Data$oldpeak), median(Data$oldpeak), max(Data$oldpeak)),2) # if missing values exist, use na.rm = T
oldpeak.DS = data.frame(Measure5, Value5)
ggplot(Data,aes(x=oldpeak,fill=target,color=target)) + geom_bar(color="black")



###########correlations###############
### Correlation between the features and the target ###


##################
#    t - test    #
##################


#age
ggplot(data = Data)+geom_density(mapping = aes(x = age, fill = target), alpha = 0.5)
t.test(Data$age ~ Data$target, mu = 0, alternative = "two.sided", var.equal = T)

#resting_blood_pressure
ggplot(data = Data)+geom_density(mapping = aes(x = trestbps, fill = target), alpha = 0.5)
t.test(Data$trestbps ~ Data$target, mu = 0, alternative = "two.sided", var.equal = T)

#serum
ggplot(data = Data)+geom_density(mapping = aes(x = chol, fill = target), alpha = 0.5)
t.test(Data$chol ~ Data$target, mu = 0, alternative = "two.sided", var.equal = T)

#maximum heart rate achieved
ggplot(data = Data)+geom_density(mapping = aes(x =thalach , fill = target), alpha = 0.5)
t.test(Data$thalach ~ Data$target, mu = 0, alternative = "two.sided", var.equal = T)

#Oldpeak
ggplot(data = Data)+geom_density(mapping = aes(x =oldpeak , fill = target), alpha = 0.5)
t.test(Data$oldpeak ~ Data$target, mu = 0, alternative = "two.sided", var.equal = T)


############################
#    Chi square - TEST     #
############################
#sex
table(Data$target,Data$sex) 
CrossTable(Data$target,Data$sex)
chisq.test(table(Data$target,Data$sex))

#chest 
table(Data$target,Data$cp)
CrossTable(Data$target,Data$cp)
chisq.test(table(Data$target,Data$cp))

#Fasting blood sugar
table(Data$target,Data$fbs)
CrossTable(Data$target,Data$fbs)
chisq.test(table(Data$target,Data$fbs))

#Resting electrocardiographic results  
table(Data$target,Data$restecg)
CrossTable(Data$target,Data$restecg)
chisq.test(table(Data$target,Data$restecg))

#exercise_induced_angina
table(Data$target,Data$exang)
CrossTable(Data$target,Data$exang)
chisq.test(table(Data$target,Data$exang))

#slope
table(Data$target,Data$slope) 
CrossTable(Data$target,Data$slope)
chisq.test(table(Data$target,Data$slope))


#major
table(Data$target,Data$NumberMV) 
CrossTable(Data$target,Data$NumberMV)
chisq.test(table(Data$target,Data$NumberMV))

#thal
table(Data$target,Data$thal) 
CrossTable(Data$target,Data$thal)
chisq.test(table(Data$target,Data$thal))


