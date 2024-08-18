###################################################
#                   Logistic Regression           #
###################################################

library(MASS)
Data = read.csv("heart.csv")

#################Factors#######################

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


#####Dividing the data into training Set and Testing Set#####
options(scipen=999)
set.seed(101)

train.size=floor(nrow(Data)*0.7) #size of training set(get 70% from the data)

#train index: ro get a randomize Rows to be in train set
train.index= sample(1:nrow(Data), train.size,rep=F) #row Number that will be in the training set

train.set=Data[train.index,] # וכל העמודותtrain index כל מספר השורות
test.set=Data[-train.index,] # וכל העמודותtrain index כל מספר השורות הלא הנמצאות ב


####### Algorithm #######


model1= glm(target  ~ . , data = train.set, family=binomial)

summary(model1)

props = predict(model1, type= "response", newdata = test.set) #probs:  נותן הסתבריות לכל תצפית שהוא חולה 

predictions = rep(0,nrow(test.set))


# rep -repet: create vector all zeros (vector size is the size of test set)
# all vectors to 0
predictions [props>= 0.5]=2  #
predictions = factor(predictions)
levels(predictions)=c("0","1")
table(predicted=predictions, actual= test.set$target)


ConfusionMatrix = function(TP,TN,FP,FN)
{
  N = TP+TN+FP+FN
  sensitivity = TP/(TP+FN)
  specificity = TN/(TN+FP)
  J.Index = sensitivity+specificity-1
  Precision = TP/(TP+FP)
  NPV = TN/(TN+FN)
  Accuracy = (TP+TN)/(N)
  F1.Score = (2*TP)/(2*TP+FP+FN)
  Expected.Accuracy = (((TP+FN)*(TP+FP)/N)+((FP+TN)*(TN+FN)/N))/(N)
  Kappa = (Accuracy-Expected.Accuracy)/(1-Expected.Accuracy)
  Measure = c("Sensitivity","Specificity","J.Index","Precision","NPV", "Accuracy","F1.Score","Kappa")
  Value = c(sensitivity,specificity,J.Index,Precision,NPV, Accuracy,F1.Score,Kappa)
  Value = round(Value,3)
  out = data.frame(Measure,Value)
  return(out)
}

evaluationLR = ConfusionMatrix(122,147,9,22)
