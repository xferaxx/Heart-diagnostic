###################################################
#                  Decision Tree                  #
###################################################

library(tree)
Data = read.csv("heart.csv")

#################Factors##########################

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
set.seed(101)

train.size=floor(nrow(Data)*0.6) # size of the training set

train.index= sample(1:nrow(Data), train.size,rep=F) #row Number that will be in the training set

train.set=Data[train.index,]
test.set=Data[-train.index,]


####### Algorithm #######


tree.m = tree(target~., data=train.set)
summary(tree.m)
plot(tree.m)
text(tree.m, pretty = 0)

tree.pred = predict(tree.m, test.set, type="class")
with(test.set, table(tree.pred, target))

cv.m = cv.tree(tree.m, FUN = prune.misclass)
cv.m

plot(cv.m)

prune.m = prune.misclass(tree.m, best = 20)
plot(prune.m)
text(prune.m, pretty=0)

tree.pred = predict(prune.m, test.set, type="class")
with(test.set, table(tree.pred, target))



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


evaluationDT = ConfusionMatrix(168,198,8,36)


