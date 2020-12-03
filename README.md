library(naivebayes)
library(psych)
library(caret)
data<-read.csv("C:/Users/User/Documents/STIS/Semester 5/Data Mining/Tugas/hepatitis.csv", header = FALSE)
head(data)
for(i in names(data)){
  data[,i]= as.factor(data[,i])
}
str(data)
set.seed(1234)
sampel <- sample(2, nrow(data), replace = T, prob = c(0.8,0.2))
trainingdat <- data[sampel==1, ]
testingdat <- data[sampel==2, ]
print(paste("Jumlah Train Data: ", nrow(trainingdat), "| Jumlah Test Data: ", nrow(testingdat)))
modelnaiv <- naive_bayes(V7~.,data=trainingdat)
modelnaiv
summary(modelnaiv)
prediksi <- predict(modelnaiv, testingdat)
confusionMatrix(table(prediksi,testingdat$V1))
