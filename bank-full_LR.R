#data <- read.csv(file.choose(),sep = ';')
data <- read.csv(file.choose())
head(data)
View(data)
str(data)


datamodel <- glm(y~., data=data,family = binomial())
summary(datamodel)


View(data$y)
data_predict <- predict(datamodel, data= data, type='response')
cm = table(data$y, data_predict>0.5)
cm

n = sum(cm) # number of instances
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
accuracy = sum(diag) / n 
accuracy #90


precision = diag/colsums
recall = diag/rowsums
f1 = 2*precision * recall/(precision+recall)
data.frame(precision,recall,f1)


library(ROCR)

pred <- prediction(data_predict,data$y)
pred
pref <- performance(pred,"tpr","fpr")
plot(pref, colorise=T,text.adj=c(-0.2,1.7))
