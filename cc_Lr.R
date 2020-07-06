credit <- read.csv(file.choose())
View(credit)

attach(credit)
credit

summary(credit)
colnames(credit)

credit <- credit[-1]
View(credit)

colnames(credit)

datamodel <-glm(factor(card)~reports+age+income+share+
                  expenditure+factor(owner)+factor(selfemp)+
                  dependents+months+majorcards+active,
                family = 'binomial')
summary(datamodel)

# Odds Ratio
exp(coef(datamodel))
# Confusion matrix table 
prob <- predict(datamodel,type=c("response"),credit)
prob

confusion<-table(prob>0.5,credit$card)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy  #85.9 nothing but 86

datamodel1 <- glm(factor(card)~age+income+share+
                    expenditure+factor(owner)+factor(selfemp)+
                    dependents+months+majorcards+active,
                  family = 'binomial')
summary(datamodel1)

#odds ratio
exp(coef(datamodel1))

#confusion matrix
prob1 < predict(datamodel1, type = c("response"),credit)
prob1

confusion1 <- table(prob1>0.5, credit$card)
confusion1

#Model Accuracy
accuracy <- sum(diag(confusion1)/sum(confusion1))
accuracy #85.9

library(ROCR)
rocrpred <- prediction(prob,credit$card)
rocrpred
rocrperf <- performance(rocrpred,'tpr','fpr') #true postive rigt , false
plot(rocrperf,col=rainbow(10),text.adj=c(0.5,0.5))

