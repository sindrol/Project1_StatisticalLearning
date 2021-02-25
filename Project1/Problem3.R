library(MASS)
library(caret)
library(class)
library(pROC)

#Get data
id <- "1i1cQPeoLLC_FyAH0nnqCnnrSBpn05_hO" # google file ID
diab <- dget(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
t = MASS::Pima.tr2

#Extract
train = diab$ctrain
test = diab$ctest

#Logreg, Estimate parameters
logreg = glm(diabetes ~., data = train, family="binomial")
logResult = predict(logreg,newdata= test,type="response")

#Make confusion matrix
confmatlog = confusionMatrix(table(logResult, test$diabetes))

#LDA, estimate parameters
lindisc = lda(diabetes ~., data = train, type = "response",prior=c(0.5,0.5))
lindiscResult= predict(lindisc, test)

lindisc2 = lda(diabetes ~., data = train, type = "response")
lindiscResult2= predict(lindisc2, test)


#Make confusion matrix
confmatlin = confusionMatrix(table(test$diabetes,lindiscResult$class))

#QDA, estimate parameters
quaddisc = qda(diabetes~., data = train, type = "response")
quaddiscResult = predict(quaddisc, test)

#Make confusion matrix
confmatquad = confusionMatrix(table(test$diabetes,quaddiscResult$class))

#K-nearest, make model
knearest = knn(train, test, train$diabetes, k = 25)
confmatknear = confusionMatrix(table(test$diabetes, knearest))

#modify
knnMod = knn(train, test, train$diabetes, k = 25, prob = T)
probKNN = ifelse(knnMod == 0, 1 - attributes(knnMod)$prob, attributes(knnMod)$prob)




ggroc(roc(factor(test$diabetes), probKNN))
