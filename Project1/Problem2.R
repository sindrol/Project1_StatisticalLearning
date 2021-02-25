install.packages("knitr") #probably already installed
install.packages("rmarkdown") #probably already installed
install.packages("ggplot2") #plotting with ggplot
install.packages("ggfortify")
install.packages("MASS")
install.packages("class")
install.packages("pROC")
install.packages("plotROC")

library(GGally)
library(ISLR)
library(ggplot2)
library(ggfortify)
library(MASS)
library(class)
library(pROC)
library(plotROC)
library(caret)

#Task 2--------------------------------------------


id <- "1yYlEl5gYY3BEtJ4d7KWaFGIOEweJIn__" # google file ID
d.corona <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                             id), header = T)


#2a----------------------------------------------


table(d.corona[,1])
table(d.corona[,c(2,4)])
table(d.corona[c(1,2)])
table(subset(d.corona,country=="France")[,c(1,2)])








cat(paste("Deceased: ",sum(d.corona$deceased),"\n",
          "Non-deceased: ",length(d.corona$deceased)-sum(d.corona$deceased),sep=""),sep="")


d.france <- subset(d.corona, country=="France")
d.japan <- subset(d.corona, country=="japan")
d.korea <- subset(d.corona, country=="Korea")
d.indonesia <- subset(d.corona, country=="indonesia")


france_male = sum(d.france$sex=="male")
france_female = sum(d.france$sex=="female")

japan_male = sum(d.japan$sex=="male")
japan_female = sum(d.japan$sex=="female")

korea_male = sum(d.korea$sex=="male")
korea_female = sum(d.korea$sex=="female")

indonesia_male = sum(d.indonesia$sex=="male")
indonesia_female = sum(d.indonesia$sex=="female")

#france_male = sum(d.corona$country=="France" && d.corona$sex=="male")
#france_female = sum(d.corona$country=="France" && d.corona$sex=="female")
#japan_male = sum(d.corona$country=="japan"&& d.corona$sex=="male")
#japan_female = sum(c(d.corona$country,d.corona$sex)==c("japan","female"))
#korea_male = sum(c(d.corona$country,d.corona$sex)==c("Korea","male"))
#korea_female = sum(c(d.corona$country,d.corona$sex)==c("Korea","female"))
#indonesia_male = sum(c(d.corona$country,d.corona$sex)==c("indonesia","male"))
#indonesia_female = sum(c(d.corona$country,d.corona$sex)==c("indonesia","female"))

sex_data <- as.data.frame(rbind(
  c("France",france_male,france_female),
  c("Japan",japan_male,japan_female),
  c("Korea",korea_male,korea_female),
  c("Indonesia",indonesia_male,indonesia_female)))
colnames(sex_data) <- c("country","male","female")

sex_data




#2b------------------------------------------------
#(i) 0.1084912 
#(ii) P-value is pretty small, 0.00275, so men is probably more likely to die than women
#(iii) P-value from chi-square test is 0.011390 which is pretty low. Most likely country is meaningful
#(iv) Odds is increased multiplicativly by 1.311721

#Linear
#(i) 0.1005556 
#(ii) Most likely: Pr(>F)=0.0046301
#(iii) Most likely: Pr(>F)=0.0008698
#(iv) Using the regression coefficient, 10 years increases by 0.013, or 1.3 percent points


koreanMan75 <- data.frame(sex="male", age=75, country="Korea")

#LINEAR MODEL
#Easy to use for inference (question (ii), (iii) and (iv))
deceased.lm  <- lm(deceased ~ . ,data=d.corona)


cat("The chance for a 75 year old Korean man to die of corona is:")
predict.lm(deceased.lm,koreanMan75)

#cat(paste("The chance for a 75 year old Korean man to die of corona is:",koreanMan75Die))

summary(deceased.lm)
anova(deceased.lm)

autoplot(deceasedModel, smooth.colour=NA)




#LOGISTIC MODEL
#Smart for question 1

deceased.logm  <- glm(deceased ~ . ,data=d.corona,family="binomial")

cat("The chance for a 75 year old Korean man to die of corona is:")
predict.glm(deceased.logm, koreanMan75, type="response")

summary(deceased.logm)

anova(deceased.logm, test="Chisq")


#2c--------------------------------------------------

#(i)

#Lin reg
decease_ageSex.lm <- lm(deceased ~ age*sex+country, data=d.corona, family="binomial")
summary(decease_ageSex.lm)
anova(decease_ageSex.lm)

#Log reg
decease_ageSex.logm <- glm(deceased ~ age*sex+country, data=d.corona, family="binomial")
summary(decease_ageSex.logm)


#Lin reg
#(i) Pr(>F)=0.121670, so one cannot conclude that age is a greater risk for men than for women

#Log reg
#(i) P-val is very high 0.95733.




#(ii)

d.corona_franceIndonesia <- rbind(d.france, d.indonesia)


#Lin reg
decease_ageEthnic.lm <- lm(deceased ~ age*country+sex, data=d.corona_franceIndonesia, family="binomial")
summary(decease_ageEthnic.lm)
anova(decease_ageEthnic.lm)

#Log reg
decease_ageEthnic.logm <- glm(deceased ~ age*country+sex, data=d.corona_franceIndonesia, family="binomial")
summary(decease_ageEthnic.logm)


#Lin reg
#(ii) Pr(>F)=0.0259754, which is pretty low, which means age probably is a higher risk factor for france than indonesia


#Log reg
#(ii) P-val is pretty low 0.02944.



#2d---------------------------------------------

#(i) By classifying every person with the response that occurs the often, every person would be predicted to be negative. Therefore, one can say the "null rate" for misclassification is the relative amount of people with positive tests, since the most frequent observation is negative. This is 0.0522.
#(ii) The probabilities is pretty similar to the logistic regression, so they might be ok. But the conclusions are useless.
#(iii) Yes. Every person who didn't die was diagnoes with not-die. Every Negative was correctly predicted.
#(iv) No, one can see that the sensitivity of QDA is bigger than in LDA. QDA predicts some positves correctly, which LDA doesn't.

deceased.lda <- lda(deceased ~ ., data=d.corona)
deceased.lda
res.lda = predict(deceased.lda, d.corona)
confusionMatrix(table(res.lda$class,d.corona[,1]))

deceased.qda <- qda(deceased ~ ., data=d.corona)
deceased.qda
res.qda = predict(deceased.qda,d.corona,type="response")
confusionMatrix(table(res.qda$class,d.corona[,1]))





#Task 4--------------------------------------------------------------



#4a---------------------------------------------------------



















