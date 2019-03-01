breastcancer<- read.csv("~/Desktop/harmanbhutani/data.csv", header = T, stringsAsFactors = FALSE)

#harman.singh.bhutani@accenture.com

#structure
str(breastcancer)

library(dplyr)
library(ggplot2)


#frequency of tumor
breastcancer %>%
  select(diagnosis)%>%
  group_by(diagnosis)%>%
  summarise(total=n())%>%
  ggplot(aes(x=diagnosis,y=total, fill=diagnosis))+
  geom_bar(stat = "identity")

#missing value

library(VIM)
miss<- aggr(breastcancer, col=c('lightblue2','indianred3'),
            numbers=TRUE,prop = F,labels=names(breastcancer), cex.axis=0.55, combined = TRUE, 
            ylab= "Combination of missing (red) and complete values(blue)",
            gap=2)

breastcancer <- breastcancer[,-33]

library(plyr)

library(psych)

#histogram

multi.hist(breastcancer[,sapply(breastcancer, is.numeric)][2:10], bcol = 'lightblue')
multi.hist(breastcancer[,sapply(breastcancer, is.numeric)][11:19], bcol = 'lightblue')
multi.hist(breastcancer[,sapply(breastcancer, is.numeric)][20:28], bcol = 'lightblue')
multi.hist(breastcancer[,sapply(breastcancer, is.numeric)][29:31], bcol = 'lightblue')

#skewness
library(fBasics)

continuous_variables<- breastcancer[,sapply(breastcancer, is.numeric)]
colSkewness(continuous_variables[-1])



#cube root transformation

normal_cube<-sign(continuous_variables[,c(4,11,
                                          12,13,14,
                                          15,16,17,18,19,
                                          20,21,25,31)]) * abs(continuous_variables[,c(4,11,
                                                                                       12,13,14,
                                                                                       15,16,17,18,19,
                                                                                       20,21,25,31)])^(1/3)

breastcancer_new<-cbind(continuous_variables[,c(1,2,3,5,
                                                6,7,8,9,10,
                                                22,23,24,
                                                26,27,28,29,30)],
                        normal_cube,breastcancer[,sapply(breastcancer,is.character)]) 

colnames(breastcancer_new)[32]<- "diagnosis"



#training and testing

library(caret)

set.seed(23564)
portion= createDataPartition(breastcancer_new$diagnosis, p=0.7, list = FALSE)
train= breastcancer_new[portion,]
test= breastcancer_new[-portion,]  

table(train$diagnosis)
table(test$diagnosis)

#cross validation

set.seed(236)
cvfolds= createMultiFolds(train$diagnosis,k=10)
cv.ctrl=trainControl(method = "repeatedcv",number = 10, repeats = 3,
                     index = cvfolds)
#modelling




naive_bayes.cv = train(diagnosis ~., data = train[,-1], method = "nb",
                       trControl=cv.ctrl,
                       tuneLength = 7)
test_pred_naivebayes=predict(naive_bayes.cv, newdata= test[,-1])
cm_naivebayes=confusionMatrix(test_pred_naivebayes,test$diagnosis)




