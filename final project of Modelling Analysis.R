
#Libraries

library(pastecs)
library(ggstatsplot)
library(dplyr)
library(ggplot2)
library(MASS)
library(ISLR)
library(dplyr)
library(factoextra)
library(summarytools)
library(gridExtra)
library(caTools)
library(caret)
library(nnet)
#=======Preliminary Analysis==========
FP<-read.csv("project_data.csv",header=TRUE)
str(FP)
#changing data format
FP$Life.expec[is.na(FP$Life.expec)] = mean(FP$Life.expec, na.rm=TRUE) 
FP$Elect.access[is.na(FP$Elect.access)] = mean(FP$Elect.access, na.rm=TRUE)
FP$Net.nat.income[is.na(FP$Net.nat.income)] = mean(FP$Net.nat.income, na.rm=TRUE)
FP$Net.nat.income.capita[is.na(FP$Net.nat.income.capita)] = mean(FP$Net.nat.income.capita, na.rm=TRUE)
FP$Mortality.rate[is.na(FP$Mortality.rate)] = mean(FP$Mortality.rate, na.rm=TRUE)
FP$Primary[is.na(FP$Primary)] = mean(FP$Primary, na.rm=TRUE)
FP$Pop.growth[is.na(FP$Pop.growth)] = mean(FP$Pop.growth, na.rm=TRUE)
FP$Pop.density[is.na(FP$Pop.density)] = mean(FP$Pop.density, na.rm=TRUE)
FP$Pop.total[is.na(FP$Pop.total)] = mean(FP$Pop.total, na.rm=TRUE)
FP$Health.exp.capita[is.na(FP$Health.exp.capita)] = mean(FP$Health.exp.capita, na.rm=TRUE)
FP$Unemployment[is.na(FP$Unemployment)] = mean(FP$Unemployment, na.rm=TRUE)
FP$GDP.growth[is.na(FP$GDP.growth)] = mean(FP$GDP.growth, na.rm=TRUE)
FP$Health.exp[is.na(FP$Health.exp)] = mean(FP$Health.exp, na.rm=TRUE)
FP$GDP.capita[is.na(FP$GDP.capita)] = mean(FP$GDP.capita, na.rm=TRUE)
FP$Birth.rate[is.na(FP$Birth.rate)] = mean(FP$Birth.rate, na.rm=TRUE)
FP$Water.services[is.na(FP$Water.services)] = mean(FP$Water.services, na.rm=TRUE)

#change comp.education into numeric
FP$Comp.education<-as.numeric(FP$Comp.education)
FP$Comp.education[is.na(FP$Comp.education)] = mean(FP$Comp.education, na.rm=TRUE)
typeof(FP$Comp.education)
#Change Covid deaths into numeric
FP$Covid.deaths<-as.numeric(FP$Covid.deaths)
FP$Covid.deaths[is.na(FP$Covid.deaths)] = mean(FP$Covid.deaths, na.rm=TRUE)
str(FP)
#descriptive summary

stat.desc(FP)

FP$Continent[FP$ï..Country.Name=="Solomon Islands"]<-"Australia/Oceania" #assigning continent name which is empty previosuly
summary(FP)

Summary_FP<-descr(FP, headings = FALSE, stats = "common") # most common descriptive statistics
Summay<-Summary_FP
#Graphical Analysis of Data

FP%>%ggplot(aes(`Covid.deaths`, `Pop.growth`, colour=`Continent`)) +geom_boxplot()

#+================Clustering=============
#removing variable covid deaths and continent
FP2 <- FP %>%
  select(-Continent, -Covid.deaths,-ï..Country.Name)
FP4 <- FP %>%
  select(-Continent,-ï..Country.Name)

par(mfrow=c(1,8))
for(i in 1:8) {
  hist(FP4[,i], main=names(FP4)[i])
}

colnames(FP2)

distance.Euclidean<-get_dist(FP2)
fviz_dist(distance.Euclidean,gradient=list(low="#00AFBB",mid="white",high="#FC4E07"))
distance.corr<-get_dist(FP2,stand=TRUE,method="pearson") 
fviz_dist(distance.corr,gradient=list(low="#00AFBB",mid="white",high="#FC4E07"))

FP3<-scale(FP2)
#K-means clustering, because we need a data into k cluster foam
set.seed(123) 
kmeans2<-kmeans(FP3,centers=2,nstart=20)
kmeans3<-kmeans(FP3,centers=3,nstart=20)
kmeans4<-kmeans(FP3,centers=4,nstart=20)
kmeans4 

f1 <- fviz_cluster(kmeans2, geom = "point", data = FP3) + ggtitle("k = 2")
f2 <- fviz_cluster(kmeans3, geom = "point", data = FP3) + ggtitle("k = 3")
f3 <- fviz_cluster(kmeans4, geom = "point", data = FP3) + ggtitle("k = 4")

grid.arrange(f1,f2,f3,nrow = 2)

#Determining the optimal number of clusters
fviz_nbclust(FP3, kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)

#binding continent again
B<-cbind(FP$Continent,FP2)
cluster=as.integer(kmeans4$cluster)
H<-cbind(B,cluster)
table(kmeans4$cluster,Continent)
#=========Logistic regression=========
#Transform Covid deaths into a binary variable.

set.seed(4500)
GLM_copy = subset(FP, select = -c(Continent, ï..Country.Name) )

GLM_copy$Covid.deaths<-ifelse(GLM_copy$Covid.deaths>mean(GLM_copy$Covid.deaths), 1, 0) #converting covid.deaths into binary variable
GLM_copy$Covid.deaths<- as.factor(GLM_copy$Covid.deaths)
GLM_copy


#split into training and tested data
table(GLM_copy$Covid.deaths)
train <-((FP$GDP.growth)<3)
train
GLM_train<-GLM_copy[!train, ]
typeof(GLM_train)

GLM_test<-GLM_copy[!train,]
#Applting GLM Function
glm.fits<-glm(Covid.deaths~., data = GLM_copy, family = binomial, subset = train)
summary(glm.fits)
#GLM Prediction and mean
glm.probs<-predict(glm.fits, GLM_test, type="response")
glm.pred<-rep(0,82)
glm.pred[glm.probs>0.5]=1
table(glm.pred, GLM_test)
mean(glm.pred==GLM_test$Covid.deaths)
                
#===============LDA and QDA===========#


# divide into 4 possible labels
set.seed(4500)
LR_FP <- FP
LR_FP <- subset(LR_FP, select = -c(Continent,ï..Country.Name) )
LR_FP$Covid.deaths[is.na(LR_FP$Covid.deaths)] = mean(LR_FP$Covid.deaths, na.rm=TRUE)
LR_FP$Covid.deaths <- ifelse((LR_FP$Covid.deaths < 250) , 0 ,
                                      ifelse((LR_FP$Covid.deaths < 1000), 1,
                                             ifelse((LR_FP$Covid.deaths < 2000), 2, 3)))
#Logistic Regression
LR_FP$Covid.deaths=as.factor(LR_FP$Covid.deaths)
LR_FP_train <-((LR_FP$GDP.growth)<3)
LR_FP_train2<-LR_FP[LR_FP_train,]
LR_FP_test_data<-LR_FP[-LR_FP_train,]
Multinorm_FP <- multinom(Covid.deaths~.,data = LR_FP_train2)
summary(Multinorm_FP)
                                                                                       
#Logistic Regression Prediction
multinorm_Predict <- predict(Multinorm_FP, LR_FP_test_data)
multinorm_Predict
mean(multinorm_Predict==LR_FP_test_data$Covid.deaths)
#LDA
set.seed(5500)
lda_fit <- lda(Covid.deaths~.,data=LR_FP_train2)
lda_fit
plot(lda_fit)
lda_predicted=predict(lda_fit, LR_FP_test_data)
lda_predicted
mean(lda_predicted$class==LR_FP_test_data$Covid.deaths)
#QDA
set.seed(5000)
qda_fit <- train(Covid.deaths~.,data=LR_FP, method="qda")
qda_fit
plot(qda_fit)
qda_predicted=predict(qda_fit, LR_FP_test_data)
qda_predicted
mean(qda_predicted==LR_FP_test_data$Covid.deaths)

#cross validation
trControl<-trainControl(method="cv", number=5)
#confusion Matrix of LDA
set.seed(7000)
lda_control<-train(Covid.deaths~., method="lda", trControl=trControl,metric="Accuracy", data=LR_FP)
lda_control_predict<-predict(lda_control,LR_FP)
lda_matrix<-confusionMatrix(lda_control)
lda_matrix
#confusion Matrix of QDA
set.seed(8000)
qda_control<-train(Covid.deaths~., method="qda", trControl=trControl,metric="Accuracy", data=LR_FP)
qda_control_predict<-predict(qda_control,LR_FP)
qda_matrix<-confusionMatrix(qda_control)
qda_matrix

#=============End.============#

lmcovid = lm(Covid.deaths~FP4) #Create the linear regression
summary(lmcovid)
model<-lm(Covid.deaths~Life.expec+Elect.access+Net.nat.income+Net.nat.income.capita+Mortality.rate+Primary+Pop.growth+Pop.density+Pop.total+Health.exp.capita+Health.exp+Unemployment+GDP.growth+GDP.capita+Birth.rate+Water.services+Comp.education)
summary(model
        )
cor(Covid.deaths,Life.expec)
plot(LR_FP_test_data$Covid.deaths[,2], qda_predicted$class, col=test$Survived+10)

