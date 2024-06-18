#### PACKAGES ####
setwd("G:/Mi unidad/Semestre VII/Analytics Research Lab/COVID-19 project")


library(caret)
library(lubridate)
library(tidyverse)
library(ROSE)
library(pROC)
library(knitr)


#### DATA ####
dep_temps <- read.csv("TempHum.csv")[,-1]
data_with_temps <- read.csv("datos_conTempHum.csv")[,c(-1,-2,-3,-8,-9)]

boxplot(data_with_temps$TempDpto~data_with_temps$atencion)
boxplot(data_with_temps$HumDpto~data_with_temps$atencion)


#Data balancing
data_with_temps <- ROSE(atencion~., data_with_temps, seed = 3)$data


#### CROSS-VALIDATION ####

train_rows <- createDataPartition(y = data_with_temps$Edad, p = 0.8, list = F)

data_train <- data_with_temps[train_rows,]
data_test <- data_with_temps[-train_rows,]

nrow(data_train)
nrow(data_test)

#Export and import (for the removed levels)
write.csv(data_train,file = "data_train.csv")
write.csv(data_test,file = "data_test.csv")

data_train <- read.csv("data_train.csv")[,-1]
data_test <- read.csv("data_test.csv")[,-1]

str(data_train)

#### LOGIT MODEL####
logReg <- glm(atencion~.,data=data_train, family=binomial("logit"))

sum <- summary(logReg)
sum$coefficients
str(data_train)

data_test$tethahat <- predict(logReg, newdata = data_test, type="response")
data_test$tethahat <- as.numeric(data_test$tethahat)

ROC <- roc(data_test$atencion~data_test$tethahat)
Matt <- data.frame(ROC$thresholds,ROC$sensitivities,ROC$specificities)
kable(head(Matt,6), digits = 3)

nbest<-which.max(abs(Matt$ROC.sensitivities +Matt$ROC.specificities-1)) 
tethabest <- Matt[nbest,]
tethacorte<-tethabest[1,1]

plot(ROC,main="Curva ROC y mejor tetha de corte")
points(tethabest[3],tethabest[2],col="red",pch=20)

data_test$yhat[data_test$tethahat>=tethacorte]<-"Recuperado"
data_test$yhat[data_test$tethahat<tethacorte]<-"Fallecido"

caret::confusionMatrix(table(data_test$yhat,data_test$atencion))


#### MODEL LOGIT WITHOUT DAYS SICK ####
logReg2 <- glm(atencion~.-dias.enfermo,data=data_train, family=binomial("logit"))

sum2 <- summary(logReg2)
sum2$coefficients

data_test$tethahat2 <- predict(logReg2, newdata = data_test, type="response")
data_test$tethahat2 <- as.numeric(data_test$tethahat)

ROC2 <- roc(data_test$atencion~data_test$tethahat2)
Matt2 <- data.frame(ROC2$thresholds,ROC2$sensitivities,ROC2$specificities)

nbest2<-which.max(abs(Matt2$ROC2.sensitivities +Matt2$ROC2.specificities-1)) 
tethabest2 <- Matt2[nbest2,]
tethacorte2<-tethabest2[1,1]

plot(ROC2,main="Curva ROC y mejor tetha de corte")
points(tethabest2[3],tethabest2[2],col="red",pch=20)

data_test$yhat2[data_test$tethahat>=tethacorte]<-"Recuperado"
data_test$yhat2[data_test$tethahat<tethacorte]<-"Fallecido"

caret::confusionMatrix(table(data_test$yhat2,data_test$atencion))

# Newdata
dep <- "Amazonas" #Insert the department you want to predict
geo_vars <-  read.csv("TempHum.csv")[,-1]

obs_geo_vars <- geo_vars[geo_vars[,1] == dep,]
obs_temp <- as.numeric(obs_geo_vars[2])
obs_Hum <- as.numeric(obs_geo_vars[3])


