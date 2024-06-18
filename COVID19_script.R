#### PACKAGES ####
setwd("G:/Mi unidad/Semestre VII/Analytics Research Lab/COVID-19 project")

install.packages("caret")
install.packages("tidyverse")
install.packages("ROSE")
install.packages("pROC")
install.packages("knitr")

library(caret)
library(lubridate)
library(tidyverse)
library(ROSE)
library(pROC)
library(knitr)


#### DATA ####
covid <- read.csv("20200518-sitrep-84-covid-19.csv")
#### PRE-PROCESSING ####

#Initial Cleaning
table(covid$atención)

covid<- covid %>% filter(atención=="Recuperado"|atención=="Fallecido") %>%
  rename(Fecha.de.notificacion=Fecha.de.notificación,Ciudad=Ciudad.de.ubicación, Atencion=atención,
               Pais.de.procedencia=País.de.procedencia) %>%
  select(-ID.de.caso, -Codigo.DIVIPOLA, -fecha.reporte.web, -Ciudad)

#Applying date format
covid$Fecha.de.notificacion<-dmy(covid$Fecha.de.notificacion)
covid$Fecha.de.muerte<-dmy(covid$Fecha.de.muerte)
covid$Fecha.diagnostico<-dmy(covid$Fecha.diagnostico)
covid$Fecha.recuperado<-dmy(covid$Fecha.recuperado)
covid$FIS<-dmy(covid$FIS)

#Variable dias enfermo (number of days between diagnosis and recovery/death)
dias_enfermo <- 0
for (i in 1:nrow(covid)) {
  if (is.na(covid$Fecha.recuperado[i])==FALSE) {
    dias_enfermo[i]<-covid$Fecha.recuperado[i]-covid$Fecha.diagnostico[i]
    
  }else{
    
    dias_enfermo[i]<-covid$Fecha.de.muerte[i]-covid$Fecha.diagnostico[i]
    
    
  }
}
covid$dias_enfermo <- dias_enfermo
covid <- filter(covid, dias_enfermo>=0)

# Variable days until diagnosis
covid$dias_respuesta<-covid$Fecha.diagnostico-covid$Fecha.de.notificacion

#Variable days with symptoms
dias_con_sint <- 0
for (i in 1:nrow(covid)) {
  if (is.na(covid$FIS[i])==FALSE) {
    if (is.na(covid$Fecha.recuperado[i])==FALSE) {
      dias_con_sint[i]<-covid$Fecha.recuperado[i]-covid$FIS[i]
      
    }else{
      dias_con_sint[i]<-covid$Fecha.de.muerte[i]-covid$FIS[i]
    }
  }
  else{
    dias_con_sint[i]<-0
  }
  
}
covid$dias_con_sintomas <-  dias_con_sint

#Variable days until notification
dias_cuidado<-0
for (i in 1:nrow(covid)) {
  if (is.na(covid$FIS[i])==FALSE) {
    dias_cuidado[i]<-covid$Fecha.de.notificacion[i]-covid$FIS[i]
    
  }else{
    dias_cuidado[i] <- 0
  }
  
}
covid$dias_hasta_notificacion <- dias_cuidado


#Dropping dates
covid <- select(covid, -Fecha.de.notificacion, -FIS, -Fecha.de.muerte,
                -Fecha.diagnostico, -Fecha.recuperado)

#NA to 0
covid[is.na(covid$dias_enfermo),8] <- 0
covid[is.na(covid$dias_respuesta),9] <- 0
covid[is.na(covid$dias_con_sintomas),10] <- 0
covid[is.na(covid$dias_hasta_notificacion),11] <- 0


#Data export and import (for the levels removed)
write.csv(covid,file = "covid.csv")
covid <- read.csv("covid.csv")[,-1]

#Data balancing
covid <- ROSE(Atencion~., covid, seed = 3)$data



#### CROSS-VALIDATION ####

train_rows <- createDataPartition(y = covid$Edad, p = 0.8, list = F)

data_train <- covid[train_rows,]
data_test <- covid[-train_rows,]

nrow(data_train)
nrow(data_test)

#Export and import (for the levels removed)
write.csv(data_train,file = "data_train.csv")
write.csv(data_test,file = "data_test.csv")

data_train <- read.csv("data_train.csv")[,-1]
data_test <- read.csv("data_test.csv")[,-1]


#Data check
str(data_train)
View(table(data_train$Departamento.o.Distrito.))
View(table(data_train$Edad))
View(table(data_train$Sexo))
View(table(data_train$Tipo))
View(table(data_train$Estado))
View(table(data_train$Pais.de.procedencia))
View(table(data_train$dias_enfermo))
View(table(data_train$dias_con_sintomas))
View(table(data_train$dias_hasta_notificacion))

#### LOGISTIC REGRESSION MODEL ####
logReg <- glm(Atencion~Departamento.o.Distrito.+Edad+Sexo+Pais.de.procedencia+
                Tipo+dias_enfermo+dias_respuesta+dias_con_sintomas+dias_hasta_notificacion,
              data=data_train, family=binomial("logit"))

sum <- summary(logReg)
sum$coefficients
str(data_train)

data_train$tethahat <- predict(logReg, newdata = data_train, type="response")
data_train$tethahat <- as.numeric(data_train$tethahat)

ROC <- roc(data_train$Atencion~data_train$tethahat)
ROC; plot(ROC)
Matt <- data.frame(ROC$thresholds,ROC$sensitivities,ROC$specificities)
nbest<-which.max(abs(Matt$ROC.sensitivities +Matt$ROC.specificities-1))  
tethabest <- Matt[nbest,]
tethacorte<-tethabest[1,1]

ROC;plot(ROC,main="Curva ROC y mejor tetha de corte\n AUC = 0.9228")+
  points(tethabest[3],tethabest[2],col="red",pch=20)

data_train$yhat[data_train$tethahat>=tethacorte]<-"Recuperado"
data_train$yhat[data_train$tethahat<tethacorte]<-"Fallecido"

caret::confusionMatrix(table(data_train$yhat,data_train$Atencion))

models <- data.frame(Model =)

