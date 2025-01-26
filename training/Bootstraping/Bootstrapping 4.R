library(readxl)
library(tidyverse)
library(caret)
library(kernlab)
library(ROSE)
library(mosaic)
library(ggplot2)
library(parallel)
library(doSNOW)
library(modelr)

data <- read.csv("datos_limpiosFINALE.csv")

data <- select(data, Departamento.o.Distrito, atencion, Edad, Sexo, dias.antes.notificar)
#View(table(data$Departamento.o.Distrito))

data$Departamento.o.Distrito <- fct_collapse(data$Departamento.o.Distrito,
  Bolivar=c("Cartagena D.T. y C.","Bolívar"))

fct_count(data$Departamento.o.Distrito)
data$Sexo[data$Sexo=="f"]="F"
data$Sexo[data$Sexo=="m"]="M"

data<-filter(data,!is.na(data$dias.antes.notificar==FALSE))
data <-filter(data, data$Departamento.o.Distrito=="Bolivar")
data <- select(data, -Departamento.o.Distrito)

str(data)
#View(table(data$atencion))
#View(table(data$Edad))
#View(table(data$Sexo))
#View(table(data$dias.antes.notificar))
data$atencion <- factor(data$atencion)
data$Sexo <- factor(data$Sexo)

data1 <- ROSE(atencion~.,data,seed=3)$data

#Dividir los datos en Train y test set
indexes <- createDataPartition(data1$atencion,
                                times = 1,
                                p = 0.7,
                                list = FALSE)

data.train <- data1[indexes,]
data.test <- data1[-indexes,]


# ============================================================================================
# TRAIN MODEL
# ============================================================================================

SN <- 30

cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

train_control <- trainControl(method="repeatedcv", number=10, repeats=3,classProbs = T)

#Colocamos a itere para mirar cual es el mejor C
svm0 <- train(atencion ~.,data=data.train,method="svmLinear",trControl=train_control,
              preProcess = c("center","scale"), tuneGrid = expand.grid(C = seq(0, 2, 
                                                                                 length = 20)))


#View the model
svm0

#Graficamos el modelo para visualizar accuracy vs Costo
plot(svm0)
svm0$bestTune

for (i in 1:SN) {
  #Generar nuevos conjuntos de datos
  data_muestra <- resample(data.train)
  #Eliminar columna que se genera
  data_muestra <- data_muestra[,-5]
  #Eliminar algunos valores que R pone negativo
  ifelse(data_muestra$dias.antes.notificar<0,0,data_muestra$dias.antes.notificar)
  
  svmi <- paste("svm",i,sep = "")
  assign(svmi,train(atencion ~.,data=data_muestra,method="svmLinear",trControl=train_control,
                    preProcess = c("center","scale"), tuneGrid = expand.grid(C=svm0$bestTune)))
}
stopCluster(cl) 

#Hay que leer la observacion que se va a predecir
#OBSERVACION <- read.csv(file="observacion)
OBSERVACION <- data.test[1,]
OBSERVACION <- OBSERVACION[,-1]
probs <- c()

for (i in 1:SN) {
  nam1 <- paste("preds",i,sep="_")
  assign(nam1,predict(get(paste("svm",i,sep="")),OBSERVACION,"prob")$Recuperado)
  probs <- cbind(probs,get(paste("preds",i,sep="_")))
}

!all((is.na(probs))==FALSE)

probs <- probs[!is.na(probs)]
probs <- t(probs)

#Eliminamos posibles columnas con NA
all_na <- function(x) any(!is.na(x))
probs <- probs %>% select_if(all_na)

medias <- data.frame(apply(probs,1,mean))
desviacion <- (data.frame(apply(probs,1,sd)))/sqrt(SN)
desviacion <- 1.96*desviacion

up_int <- medias+desviacion
low_int <- medias-desviacion

int_boots <- cbind(OBSERVACION$atencion,low_int,medias,up_int)
names(int_boots) <- c("Atencion","Lower","Mean","Upper")
View(int_boots)

#############################
intervalo <- function(newdata){
  probs <- c()
  for (i in 1:SN) {
    nam1 <- paste("preds",i,sep="_")
    assign(nam1,predict(get(paste("svm",i,sep="")),OBSERVACION,"prob")$Recuperado)
    probs <- cbind(probs,get(paste("preds",i,sep="_")))
  }
  all_testing <- !all((is.na(probs))==FALSE)
  if(all_testing){
  probs <- probs[!is.na(probs)]
  probs <- t(probs)  
  }
  
  medias <- data.frame(apply(probs,1,mean))
  desviacion <- (data.frame(apply(probs,1,sd)))/sqrt(SN)
  desviacion <- 1.96*desviacion
  
  up_int <- medias+desviacion
  low_int <- medias-desviacion
  
  int_boots <- cbind(low_int,medias,up_int)
  names(int_boots) <- c("Lower","Mean","Upper")
  return(int_boots)
}


intervalo(OBSERVACION) #sin variable de respuesta (3 columnas)


svm1 <- readRDS("svm1.RDS")
saveRDS(svm1, file = "svm1.RDS")

######
load("G:/Mi unidad/Semestre VII/Analytics Research Lab/COVID-19 project/Bootstraping/Antioquia.RData")
dep_i <- "Antioquia"
for (i in 1:SN){
name_i <- paste("svm",i,dep_i, sep = "")
assign(name_i, get(paste("svm",i,sep="")))
}
load("G:/Mi unidad/Semestre VII/Analytics Research Lab/COVID-19 project/Bootstraping/Atlantico.RData")
dep_i <- "Atlantico"
for (i in 1:SN){
  name_i <- paste("svm",i,dep_i, sep = "")
  assign(name_i, get(paste("svm",i,sep="")))
}
load("G:/Mi unidad/Semestre VII/Analytics Research Lab/COVID-19 project/Bootstraping/Bolivar.RData")
dep_i <- "Bolivar"
for (i in 1:SN){
  name_i <- paste("svm",i,dep_i, sep = "")
  assign(name_i, get(paste("svm",i,sep="")))
}
setwd("G:/Mi unidad/Semestre VII/Analytics Research Lab/COVID-19 project/Bootstraping/Valle")
dep_i <- "Valle"
for (i in 1:SN){
  name_i <- paste("svm",i,dep_i, sep = "")
  assign(name_i, readRDS(paste("svmValleDC",i, ".rds",sep = "")))
}
setwd("G:/Mi unidad/Semestre VII/Analytics Research Lab/COVID-19 project/Bootstraping/Bogota")
dep_i <- "Bogota"
for (i in 1:SN){
  name_i <- paste("svm",i,dep_i,".RDS", sep = "")
  assign(paste("svm",i,"Bogota",sep=""),readRDS(name_i))
}

################ Calibracion ################ 
load("G:/Mi unidad/Semestre VII/Analytics Research Lab/COVID-19 project/Bootstraping/All_models.RData")


