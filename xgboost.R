library(e1071)
library(caret)
library(doSNOW)
library(ipred)
library(xgboost)
library(utils)
library(ParallelLogger)
library(parallel)
library(ROSE)
library(dplyr)
library(readxl)




for (i in 1:5){

  
data<-read_excel("datos_limpiosFINALE.xlsx") 
attach(data)

  if (i == 1){
    data<-filter(data,Departamento.o.Distrito=="Antioquia") 
  } else if (i == 2){
    data<-filter(data,Departamento.o.Distrito=="Barranquilla D.E."|Departamento.o.Distrito=="Atlántico")
  } else if (i == 3){
    data<-filter(data,Departamento.o.Distrito=="Bogotá D.C.")
  } else if (i == 4){
    data<-filter(data,Departamento.o.Distrito=="Cartagena D.T. y C."|Departamento.o.Distrito=="Bolívar")
  } else if (i == 5){
    data<-filter(data,Departamento.o.Distrito=="Valle del Cauca")
  }
 

data<-filter(data,Sexo=="F"|Sexo=="M")
data<-select(data,-Departamento.o.Distrito, -Tipo, -EdadInt, -dias.antes.notificar.Int)


data$atencion<-as.factor(data$atencion)
data$Sexo<-as.factor(data$Sexo)
data$Edad<-as.factor(data$Edad)
data$dias.antes.notificar<-as.factor(data$dias.antes.notificar)

data<- ROSE(atencion~.,data,seed=3)$data

# ============================================================================================
# DATA PARTITION
# ============================================================================================

set.seed(54321)
indexes <- createDataPartition(data$atencion,
                               times = 1,
                               p = 0.7,
                               list = FALSE)

data.train <- data[indexes,]
data.test <- data[-indexes,]

attach(data.train)
attach(data.test)

# ============================================================================================
# TRAIN MODEL
# ============================================================================================

# Set  ip caret to perform 3-fold cross validation repeated 3 times and to use a grid search
# for optimas model hyperparameter values
train.control <- trainControl(method = "repeatedcv",
                              number = 3,
                              repeats = 3,
                              search = "grid")

# Leverage a grid search of hyperparameter for xgboost.
tune.grid<- expand.grid(nrounds=c(50, 70, 100, 500),
                       max_depth=c(3,6,9),
                       eta=c(0.001, 0.01, 0.1),
                       gamma=0,
                       colsample_bytree=1,
                       min_child_weight=1,
                       subsample=c(0.8, 1))
# View(tune.grid)

# Use the doSNOW package to enable caret train in parallel. While there are many package
# options in this space, doSNOW has the advantage of working on both windows and Mac OS X.
# 
# Create a Socket cluster usign 10 procedures
#
# NOTE - Tune this number based on the number of cores/threads available on your machine.

cl <- makeCluster(4, type = "SOCK")

# Register cluster so that Caret will know to train in parallel
registerDoSNOW(cl)
caret.cv <- train(atencion ~.,
                  data = data.train,
                  method = "xgbTree",
                  tuneGrid = tune.grid,
                  trControl = train.control)
stopCluster(cl) 
# Train a xgboost model using 10-fold CV repeated 3 times and a hyperparameter grid
# search to train the optimal model


#Examine caret?s processing results
caret.cv


if (i == 1){
  final_model_xgboost_Antioquia <- caret.cv 
} else if (i == 2){
  final_model_xgboost_Atlantico <- caret.cv
} else if (i == 3){
  final_model_xgboost_Bogota <- caret.cv
} else if (i == 4){
  final_model_xgboost_Bolivar <- caret.cv
} else if (i == 5){
  final_model_xgboost_ValledelCauca <- caret.cv
}


}

