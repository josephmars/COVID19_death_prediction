library(shiny)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(viridisLite)
library(ggiraph)
library(magrittr)
library(dplyr)
library(rsconnect)
library(reshape2)
library(kernlab)
library(caret)
library(htmltools)


data_input_xgboost <- data.frame(Edad=rep("",1),Sexo=rep("",1),
                                 dias.antes.notificar=rep("",1),
                                 stringsAsFactors=FALSE)

data_input_xgboost$Edad<-as.factor(data_input_xgboost$Edad)
data_input_xgboost$Sexo<-as.factor(data_input_xgboost$Sexo)
data_input_xgboost$dias.antes.notificar<-as.factor(data_input_xgboost$dias.antes.notificar)

SN <- 30

load("Calibrated_models.RData")

intervalo <- function(newdata, depart){
  if (depart == "Valle del Cauca"){
    depart <- "Valle"
  }
  probs <- c()
  for (i in 1:SN) {
    nam1 <- paste("preds",i,sep="_")
    assign(nam1,predict(get(paste("svm",i,depart,sep="")),newdata,"prob")$Recuperado)
    x_i <- data.frame(get(nam1))
    colnames(x_i) <- "x"
    
    if(exists(paste("reglog",i,depart,sep=""))){
      reglog_i <- get(paste("reglog",i,depart,sep=""))
      calib_prob_i <- predict(reglog_i, x_i,type = "response")
      assign(nam1,as.numeric(calib_prob_i))
      probs <- cbind(probs,get(paste("preds",i,sep="_")))  
    }
    
  }

  
  all_testing <- !all((is.na(probs))==FALSE) #eliminar na
  if(all_testing){
    probs <- probs[!is.na(probs)]
    probs <- t(probs)  
  }
  
  medias <- data.frame(apply(probs,1,mean))
  desviacion <- (data.frame(apply(probs,1,sd)))/sqrt(SN)
  desviacion <- 1.96*desviacion
  
  up_int <- medias+desviacion
  low_int <- medias-desviacion
  
  up_int_pred <- medias+2.045*desviacion*sqrt(1+(1/SN))
  low_int_pred <-medias-2.045*desviacion*sqrt(1+(1/SN))
  
  int_boots <- cbind(low_int,up_int,medias,low_int_pred,up_int_pred)
  names(int_boots) <- c("Lower_Conf","Upper_Conf","Mean","Lower_Pred","Upper_Pred")
  return(int_boots)
}



###### 5 DEPARTAMENTOS

