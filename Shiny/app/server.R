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

  
  
server<-function(input,output){

    
  
# DONUT PLOT ===================================================================================    

    
  output$result2<-renderggiraph({
    
    departamento <-input$departamento
    data_input_xgboost$Edad<-input$edad
    data_input_xgboost$Sexo<-ifelse(input$genero=="Female","F","M")
    data_input_xgboost$dias.antes.notificar<-input$diasennotificar
    
    int <- intervalo(data_input_xgboost,departamento)
    
    int[2,]<-c(1-int$Lower_Conf,1-int$Upper_Conf,1-int$Mean,1-int$Lower_Pred,1-int$Upper_Pred)
    
    
    df <- structure(list(Lower_Conf = c(int[1,1], int[2,1]), 
                         Upper_Conf = c(int[1,2], int[2,2]), 
                         Mean = c(int[1,3], int[2,3]), 
                         Lower_Pred = c(int[1,4], int[2,4]), 
                         Upper_Pred = c(int[1,5], int[2,5])), 
                    class = "data.frame", row.names = c("1","2"))
    
    df$id <- 1:dim(df)[1]
    #Melt
    df.melted <- melt(df,id.vars = 'id')
    df.melted$id <- factor(df.melted$id)
    df.melted$id <- relevel(df.melted$id,ref = '2')
    df.melted2<-df.melted[5:6,1:3]
    
    
    donut_plot<-ggplot(df.melted2, aes(x=variable, y=value,fill=id,
                                         label=round(value,3)))+
        geom_bar(stat="identity",position =position_stack(reverse = TRUE))+
        scale_fill_manual(values=c('dodgerblue4','lightsteelblue'),
                          guide = guide_legend(reverse=TRUE))+
        coord_polar('y')+
        theme_void()+
        theme(
          legend.position = "none"
        )+
        annotate(
          geom = "text",
          x = -1.8,
          y = 0,
          label = ifelse(df.melted2[1,3]<0.5,"Fallecido","Recuperado"),
          size = 10,
          color = "lightblue4"
        )+
      annotate(
        geom = "text",
        x = 1,
        y = (df.melted2[2,3])*0.5,
        label = paste0(round(100 * df.melted2[2,3], 1), "%"),
        size = 4,
        color = "white"
      )
    
    ggiraph(ggobj = donut_plot)

  })
  
  output$progressBox1 <- renderValueBox({
    
    departamento <-input$departamento
    data_input_xgboost$Edad<-input$edad
    data_input_xgboost$Sexo<-ifelse(input$genero=="Female","F","M")
    data_input_xgboost$dias.antes.notificar<-input$diasennotificar
    
    int <- intervalo(data_input_xgboost,departamento)
    
    int[2,]<-c(1-int$Lower_Conf,1-int$Upper_Conf,1-int$Mean,1-int$Lower_Pred,1-int$Upper_Pred)
    
    valueBox(
      value = tags$p("Intervalos de confianza", style = "font-size: 50%;"), 
      paste0(round(100*int[2,2],1), "% - ",round(100*int[2,1],1),"%"),
      color = "navy")
  })

  
  output$progressBox2 <- renderValueBox({
    
    departamento <-input$departamento
    data_input_xgboost$Edad<-input$edad
    data_input_xgboost$Sexo<-ifelse(input$genero=="Female","F","M")
    data_input_xgboost$dias.antes.notificar<-input$diasennotificar
    
    int <- intervalo(data_input_xgboost,departamento)
    
    int[2,]<-c(1-int$Lower_Conf,1-int$Upper_Conf,1-int$Mean,1-int$Lower_Pred,1-int$Upper_Pred)
    
    valueBox(
      value = tags$p("Intervalos de prediccion", style = "font-size: 50%;"), 
      paste0(round(100*int[2,5],1), "% - ",round(100*int[2,4],1),"%"),
      color = "navy")
    
  })
  
  
  
  # ==========================================
  #XGBoost
  # ==========================================
  

  
  
  
}

