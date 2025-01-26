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


ui<-navbarPage(title = "Predicción de riesgo de muerte por COVID-19", id = "navBar",
              theme = shinytheme("flatly"),
              # PANEL 1
               
               tabPanel("Inicio", value = "home",
                        HTML('<center><img src="LOGO.jpg"width="100px" height="100px"></center>'),
                        tags$h2("COVID-19 PROJECT",align="center"),
                        fluidRow(
                        column(width = 2),
                        column(width = 8,
                               br(),
                              h4("A lo largo de este proyecto se busca determinar un modelo de 
                              clasificación que permita calcular el riesgo de muerte de una persona 
                              por contagio de COVID-19 para dar prioridad a paciente en estado 
                              crítico y analizar como cambia este 
                              riesgo luego de haberle dado atencion al paciente.",
                                  style = "color:grey"),
                              align="center",
                              HTML('<hr style="color: purple;">')),
                        column(width = 2)
                        ),
                        br(),
                        fluidRow(
                          column(width = 1),
                          column(width = 5,
                                 h3("Modelos",style="color:grey",align="center"),
                                 HTML('<hr style="color: purple;">'),
                                 h5("Para determinar el riesgo de muerte de cada paciente se 
                                    hizo uso del modelo de Machine Learning SVM."),
                                 br(),
                                 tabBox(side = "right", width = NULL,
                                        tabPanel("Acerca de SVM",
                                                 column(width=12,
                                                        br(),
                                                        
                                                 "Un SVM es un modelo que representa a los puntos 
                                                 de muestra en el espacio, separando las clases a 
                                                 2 espacios lo mas amplios posibles mediante un 
                                                 hiperplano de separacion definido como el vector 
                                                 entre los 2 puntos, de las 2 clases, mas cercanos 
                                                 al que se llama vector soporte. Cuando las nuevas 
                                                 muestras se ponen en correspondencia con dicho modelo, 
                                                 en funcion de los espacios a los que pertenezcan, pueden 
                                                 ser clasificadas a una o la otra clase",
                                                 br(),
                                                 br())))),
                          column(width = 5,
                                 h3("Variables",style="color:grey",align="center"),
                                 HTML('<hr style="color: purple;">'),
                                 h5("Las varibles utilizadas para hacer realizar el modelo predictivo
                                    se definen a continuación:"),
                                 br(),
                                 h5("     - Edad",
                                    br(),
                                    br(),
                                    "     - Género",
                                   br(),
                                   br(),
                                    "     - Días antes de notificar",
                                   br(),
                                   h5("Esta variable hace referencia al tiempo 
                                      ocurrido entre los primeros síntomas de la
                                      persona hasta que lo notifica a una entidad
                                      de salud pública.",
                                      style = "color:grey")))),
                          column(width = 1)),
          
              
              # PANEL 2 
               
               tabPanel("Modelo para clasificación", value = "home2",
                        fluidRow(HTML('<center><img src="LOGO.jpg"width="100px" height="100px"></center>'),
                                 tags$h2("Dashboard COVID-19",align="center"),
                          
                          column(width = 12,
                                tabBox(side = "left", width = 12,
                                       tabPanel("Modelo SVM",
                                            br(),
                                            
                                            
                                            column(width = 6,
                                            "En esta sección se muestran las variables predictoras para 
                                            el modelo SVM de clasificación",
                                            br(),
                                            br(),
                                            selectInput("departamento",
                                                        "Departamento",
                                                        choices = c("Antioquia", "Atlantico", "Bogota",
                                                                    "Bolivar","Valle del Cauca")),
                                            chooseSliderSkin("Flat"),
                                            setSliderColor(c("LightGrey","LightSeaGreen","LightSeaGreen",""),
                                                           sliderId = c(1,2,3,4)),
                                            sliderInput("edad",
                                                        "Edad",
                                                        min = 0,
                                                        max = 120,
                                                        value = 50),
                                            radioButtons("genero",
                                                         "Genero",
                                                         choices = c("Female","Male")),
                                            sliderInput("diasennotificar",
                                                        "Días en notificar la enfermedad",
                                                        min = 0,
                                                        max = 100,
                                                        value = 10),
                                            p(class = "text-muted",
                                            paste("Esta variable hace referencia al tiempo 
                                                  que le toma a la entidad encargada en notificar
                                                  la enfermedad desde los primeros síntomas"))),
                                            
                                            
                                            
                                            column(width = 6,
                                                   h3("Resultados de la predicción"),
                                                   h5("En esta sección se muestran los resultados del modelo de 
                                                      clasificación de acuerdo a los valores especificados"),
                                                   ggiraphOutput("result2"),
                                                   p(class = "text-muted",
                                                   paste("El porcentaje mostrado hace referencia a 
                                                         la probabilidad de fallecer")),
                                                   tags$style(HTML(".small_icon_test { font-size: 12px; }")),
                                                   valueBoxOutput("progressBox1",width = 5),
                                                   valueBoxOutput("progressBox2",width = 5))
                                            ))))))

