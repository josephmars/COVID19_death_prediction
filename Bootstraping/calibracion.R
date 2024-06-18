load("G:/Mi unidad/Semestre VII/Analytics Research Lab I/COVID-19 project/Bootstraping/All_models.RData")
LogLoss<-function(act, pred){
  eps = 1e-15;
  nr = length(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred) + (1-act)*log(1-pred))
  ll = ll * -1/(length(act))
  return(ll);
}
at <- fct_collapse(data$atencion,
                   "0"=c("Fallecido"),
                   "1"=c("Recuperado"))
data2 <- data.frame(at,data)

# para svm1Bolivar
result_cv<-as.data.frame(predict(svm1Bolivar,data2,type="prob")) 
colnames(result_cv)<-c("1","0")

LogLoss(as.numeric(as.character(data2$at)),result_cv$`1`)


# performing platt scaling on the dataset
dataframe<-data.frame(result_cv$`1`,data2$at)

colnames(dataframe)<-c("x","y")

model_log<-glm(y~x,data = dataframe,family = binomial)

result_cv_platt<-predict(model_log,dataframe[-2],type = "response")

result_cv_platt <- as.data.frame(result_cv_platt)

LogLoss(as.numeric(as.character(data2$at)),result_cv_platt)



anyNA(result_cv)

library(SpecsVerification)
# The line below computes the reliability plot data for cross validation dataset without platt scaling
k <-ReliabilityDiagram(result_cv$`1`,as.numeric(as.character(data2$at)),bins = 10,plot = T)
k <-ReliabilityDiagram(result_cv_platt$result_cv_platt,as.numeric(as.character(data2$at)),bins = 10,plot = T)

lines(k$V2, k$V1, xlim=c(0,1), ylim=c(0,1), xlab="Mean Prediction", ylab="Observed Fraction", col="red", type="o", main="Reliability Plot")
legend("topright",lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","red"),legend = c("platt scaling","without plat scaling"))

######### Newdata
newdata <- data.frame(Edad = 20, Sexo = "M", dias.antes.notificar = 2)
pred_xgboost <- predict(svm1Bolivar,newdata,type="prob")
colnames(pred_xgboost)<-c("x","1-x")
prob_log <- predict(model_log,pred_xgboost,type = "response")

View(table(datos_limpiosFINALE$Departamento.o.Distrito))
######### Para los 30 modelos:
datos_limpiosFINALE$Sexo[datos_limpiosFINALE$Sexo=="f"]="F"
datos_limpiosFINALE$Sexo[datos_limpiosFINALE$Sexo=="m"]="M"

data_Atlantico <- filter(datos_limpiosFINALE,
                         Departamento.o.Distrito == "Atlántico")
data_Atlantico <- select(data_Atlantico, atencion, Edad, Sexo, dias.antes.notificar)

data_Antioquia <- filter(datos_limpiosFINALE,
                         Departamento.o.Distrito == "Antioquia")
data_Antioquia <- select(data_Antioquia, atencion, Edad, Sexo, dias.antes.notificar)

data_Bogota <- filter(datos_limpiosFINALE,
                         Departamento.o.Distrito == "Bogotá D.C.")
data_Bogota <- select(data_Bogota, atencion, Edad, Sexo, dias.antes.notificar)

data_Bolivar <- filter(datos_limpiosFINALE,
                      Departamento.o.Distrito == "Cartagena D.T. y C."|Departamento.o.Distrito == "Bolívar")
data_Bolivar <- select(data_Bolivar, atencion, Edad, Sexo, dias.antes.notificar)

data_Valle <- filter(datos_limpiosFINALE,
                      Departamento.o.Distrito == "Valle del Cauca")
data_Valle <- select(data_Valle, atencion, Edad, Sexo, dias.antes.notificar)

dep <- c("Atlantico", "Antioquia", "Bogota", "Bolivar", "Valle")
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

j <- 3
    for (i in c(1,3:19,21,23:24,26,28:30)){
    name_i <- paste("reglog",i,dep[j], sep = "")
    data <- get(paste("data_",dep[j], sep = ""))
    at <- fct_collapse(data$atencion,
                       "0"=c("Fallecido"),
                       "1"=c("Recuperado"))
    data2 <- data.frame(at,data)
    model_i <- get(paste("svm",i,dep[j],sep=""))
    result_cv<-as.data.frame(predict(model_i,data2,type="prob"))  
    colnames(result_cv)<-c("1","0")
    
    dataframe<-data.frame(result_cv$`1`,data2$at)
    colnames(dataframe)<-c("x","y")
    model_log<-glm(y~x,data = dataframe,family = binomial)
    assign(name_i,model_log)
    i
  }


stopCluster(cl) 