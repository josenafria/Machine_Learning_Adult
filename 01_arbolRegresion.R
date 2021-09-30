
#Librerias --------------------------------------------------------------------

library(caret)
library(inspectdf)
library(DataExplorer) #Automatic EDA
library(dplyr) # Data processing with pipes
library(missRanger)
library(magrittr)       #pipelines
library(questionr)
library(stringr)
library(outliers)
library(car)
library(pROC)
library(rpart)
library(ggpubr)


#Cargamos el dataframe limpio---------------------------------------

load("./data/adult_limpio.Rda")

#Variables elegidas:

predictors_names <- c("capital","education","occupation","hours.per.week","relationship",
                      "age","gender","marital.status")

x <- setNames(c(0.5844, 0.8294, 0.907, abs(0.8293-0.8296)), c("kappa","acc","AUC", "diffSensSpec"))

datosModelos <- data.frame(x)
colnames(datosModelos) <- "mejorLogi"

#Mejor Arbol de Regresion -----------------------------------------------------

#Recodificamos 0 como "<=50K" y 1 como ">50K" para evitar errores
adult %<>% mutate(income = as.factor(if_else(income=="<=50K", "less50K", "greater50K")))

#Vamos a usar directamente caret sin pasar por la librera rpart

# Validación cruzada simple

control<-trainControl(method = "cv",number=4,
                      classProbs=TRUE,savePredictions = "all")


# Prueba incial con minbucket=30 (no se puede colocar en el grid)

arbolgrid <-  expand.grid(cp=c(0))

set.seed(12)
arbolcaret <- train(as.formula(paste("income~",paste(predictors_names,collapse = "+"),sep="")),
                    data= adult,
                    method = "rpart",
                    trControl = control, 
                    tuneGrid = arbolgrid, 
                    control = rpart.control(minbucket = 30) )

arbolcaret

sal<-arbolcaret$pred

salconfu<-confusionMatrix(sal$pred,sal$obs)
salconfu
# Accuracy = 0.8529
# Kappa = 0.5671
# Sens = 0.5855 spec = 0.9338 (un poco desbalanceado)


curvaroc<-roc(response=sal$obs,predictor=sal$greater50K)
auc<-curvaroc$auc
auc
#auc = 0.8869


#VAmos a probar distintos valores de min.bucket usando un for ya que no se puede meter en el grid

min.buckets <- seq(from=5, to=105, by=10)

accs <- c()
kpps <- c()
aucs <- c()

for (minbu in seq(from=5, to=105, by=10)){
    
    cat("\n")
    print(paste("MIN.BUCKET = ", minbu ,sep=""))
    cat("\n")
    
    arbolgrid <-  expand.grid(cp=c(0))
    
    set.seed(12345)
    arbolcaret<- train(as.formula(paste("factor(income)~",paste(predictors_names,collapse = "+"),sep="")),
                       data=adult,
                       method="rpart",
                       trControl=control,
                       tuneGrid=arbolgrid,
                       control = rpart.control(minbucket = minbu))
    
    
    sal<-arbolcaret$pred
    
    salconfu<-confusionMatrix(sal$pred,sal$obs)
    print(salconfu)
    accs <- c(accs, salconfu$overall["Accuracy"])
    kpps <- c(kpps, salconfu$overall["Kappa"])
    
    curvaroc <- roc(response=sal$obs,predictor=sal$greater50K)
    auc <- curvaroc$auc
    print(auc)
    aucs <- c(aucs,auc)
    
    
}

#Graficamente:

metrics <- as.data.frame(cbind(min.buckets,accs,kpps,aucs))
rownames(metrics) <- NULL

g1 <- ggplot(data=metrics,aes(min.buckets,accs)) + geom_line(col="red") + geom_point() +scale_x_continuous(breaks=seq(5,105,10))
g2 <- ggplot(data=metrics,aes(min.buckets,kpps)) + geom_line(col="blue") + geom_point() +scale_x_continuous(breaks=seq(5,105,10))
g3 <- ggplot(data=metrics,aes(min.buckets,aucs)) + geom_line(col="green") + geom_point() +scale_x_continuous(breaks=seq(5,105,10))
ggarrange(g1,g2,g3,labels = c("accuracies","kappas","AUCs"),nrow=3,ncol=1)

#La diferencia en las distintas metricas es muy pequeña pero vamos a elegir 25 ya
#que mas o menos se estabiliza la accuracy y la AUC no desciende mucho

set.seed(12345)
mejorArbol <- train(as.formula(paste("factor(income)~",paste(predictors_names,collapse = "+"),sep="")),
                    data= adult,
                    method = "rpart",
                    trControl = control, 
                    tuneGrid = arbolgrid, 
                    control = rpart.control(minbucket = 25),
)

mejorArbol

sal<-mejorArbol$pred

salconfu<-confusionMatrix(sal$pred,sal$obs)
salconfu
# Accuracy = 0.8534
# Kappa = 0.57
# Sens = 0.60, spec = 0.9328 (un poco desbalanceado)


curvaroc<-roc(response=sal$obs,predictor=sal$greater50K)
auc<-curvaroc$auc
auc
#auc = 0.8948

#Añadimos al dataframe con los datos del modeleos los nuevos resultados

datosModelos$mejoArbol <- c(0.57,0.8534,0.8948,abs(0.6009-0.9328))
datosModelos





