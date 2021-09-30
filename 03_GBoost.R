
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
library(randomForest)


#Cargamos el dataframe limpio y datos previos ---------------------------------------

load("./data/adult_limpio.Rda")

predictors_names <- c("capital","education","occupation","hours.per.week","relationship",
                      "age","gender","marital.status")


x <- setNames(c(0.5844, 0.8294, 0.907, abs(0.8293-0.8296)), c("kappa","acc","AUC", "diffSensSpec"))
datosModelos <- data.frame(x)
colnames(datosModelos) <- "mejorLogi"
datosModelos$mejoArbol <- c(0.57,0.8534,0.8948,abs(0.6009-0.9328))
datosModelos$mejorBagging<- c(0.5852,0.8596,0.895,abs(0.6038-0.9401))
datosModelos$mejorRF<- c(0.5857,0.8598,0.8912,abs(0.6041-0.9403))


#Mejor Gradient Boosting ------------------------------------------------------

# 	shrinkage (parámetro v de regularización, mide la velocidad de ajuste, a menor v, más lento y necesita más iteraciones, pero es más fino en el ajuste)
# 	n.minobsinnode: tamaño máximo de nodos finales (el principal parámetro que mide la complejidad)
# 	n.trees=el número de iteraciones (árboles)
# 	interaction.depth =2 siempre para arboles binarios

#Recodificamos No como "<=50K" y Yes como ">50K" para evitar errores en la funcion dada
adult %<>% mutate(income = as.factor(if_else(income=="<=50K", "No", "Yes")))

set.seed(12345)

#Probamos con muchos parametros ya que en gb los hiperparametros son interdependientes

gbmgrid<-expand.grid(shrinkage=c(0.1,0.05,0.01,0.001),
                     n.minobsinnode=c(5,10,20),
                     n.trees=c(100,300,500),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

gbm<- train(as.formula(paste("income~",paste(predictors_names,collapse = "+"),sep="")),
            data=adult,
            method="gbm",
            trControl=control,
            tuneGrid=gbmgrid,
            distribution="bernoulli",
            bag.fraction=1,
            verbose=TRUE)

gbm

dev.off()
plot(gbm)

#En el gráfico vemos como ntrees=500 siempre da el mejor accuracy y ya se parece mucho
#a 400 por lo que no tiene mucho sentido explorar numeros mas grandes. FIJAMOS ntrees=500
#
#El shrinkage parece que esta practicamente convergiendo en 0.1 (se podria probar hasta 0.2
#pero no tiene pinta de mejorar mucho mas (aunque lo haria mas rápido))
#
#el numero de observaciones minimas por node parece no estar influyendo mucho
#
#
#Puede ser que sean poca interaciones para los skrinkage mas pequeños pero por
#el poder comptacional disponible vamos a probar con shrinkage mas grandes que necesitan de menos
#iteraciones para converger aunque lo hagan de forma no tan fina.
#



#ESTUDIAMOS EARLY STOPPING fijando


gbmgrid<-expand.grid(shrinkage=c(0.07,0.1,0.13,0.16,0.19),
                     n.minobsinnode=c(10),
                     n.trees=c(500),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

gbm<- train(as.formula(paste("income~",paste(predictors_names,collapse = "+"),sep="")),
            data=adult,
            method="gbm",
            trControl=control,
            tuneGrid=gbmgrid,
            distribution="bernoulli",
            bag.fraction=1,
            verbose=TRUE)

gbm


dev.off()
plot(gbm)

mejorGbm <- gbm

sal<-mejorGbm$pred

salconfu<-confusionMatrix(sal$pred,sal$obs)
salconfu

# Accuracy = 0.8672
# Kappa = 0.6069
# Spec = 0.6176, sens = 0.9457  (un poco desbalanceado)


curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
auc<-curvaroc$auc
auc
# Area under the curve: 0.9226
datosModelos$mejorGbm<- c(0.6069,0.8672,0.9226,abs(0.9457-0.6176))


