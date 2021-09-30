
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

source("./cruzada xgboost binaria.R")

predictors_names <- c("capital","education","occupation","hours.per.week","relationship",
                      "age","gender","marital.status")


x <- setNames(c(0.5844, 0.8294, 0.907, abs(0.8293-0.8296)), c("kappa","acc","AUC", "diffSensSpec"))
datosModelos <- data.frame(x)
colnames(datosModelos) <- "mejorLogi"
datosModelos$mejoArbol <- c(0.57,0.8534,0.8948,abs(0.6009-0.9328))
datosModelos$mejorBagging<- c(0.5852,0.8596,0.895,abs(0.6038-0.9401))
datosModelos$mejorRF<- c(0.5857,0.8598,0.8912,abs(0.6041-0.9403))
datosModelos$mejorGbm<- c(0.6069,0.8672,0.9226,abs(0.9457-0.6176))


#MEJOR XGBOOST ------------------------------------------------------

# Caret permite tunear estos parámetros básicos:



# nrounds (# Boosting Iterations)=número de iteraciones
# 
# max_depth (Max Tree Depth)=profundida máxima de los árboles
# 
# eta (Shrinkage)=parámetro v gradient boosting
# 
# gamma (Minimum Loss Reduction)=gamma cte regularización. Dejar a 0 por defecto
# 
# colsample_bytree (Subsample Ratio of Columns) % Sorteo variables antes de cada árbol , 
# al estilo de random forest pero antes del árbol, no en cada nodo. Dejar
# a 1 por defecto.
# 
# min_child_weight (Minimum Sum of Instance Weight).
# observaciones mínimas en el nodo final. Similar al minobsinnode del gbm.
# 
# # subsample (Subsample Percentage)  % Sorteo de observaciones antes de cada árbol , al estilo de random forest.
# Dejar a 1 por defecto.

#Recodificamos No como "<=50K" y Yes como ">50K" para evitar errores en la funcion dada
adult %<>% mutate(income = as.factor(if_else(income=="<=50K", "No", "Yes")))


set.seed(12345)

#En el grid vamos a tomar la misma tonica que con gbm sin utiizar numero de arboles
#nrounds en este caso muy altos por la limitacion computacional,

xgbmgrid<-expand.grid(
    min_child_weight=c(5,10,20),
    eta=c(0.1,0.05,0.03,0.01,0.001),
    nrounds=c(300,500,700),
    max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm<- train(as.formula(paste("income~",paste(predictors_names,collapse = "+"),sep="")),
             data=adult,
             method="xgbTree",
             trControl=control,
             tuneGrid=xgbmgrid,
             verbose=FALSE)

xgbm

plot(xgbm)

#Early Stoppping fijando algunos parametros

xgbmgrid<-expand.grid(
    min_child_weight=c(20),
    eta=c(0.1,0.3,0.6,0.9),
    nrounds=c(300),
    max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm<- train(as.formula(paste("income~",paste(predictors_names,collapse = "+"),sep="")),
             data=adult,
             method="xgbTree",
             trControl=control,
             tuneGrid=xgbmgrid,
             verbose=FALSE)

xgbm

plot(xgbm)

mejorXgbm <- xgbm

sal<-mejorXgbm$pred

salconfu<-confusionMatrix(sal$pred,sal$obs)
salconfu

# Accuracy = 0.8657
# Kappa = 0.6113
# Spec = 0.6451, sens = 0.9351  (un poco desbalanceado)


curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
auc<-curvaroc$auc
auc
# Area under the curve: 0.9226
datosModelos$mejorXgbm<- c(0.6113 ,0.8657,0.9219,abs(0.9351-0.6451))













