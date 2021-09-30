#Librerias ----------------------------------------------------------------
library(MASS)
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
library(recipes)
library(fastDummies)


#Cargamos el dataframe limpio y datos previos ---------------------------------------

x <- setNames(c(0.5844, 0.8294, 0.907, abs(0.8293-0.8296)), c("kappa","acc","AUC", "diffSensSpec"))
datosModelos <- data.frame(x)
colnames(datosModelos) <- "mejorLogi"
datosModelos$mejoArbol <- c(0.57,0.8534,0.8948,abs(0.6009-0.9328))
datosModelos$mejorBagging<- c(0.5852,0.8596,0.895,abs(0.6038-0.9401))
datosModelos$mejorRF<- c(0.5857,0.8598,0.8912,abs(0.6041-0.9403))
datosModelos$mejorGbm<- c(0.6069,0.8672,0.9226,abs(0.9457-0.6176))
datosModelos$mejorXgbm<- c(0.6113 ,0.8657,0.9219,abs(0.9351-0.6451))


load("./data/adult_limpio_scaled_dummies.Rda")


#Vamos a usar como variables predictoras las elegidas en la exploracion de variables
#en el apartado (apartado 5)

predictors_names_BIC <- c("marital.status_Married", "capital", "education_Post.Graduate", 
                         "education_Bachelors", "hours.per.week", "education_Before.Highschool", 
                         "age", "occupation_Exec.managerial", "education_HS.grad", "occupation_Farming.fishing", 
                         "occupation_Other.service", "relationship_Own.child", "relationship_Wife", 
                         "gender_Male", "occupation_Prof.specialty", "occupation_Tech.support", 
                         "occupation_Handlers.cleaners", "marital.status_Never.married", 
                         "occupation_Machine.op.inspct", "relationship_Other.relative", 
                         "fnlwgt", "native.country_USA", "occupation_Sales", "workclass_Self.emp", 
                         "relationship_Unmarried", "relationship_Not.in.family", "occupation_Protective.serv"
)


#  SVM LINEAL: SOLO PARÁMETRO C

SVMgrid<-expand.grid(C=c(0.01,0.1,0.2,2,5,10,20))

control<-trainControl(method = "cv",number=4,savePredictions = "all")

SVM<- train(data=adult,
            as.formula(paste("income~",paste(predictors_names_BIC,collapse = "+"),sep="")),
            method="svmLinear",
            trControl=control,
            tuneGrid=SVMgrid,
            verbose=TRUE)

#  soluti <- SVM[["pred"]]
# SVM$results
# plot(SVM$results$C,SVM$results$Accuracy)


SVMgrid_poly<-expand.grid(C=c(0.01,0.1,0.2,2,5,10,20),
                          degree = c(2,3),
                          scale = c(0.1,0.5,1,2))


SVM_poly <- train(data=adult,
                  as.formula(paste("income~",paste(predictors_names_BIC,collapse = "+"),sep="")),
                  method="svmPoly",
                  trControl=control,
                  tuneGrid=SVMgrid_poly,
                  verbose=TRUE)





