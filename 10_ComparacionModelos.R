
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
library(data.table)
library(reshape2)


#Cargamos las metricas de los modelos ---------------------------------------

x <- setNames(c(0.5844, 0.8294, 0.907, abs(0.8293-0.8296)), c("kappa","acc","AUC", "diffSensSpec"))
datosModelos <- data.frame(x)
colnames(datosModelos) <- "mejorLogi"
datosModelos$mejoArbol <- c(0.57,0.8534,0.8948,abs(0.6009-0.9328))
datosModelos$mejorBagging<- c(0.5852,0.8596,0.895,abs(0.6038-0.9401))
datosModelos$mejorRF<- c(0.5857,0.8598,0.8912,abs(0.6041-0.9403))
datosModelos$mejorGbm<- c(0.6069,0.8672,0.9226,abs(0.9457-0.6176))
datosModelos$mejorXgbm<- c(0.6113 ,0.8657,0.9219,abs(0.9351-0.6451))
datosModelos$mejorRed<- c(0.534 ,0.8493,0.9055,abs(0.9522-0.5221))
datosModelos$mejorEnsamblado <- c(0.5319,0.8380,0.8937,abs(0.5769-0.9222))
datosModelos$mejorStacking<- c(0.7563,0.9113,0.9223,abs(0.9419-0.8140))

#Guardamos el datframe
write.csv(datosModelos,file="./salvados/datosModelos.csv")

#Trasponemos el dataframe para plotear
datosModelos_transpose <- transpose(datosModelos)
datosModelos_transpose
#rownames(datosModelos_transpose) <- colnames(datosModelos)
colnames(datosModelos_transpose) <- rownames(datosModelos)
datosModelos_transpose$modelo <- colnames(datosModelos)

datosModelos_transpose


g1 <- ggplot(data=datosModelos_transpose,
             aes(modelo,acc,group = 1)) + geom_line(col="red") + geom_point() 

g2 <- ggplot(data=datosModelos_transpose,
             aes(modelo,kappa,group = 1)) + geom_line(col="blue") + geom_point() 

g3 <- ggplot(data=datosModelos_transpose,
             aes(modelo,AUC,group = 1)) + geom_line(col="green") + geom_point() 

g4 <- ggplot(data=datosModelos_transpose,
             aes(modelo,diffSensSpec,group = 1)) + geom_line(col="grey") + geom_point() +
                scale_y_reverse(limits = c(0.45, 0.00))

ggarrange(g1,g2,g3,g4,nrow=4,ncol=1)

