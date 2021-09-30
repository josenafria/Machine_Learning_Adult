
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
datosModelos$mejorRed<- c(0.534 ,0.8493,0.9055,abs(0.9522-0.5221))

load("./data/adult_limpio.Rda")

str(adult)

predictors_names <- c("capital","education","occupation","hours.per.week","relationship",
                      "age","gender","marital.status")

# CONSTRUCCIÓN BÁSICA DE ENSAMBLADO

# 1) Obtener las predicciones de cada algoritmo (mediante samples de bagging)
# 2) unirlas en paralelo en un data frame
# 3) aplicar cualquier función sobre ellas (Vamos a probar promedio y max voting)
# 
# 
# En este caso vamos a ensamblar un modelo de bagging con una regresion logística obtenida en el apartado 1 ya que dio bastante buen resultado siendo un algoritmo bastante simple

#Vamos a recodificar <=50 como 0 y >50K como 1 por facilidad
adult$income <- as.factor(as.integer(adult$income)-1) 

set.seed(1230)
sample <- sample.int(n = nrow(adult),
                     size = floor(0.85*nrow(adult)), replace = F)

train <- adult[sample, ]
test  <- adult[-sample, ]

########### para realiazr el baging muestreamos sobre train cuatro
## veces para formar cuatro muestras sobre las que aprender nuestros modelos 

s1 <- sample.int(n = nrow(train),
                 size = floor(0.45*nrow(train)), replace = T)

train_s1 <- train[s1,]

s2 <- sample.int(n = nrow(train),
                 size = floor(0.45*nrow(train)), replace = T)
train_s2 <- train[s2,]

s3 <- sample.int(n = nrow(train),
                 size = floor(0.45*nrow(train)), replace = T)

train_s3 <- train[s3,]

s4 <- sample.int(n = nrow(train),
                 size = floor(0.45*nrow(train)), replace = T)

train_s4 <- train[s4,]

s5 <- sample.int(n = nrow(train),
                 size = floor(0.45*nrow(train)), replace = T)

train_s5<- train[s5,]

#Entrenamos los modelos con los smaplings

set.seed(1234)
logi<-glm(data=train,
          as.formula(paste("income~",paste(predictors_names,collapse = "+"),sep="")),
          family=binomial(link = "logit"))

set.seed(1234)
logi_s1<-glm(data=train_s1,
             as.formula(paste("income~",paste(predictors_names,collapse = "+"),sep="")),
             family=binomial(link = "logit"))

set.seed(1234)
logi_s2<-glm(data=train_s2,
             as.formula(paste("income~",paste(predictors_names,collapse = "+"),sep="")),
             family=binomial(link = "logit"))

set.seed(1234)
logi_s3<-glm(data=train_s3,
             as.formula(paste("income~",paste(predictors_names,collapse = "+"),sep="")),
             family=binomial(link = "logit"))

set.seed(1234)
logi_s4<-glm(data=train_s4,
             as.formula(paste("income~",paste(predictors_names,collapse = "+"),sep="")),
             family=binomial(link = "logit"))

set.seed(1234)
logi_s5<-glm(data=train_s5,
             as.formula(paste("income~",paste(predictors_names,collapse = "+"),sep="")),
             family=binomial(link = "logit"))


preds_base <- predict(logi,newdata = test,type="response")
# preds_base <- as.factor( if_else(preds<0.2827701,0,1) ) 

preds_1<-predict(logi_s1,newdata=test,type = "response")
preds_2<-predict(logi_s2,newdata=test,type = "response")
preds_3<-predict(logi_s3,newdata=test,type = "response")
preds_4<-predict(logi_s4,newdata=test,type = "response")
preds_5<-predict(logi_s5,newdata=test,type = "response")

unionfinal_glm<-cbind.data.frame(income=test$income, preds_base, preds_1,
                                 preds_2,preds_3,preds_4,preds_5)

#######################################################
#### agregacion por voto o por media ?????? #####

#PROBAMOS POR MEDIA---------------------------------------------------------

predi_test_aver <- (preds_1 + preds_2 + preds_3 + preds_4 + preds_5)/5 

compa_aver <- cbind.data.frame(income=test$income,predi_test_aver,preds_base)

write.csv(compa_aver, file='./salvados/bagin_logistica_average.csv')


#Decidimos si son 0s o 1s con el punto de corte de 0.5
compa_aver %<>% mutate(predi_test_aver= as.factor( if_else(predi_test_aver<0.5,0,1) ),
                      preds_base = as.factor( if_else(preds_base<0.5,0,1) )
)

confMatrix_base<- confusionMatrix(data = compa_aver$preds_base, reference = test$income)
confMatrix_base

confMatrix_ave<- confusionMatrix(data = compa_aver$predi_test_aver, reference = test$income)
confMatrix_ave

#el modelo de ensamblado es muy igual respecto al base, probamos


#PROBAMOS POR votos---------------------------------------------------------


predi_test_maxVote <- cbind.data.frame(preds_1, preds_2 , preds_3 , preds_4,preds_5)
predi_test_maxVote %<>% mutate(preds_1= if_else(preds_1<0.5,0,1) ,
                              preds_2= if_else(preds_2<0.5,0,1) ,
                              preds_3= if_else(preds_3<0.5,0,1) ,
                              preds_4= if_else(preds_4<0.5,0,1) ,
                              preds_5= if_else(preds_5<0.5,0,1)
                            ) 
predi_test_maxVote %<>% mutate(pred_voted =
                                as.factor( if_else(preds_1+preds_2+preds_3+preds_4+preds_5 > 2, 1,0) )
                        ) %>% select(pred_voted)

compa_maxVote <- cbind.data.frame(income=test$income,pred_max = predi_test_maxVote$pred_voted,preds_base)


#Decidimos si son 0s o 1s con el punto de corte de 0.5 (en el base)
compa_maxVote %<>% mutate(preds_base = as.factor( if_else(preds_base<0.5,0,1) ))

confMatrix_maxVote<- confusionMatrix(data = compa_maxVote$pred_max, reference = test$income)
confMatrix_maxVote

aux <- compa_maxVote%>%select(pred_max,preds_base) %>% mutate(mycomp= pred_max==preds_base)
sum(as.vector(aux$mycomp))/nrow(test)

compa_aver2 <- cbind.data.frame(income=test$income,predi_test_aver,preds_base)
curvaroc<-roc(compa_aver2$income,compa_aver2$predi_test_aver)
auc<-curvaroc$auc
auc
#0.8937

datosModelos$mejorEnsamblado <- c(0.5319,0.8380,0.8937,abs(0.5769-0.9222))
