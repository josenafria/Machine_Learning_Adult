

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


#Funciones a usar ------------------------------------------------------------

#Gr?fico con la importancia de las variables en regr. log?stica
importanciaVariablesLog<-function(modelo){
    a<-as.matrix(Anova(modelo,type = "III"))
    aa<-summary(modelo)
    cc<-sort((a[,1])/(modelo$null.deviance),decreasing =T)
    bb<-barplot(cc,horiz=T,las=2,main="Importancia de las variables (R2)",xlim=c(0,max(cc)*1.2))
    text(lapply(cc,function(x) x+max(cc)*0.1),bb,labels = round(cc,4))
}

#Calcula medidas de calidad para un punto de corte dado
sensEspCorte<-function(modelo,dd,nombreVar,ptoCorte,evento){
    probs <-predict(modelo,newdata=dd,type="response")
    cm<-confusionMatrix(data=factor(ifelse(probs>=ptoCorte,1,0)), reference=dd[,nombreVar],positive=evento)
    c(cm$overall[1],cm$byClass[1:2])
} 


#Mejor Logística --------------------------------------------------------------


#Vamos a recodificar <=50 como 0 y >50K como 1 por facilidad
adult$income <- as.factor(as.integer(adult$income)-1)

#Obtengo la partición para un primer modelo sin validación cruzada como modelo inicial

set.seed(12345)
trainIndex <- createDataPartition(adult$income, p=0.9, list=FALSE)
data_train <- adult[trainIndex,]
data_test <- adult[-trainIndex,]

#logiInicial (todas las variables)
set.seed(1234)
logiInicial<-glm(income ~ . , data=data_train, family=binomial)
summary(logiInicial)
par(mar=c(7, 12, 4.1, 2.1))
importanciaVariablesLog(logiInicial) 


preds <- predict(logiInicial,newdata = data_test,type="response")
income_predicts <- data.frame(logiInicial = as.factor(round(preds,0)))


confMatrix_LogiInicial <- confusionMatrix(data = income_predicts$logiInicial, reference = data_test$income)#Vamos a fijarnos en kappa ya que income esta desbalanceado 
#KappA = 0.5631
confMatrix_LogiInicial
kappas <- data.frame(logiInicial = 0.5631)
accuracies <- data.frame(logiInicial = 0.8521)


#Vamos a elegir las variables con mas importancia del modelo anterior
#descartamos: native.country, fnlwgt, race, workclass:

set.seed(1234)
logi8Var<-glm(income ~ capital+education+occupation+hours.per.week+relationship+
                  age+gender+marital.status, data=data_train, family=binomial)
summary(logi8Var)
par(mar=c(7, 12, 4.1, 2.1))
importanciaVariablesLog(logi8Var) 


preds <- predict(logi8Var,newdata = data_test,type="response")
income_predicts$logi8Var <- as.factor(round(preds,0))

confMatrix_logi8Var <- confusionMatrix(data = income_predicts$logi8Var, reference = data_test$income, positive = "1")
confMatrix_logi8Var
kappas$logi8Var <- 0.5704
accuracies$logi8Var <- 0.8540

#Ha mejorado ligeramente y es mas simple por lo que de momento nos quedamos con estas
#variables, vamos a probar a eliminar marital.status y gender.

set.seed(1234)
logi5Var<-glm(income ~ capital+education+occupation+hours.per.week+relationship+
                  age, data=data_train, family=binomial,)
summary(logi5Var)
par(mar=c(7, 12, 4.1, 2.1))
importanciaVariablesLog(logi5Var) 


preds <- predict(logi5Var,newdata = data_test,type="response")
income_predicts$logi5Var <- as.factor(round(preds,0))

confMatrix_logi5Var <- confusionMatrix(data = income_predicts$logi5Var, reference = data_test$income)
confMatrix_logi5Var
kappas$logi5Var <- 0.5657
accuracies$logi5Var <- 0.8530

#Ha empeorado ligeramente y aunque sea infimo vamos a mantener estas dos variables ya que
#no añaden una gran complejida y pueden ser útiles en futuros modelos

predictors_names <- c("capital","education","occupation","hours.per.week","relationship",
                      "age","gender","marital.status")
#as.formula(paste("income~",paste(predictors_names,collapse = "+"),sep=""))

#Vamos a ajustar el punto de corte (threshold) para la decision para subir un poco la sensitivity.

#Referencia:
freq(adult$income)

#Buscamos el que maximiza el ?ndice de youden
test_roc<-roc(data_test$income, predict(logi8Var,data_test,type = "response"), direction="<")
plot(test_roc,print.thres="best") #best es el punto de corte maximiza youden #0.245

#busco el que iguala sensibilidad y especificidad
test_roc$thresholds[which.min(abs(test_roc$sensitivities-test_roc$specificities))] #0.2827701


#Comparamos las 3 opciones
sensEspCorte(logi8Var,data_test,"income",0.5,"1") #max. tasa de acierto
sensEspCorte(logi8Var,data_test,"income",0.245,"1") #max. ?ndice de Youden
sensEspCorte(logi8Var,data_test,"income",0.2827701,"1") #Igualar sens. y esp.

test_roc$auc
#0.9066 = AUC

#Elegimos el modelo logi8Var con puto de corte 0.2827701 ya que aunque bajemos ligeramente
#la accuracy igualamos la sensitivty y la specificty la cual estaba muy desbalanceada en
# usando 0.5 comoo punto de corte

mejorLogi <- logi8Var

set.seed(1234)
preds <- predict(mejorLogi,newdata = data_test,type="response")
income_predicts <- data.frame( mejorLogi = as.factor( if_else(preds<0.2827701,0,1) ) )

confMatrix_mejorLogi <- confusionMatrix(data = income_predicts$mejorLogi, reference = data_test$income)
confMatrix_mejorLogi

#Kappa improves to 0.5836
#Vamos a crear estos dataframe para comparar lo smodelos mas tarde

x <- setNames(c(0.5844, 0.8294, 0.907,abs( 0.8293-0.8296)), c("kappa","acc","AUC", "diffSensSpec"))

datosModelos <- data.frame(x)
colnames(datosModelos) <- "mejorLogi"
