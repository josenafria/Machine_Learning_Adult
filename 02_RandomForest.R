
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

source("./cruzada rf binaria.R")

predictors_names <- c("capital","education","occupation","hours.per.week","relationship",
                      "age","gender","marital.status")


x <- setNames(c(0.5844, 0.8294, 0.907, abs(0.8293-0.8296)), c("kappa","acc","AUC", "diffSensSpec"))
datosModelos <- data.frame(x)
colnames(datosModelos) <- "mejorLogi"
datosModelos$mejoArbol <- c(0.57,0.8534,0.8948,abs(0.6009-0.9328))
datosModelos$mejorBagging<- c(0.5852,0.8596,0.895,abs(0.6038-0.9401))



#MEJOR RANDOM FOREST------------------------------------
#
#COmo ya encontramos los mejores hiperparametro para el bagging t lo unico que se diferencia del
#RF es que mtry es = 8, solo nos queda tunear mtry para ver como cambia (1,3,5,7)
#manteniendo los mismo hiperaprametros elegidos en 02_bagging.r

#Recodificamos No como "<=50K" y Yes como ">50K" para evitar errores en la funcion dada
adult %<>% mutate(income = as.factor(if_else(income=="<=50K", "No", "Yes")))


medias1<-cruzadarfbin(data=adult,
                      vardep=c("income"),
                      listconti=c("capital", "hours.per.week","age"),
                      listclass=c("education","occupation","relationship","gender","marital.status"),
                      grupos=4,
                      sinicio=1234,
                      repe=10,
                      nodesize=20,
                      mtry=1,
                      ntree=100,
                      replace=TRUE,
                      sampsize=15*1000)

medias1$modelo="bagging_1mtry"


medias2<-cruzadarfbin(data=adult,
                      vardep=c("income"),
                      listconti=c("capital", "hours.per.week","age"),
                      listclass=c("education","occupation","relationship","gender","marital.status"),
                      grupos=4,
                      sinicio=1234,
                      repe=10,
                      nodesize=20,
                      mtry=3,
                      ntree=100,
                      replace=TRUE,
                      sampsize=15*1000)

medias2$modelo="bagging_3mtry"


medias3 <-cruzadarfbin(data=adult,
                      vardep=c("income"),
                      listconti=c("capital", "hours.per.week","age"),
                      listclass=c("education","occupation","relationship","gender","marital.status"),
                      grupos=4,
                      sinicio=1234,
                      repe=10,
                      nodesize=20,
                      mtry=5,
                      ntree=100,
                      replace=TRUE,
                      sampsize=15*1000)

medias3$modelo="bagging_5mtry"



medias4 <-cruzadarfbin(data=adult,
                      vardep=c("income"),
                      listconti=c("capital", "hours.per.week","age"),
                      listclass=c("education","occupation","relationship","gender","marital.status"),
                      grupos=4,
                      sinicio=1234,
                      repe=10,
                      nodesize=20,
                      mtry=7,
                      ntree=100,
                      replace=TRUE,
                      sampsize=15*1000)

medias4$modelo="bagging_7mtry"


union1<-rbind(medias1,medias2,medias3,medias4)

par(mar = c(9, 5, 2, 2), cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="pink")

uni<-union1
uni$modelo <- with(uni,
                   reorder(modelo,tasa, mean))
par(mar = c(11, 5, 2, 2), cex.axis=0.8, las=2)
uni$modelo
boxplot(data=uni,tasa~modelo,col="pink")

#Vamos a elegir mtry=7 ya que es el que mejor resultados en cuanto a tasa dde fallos da

#Preparamos el modelo final

data <-adult
vardep<-c("income")
listconti<-c("capital", "hours.per.week","age")
listclass<-c("education","occupation","relationship","gender","marital.status")
grupos<-4
sinicio<-1234
nodesize<-20
mtry<-7
ntree<-100
replace<-TRUE
sampsize<-15*1000


# pasar las categóricas a dummies

if  (listclass!=c(""))
{
    databis<-data[,c(vardep,listconti,listclass)]
    databis<- dummy.data.frame(databis, listclass, sep = ".")
}  else   {
    databis<-data[,c(vardep,listconti)]
}

# c)estandarizar las variables continuas

# Calculo medias y dtipica de datos y estandarizo (solo las continuas)

means <-apply(databis[,listconti],2,mean)
sds<-sapply(databis[,listconti],sd)

# Estandarizo solo las continuas y uno con las categoricas

datacon<-scale(databis[,listconti], center = means, scale = sds)
numerocont<-which(colnames(databis)%in%listconti)
databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])

databis[,vardep]<-as.factor(databis[,vardep])

formu<-formula(paste("factor(",vardep,")~.",sep=""))

# Preparo caret   

set.seed(sinicio)
control<-trainControl(method = "cv",number=grupos,
                      savePredictions = "all",classProbs=TRUE) 

# Aplico caret y construyo modelo

rfgrid <-expand.grid(mtry=mtry)

mejorRF<- train(formu,data=databis,
                     method="rf",
                     trControl=control,
                     tuneGrid=rfgrid,
                     nodesize=nodesize,
                     replace=replace,
                     sampsize=sampsize,
                     ntree=ntree)

sal<-mejorRF$pred

salconfu<-confusionMatrix(sal$pred,sal$obs)
salconfu

# Accuracy = 0.8598
# Kappa = 0.5857
# Sens = 0.6041, spec = 0.9403 (un poco desbalanceado)


curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
auc<-curvaroc$auc
auc
#auc = 0.8912

#Añadimos al dataframe con los datos del modeleos los nuevos resultados

datosModelos$mejorRF<- c(0.5857,0.8598,0.8912,abs(0.6041-0.9403))
datosModelos









