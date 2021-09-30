
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



#Mejor Bagging Tree ----------------------------------------------

#Parámetros a tunear en caret
#   mtry =8 BAGGINBG. bagging=nº total de variables independientes (8)
#   ntree = m
#   nodesize: tamaño max nodos finales. (complejidad)
#   sampsize = tamaño de cada muestra de bagging
#   replace = TRUE (como tenemos un dataset grande no cambiara mucho de TRUE a FALSE)
#   fijar semillla para reproducir en caret

#Recodificamos No como "<=50K" y Yes como ">50K" para evitar errores en la funcion dada
adult %<>% mutate(income = as.factor(if_else(income=="<=50K", "No", "Yes")))

#Vamos a realizar un primer arbol con la libreria randomForest para ver la evolucion del error
#al subir

set.seed(12345)

rfbis<-randomForest(as.formula(paste("income~",paste(predictors_names,collapse = "+"),sep="")),
                    data=adult,
                    mtry=length(predictors_names),
                    ntree=300,
                    sampsize=300,
                    nodesize=10,
                    replace=TRUE) 

plot(rfbis$err.rate[,1])

#Elegimos m=100=ntree ya que el error ya esta muy estable


# Ahora con ntree fijado vamos a ver cual es el numero mas adecuado para sampsize usando repeated CV
# siendo el maximo para un CV con 4 folds: (3/4)*nº obs = (3/4)*48842 = 36631.5 lo cual corresponde
# al modelo base si no se indixa el numero de sampsize.
# Vamos a probar 10K,15K,20K,25K,30K

medias1<-cruzadarfbin(data=adult,
                      vardep=c("income"),
                      listconti=c("capital", "hours.per.week","age"),
                      listclass=c("education","occupation","relationship","gender","marital.status"),
                      grupos=4,
                      sinicio=1234,
                      repe=10,
                      nodesize=10,
                      mtry=length(predictors_names),
                      ntree=100,
                      replace=TRUE,
                      sampsize=10*1000)

medias1$modelo="bagging10Ksampsize"


medias2<-cruzadarfbin(data=adult,
                      vardep=c("income"),
                      listconti=c("capital", "hours.per.week","age"),
                      listclass=c("education","occupation","relationship","gender","marital.status"),
                      grupos=4,
                      sinicio=1234,
                      repe=10,
                      nodesize=10,
                      mtry=length(predictors_names),
                      ntree=100,
                      replace=TRUE,
                      sampsize=15*1000)

medias2$modelo="bagging15Ksampsize"


medias3<-cruzadarfbin(data=adult,
                      vardep=c("income"),
                      listconti=c("capital", "hours.per.week","age"),
                      listclass=c("education","occupation","relationship","gender","marital.status"),
                      grupos=4,
                      sinicio=1234,
                      repe=10,
                      nodesize=10,
                      mtry=length(predictors_names),
                      ntree=100,
                      replace=TRUE,
                      sampsize=20*1000)

medias3$modelo="bagging20Ksampsize"


medias4<-cruzadarfbin(data=adult,
                      vardep=c("income"),
                      listconti=c("capital", "hours.per.week","age"),
                      listclass=c("education","occupation","relationship","gender","marital.status"),
                      grupos=4,
                      sinicio=1234,
                      repe=10,
                      nodesize=10,
                      mtry=length(predictors_names),
                      ntree=100,
                      replace=TRUE,
                      sampsize=25*1000)

medias4$modelo="bagging25Ksampsize"

medias5<-cruzadarfbin(data=adult,
                      vardep=c("income"),
                      listconti=c("capital", "hours.per.week","age"),
                      listclass=c("education","occupation","relationship","gender","marital.status"),
                      grupos=4,
                      sinicio=1234,
                      repe=10,
                      nodesize=10,
                      mtry=length(predictors_names),
                      ntree=100,
                      replace=TRUE,
                      sampsize=30*1000)

medias5$modelo="bagging30Ksampsize"


medias6<-cruzadarfbin(data=adult,
                      vardep=c("income"),
                      listconti=c("capital", "hours.per.week","age"),
                      listclass=c("education","occupation","relationship","gender","marital.status"),
                      grupos=4,
                      sinicio=1234,
                      repe=10,
                      nodesize=10,
                      mtry=length(predictors_names),
                      ntree=100,
                      replace=TRUE,
                      #sampsize=30*1000
                      )

medias6$modelo="baggingBASEsampsize"

union1<-rbind(medias1,medias2,medias3,medias4,medias5,
              medias6)

par(mar = c(11, 5, 2, 2), cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="pink")

uni<-union1
uni$modelo <- with(uni,
                   reorder(modelo,tasa, mean))
par(mar = c(11, 5, 2, 2), cex.axis=0.8, las=2)
uni$modelo
boxplot(data=uni,tasa~modelo,col="pink")

#No hay una diferencia notable pero vamos a elegir el de 15K debido a que su varianza y su
#maximo no son muy grandes

#SAMPSIZE = 15K
#mtry=8=len(predictor_names)
#ntree=100

#Vamos a tunear nodesize a ver si notamos difernecia notable
#posibles nodesize= 5,10 (medias2),15,20,30,40

medias7<-cruzadarfbin(data=adult,
                      vardep=c("income"),
                      listconti=c("capital", "hours.per.week","age"),
                      listclass=c("education","occupation","relationship","gender","marital.status"),
                      grupos=4,
                      sinicio=1234,
                      repe=10,
                      nodesize=5,
                      mtry=length(predictors_names),
                      ntree=100,
                      replace=TRUE,
                      sampsize=15*1000)

medias7$modelo="bagging_15Kss_5nsize"



medias2$modelo="bagging_15Kss_10nsize"



medias8<-cruzadarfbin(data=adult,
                      vardep=c("income"),
                      listconti=c("capital", "hours.per.week","age"),
                      listclass=c("education","occupation","relationship","gender","marital.status"),
                      grupos=4,
                      sinicio=1234,
                      repe=10,
                      nodesize=15,
                      mtry=length(predictors_names),
                      ntree=100,
                      replace=TRUE,
                      sampsize=15*1000)

medias8$modelo="bagging_15Kss_15nsize"


medias9<-cruzadarfbin(data=adult,
                      vardep=c("income"),
                      listconti=c("capital", "hours.per.week","age"),
                      listclass=c("education","occupation","relationship","gender","marital.status"),
                      grupos=4,
                      sinicio=1234,
                      repe=10,
                      nodesize=20,
                      mtry=length(predictors_names),
                      ntree=100,
                      replace=TRUE,
                      sampsize=15*1000)

medias9$modelo="bagging_15Kss_20nsize"


medias10<-cruzadarfbin(data=adult,
                      vardep=c("income"),
                      listconti=c("capital", "hours.per.week","age"),
                      listclass=c("education","occupation","relationship","gender","marital.status"),
                      grupos=4,
                      sinicio=1234,
                      repe=10,
                      nodesize=30,
                      mtry=length(predictors_names),
                      ntree=100,
                      replace=TRUE,
                      sampsize=15*1000)

medias10$modelo="bagging_15Kss_30nsize"


medias11<-cruzadarfbin(data=adult,
                      vardep=c("income"),
                      listconti=c("capital", "hours.per.week","age"),
                      listclass=c("education","occupation","relationship","gender","marital.status"),
                      grupos=4,
                      sinicio=1234,
                      repe=10,
                      nodesize=40,
                      mtry=length(predictors_names),
                      ntree=100,
                      replace=TRUE,
                      sampsize=15*1000)

medias11$modelo="bagging_15Kss_40nsize"

union1<-rbind(medias7,medias2,medias8,medias9,medias10,
              medias11)

par(mar = c(11, 5, 2, 2), cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="pink")

uni<-union1
uni$modelo <- with(uni,
                   reorder(modelo,tasa, mean))
par(mar = c(11, 5, 2, 2), cex.axis=0.8, las=2)
uni$modelo
boxplot(data=uni,tasa~modelo,col="pink")

#Elegimos el de nodesize=20 ya que claramente es el mejor aunque no sea mucha la diferencia

#Prepramos el modelo final

data<-adult
vardep<-c("income")
listconti<-c("capital", "hours.per.week","age")
listclass<-c("education","occupation","relationship","gender","marital.status")
grupos<-4
sinicio<-1234
nodesize<-20
mtry<-length(predictors_names)
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

mejorBagging<- train(formu,data=databis,
           method="rf",
           trControl=control,
           tuneGrid=rfgrid,
           nodesize=nodesize,
           replace=replace,
           sampsize=sampsize,
           ntree=ntree)

sal<-mejorBagging$pred

salconfu<-confusionMatrix(sal$pred,sal$obs)
salconfu
# Accuracy = 0.8596
# Kappa = 0.5852
# Sens = 0.60, spec = 0.9401 (un poco desbalanceado)


curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
auc<-curvaroc$auc
auc
#auc = 0.895

#Añadimos al dataframe con los datos del modeleos los nuevos resultados

datosModelos$mejorBagging<- c(0.5852,0.8596,0.895,abs(0.6038-0.9401))
datosModelos






