
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
datosModelos$mejorEnsamblado <- c(0.5319,0.8380,0.8937,abs(0.5769-0.9222))

load("./data/adult_limpio.Rda")


str(adult)

predictors_names <- c("capital","education","occupation","hours.per.week","relationship",
                      "age","gender","marital.status")

#Recodificamos No como "<=50K" y Yes como ">50K" para evitar errores en la funcion dada
adult %<>% mutate(income = as.factor(if_else(income=="<=50K", "No", "Yes")))


set.seed(120)
sample <- sample.int(n = nrow(adult),
                     size = floor(0.85*nrow(adult)), replace = F)

train <- adult[sample, ]
test  <- adult[-sample, ]





############## entrenamos ##################

#Vamos a utilizar un random Forest, una logistica y un xgb
#Los parametros de cada uno van a ser los ayados para cada uno de los mejores modelos
#encontrados en su respectivos apartados.
#Como variables predictoras usaremos las hayadas en regresion logistica


nodesize<-20
mtry<-7
ntree<-100
replace<-TRUE
sampsize<-15*1000

rf<-randomForest(data=train,
                 as.formula(paste("income~",paste(predictors_names,collapse = "+"),sep="")),
                 mtry=mtry,
                 ntree=ntree,
                 replace=replace,
                 sampsize = sampsize)



logi<-glm(data=train,
          as.formula(paste("income~",paste(predictors_names,collapse = "+"),sep="")),
          family=binomial(link = "logit"))



xgbmgrid<-expand.grid(
    min_child_weight=20,
    eta=0.1,
    nrounds=300,
    max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "none") 

xgbm<- train(data=train,
             as.formula(paste("income~",paste(predictors_names,collapse = "+"),sep="")),
             method="xgbTree",
             tuneGrid= xgbmgrid,
             trControl = control,
             verbose=FALSE)



######### prediccion sobre el test

### para medir el overall 

preditestrf<-predict(rf,newdata=train,type = "prob")
preditestglm<-predict(logi,newdata=train,type = "response")
preditestxgbm<-predict(xgbm,newdata=train, type = "prob")

# Esto por si acaso
preditestrf<-as.data.frame(preditestrf)
preditestglm<-as.data.frame(preditestglm)
preditestxgbm<-as.data.frame(preditestxgbm)


union_train <- cbind.data.frame(income=train$income, 
                                RF_Yes=preditestrf$Yes,RF_No=preditestrf$No,
                                preditestglm, 
                                xgbm_Yes=preditestxgbm$Yes, 
                                xgbm_No=preditestxgbm$No )



### para medir el overall 

preditestrf<-predict(rf,newdata=test,type = "prob")
preditestglm<-predict(logi,newdata=test,type = "response")
preditestxgbm<-predict(xgbm,newdata=test, type = "prob")

# Esto por si acaso
preditestrf<-as.data.frame(preditestrf)
preditestglm<-as.data.frame(preditestglm)
preditestxgbm<-as.data.frame(preditestxgbm)

#### acuracy indivudual
estima <- as.data.frame(as.numeric(preditestglm>0.5))
comp <- cbind(estima,test$income) 

M <- table(comp) ### MATRIZ DE CONFUSION ####

overall <- (M[1,1]+M[2,2])/(M[1,1]+M[2,2]+M[1,2]+M[2,1])
###########################################


union_test <- cbind.data.frame(income=test$income, 
                                RF_Yes=preditestrf$Yes,RF_No=preditestrf$No,
                                preditestglm, 
                                xgbm_Yes=preditestxgbm$Yes, 
                                xgbm_No=preditestxgbm$No )


########################## STACKING ~~ 



# Classification Tree with rpart


# grow tree
fit <- rpart(as.factor(income) ~ RF_Yes + preditestglm + xgbm_Yes,
             method="class", data=union_train)

################################3
fit2<-glm(data=union_train,
          factor(income)~RF_Yes + preditestglm + xgbm_Yes,
          family=binomial(link = "logit"))

predi <-  predict(fit2,newdata=union_test,type = "response")


estima <- as.data.frame(as.numeric(predi>0.5))
comp <- cbind(estima,test$income) 

M <- table(comp) ### MATRIZ DE CONFUSION ####

overall <- (M[1,1]+M[2,2])/(M[1,1]+M[2,2]+M[1,2]+M[2,1])
overall
#################################3333

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE,
     main="Classification Tree for Union")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


predi_stacking<-predict(fit,newdata=union_test)

predi_stacking <- as.data.frame(predi_stacking)

estima <- predi_stacking$Yes

comp <- cbind.data.frame(estima,test$income) 

M <- table(comp) ### MATRIZ DE CONFUSION ####

overall <- (M[1,1]+M[2,2])/(M[1,1]+M[2,2]+M[1,2]+M[2,1])
overall


#train$chd <- as.numeric(as.factor(train$chd))
#train$chd <- train$chd -1
estima <- as.data.frame(as.numeric(preditestglm>0.5))
comp <- cbind(estima,train$chd) 
library(dplyr)
M <- table(comp) ### MATRIZ DE CONFUSION ####

overall <- (M[1,1]+M[2,2])/(M[1,1]+M[2,2]+M[1,2]+M[2,1])



#Probamos otro arbol:
fit3 <- rpart(as.factor(income) ~ RF_Yes + preditestglm + xgbm_Yes,
             method="class", data=union_train,
             control=rpart.control(minsplit=2, minbucket=1, cp=0.001))


printcp(fit3) # display the results
plotcp(fit3) # visualize cross-validation results
summary(fit3) # detailed summary of splits

# plot tree
plot(fit3, uniform=TRUE,
     main="Classification Tree for Union")
text(fit3, use.n=TRUE, all=TRUE, cex=.8)


predi_stacking<-predict(fit3,newdata=union_test)

predi_stacking <- as.data.frame(predi_stacking)

estima <- predi_stacking$Yes

comp <- cbind.data.frame(estima,test$income) 

M <- table(comp) ### MATRIZ DE CONFUSION ####

overall <- (M[1,1]+M[2,2])/(M[1,1]+M[2,2]+M[1,2]+M[2,1])
overall
#0.8776

#train$chd <- as.numeric(as.factor(train$chd))
#train$chd <- train$chd -1
estima <- as.data.frame(as.numeric(preditestglm>0.5))
comp <- cbind(estima,test$income) 
library(dplyr)
M <- table(comp) ### MATRIZ DE CONFUSION ####

overall <- (M[1,1]+M[2,2])/(M[1,1]+M[2,2]+M[1,2]+M[2,1])
overall #0.83

#vamos a probar 

#Vemos como tocando los parametros el arbol mejora considerablemente su funcionamiento.
#Vamos a hacer un pequeño grid search

#VAmos a probar distintos valores de min.bucket usando un for ya que no se puede meter en el grid
# con cp fijado a 0.001


#Unimos Train y test

union_train_test <- rbind(union_train,union_test)


min.buckets <- seq(from=1, to=50, by=2)

accs <- c()
kpps <- c()
aucs <- c()

control<-trainControl(method = "cv",number=4,
                      classProbs=TRUE,savePredictions = "all")


for (minbu in seq(from=1, to=50, by=2)){
    
    cat("\n")
    print(paste("MIN.BUCKET = ", minbu ,sep=""))
    cat("\n")
    
    arbolgrid <-  expand.grid(cp=c(0))
    
    set.seed(12345)
    arbolcaret<- train(factor(income) ~ RF_Yes + preditestglm + xgbm_Yes,
                       data=union_train_test,
                       method="rpart",
                       trControl=control,
                       tuneGrid=arbolgrid,
                       control = rpart.control(minbucket = minbu))
    
    
    sal<-arbolcaret$pred
    
    salconfu<-confusionMatrix(sal$pred,sal$obs)
    print(salconfu)
    accs <- c(accs, salconfu$overall["Accuracy"])
    kpps <- c(kpps, salconfu$overall["Kappa"])
    
    curvaroc <- roc(response=sal$obs,predictor=sal$Yes)
    auc <- curvaroc$auc
    print(auc)
    aucs <- c(aucs,auc)
    
    
}


metrics <- as.data.frame(cbind(min.buckets,accs,kpps,aucs))
rownames(metrics) <- NULL

g1 <- ggplot(data=metrics,aes(min.buckets,accs)) + geom_line(col="red") + geom_point() +scale_x_continuous(breaks=seq(1,50,2))
g2 <- ggplot(data=metrics,aes(min.buckets,kpps)) + geom_line(col="blue") + geom_point() +scale_x_continuous(breaks=seq(1,50,2))
g3 <- ggplot(data=metrics,aes(min.buckets,aucs)) + geom_line(col="green") + geom_point() +scale_x_continuous(breaks=seq(1,50,2))
ggarrange(g1,g2,g3,labels = c("accuracies","kappas","AUCs"),nrow=3,ncol=1)



#Vemos como subiendo el mi bucket a 10 sube el accuracy, probamos distintos cps con el 
#minbucket fijado y sera nuestro metodo final

control<-trainControl(method = "cv",number=4,
                      classProbs=TRUE,savePredictions = "all")

arbolgrid <-  expand.grid(cp=c(0,0.1,0.05,0.001))

mejorStacking <- train(factor(income) ~ RF_Yes + preditestglm + xgbm_Yes,
                      data=union_train_test,
                      method="rpart",
                      trControl=control,
                      tuneGrid=arbolgrid,
                      control = rpart.control(minbucket = 35))

mejorStacking

library(rattle)
fancyRpartPlot(mejorStacking$finalModel) #Dibujamos el arbol

sal<-mejorStacking$pred

salconfu<-confusionMatrix(sal$pred,sal$obs)
salconfu

# Accuracy = 0.9113 
# Kappa = 0.7563 
# Sens = 0.9419 , spec = 0.8140 (corregido el desbalanceo)
# 195368


curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
auc<-curvaroc$auc
auc
#auc = 0.9223
plot(curvaroc)

#Añadimos al dataframe con los datos del modeleos los nuevos resultados

datosModelos$mejorStacking<- c(0.7563,0.9113,0.9223,abs(0.9419-0.8140))
datosModelos







