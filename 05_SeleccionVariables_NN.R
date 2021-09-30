
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

load("./data/adult_limpio_scaled_dummies.Rda")

str(adult)



full<-glm(factor(income)~.,data=adult,family = binomial(link="logit")) 
null<-glm(factor(income)~1,data=adult,family = binomial(link="logit"))

seleccion<-stepAIC(null,scope=list(upper=full),direction="both")

# Para ver los efectos escogidos
dput(names(seleccion$coefficients))
length(dput(names(seleccion$coefficients)))
# Esto si se quiere en versión formula
formula(seleccion)


# APLICANDO steprepetidobinaria

source("./funcion steprepetido binaria.R")

listconti <- c("marital.status_Married", "capital", "education_Post.Graduate", 
"education_Bachelors", "hours.per.week", "education_Before.Highschool", 
"age", "occupation_Exec.managerial", "education_HS.grad", "occupation_Farming.fishing", 
"occupation_Other.service", "relationship_Own.child", "relationship_Wife", 
"gender_Male", "occupation_Prof.specialty", "occupation_Handlers.cleaners", 
"occupation_Tech.support", "marital.status_Never.married", "occupation_Machine.op.inspct", 
"relationship_Other.relative", "workclass_Self.emp", "occupation_Sales", 
"native.country_USA", "fnlwgt", "occupation_Protective.serv", 
"relationship_Unmarried", "relationship_Not.in.family", "race_Black", 
"occupation_Priv.house.serv", "education_Some.college", "occupation_Transport.moving"
)

vardep<-c("income")

data <- adult

listaAIC <-steprepetidobinaria(data=data,
                           vardep=vardep,listconti=listconti,sinicio=12345,
                           sfinal=12355,porcen=0.8,criterio="AIC")
tabla<-listaAIC[[1]]
tabla

listaBIC <-steprepetidobinaria(data=data,
                           vardep=vardep,listconti=listconti,sinicio=12345,
                           sfinal=12355,porcen=0.8,criterio="BIC")
tablaBIC<-listaBIC[[1]]
tablaBIC

tabla$modelo[1]
tablaBIC$modelo[1]

#Nos quedamos con el mejor mdoelo mediante AIC y el mejor mediante BIC para 
#explorarlos en el aprtado 6

predictor_names_AIC <- unlist(strsplit(tabla$modelo[1],"+",fixed=T))
predictor_names_AIC
#Vamos a escribirlo a mano para poder copiarlo en el siguiente script
dput(unlist(strsplit(tabla$modelo[1],"+",fixed=T)))
predictor_names_AIC <- c("marital.status_Married", "capital", "education_Post.Graduate", 
  "education_Bachelors", "hours.per.week", "education_Before.Highschool", 
  "age", "occupation_Exec.managerial", "education_HS.grad", "occupation_Farming.fishing", 
  "occupation_Other.service", "relationship_Own.child", "relationship_Wife", 
  "gender_Male", "occupation_Prof.specialty", "occupation_Tech.support", 
  "occupation_Handlers.cleaners", "marital.status_Never.married", 
  "occupation_Machine.op.inspct", "relationship_Other.relative", 
  "fnlwgt", "native.country_USA", "occupation_Sales", "workclass_Self.emp", 
  "relationship_Unmarried", "relationship_Not.in.family", "occupation_Protective.serv", 
  "occupation_Priv.house.serv", "race_Black", "occupation_Transport.moving", 
  "education_Some.college")

predictor_names_BIC <- unlist(strsplit(tablaBIC$modelo[1],"+",fixed=T))
predictor_names_BIC
#Vamos a escribirlo a mano para poder copiarlo en el siguiente script
dput(unlist(strsplit(tablaBIC$modelo[1],"+",fixed=T)))
predictor_names_BIC <- c("marital.status_Married", "capital", "education_Post.Graduate", 
  "education_Bachelors", "hours.per.week", "education_Before.Highschool", 
  "age", "occupation_Exec.managerial", "education_HS.grad", "occupation_Farming.fishing", 
  "occupation_Other.service", "relationship_Own.child", "relationship_Wife", 
  "gender_Male", "occupation_Prof.specialty", "occupation_Tech.support", 
  "occupation_Handlers.cleaners", "marital.status_Never.married", 
  "occupation_Machine.op.inspct", "relationship_Other.relative", 
  "fnlwgt", "native.country_USA", "occupation_Sales", "workclass_Self.emp", 
  "relationship_Unmarried", "relationship_Not.in.family", "occupation_Protective.serv"
)
