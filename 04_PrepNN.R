

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
library(recipes)
library(fastDummies)


#Se va a repetir aqui el codigo de 00_exploracion_limpieza que se realizo previo 
#a todos los anteriores apartados y se añadirá alguna cosa más



#Carga de los datos -----------------------------------------------------------

adult <- read.csv("data/adult.csv")

nrow(adult)
str(adult)
head(adult)

#Vamos a eliminar la variable education.num ya que contiene la misma informacion que education pero en valores de int
adult$educational.num <- NULL
#Vamos a crear una variable capital que relacion capital gain y loss eliminando asi estas dfos
adult <- adult %>% mutate(capital = as.integer(capital.gain - capital.loss))
mode(adult$capital)
adult$capital.gain <- NULL
adult$capital.loss <- NULL


#Missing en factores ---------------------------------------------------------

#Fuente: https://rpubs.com/juliaHuynh/datapreprocess-assignment3

#Convertimos los character a factor
adult <- adult %>% mutate_if(is.character, as.factor)

#Vamos a analizar los niveles de los factores
fac_cols <- sapply(adult, is.factor)
lapply(adult[, fac_cols], levels)

#Workclas missing level ?
#Ocuppation missing level ?
#native_country missing level ?

adult$workclass[which(adult$workclass=="?")] <- NA
adult$occupation[which(adult$occupation=="?")] <- NA
adult$native.country[which(adult$native.country=="?")] <- NA
#Drop unused levels:
adult <- droplevels(adult)


#Recategorizacion de factores ------------------------------------------------

#vamos a echarle un vistazo a las frecuenciuas de las variables categoricas
apply(adult%>%select_if(is.factor),2,freq)

#Recategorizamos la variable workclass en Gov, Slef-emp, Private, Other
adult <- adult %>% 
    mutate(workclass = 
               ifelse(grepl(".gov$", str_trim(workclass)), "Gov", 
                      ifelse(grepl("^Self.",str_trim(workclass)),"Self-emp",
                             ifelse(grepl("^Private$", str_trim(workclass)),"Private", 
                                    ifelse(is.na(workclass),NA,"Other"
                                    )))))
adult$workclass <- as.factor(adult$workclass)
levels(adult$workclass)


#Recategorizamos la variable education en: Before-Highschool, Associate, Post-graduate, HS-grad, Some-college and Bachelors
adult <- adult %>% mutate(education = ifelse(grepl(".th$|^Preschool$", (education)), "Before-Highschool",
                                             ifelse(grepl("^Assoc.", (education)),"Associate",
                                                    ifelse(grepl("^Masters$|^Doctorate$|^Pro.",(education)), "Post-Graduate", 
                                                           as.character((education))))))
adult$education <- as.factor(adult$education)
levels(adult$education)

#Recatagprizamos marital.status
adult <- adult %>% mutate(marital.status = ifelse(grepl("^Married.", marital.status), "Married", as.character(marital.status)))
adult$marital.status <- as.factor(adult$marital.status)
levels(adult$marital.status)

#Recategorizamos country
adult<- adult %>% 
    mutate(native.country = 
               ifelse(grepl("United.",native.country), "USA",
                      ifelse(is.na(workclass),NA,"Non-USA")))
adult$native.country <- as.factor(adult$native.country)
levels(adult$native.country)


#Revisamos otra vez las categorias
apply(adult%>%select_if(is.factor),2,freq)
#Conforme con la informacion que contien o pueden aportar las categorias ahora.





#Missing o outliers en continuas ----------------------------------------------

#fnlwgt significa Final Weigth y es un valor dado por el censo de USA que estima
#el numero de personas que cad aentrada puede representar por lo que vamos a dejarlo com está

summary(adult%>%select_if(is.integer))

# Histograms for numeric columns
x <- inspect_num(adult)
show_plot(x)


# Using z-score to check outliers

#HOURS.PER.WEEK
# Draw Boxplot chart of this variable
boxplot(x = adult$hours.per.week, main = "Box Plot of Hours Per Week", ylab= "Hours per week")
z.scores.hours<- adult$hours.per.week %>% scores(type="z")
z.scores.hours %>% summary()
length(which(abs(z.scores.hours)>3))#681 outliers en hours.per.week
adult$hours.per.week[which(abs(z.scores.hours)>3)] <- NA
# Using boxplot to see changes after imputing outliers
boxplot(x = adult$hours.per.week, main = "Box Plot of Hours Per Week After Imputing Outliers", ylab= "Hours per week")


#CAPITAL
# Draw Boxplot chart of this variable
boxplot(x = adult$capital, main = "Box Plot of Capital")
table(adult$capital)
#Los 9999 son sospechosos de ser missing ya que los anteriores valores son de 41310 y ya son muy pocos.
adult$capital[which(adult$capital==99999)] <- NA
boxplot(x = adult$capital, main = "Box Plot of Capital")
#Los demas valores vamos a dejarlos ya que son valores razonbales y es normal que no sean muy comunes para el capital de los ciudadanos


#Tratamiento de missings -----------------------------------------------------

adult <- missRanger(adult,pmm.k = 5, num.trees = 100)



#Exploracion de los datos tras limpieza----------------------------------------


# Horizontal bar plot for categorical column composition
x <- inspect_cat(adult) 
show_plot(x)

# Correlation betwee numeric columns + confidence intervals
x <- inspect_cor(adult)
show_plot(x)

# Bar plot of most frequent category for each categorical column
x <- inspect_imb(adult)
show_plot(x)

# Bar plot showing memory usage for each column
x <- inspect_mem(adult)
show_plot(x)

# Occurence of NAs in each column ranked in descending order
x <- inspect_na(adult)
show_plot(x)

# Histograms for numeric columns
x <- inspect_num(adult)
show_plot(x)

# Barplot of column types
x <- inspect_types(adult)
show_plot(x)

#Puede ser que la variable objetivo income este desbalanceada (75%-25%) pero al haber
#muchas muestras vamos a probar como lo tenemos

str(adult)

#Recodificamos No como "<=50K" y Yes como ">50K" para evitar errores en la funcion dada
adult %<>% mutate(income = as.factor(if_else(income=="<=50K", "No", "Yes")))

# Es bueno crear listas de variables continuas, categóricas 
# y la dependiente en un vector

listconti <- c("age","capital","hours.per.week","fnlwgt")

listclass <- c("workclass", "education", "marital.status", "occupation", "relationship", 
               "race", "gender", "native.country")

vardep<-c("income")

income<-adult[,vardep]

# ESTANDARIZACIÓN DE TODAS LAS VARIABLES CONTINUAS

means <-apply(adult[,listconti],2,mean,na.rm=TRUE)
sds<-sapply(adult[,listconti],sd,na.rm=TRUE)

adult_conti<-scale(adult[,listconti], center = means, scale = sds)


#CATEGORICAS A DUMMIES
adult_dummies<- dummy_cols(adult[,listclass], 
                           select_columns = listclass,
                           remove_selected_columns = TRUE,
                           remove_first_dummy = TRUE,)
names(adult_dummies) #Podemos ver que solo cogemos k-1 factores para las dummies


#Juntamos todas las variables
adult_bien<-data.frame(cbind(adult_conti,adult_dummies,income))

names(adult_bien)
str(adult_bien)

#Ya tenemos el dataset con las variables continuas estandarizadas y las categoricas
#pasadas a dummies (excepto income que es la variable objetivo)
#
#LO GUARDAMOS

adult <- adult_bien #Para que se llame adult al cargar
save(adult,file="./data/adult_limpio_scaled_dummies.Rda")









