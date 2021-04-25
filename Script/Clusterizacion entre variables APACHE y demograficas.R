
# Importar librerias generales --------------------------------------------

library(openxlsx) # Importar y exportar a excel
library(tidyverse) # ManipulaciÃ³n de datos
library(magrittr) # Pipe %>%, %$%
library(ggplot2) # GrÃ¡ficos estÃ¡ticos
library(DataExplorer)
library(tibble)
library(dplyr)
library(mlogit)# para regresion loggistica
library(tidyr)
library(psych)
library(pROC) #para curva roc
library(car) #para ver grafico de residuos
library(lmtest) # para ver residuos, esta la preuba de bptest
library(DescTools) #para calcular psudo r2

library(stringr) # Manipulacion de texto
library(cowplot) # graficos ggplot
library(plotly) # graficos js
library(corrplot) # Grafico Correlacion
library(skimr) # Descriptivas
library(tidymodels) # Facilita modelamiento - new way
library(glmnet) # Regularizacion L2/L1/elastic net models\
library(glmnetUtils)
library(earth) # MARS
library(discrim) # Naive Bayes desde tidymodels
library(klaR) # Naive Bayes
library(naivebayes) # Naive Bayes
library(vip) # for variable importance plots
library(doParallel) # Computacion en paralelo
library(themis) #para balancear datos con up
library(tune)
library(yardstick)


# EXTRACCION DE BASE DE DATOS -----------------------------------------------------

# Generar ambiente ----

setwd("C:/Users/DELL/OneDrive/Escritorio/clasesShinySEE/datathon/Datos")
getwd()



# Bajar base de datos----
training_v2 <- read.csv(file = "training_v2.csv")
unlabeled <- read.csv(file = "unlabeled.csv")


# TRANSFORMAR BASE DE DATOS  -----------------------------------------------
#unificar las bases de datos en una unica
data_cluster <- training_v2
attach(data_cluster) #para no tener que siempre estar poniendo data antes de las variables


# LIMPIEZA DE BASE DE DATOS --------------------------------------------

#Quitan varibles identificadores, que no tienen datos, o todos los datos son iguales
data_cluster$encounter_id <- NULL #se retira porque es un identificador
data_cluster$patient_id <- NULL #se retira porque es un identificador
data_cluster$icu_id <- NULL #se retira porque es un identificador
data_cluster$readmission_status <- NULL # se retira porque todos son 0
data_cluster$icu_admit_type <- NULL # se retira porque no tiene elementos
data_cluster$gcs_unable_apache <- NULL # todas son cero

#variables demograficas que no desea quitarse
data_cluster$ethnicity  # No se usa porque parece que esta variable no influye, hispanos son 3.600 y nativos 770, son los que tienen mayor prevalencia de mortalidad pero es muy baja
data_cluster$height  # se retira porque parece ser que no influye 
data_cluster$weight  # se retira porque su influencia parece ya estar recogida en BMI

#transformar variales a numeros todos positivos y sin desimales (esta variable parece fue mal recolectada)
data_cluster$pre_icu_los_days  <-   abs(data_cluster$pre_icu_los_days) #se transforman los numeros negativos en absolutos porque asumo error al digitar y en algunos centros lo reportaron como negativo
data_cluster$pre_icu_los_days <- round(data_cluster$pre_icu_los_days, digits = 0) #se le quita los decimales al los dias de hospitalizacion

#transformar espacios en blanco a NA
data_cluster[data_cluster$gender == "",] <- NA #transformar los espacios vacios en NA


#se quitan las variables que no son del apache ni demograficas
data_cluster[42:182] <- NULL #se retiran variables repetidad a las que estan cofificadas como apache



# #POR TIEMPO SE COMENTA ESTOS ANALISIS
# # Creacion de reporte ----
# #create_report(data_cluster) # se identifican variables que tienen mucha missing data

#Analalisis de los pacientes que fallecieron
#se escoen solo los que fallecieron
data_cluster_fallecio <- data_cluster[(data_cluster2$hospital_death == 1),]

# #POR TIEMPO SE COMENTA ESTOS ANALISIS
# #Creacion de reporte de los que fallecieron ----
# #create_report(data_cluster_fallecio) # se identifican variables que tienen mucha missing data

# #NOTA:
# #tanto en los que fallecieron como los que no fallecieron presentaron missing data de mas del 50%
        # albumin_apache, bilirubin_apache, urineoutput_apache
# #y presentaron un missing data por encima del 70% los resultados de gasometria
        #fio2_apache, paco2_apache, paco2_for_ph_apache, pao2_apache, ph_apache

#se procede a quitra estas variables con mas de un 50% de missing data segun el reporte
data_cluster$albumin_apache <- NULL
data_cluster$bilirubin_apache <- NULL
data_cluster$urineoutput_apache <- NULL
#los siguientes tienen mas de un 75% de datos missing (las gasometrias), segun el reporte
data_cluster$fio2_apache <- NULL
data_cluster$paco2_apache <- NULL
data_cluster$paco2_for_ph_apache <- NULL
data_cluster$pao2_apache <- NULL
data_cluster$ph_apache <- NULL


# MODELAMIENTO DE VARIABLES DEMOGRAFICAS Y APACHE -------------------------
#transformar las variables categoricas
data_modelo <- data_cluster
data_modelo$hospital_id <- as.character(data_modelo$hospital_id)
data_modelo$apache_2_diagnosis <- NULL
data_modelo$apache_3j_diagnosis <- NULL
data_modelo$hospital_death <- factor(data_modelo$hospital_death,
                                   labels = c("Sobrevivió", 
                                              "Falleció"))

create_report(data_modelo)
str(data_modelo)

# Se crea el recipiene y se codifica Preprosezamiento con tidymodel
rct_dataModelo <- data_modelo %>% recipe(hospital_death ~ . ) %>%
  step_normalize( all_numeric(), -all_outcomes()) %>% # Normalizacion
  step_other(all_nominal(), -all_outcomes() ) %>% #tienen menos de un 5%
  step_dummy(all_nominal(), -all_outcomes() ) %>% # Dummy
  step_nzv(all_predictors()) %>% #quita outlayers
  themis::step_upsample(hospital_death, over_ratio = 1, skip= TRUE, seed= 123)
#Nota: la variable outcome debe estar como factor

# table que demuestra que el outcome estaba desvalanceado
table(data_modelo$hospital_death)

#se prepara la receta con el preprocesamiento de tindymodel y la base de datos
recetaoreoarada <- prep(rct_dataModelo, training = data_modelo)

#se horne la receta generando el proceso con los datos balanceados
trainpreprocesado <- bake(recetaoreoarada, new_data = NULL)

#se observa tabla con datos ya balanceada
table(trainpreprocesado$hospital_death)

#modelos que se usara    
reglog_demo <- 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification") #No Necesario  

#Flujo donde se une el tipo de modelo y el recipiente de los datos
reglog_wflow <-
  workflow() %>%
  add_recipe(rct_dataModelo) %>%
  add_model(reglog_demo)

#opcion 2
reglog_wf <-
  workflow() %>%
  add_formula(hospital_death ~ . ) %>%
  add_model(reglog_demo) %>%
  fit(data = trainpreprocesado)
  

#entrenamiento del modelo
reglog_fitted <- fit(reglog_wflow, data = data_modelo)

reglog_fitted %>% tidy

reglog_prediccion <-data_modelo %>%
  predict(reglog_fitted, new_data = . ) %>%
  mutate(Real= data_modelo$hospital_death) %>%
  conf_mat(truth = Real, estimate = .pred_class ) %>%
  summary

#opcion 2
trainpreprocesado %>%
  predict(reglog_wf, new_data = . ) %>%
  mutate(Real= trainpreprocesado$hospital_death) %>%
  conf_mat(truth = Real, estimate = .pred_class ) %>%
  summary

# # POR DE QUE SE DESEAN DATOS NUMERICOS SE COMENTA ESTA TRANSFORMACION
# data_cluster$hospital_death <- ifelse(test = data_cluster$hospital_death == 1, yes = "Sobrevivió", no = "Falleció")
# 
# data_modelo$hospital_death <-  factor(data_modelo$hospital_death, 
#                                        labels = c("Sobrevivió",
#                                                   "Falleció"),
#                                        ordered = T)
# 
# data_modelo$elective_surgery <- factor(data_modelo$elective_surgery,
#                                         labels  = c("No",
#                                                        "Si"),
#                                             ordered = T)
# 
# data_cluster$apache_post_operative <- factor(data_cluster$apache_post_operative ,
#                                         levels  = c("No",
#                                                     "Si"),
#                                         ordered = T)




# se quitan las variables categoricas
data_cluster$ethnicity <- NULL #variable categorica
data_cluster$icu_stay_typ <- NULL #variable categorica
data_cluster$icu_type <- NULL #variable categorica
data_cluster$hospital_admit_source <- NULL #variable categorica
data_cluster$icu_admit_source <- NULL #variable categorica
data_cluster$icu_stay_type <- NULL #variable categorica
data_cluster$hospital_id <- NULL #variable categorica
data_cluster$gender <- NULL # Se retira por tener NA, que afectan la transformacion

# se transforma la variable dicotomica en continua
data_cluster$elective_surgery <- as.numeric(data_cluster$elective_surgery)
#data_cluster$gender <- as.numeric(data_cluster$gender) #Los NA no permiten qe se transforme en nuerica
data_cluster$hospital_death <- as.numeric(data_cluster$hospital_death)
data_cluster$intubated_apache <- as.numeric(data_cluster$intubated_apache)
data_cluster$ventilated_apache <- as.numeric(data_cluster$ventilated_apache)
data_cluster$apache_post_operative <- as.numeric(data_cluster$apache_post_operative)
data_cluster$arf_apache <- as.numeric(data_cluster$arf_apache)


# se quitan columnas con missing data
# se define como data_cluster_2 a los que ya no tienen missing data
#ahora si no tenemos missing data. se cuenta con 58mil pacientes
data_cluster2<-data_cluster[!(is.na(data_cluster$age) | 
                     is.na(data_cluster$bmi) | 
                     is.na(data_cluster$elective_surgery) | 
                     #is.na(data_cluster$gender)|
                     #is.na(data_cluster$icu_stay_typ)|
                     #is.na(data_cluster$icu_type)|
                     is.na(data_cluster$pre_icu_los_days) |
                     is.na(data_cluster$apache_post_operative) |
                     #is.na(data_cluster$hospital_id) | 
                     is.na(data_cluster$hospital_death) | 
                     #is.na(data_cluster$ethnicity) |
                     is.na(data_cluster$height) |
                     #is.na(data_cluster$hospital_admit_source)|
                     #is.na(data_cluster$icu_admit_source)|
                     #is.na(data_cluster$icu_stay_type) |
                     is.na(data_cluster$arf_apache) | 
                     is.na(data_cluster$weight) |
                     is.na(data_cluster$apache_2_diagnosis)|
                     is.na(data_cluster$apache_3j_diagnosis)|
                     is.na(data_cluster$gcs_eyes_apache) |
                     is.na(data_cluster$gcs_motor_apache) | 
                     is.na(data_cluster$gcs_verbal_apache) |
                     is.na(data_cluster$heart_rate_apache) |
                     is.na(data_cluster$intubated_apache) |
                     is.na(data_cluster$map_apache)|
                     is.na(data_cluster$resprate_apache)|
                     is.na(data_cluster$temp_apache) |
                     is.na(data_cluster$ventilated_apache) |
                     is.na(data_cluster$glucose_apache) |
                     is.na(data_cluster$sodium_apache)|
                     is.na(data_cluster$creatinine_apache) |
                     is.na(data_cluster$bun_apache) |
                     is.na(data_cluster$hematocrit_apache) |
                     is.na(data_cluster$wbc_apache)),]

#poner los datos en orden logico
data_cluster2 = data_cluster2 %>% dplyr::select(hospital_death, age, bmi, height, weight, #datos demograficos
                                         elective_surgery, apache_post_operative, pre_icu_los_days, intubated_apache, # estatus hospitalario
                                         apache_2_diagnosis, apache_3j_diagnosis, #criterios de gravedad
                                         gcs_eyes_apache, gcs_motor_apache, gcs_verbal_apache, #estado neurologico
                                         creatinine_apache, bun_apache, arf_apache, # estado renal
                                         heart_rate_apache, map_apache, resprate_apache, temp_apache, #signos vitales
                                         hematocrit_apache, wbc_apache, glucose_apache, sodium_apache #lab sangineos
                                         )

# #POR TIEMPO SE COMENTA ESTOS ANALISIS
# #crear reporte
# create_report(data_cluster2)


#escalar los datos
#Nota: parece ser que escalar los datos no sirve mucho en el reporte, sale muy parecido
data_cluster2_scale <- as.data.frame(scale(data_cluster2))

# #POR TIEMPO SE COMENTA ESTOS ANALISIS
# #crear reporte de los datos escalados
# create_report(data_cluster2_scale)

# crear dataframe de los cluster2 que fallecieron
data_cluster2_fallecio <- data_cluster2[(data_cluster2$hospital_death == 1),]
# esta icuib tanbueb sirve para filtar
#data_cluster2_fallecio <- data_cluster2[(data_cluster2['hospital_death'] != 0),]

# #POR TIEMPO SE COMENTA ESTOS ANALISIS
# #Crear reporte de los que fallecieron
# create_report(data_cluster2_fallecio)

#limpiar data_cluster2_fallecio, dejar solo variables numerica y quitar lake variables
data_fallecio_num <-data_cluster2_fallecio
data_fallecio_num$hospital_death <- NULL
data_fallecio_num$elective_surgery <- NULL
data_fallecio_num$apache_post_operative <- NULL
data_fallecio_num$intubated_apache <- NULL
data_fallecio_num$apache_2_diagnosis <- NULL #se quita porque es una variable lake
data_fallecio_num$apache_3j_diagnosis <- NULL #se quita porque es una variable lake
data_fallecio_num$arf_apache <- NULL
data_fallecio_num <- mutate(data_fallecio_num, escala_glasgow = gcs_eyes_apache + gcs_motor_apache + gcs_verbal_apache)
data_fallecio_num$gcs_eyes_apache <- NULL #se quita porque se la unio a la variable escala de glasgow
data_fallecio_num$gcs_motor_apache <- NULL #se quita porque se la unio a la variable escala de glasgow
data_fallecio_num$gcs_verbal_apache <- NULL #se quita porque se la unio a la variable escala de glasgow
data_fallecio_num$height <- NULL # se la quita porque esta en variable BMI
data_fallecio_num$weight <- NULL # se la quita porque esta en variable BMI









# ANALISIS ESTADISTICA  -------------------------------------------------
summary(data_fallecio_num)

# #POR TIEMPO SE COMENTA ESTOS ANALISIS
# #Crear reporte de variables numericas de los que fallecieron 
# create_report(data_fallecio_num)
# 
# # clusterizaciobarplot()---
# library(clustertend)
# set.seed(321)
# hopkins(data_fallecio_num, n=nrow(data_fallecio_num)-1)
# # pareceria que estos datos no son clusterizables porque el vlaor H es menor a 0.5
# #valor H es de 0.2958773


#se escalan las variables numericas
data_fallecio_num_scale <- as.data.frame(scale(data_fallecio_num))



#POR TIEMPO SE COMENTA ESTOS ANALISIS
# #Reporte de los datos eslados
# create_report(data_fallecio_num_scale)
# 
# #analisis para determinar se hay cluster
# set.seed(321)
# hopkins(data_fallecio_num_scale, n=nrow(data_fallecio_num_scale)-1)
# #escalandolo tampoco funciona $H 0.2513681


# RECODIFICACION DE VARIABLES ---------------------------------------------

# transformar a valores absolutos la estandarizacion de las variables
# se asumiria que el dato medio es lo normal, para edad aplicaria
data_fallecio_EscaEstan <- data_fallecio_num

#se escala de 0 a 1 las siguientes variables
data_fallecio_EscaEstan$age <- (data_fallecio_EscaEstan$age-min(data_fallecio_EscaEstan$age))/diff(range(data_fallecio_EscaEstan$age)) #se escala la edad de 0 a 1
data_fallecio_EscaEstan$bmi <- (data_fallecio_EscaEstan$bmi-min(data_fallecio_EscaEstan$bmi))/diff(range(data_fallecio_EscaEstan$bmi)) #se escala la edad de 0 a 1
data_fallecio_EscaEstan$pre_icu_los_days <- (data_fallecio_EscaEstan$pre_icu_los_days-min(data_fallecio_EscaEstan$pre_icu_los_days))/diff(range(data_fallecio_EscaEstan$pre_icu_los_days)) #se escala la edad de 0 a 1
data_fallecio_EscaEstan$creatinine_apache <- (data_fallecio_EscaEstan$creatinine_apache-min(data_fallecio_EscaEstan$creatinine_apache))/diff(range(data_fallecio_EscaEstan$creatinine_apache)) #se escala la edad de 0 a 1
data_fallecio_EscaEstan$bun_apache <- (data_fallecio_EscaEstan$bun_apache-min(data_fallecio_EscaEstan$bun_apache))/diff(range(data_fallecio_EscaEstan$bun_apache)) #se escala la edad de 0 a 1


# Se escala el latido cardiaco asumiento como 0 (normal) en valor de 90 latidos por minuto
data_fallecio_EscaEstan$heart_rate_apache <- (data_fallecio_EscaEstan$heart_rate_apache-90)/(max(data_fallecio_EscaEstan$heart_rate_apache-90)) 
data_fallecio_EscaEstan$heart_rate_apache <- abs(data_fallecio_EscaEstan$heart_rate_apache)

# Se saca el log de la presion media, donde la media de esta presion daria 90mmHg representado por su logaritmo de 4.5
data_fallecio_EscaEstan$map_apache <- log(data_fallecio_EscaEstan$map_apache)
data_fallecio_EscaEstan$map_apache <- (data_fallecio_EscaEstan$map_apache-4.5)/(max(data_fallecio_EscaEstan$map_apache)-4.5) #se escala la edad de 0 a 1
data_fallecio_EscaEstan$map_apache <- abs(data_fallecio_EscaEstan$map_apache)


# se transforma a logaritmo y luego se coge la FR de 16, que seria 2.77 
data_fallecio_EscaEstan$resprate_apache <- log(data_fallecio_EscaEstan$resprate_apache)
data_fallecio_EscaEstan$resprate_apache <- (data_fallecio_EscaEstan$resprate_apache-2.77)/(2.77 - min(data_fallecio_EscaEstan$resprate_apache)) #se escala la edad de 0 a 1
data_fallecio_EscaEstan$resprate_apache <- abs(data_fallecio_EscaEstan$resprate_apache)


# AUnque la temperatura normal es 37, se toma como punto medio 36 porque la hipotermia influye mas que la hipotermia influye mas que la hipertermia
# pendiente confirmar
data_fallecio_EscaEstan$temp_apache <- (data_fallecio_EscaEstan$temp_apache-36)/(36-min(data_fallecio_EscaEstan$temp_apache)) 
data_fallecio_EscaEstan$temp_apache <- abs(data_fallecio_EscaEstan$temp_apache)

#hematocirto no es lo ideal pero creo que lo mejor es escalarla a la mean que seria 31
data_fallecio_EscaEstan$hematocrit_apache <- (data_fallecio_EscaEstan$hematocrit_apache-mean(data_fallecio_EscaEstan$hematocrit_apache))/(max(data_fallecio_EscaEstan$hematocrit_apache)-mean(data_fallecio_EscaEstan$hematocrit_apache)) #se escala la edad de 0 a 1
data_fallecio_EscaEstan$hematocrit_apache <- abs(data_fallecio_EscaEstan$hematocrit_apache)

#hematocirto no es lo ideal pero creo que lo mejor es escalarla a la meadian que seria 13.6
data_fallecio_EscaEstan$wbc_apache <- (data_fallecio_EscaEstan$wbc_apache-median(data_fallecio_EscaEstan$wbc_apache))/(max(data_fallecio_EscaEstan$wbc_apache)-median(data_fallecio_EscaEstan$wbc_apache)) #se escala la edad de 0 a 1
data_fallecio_EscaEstan$wbc_apache <- abs(data_fallecio_EscaEstan$wbc_apache)

#la glucosa la transofraremos en logaritmo y luego sacaremos su media que seria el equivalente a 148.4mg/dl
# pendiente confirmar
data_fallecio_EscaEstan$glucose_apache <- log(data_fallecio_EscaEstan$glucose_apache)
data_fallecio_EscaEstan$glucose_apache <- (data_fallecio_EscaEstan$glucose_apache-mean(data_fallecio_EscaEstan$glucose_apache))/(max(data_fallecio_EscaEstan$glucose_apache)-mean(data_fallecio_EscaEstan$glucose_apache)) #se escala la edad de 0 a 1
data_fallecio_EscaEstan$glucose_apache <- abs(data_fallecio_EscaEstan$glucose_apache)

#la sodio sacaremos su media que seria 138mg/dl (muy similar al rango normal que es 140mg/dl)
data_fallecio_EscaEstan$sodium_apache <- (data_fallecio_EscaEstan$sodium_apache-mean(data_fallecio_EscaEstan$sodium_apache))/(mean(data_fallecio_EscaEstan$sodium_apache)-min(data_fallecio_EscaEstan$sodium_apache)) #se escala la edad de 0 a 1
data_fallecio_EscaEstan$sodium_apache <- abs(data_fallecio_EscaEstan$sodium_apache)

#se escala de 0 a 1 las siguientes variables
data_fallecio_EscaEstan$escala_glasgow <- (data_fallecio_EscaEstan$escala_glasgow-min(data_fallecio_EscaEstan$escala_glasgow))/diff(range(data_fallecio_EscaEstan$escala_glasgow)) 

# Estadistica descriptiva y visualizacion----
summary(data_fallecio_EscaEstan)

# #POR TIEMPO SE COMENTA ESTOS ANALISIS
# # Crear reporte y graficos
# create_report(data_fallecio_EscaEstan)
# 
# #cluster
# set.seed(321)
# hopkins(data_fallecio_EscaEstan, n=nrow(data_fallecio_EscaEstan)-1)
# 



