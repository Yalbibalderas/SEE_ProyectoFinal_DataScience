#
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
library(gamlss)
library(mgcv)



# Generar ambiente --------------------------------------------------------

setwd("C:/Users/DELL/OneDrive/Escritorio/clasesShinySEE/datathon/Datos")
getwd()


# Bajar base de datos -----------------------------------------------------


training_v2 <- read.csv(file = "training_v2.csv")
unlabeled <- read.csv(file = "unlabeled.csv")


# Limpiesa de la base de datos --------------------------------------------

#elective_surgery= The common national or cultural tradition which the person belongs to


data_demo <- training_v2
data_demo$encounter_id <- NULL #se retira porque es un identificador
data_demo$patient_id <- NULL #se retira porque es un identificador
data_demo$icu_id <- NULL #se retira porque es un identificador
data_demo$readmission_status <- NULL # se retira porque todos son 0
data_demo$icu_admit_type <- NULL # se retira porque no tiene elementos
data_demo$ethnicity  # No se usa porque parece que esta variable no influye, hispanos son 3.600 y nativos 770, son los que tienen mayor prevalencia de mortalidad pero es muy baja
data_demo$height  # se retira porque parece ser que no influye 
data_demo$weight  # se retira porque su influencia parece ya estar recogida en BMI
data_demo$pre_icu_los_days <- abs(data_demo$pre_icu_los_days) #se transforman los numeros negativos en absolutos porque asumo error al digitar y en algunos centros lo reportaron como negativo
data_demo$pre_icu_los_days <- round(data_demo$pre_icu_los_days, digits = 0) #se le quita los decimales al los dias de hospitalizacion
data_demo$hospital_death <- factor(data_demo$hospital_death,
                                   levels = c(0,1),
                              labels = c("Sobrevivió", 
                                         "Falleció"))

data_demo$elective_surgery <- factor(data_demo$elective_surgery,
                                     levels = c(0,1),
                                     labels = c("No", 
                                                "Si"))

data_demo$apache_post_operative <- as.character(data_demo$apache_post_operative)

                                  



data_demo[data_demo$gender == "",] <- NA #transformar los espacios vacios en NA

data_demo$intubated_apache <- factor(data_demo$intubated_apache,
                                     levels = c(0,1),
                                     labels = c("No", 
                                                "Si"))

data_demo$ventilated_apache <- factor(data_demo$ventilated_apache,
                                     levels = c(0,1),
                                     labels = c("No", 
                                                "Si"))


# DEMOGRAFICAS  ----------------------------


data_demo <- mutate(data_demo, escala_glasgow = gcs_eyes_apache + gcs_motor_apache + gcs_verbal_apache)



#interaccion entre la escala de glasow y la intubacion
plot(prop.table(table(data_demo$escala_glasgow, data_demo$hospital_death, data_demo$ventilated_apache), margin = 1)*100)
##    Paciente intubado
#       No        Si
# 3  44.871795 55.128205
# 4  54.521964 45.478036
# 5  56.338028 43.661972
# 6  54.127358 45.872642
# 7  56.684715 43.315285
# 8  54.715302 45.284698
# 9  62.032333 37.967667
# 10 62.569649 37.430351
# 11 69.672131 30.327869
# 12 81.796502 18.203498
# 13 90.467585  9.532415
# 14 96.105224  3.894776
# 15 93.947384  6.052616

#no se puede calcluar cirugias de emergencia con los datos que se tienen
#data_demo <- mutate(data_demo, emergency_surgery = as.numeric(apache_post_operative) - as.numeric(elective_surgery))


attach(data_demo) #para no tener que siempre estar poniendo data antes de las variables


# data$hospital_admit_source <- ifelse(test = data$hospital_admit_source == c("Direct Admit", 
#                                          "Emergency Department", 
#                                          "Floor",
#                                          "ICU",
#                                          "ICU to SDU",
#                                          "Other Hospital",
#                                          "Other ICU ",
#                                          "Step-Down Unit (SDU)"), 
#                                      yes = "Admición por área crítica", 
#                                      no = "Admición por área no crítica") #transformar en dicotomica area critica (# asociacion con adminsion directa, emergencia, floor, otro hospital, otro UCI, step Down Unit)

# transformar variables con puntos de corte -------------------------------
ggplot(data_demo, aes(pre_icu_los_days)) + 
  geom_boxplot() + 
  facet_grid(data_demo$hospital_death ~ .)

# Dias previos de hospitalizacion ----
plot(roc(data_demo$hospital_death, data_demo$pre_icu_los_days))
auc_pre_icu_los_days <- auc(roc(data_demo$hospital_death, data_demo$pre_icu_los_days))
#Area under the curve: 0.5378 # no tan buena variable, pero mas cercano 

plot(table(data_demo$pre_icu_los_days, data_demo$hospital_death))

plot(prop.table(table(data_demo$pre_icu_los_days, data_demo$hospital_death), margin = 1))


data_demo <- mutate(data_demo,
                    preIcuLosDays_cat = cut(data_demo$pre_icu_los_days, breaks = c(-Inf, 1, 7, Inf),
                                            levels(c) <- c("Menos de 2 días",
                                                           "De 2 a una semana", 
                                                           "Mas de una semana"),
                                            ordered_result = TRUE)
)




plot(table(data_demo$preIcuLosDays_cat, data_demo$hospital_death))
#                   Sobrevivió Falleció   Cuanto riesgo aumentar
# Menos de 2 días        73182     6321   #0
# De 2 a una semana       9051     1211   #1
# Mas de una semana       1548      375   #2

plot(prop.table(table(data_demo$preIcuLosDays_cat, data_demo$hospital_death), margin = 1))
#                   Sobrevivió   Falleció 
# Menos de 2 días   0.92049357 0.07950643   #0.75
# De 2 a una semana 0.88199181 0.11800819   #1.25
# Mas de una semana 0.80499220 0.19500780   #2

# Edad ----
plot(roc(data_demo$hospital_death, data_demo$age))
auc(roc(data_demo$hospital_death, data_demo$age))
#Area under the curve: 0.6187, edad es variable importante

plot(table(round(data_demo$age, digits = -1), data_demo$hospital_death))
#    Sobrevivió Falleció    Cuanto riesgo aumentar  
# 20       2998       88    #0.5
# 30       3929      153    #0.5
# 40       6642      285    #0.5
# 50      10531      619    #0.5
# 60      18762     1527    #0.75
# 70      16887     1707    #1
# 80      16599     2252    #1.25
# 90       3852      641    #1.25

plot(prop.table(table(round(data_demo$age, digits = -1), data_demo$hospital_death), margin = 1))
#   Sobrevivió   Falleció     Cuanto riesgo aumentar  
# 20 0.97148412 0.02851588    #0.25
# 30 0.96251837 0.03748163    #0.25
# 40 0.95885665 0.04114335    #0.5
# 50 0.94448430 0.05551570    #0.5
# 60 0.92473754 0.07526246    #0.75
# 70 0.90819619 0.09180381    #1
# 80 0.88053684 0.11946316    #1.25
# 90 0.85733363 0.14266637    #1.5
## Nota: la edad parece tener una relacion polinomial de 2 grado




#se escogieron puntos de corte para la edad buscando
#1 que la probabilidad vaya aumentando de ser posible 2% en cada categoria
#2 que los grupos tengan tamaños muestrales parecidos, de preferencia mas de mil

# se usa los mismos puntos de corte del APACHE II
data_demo <- mutate(data_demo,
                    age_cat2 = cut(data_demo$age, breaks = c(0, 45, 55, 65, 75, Inf),
                                   levels(c) <- c("Menor de  45 años", 
                                                  "De 45 a 54 años", 
                                                  "De 55 a 65 años", 
                                                  "De 65 a 75 años",
                                                  "Mayor de 75 años"),
                                   ordered_result = TRUE)
)

plot(table(data_demo$age_cat2, data_demo$hospital_death))
#                     Sobrevivió Falleció  Cuanto riesgo aumentar
# Menor de  45 años      13569      526     #0.25
# De 45 a 54 años        12070      724     #0.5
# De 55 a 65 años        17223     1422     #0.75
# De 65 a 75 años        18609     1900     #1
# Mayor de 75 años       18729     2700     #1.25

plot(prop.table(table(data_demo$age_cat2, data_demo$hospital_death), margin = 1))
#                   Sobrevivió   Falleció   Cuanto riesgo aumentar
# Menor de  45 años 0.96268180 0.03731820   #0.25
# De 45 a 54 años   0.94341097 0.05658903   #0.5
# De 55 a 65 años   0.92373290 0.07626710   #0.75
# De 65 a 75 años   0.90735775 0.09264225   #1
# Mayor de 75 años  0.87400252 0.12599748   #1.25


#Se proponene nuevos puntos de corte para edad
#actualmente se tienen mas paciente que llegan por encima de los 80 años, 
# por lo que podria ser buena idea reajustar el punto de corte de edad.
data_demo <- mutate(data_demo,
               age_cat = cut(data_demo$age, breaks = c(0, 30, 50, 60, 70, 80, Inf),
               levels(c) <- c("Menor de 30 años", 
                              "De 31 a 50 años", 
                              "De 51 a 60 años", 
                              "De 61 a 70 años", 
                              "De 71 a 80 años", 
                              "Mayor de 80 años"),
               ordered_result = TRUE)
               )

plot(table(data_demo$age_cat, data_demo$hospital_death))
#                   Sobrevivió Falleció   Cuanto riesgo aumentar
# Menor de 30 años       5100      152    #0
# De 31 a 50 años       13378      635    #0.5
# De 51 a 60 años       15459     1064
# De 61 a 70 años       18488     1738
# De 71 a 80 años       17098     2035
# Mas de 80 años        10689     1649


plot(prop.table(table(data_demo$age_cat, data_demo$hospital_death), margin = 1))
#                  Sobrevivió   Falleció  Cuanto riesgo aumentar
# Menor de 30 años 0.97105864 0.02894136  #0
# De 31 a 50 años  0.95468494 0.04531506  #0.5
# De 51 a 60 años  0.93560491 0.06439509  #0.5
# De 61 a 70 años  0.91407100 0.08592900  #1
# De 71 a 80 años  0.89363926 0.10636074  #1  
# Mas de 80 años   0.86634787 0.13365213  #1.25




# BMI indice de masa corporal----

#Se dividio el BMI basado en sus puntos de corte usuales,
# se busca que por lo menos cada grupo tenga 5 mil pacientes
# un IMC < 16 presento mayor mortalidad, pero habian solo 800 paientes, por lo que no se lo puso
# otros estudios corroboran que mortalidad se asocio a bajo peso
plot(roc(data_demo$hospital_death, data_demo$bmi))
auc(roc(data_demo$hospital_death, data_demo$bmi))
#Area under the curve: 0.5452 #variable no tan buena
plot(table(round(data_demo$bmi, digits = -1), data_demo$hospital_death))
#   Sobrevivió Falleció
# 10        403       88
# 20      25835     2927
# 30      39371     3235
# 40      11328      950
# 50       2588      207
# 60        716       85
# 70        477       56

plot(prop.table(table(round(data_demo$bmi, digits = -1), data_demo$hospital_death), margin = 1))
#    Sobrevivió   Falleció   
# 10 0.82077393 0.17922607    #1.75
# 20 0.89823378 0.10176622    #1
# 30 0.92407173 0.07592827    #0.75
# 40 0.92262583 0.07737417    #0.75
# 50 0.92593918 0.07406082    #0.75
# 60 0.89388265 0.10611735    #1
# 70 0.89493433 0.10506567    #1


data_demo <- mutate(data_demo,
               bmi_cat = cut(data_demo$bmi, breaks = c(0, 18.5, 25, 30, 35., 40, Inf),
                             levels(c) <- c("Bajo peso",
                                            "Peso normal", 
                                            "Sobrepeso", 
                                            "Obesidad grado I", 
                                            "Obesidad grado II", 
                                            "Obesidad grado III"),
                             ordered_result = TRUE)
)

plot(table(data_demo$bmi_cat, data_demo$hospital_death))
#                     Sobrevivió Falleció     Cuanto riesgo aumentar
# Bajo peso                3172      551      #1.5
# Peso normal             23068     2465      #1
# Sobrepeso               24056     2059      #0.75
# Obesidad grado I        15331     1179      #0.75
# Obesidad grado II        7601      642      #0.75
# Obesidad grado III       7504      656      #0.75

plot(prop.table(table(data_demo$bmi_cat, data_demo$hospital_death), margin = 1))
#                    Sobrevivió   Falleció
# Bajo peso          0.85200107 0.14799893 #1.5
# Peso normal        0.90345827 0.09654173 #1
# Sobrepeso          0.92115642 0.07884358 #0.75
# Obesidad grado I   0.92858873 0.07141127
# Obesidad grado II  0.92211573 0.07788427
# Obesidad grado III 0.91960784 0.08039216



# SIGNOS VITALES ----------------------------------------------------------


# Temperatura ----
plot(roc(data_demo$hospital_death, data_demo$temp_apache))
auc(roc(data_demo$hospital_death, data_demo$temp_apache))
#Area under the curve: 0.6113 # muy buena variable pero comportamienti signoide

plot(table(round(data_demo$temp_apache), data_demo$hospital_death))
#       Sobrevivió Falleció  Cuanto riesgo aumentar
# 32        366      373      #5
# 33        270      280      #5
# 34        630      356      #2.5
# 35       2597      601      #2.5
# 36      40467     3339      #0.5
# 37      33087     1936      #0.5
# 38       1228      151      #1.5
# 39       1093      196      #1.5
# 40        498      117      #1.5
plot(prop.table(table(round(data_demo$temp_apache), data_demo$hospital_death), margin = 1))
#    Sobrevivió   Falleció
# 32 0.49526387 0.50473613 #5
# 33 0.49090909 0.50909091 #5
# 34 0.63894523 0.36105477 #3
# 35 0.81207004 0.18792996 #1.8
# 36 0.92377756 0.07622244 #0.7
# 37 0.94472204 0.05527796 #0.5
# 38 0.89050036 0.10949964 #1
# 39 0.84794414 0.15205586 #1.5
# 40 0.80975610 0.19024390 #2


break_temperatura <- c(-Inf, 30, 32, 34, 36, 38.5, 39, 40, Inf)
levels_temperatura <- c(-4, -3, -2, -1, 0, 1, 3, 4)


data_demo <- mutate(data_demo,
                    temp_apache_cat2 = cut(data_demo$temp_apache, breaks = break_temperatura,
                                           levels(c) <-  levels_temperatura,
                                           ordered_result = TRUE)
)

data_demo <- mutate(data_demo,
                    d1_temp_max_cat2 = cut(data_demo$d1_temp_max, breaks = break_temperatura,
                                           levels(c) <-  levels_temperatura,
                                           ordered_result = TRUE)
)

data_demo <- mutate(data_demo,
                    d1_temp_min_cat2 = cut(data_demo$d1_temp_min, breaks = break_temperatura,
                                           levels(c) <-  levels_temperatura,
                                           ordered_result = TRUE)
)

data_demo <- mutate(data_demo,
                    h1_temp_max_cat2 = cut(data_demo$h1_temp_max, breaks = break_temperatura,
                                           levels(c) <-  levels_temperatura,
                                           ordered_result = TRUE)
)

data_demo <- mutate(data_demo,
                    h1_temp_min_cat2 = cut(data_demo$h1_temp_min, breaks = break_temperatura,
                                           levels(c) <-  levels_temperatura,
                                           ordered_result = TRUE)
)

#se transforma los numeros negativos a positivos
data_demo$temp_apache_cat2 <- factor(abs(as.numeric(as.character(data_demo$temp_apache_cat2))))

data_demo$d1_temp_max_cat2 <- factor(abs(as.numeric(as.character(data_demo$d1_temp_max_cat2))))
data_demo$d1_temp_min_cat2 <- factor(abs(as.numeric(as.character(data_demo$d1_temp_min_cat2))))
data_demo$h1_temp_max_cat2 <- factor(abs(as.numeric(as.character(data_demo$h1_temp_max_cat2))))
data_demo$h1_temp_min_cat2 <- factor(abs(as.numeric(as.character(data_demo$h1_temp_min_cat2))))



#los que mas mortalidad tienen son los que estan en rango de temperatura 34 a 46 que seria -2

# Frecuencia cardiaca ----
plot(roc(data_demo$hospital_death, data_demo$heart_rate_apache))
auc(roc(data_demo$hospital_death, data_demo$heart_rate_apache))
#Area under the curve: 0.622 #variable interesante

plot(table(round(data_demo$heart_rate_apache, digits = -1), data_demo$hospital_death))
#      Sobrevivió Falleció    Cuanto riesgo aumentar
# 30         654      274     #3
# 40        2463      245     #1
# 50        6890      300     #0.5
# 60        9072      433     #0.5
# 70         805       63     #0.5
# 80        1080       68     #0.5
# 90        8104      412     #0.5
# 100      16038      990     #0.5
# 110      12590     1014     #0.75
# 120      11227     1236     #1
# 130       6125      919     #1.25
# 140       4171      846     #1.5
# 150       1865      427     #1.75
# 160       1090      300     #2
# 170        416      117     #2
# 180        420      157     #2.73

plot(prop.table(table(round(data_demo$heart_rate_apache, digits = -1), data_demo$hospital_death), margin = 1))
#     Sobrevivió   Falleció  Cuanto riesgo aumentar
# 30  0.70474138 0.29525862     #3
# 40  0.90952733 0.09047267     #1
# 50  0.95827538 0.04172462     #0.5
# 60  0.95444503 0.04555497     #0.5
# 70  0.92741935 0.07258065     #0.5
# 80  0.94076655 0.05923345     #0.5
# 90  0.95162048 0.04837952     #0.5
# 100 0.94186047 0.05813953     #0.5
# 110 0.92546310 0.07453690     #0.75
# 120 0.90082645 0.09917355     #1
# 130 0.86953436 0.13046564     #1.25
# 140 0.83137333 0.16862667     #1.5
# 150 0.81369983 0.18630017     #1.75
# 160 0.78417266 0.21582734     #2
# 170 0.78048780 0.21951220     #2
# 180 0.72790295 0.27209705     #2.75


plot(prop.table(table(round(data_demo$temp_apache), data_demo$hospital_death), margin = 1))
plot(table(round(data_demo$temp_apache), data_demo$hospital_death))

attach(data_demo) #al poner attach puedo despues hacer copy paste de la base de datos a otro codio
breaks_FrecCardiaca <- c(-Inf, 40, 55, 70, 110, 130, 180, Inf)
level_FrecCardiaca <- c(-4, -3, -2, 0, 2, 3, 4)

data_demo <- mutate(data_demo,
                    heart_rate_apache_cat2 = cut(data_demo$heart_rate_apache, breaks = breaks_FrecCardiaca,
                                           levels(c) <-  level_FrecCardiaca,
                                           ordered_result = TRUE)
)


#se transforma los numeros negativos a positivos
data_demo$heart_rate_apache_cat2 <- factor(abs(as.numeric(as.character(data_demo$heart_rate_apache_cat2))))
# la frecuencia cardiaca se ve buena correlacion con mortalidad segun el factor

#Presion arterial media 
plot(roc(data_demo$hospital_death, data_demo$map_apache))
auc(roc(data_demo$hospital_death, data_demo$map_apache))
# Area under the curve: 0.593, es interesante
plot(table(round(log(data_demo$map_apache), digits = 0), data_demo$hospital_death))
plot(prop.table(table(round(data_demo$map_apache), data_demo$hospital_death), margin = 1))


breaks_PreArtMedia <- c(-Inf, 50, 70, 110, 130, 160, Inf)
level_PreArtMedia <- c(-4, -2, 0, 2, 3, 4)

data_demo <- mutate(data_demo,
                    map_apache_cat2 = cut(data_demo$map_apache, breaks = breaks_PreArtMedia,
                                                 levels(c) <-  level_PreArtMedia,
                                                 ordered_result = TRUE)
)


#se transforma los numeros negativos a positivos
data_demo$map_apache_cat2 <- factor(abs(as.numeric(as.character(data_demo$map_apache_cat2))))
# la presion arterial media se ve buena correlacion con mortalidad segun el factor


# Frecuencia respiratoria ----
plot(roc(data_demo$hospital_death, data_demo$resprate_apache))
auc(roc(data_demo$hospital_death, data_demo$resprate_apache))
#Area under the curve: 0.5893 #variable mas o menos


plot(table(round(log(data_demo$resprate_apache), digits = 1), data_demo$hospital_death))

#     Sobrevivió Falleció     Cuanto riesgo aumentar
# 0        5093      509      #1
# 10      26232     1346      #0.5
# 20       5727      425      #0.5
# 30      22220     2271      #1
# 40      14932     2144      #1.25
# 50       5231      693      #1.25
# 60       3236      396      #1.25

plot(prop.table(table(round(log(data_demo$resprate_apache), digits = 1), data_demo$hospital_death), margin = 1))
# Sobrevivió   Falleció      Cuanto riesgo aumentar
# 0  0.90913959 0.09086041    #1
# 10 0.95119298 0.04880702    #0.5
# 20 0.93091678 0.06908322    #0.75
# 30 0.90727206 0.09272794    #1
# 40 0.87444366 0.12555634    #1.25
# 50 0.88301823 0.11698177    #1.25
# 60 0.89096916 0.10903084    #1

plot(prop.table(table(round(data_demo$temp_apache), data_demo$hospital_death), margin = 1))
plot(table(round(data_demo$temp_apache), data_demo$hospital_death))


breaks_FrecRespira <- c(-Inf, 6, 10, 12, 25, 35, 50, Inf)
level_FrecRespira <- c(-4, -2, -1, 0, 1, 3, 4)

data_demo <- mutate(data_demo,
                    resprate_apache_cat2 = cut(data_demo$resprate_apache, breaks = breaks_FrecRespira,
                                          levels(c) <-  level_FrecRespira,
                                          ordered_result = TRUE)
)


#se transforma los numeros negativos a positivos
data_demo$resprate_apache_cat2 <- factor(abs(as.numeric(as.character(data_demo$resprate_apache_cat2))))
# la frecuencia respiratoria, el -2 no se correlaciona tan bien con mortalidad

# LABS EXAMENES DE LABORATORIO -----------------------------------
#FIO2 ----
#no se pudo hacer
plot(roc(data_demo$hospital_death, data_demo$fio2_apache))
auc(roc(data_demo$hospital_death, data_demo$fio2_apache))
#Area under the curve: 0.6502 #buena variable para ingresar al modelo


plot(table(round(data_demo$fio2_apache, digits = 1), data_demo$hospital_death))
#      Sobrevivió Falleció    Cuanto riesgo aumentar
# 0.2       1088       76     #1
# 0.3       2153      280     #1
# 0.4       3955      476     #1
# 0.5       3055      541     #1.5
# 0.6       1858      354     #1.5
# 0.7        693      167     #2
# 0.8        751      216     #2
# 0.9        217      104     #3
# 1         3362     1494     #3
plot(prop.table(table(round(data_demo$fio2_apache, digits = 1), data_demo$hospital_death), margin = 1))
#      Sobrevivió  Falleció  Cuanto riesgo aumentar
# 0.2  0.9347079 0.0652921    #0.75
# 0.3  0.8849157 0.1150843    #1
# 0.4  0.8925750 0.1074250    #1
# 0.5  0.8495551 0.1504449    #1.5
# 0.6  0.8399638 0.1600362    #1.5
# 0.7  0.8058140 0.1941860    #2
# 0.8  0.7766287 0.2233713    #2.25 
# 0.9  0.6760125 0.3239875    #3
# 1    0.6923394 0.3076606    #3

plot(table(round(data_demo$temp_apache), data_demo$hospital_death))
plot(prop.table(table(round(data_demo$temp_apache), data_demo$hospital_death), margin = 1))



breaks_FiO2 <- c(-Inf, 56, 60, 70, 200, 350, 500, Inf)
level_FiO2 <- c(-4, -3 -1, 0, 2, 3, 4)

data_demo <- mutate(data_demo,
                    fio2_apache_cat2 = cut(data_demo$fio2_apache, breaks = breaks_FrecRespira,
                                               levels(c) <-  level_FrecRespira,
                                               ordered_result = TRUE)
)


#se transforma los numeros negativos a positivos
data_demo$fio2_apache_cat2 <- factor(abs(as.numeric(as.character(data_demo$fio2_apache_cat2))))
# la frecuencia respiratoria, el -2 no se correlaciona tan bien con mortalidad


#FIO2 no se pudo hacer
breaks_ph <- c(-Inf, 7.15, 7.25, 7.33, 7.50, 7.6, 7.7, Inf)
level_ph <- c(-4, -3, -2, 0, 1, 3, 4)

data_demo <- mutate(data_demo,
                    ph_apache_cat2 = cut(data_demo$ph_apache, breaks = breaks_ph,
                                           levels(c) <-  level_ph,
                                           ordered_result = TRUE)
)


#se transforma los numeros negativos a positivos
data_demo$ph_apache_cat2 <- factor(abs(as.numeric(as.character(data_demo$ph_apache_cat2))))
# la frecuencia respiratoria, el -2 no se correlaciona tan bien con mortalidad



#Urea ----
# queda mejor la logistica
plot(table(round(log(data_demo$bun_apache), digits = 1), data_demo$hospital_death))
#    Sobrevivió Falleció
# 1        760       19
# 2      16431      519
# 3      35194     2962
# 4      12363     2668
# 5       1227      295

plot(prop.table(table(round(log(data_demo$bun_apache), digits = 0), data_demo$hospital_death), margin = 1))
#   Sobrevivió   Falleció
# 1 0.97560976 0.02439024
# 2 0.96938053 0.03061947
# 3 0.92237132 0.07762868
# 4 0.82250017 0.17749983
# 5 0.80617608 0.19382392

#Creatinina ---- 
# la urea se asocio mejor con mortalidad que la creatinina
# tanto la log como la creatinina no se correlacionana tan bien
plot(table(round(log(data_demo$creatinine_apache)), data_demo$hospital_death))
#     Sobrevivió Falleció
# 0        4081      284
# 1       46566     2801
# 2        8766     1774
# 3        2580      706
# 4        1394      405
# 5         861      183
# 6         653      130
# 7         401       59
# 8         313       37
# 9         186       26
# 10        181       24
# 11        407       29

plot(prop.table(table(round(log(data_demo$creatinine_apache), digits = 1), data_demo$hospital_death), margin = 1))
#    Sobrevivió   Falleció
# 0  0.93493700 0.06506300
# 1  0.94326169 0.05673831
# 2  0.83168880 0.16831120
# 3  0.78514912 0.21485088
# 4  0.77487493 0.22512507
# 5  0.82471264 0.17528736
# 6  0.83397190 0.16602810
# 7  0.87173913 0.12826087
# 8  0.89428571 0.10571429
# 9  0.87735849 0.12264151
# 10 0.88292683 0.11707317
# 11 0.93348624 0.06651376

#Glucosa ----
# el logaritmo se comporta mejor y parece un polinomio de tercer grado
plot(table(round(log(data_demo$glucose_apache), digits = 1), data_demo$hospital_death))
#   Sobrevivió Falleció
# 4      13203     1333
# 5      50503     4232
# 6       9889     1504
plot(prop.table(table(round(log(data_demo$glucose_apache), digits = 0), data_demo$hospital_death), margin = 1))
#   Sobrevivió   Falleció
# 4 0.90829664 0.09170336
# 5 0.92268201 0.07731799
# 6 0.86798912 0.13201088

# Hematocrito ----
# no necesita logaritmo, parece un polinomio de segundo grado
plot(table(round(data_demo$hematocrit_apache, digits = 0), data_demo$hospital_death))
#    Sobrevivió Falleció
# 20       8616     1291
# 30      29872     2997
# 40      24828     1742
# 50       2176      301

plot(prop.table(table(round(data_demo$hematocrit_apache, digits = 0), data_demo$hospital_death), margin = 1))
#     Sobrevivió   Falleció
# 20 0.86968810 0.13031190
# 30 0.90881986 0.09118014
# 40 0.93443734 0.06556266
# 50 0.87848203 0.12151797

# sodio ----
plot(table(round(data_demo$sodium_apache), data_demo$hospital_death))
plot(prop.table(table(round(data_demo$sodium_apache), data_demo$hospital_death), margin = 1))

# Globulos blancos ----
plot(table(round(log(data_demo$wbc_apache), digits = 1), data_demo$hospital_death))
plot(prop.table(table(round(log(data_demo$wbc_apache), digits = 1), data_demo$hospital_death), margin = 1))



# COMORBILIDADES APACHE ---------------------------------------------------

table(aids, hospital_death)
table(cirrhosis, hospital_death)
table(diabetes_mellitus, hospital_death)
table(hepatic_failure, hospital_death)
table(immunosuppression, hospital_death)
table(leukemia, hospital_death)
table(lymphoma, hospital_death)
table(solid_tumor_with_metastasis, hospital_death)



# Realizar correlaciones entre las variables del modelo -------------------
pairs(~ age_cat + # asociacion positiva 2.7
        bmi_cat + # cambiarla a variable ordinar
        elective_surgery +  # asociacion negativa
        gender + # no hay asociacion
        icu_stay_type + #si hay asociacion
        icu_type + # si hay asociacion
        preIcuLosDays_cat +  #si hay asociacion
        apache_post_operative, 
      data = data_demo)


# MODELAMIENTO MG LOGARITMICO ---------------------------------------------
# Tenemos la MALDICION DE LA DIMENCIONALIDAD
# Para n fijo (91mil pacientes), a medida que aumenta p (186 variales, más las variales compuestas), los datos se vuelven esparcidos.
# A medida que aumenta p, el número de modelos posibles se dispara.
# Para p grande, la mayoría de los conjuntos de datos son multicolineales (o concurvos, que es generalización no paramétrica de multicolinealidad).
# Formas en manejar esta maldicion son MARS, CART, LOESS, Bosques aleatorios y famosas máquinas de vectores de soporte.

# La forma de manejar la multicolineanilidad o la concurvidad es con:
# Evaluación del ajuste del modelo y Estimación de incertidumbre
# que se consigue con: Validación cruzada y sus variantes; y Bootstrap

# ademas tenemos data no balanceada (que se esta manejando con upsample)

prop.table(table(glm_mortalidad_demografica_5$model$hospital_death, round(predict(glm_mortalidad_demografica_5, new_data = data_demo ))), margin = 2)

table(round(glm_mortalidad_demografica_12$fitted.value), glm_mortalidad_demografica_12$model$hospital_death)

prop.table(table(round(glm_mortalidad_demografica_12$fitted.value), glm_mortalidad_demografica_12$model$hospital_death), margin = 1)*100

#AIC: 
glm_mortalidad_demografica_12 <- gam(hospital_death ~ age + # 3.090e-02
                                       I(age^2)+ #-6.159e-05
                                       gender + # (Masculino) -6.185e-02 (ya no es significativo)
                                       bmi + #-8.346e-02
                                       I(bmi^2) + # 9.241e-04
                                       icu_stay_type + #(typetransfer)-2.381e-01
                                       icu_type + # si hay asociacion
                                       preIcuLosDays_cat +  #(L) 5.169e-01
                                       #apache_post_operative + #-3.134e-01
                                       elective_surgery +  # (si)-1.083e+00 
                                       heart_rate_apache + #-1.371e-02
                                       I(heart_rate_apache^2) + #1.094e-04
                                       map_apache + #-2.762e-02
                                       I(map_apache^2)+ #1.094e-04
                                       log(resprate_apache) + #-4.695e-01
                                       I(log(resprate_apache)^2)+ #1.261e-01
                                       temp_apache+ #-4.058e+00
                                       I(temp_apache^2)+ #5.356e-02
                                       data_demo$creatinine_apache + #6.416e-01
                                       I(data_demo$creatinine_apache^2) + #-1.181e-01
                                       I(data_demo$creatinine_apache^3) + #5.565e-0
                                       log(data_demo$bun_apache) * #3.718e-01
                                       data_demo$arf_apache + #1.199e+00
                                       (log(data_demo$glucose_apache)+ #-3.452e+01
                                          I(log(data_demo$glucose_apache)^2)+ #6.540e+00
                                          I(log(data_demo$glucose_apache)^3))* #-4.271e-01
                                       diabetes_mellitus+
                                       hematocrit_apache+ #-7.511e-02
                                       I(hematocrit_apache^2)+ #1.123e-03
                                       sodium_apache+ #-4.070e-01
                                       I(sodium_apache^2)+ # 1.333e-03
                                       log(wbc_apache)+ # -1.232e+00
                                       I(log(wbc_apache)^2)+ #3.131e-01
                                       aids +
                                       immunosuppression+
                                       solid_tumor_with_metastasis+
                                       leukemia+
                                       lymphoma+
                                       cirrhosis+
                                       hepatic_failure+
                                       (gcs_eyes_apache + #-1.071e-01
                                          gcs_motor_apache + #-1.701e-01
                                          gcs_verbal_apache) * # -1.257e-01
                                       ventilated_apache, #6.503e-01
                                     #gcs_verbal_apache:ventilated_apacheSi  1.268e-01
                                     #log(data_demo$bun_apache):data_demo$arf_apache -2.839e-01
                                     data = data_demo, family = "binomial")
summary(glm_mortalidad_demografica_12)

# se aaumentan comobrilidades como cirrocis, falla hepatica,  cancer, inmunosupresion y diabetes interactuando con glicemia
#AIC: 24739
glm_mortalidad_demografica_11 <- glm(hospital_death ~ age + # 3.090e-02
                                       I(age^2)+ #-6.159e-05
                                       gender + # (Masculino) -6.185e-02 (ya no es significativo)
                                       bmi + #-8.346e-02
                                       I(bmi^2) + # 9.241e-04
                                       icu_stay_type + #(typetransfer)-2.381e-01
                                       icu_type + # si hay asociacion
                                       preIcuLosDays_cat +  #(L) 5.169e-01
                                       #apache_post_operative + #-3.134e-01
                                       elective_surgery +  # (si)-1.083e+00 
                                       heart_rate_apache + #-1.371e-02
                                       I(heart_rate_apache^2) + #1.094e-04
                                       map_apache + #-2.762e-02
                                       I(map_apache^2)+ #1.094e-04
                                       log(resprate_apache) + #-4.695e-01
                                       I(log(resprate_apache)^2)+ #1.261e-01
                                       temp_apache+ #-4.058e+00
                                       I(temp_apache^2)+ #5.356e-02
                                       data_demo$creatinine_apache + #6.416e-01
                                       I(data_demo$creatinine_apache^2) + #-1.181e-01
                                       I(data_demo$creatinine_apache^3) + #5.565e-0
                                       log(data_demo$bun_apache) * #3.718e-01
                                       data_demo$arf_apache + #1.199e+00
                                       (log(data_demo$glucose_apache)+ #-3.452e+01
                                       I(log(data_demo$glucose_apache)^2)+ #6.540e+00
                                       I(log(data_demo$glucose_apache)^3))* #-4.271e-01
                                       diabetes_mellitus+
                                       hematocrit_apache+ #-7.511e-02
                                       I(hematocrit_apache^2)+ #1.123e-03
                                       sodium_apache+ #-4.070e-01
                                       I(sodium_apache^2)+ # 1.333e-03
                                       log(wbc_apache)+ # -1.232e+00
                                       I(log(wbc_apache)^2)+ #3.131e-01
                                       aids +
                                       immunosuppression+
                                       solid_tumor_with_metastasis+
                                       leukemia+
                                       lymphoma+
                                       cirrhosis+
                                       hepatic_failure+
                                       (gcs_eyes_apache + #-1.071e-01
                                          gcs_motor_apache + #-1.701e-01
                                          gcs_verbal_apache) * # -1.257e-01
                                       ventilated_apache, #6.503e-01
                                     #gcs_verbal_apache:ventilated_apacheSi  1.268e-01
                                     #log(data_demo$bun_apache):data_demo$arf_apache -2.839e-01
                                     data = data_demo, family = "binomial")
summary(glm_mortalidad_demografica_11)


#interaccion entre Bun y falla renal y se aumenta Creatinina logaritmica polinomial de grado 3
#AIC: 24991
glm_mortalidad_demografica_10 <- glm(hospital_death ~ age + # 3.090e-02
                                      I(age^2)+ #-6.159e-05
                                      gender + # (Masculino) -6.185e-02 (ya no es significativo)
                                      bmi + #-8.346e-02
                                      I(bmi^2) + # 9.241e-04
                                      icu_stay_type + #(typetransfer)-2.381e-01
                                      icu_type + # si hay asociacion
                                      preIcuLosDays_cat +  #(L) 5.169e-01
                                      #apache_post_operative + #-3.134e-01
                                      elective_surgery +  # (si)-1.083e+00 
                                      heart_rate_apache + #-1.371e-02
                                      I(heart_rate_apache^2) + #1.094e-04
                                      map_apache + #-2.762e-02
                                      I(map_apache^2)+ #1.094e-04
                                      log(resprate_apache) + #-4.695e-01
                                      I(log(resprate_apache)^2)+ #1.261e-01
                                      temp_apache+ #-4.058e+00
                                      I(temp_apache^2)+ #5.356e-02
                                      data_demo$creatinine_apache + #6.416e-01
                                      I(data_demo$creatinine_apache^2) + #-1.181e-01
                                      I(data_demo$creatinine_apache^3) + #5.565e-0
                                      log(data_demo$bun_apache) * #3.718e-01
                                      data_demo$arf_apache + #1.199e+00
                                      log(data_demo$glucose_apache)+ #-3.452e+01
                                      I(log(data_demo$glucose_apache)^2)+ #6.540e+00
                                      I(log(data_demo$glucose_apache)^3)+ #-4.271e-01
                                      hematocrit_apache+ #-7.511e-02
                                      I(hematocrit_apache^2)+ #1.123e-03
                                      sodium_apache+ #-4.070e-01
                                      I(sodium_apache^2)+ # 1.333e-03
                                      log(wbc_apache)+ # -1.232e+00
                                      I(log(wbc_apache)^2)+ #3.131e-01
                                      (gcs_eyes_apache + #-1.071e-01
                                         gcs_motor_apache + #-1.701e-01
                                         gcs_verbal_apache) * # -1.257e-01
                                      ventilated_apache, #6.503e-01
                                    #gcs_verbal_apache:ventilated_apacheSi  1.268e-01
                                    #log(data_demo$bun_apache):data_demo$arf_apache -2.839e-01
                                    data = data_demo, family = "binomial")
summary(glm_mortalidad_demografica_10)

# se aumenta Laboratorios, algunos laboratorios con logaritmica
#AIC: 25192
glm_mortalidad_demografica_9 <- glm(hospital_death ~ age + # 3.090e-02
                                      I(age^2)+ #-4.247e-05
                                      bmi + #-7.320e-02
                                      I(bmi^2) + # 9.241e-04
                                      gender + # (Masculino) -2.232e-02 (ya no es significativo)
                                      icu_stay_type + #(typetransfer)-2.326e-01
                                      icu_type + # si hay asociacion
                                      preIcuLosDays_cat +  #(L) 5.295e-01
                                      apache_post_operative + #-3.134e-01
                                      elective_surgery +  # (si)-8.189e-01 
                                      heart_rate_apache + #-1.459e-02
                                      I(heart_rate_apache^2) + #1.133e-04
                                      map_apache + #-2.921e-02
                                      I(map_apache^2)+ #1.285e-04
                                      resprate_apache + #2.017e-02
                                      I(resprate_apache^2)+ #-1.496e-04
                                      temp_apache+ #-4.055e+0
                                      I(temp_apache^2)+ #5.354e-02
                                      log(data_demo$bun_apache)+ #5.624e-01
                                      log(data_demo$glucose_apache)+ #-3.452e+01
                                      I(log(data_demo$glucose_apache)^2)+ #6.784e+00
                                      I(log(data_demo$glucose_apache)^3)+ #-4.406e-01
                                      hematocrit_apache+ #-7.588e-02
                                      I(hematocrit_apache^2)+ #1.117e-03
                                      sodium_apache+ #-3.826e-01
                                      I(sodium_apache^2)+ # 1.333e-03
                                      log(wbc_apache)+ # -1.276e+00
                                      I(log(wbc_apache)^2)+ #3.257e-01
                                      (gcs_eyes_apache + #-1.097e-01
                                      gcs_motor_apache + #-1.665e-01
                                      gcs_verbal_apache) * # -1.262e-01
                                      ventilated_apache, #6.920e-01
                                      #gcs_verbal_apache:ventilated_apacheSi  1.317e-01 
                                    data = data_demo, family = "binomial")
summary(glm_mortalidad_demografica_9)



#escala de glasgow en lugar de diferentes, bajo la prediccion
#bajo la sencibulidad comparado con el modelo anterior
#AIC: 33726
glm_mortalidad_demografica_8 <- glm(hospital_death ~ age + # 5.282e-02
                                      I(age^2)+ #-1.610e-04
                                      bmi + #-6.879e-02
                                      I(bmi^2) + # 9.003e-04
                                      elective_surgery +  # -1.082e+00 
                                      gender + # (Masculino) 7.834e-02
                                      icu_stay_type + #(typetransfer)-2.953e-01
                                      icu_type + # si hay asociacion
                                      preIcuLosDays_cat +  #(L) 7.227e-01
                                      apache_post_operative + #-3.915e-01
                                      heart_rate_apache + #-1.665e-02
                                      I(heart_rate_apache^2) + #1.314e-04
                                      map_apache + #-4.444e-02
                                      I(map_apache^2)+ #1.945e-04
                                      resprate_apache + #2.693e-02
                                      I(resprate_apache^2)+ #-2.142e-04
                                      temp_apache+ #-4.523e+00
                                      I(temp_apache^2)+ #5.950e-02
                                      (escala_glasgow * # -1.534e-01
                                      ventilated_apache), #5.508e-01
                                    data = data_demo, family = "binomial")
summary(glm_mortalidad_demografica_8)


#aunentar interaccion entre Glasgow y ventilacion mecanica
#AIC: 33645
glm_mortalidad_demografica_7 <- glm(hospital_death ~ age + # 5.282e-02
                                      I(age^2)+ #-1.610e-04
                                      bmi + #-6.879e-02
                                      I(bmi^2) + # 9.003e-04
                                      elective_surgery +  # -1.082e+00 
                                      gender + # (Masculino) 7.834e-02
                                      icu_stay_type + #(typetransfer)-2.953e-01
                                      icu_type + # si hay asociacion
                                      preIcuLosDays_cat +  #(L) 7.227e-01
                                      apache_post_operative + #-3.915e-01
                                      heart_rate_apache + #-1.665e-02
                                      I(heart_rate_apache^2) + #1.314e-04
                                      map_apache + #-4.444e-02
                                      I(map_apache^2)+ #1.945e-04
                                      resprate_apache + #2.693e-02
                                      I(resprate_apache^2)+ #-2.142e-04
                                      temp_apache+ #-4.523e+00
                                      I(temp_apache^2)+ #5.950e-02
                                      (gcs_eyes_apache + #-1.863e-01
                                      gcs_motor_apache + #-1.549e-01
                                      gcs_verbal_apache)* # -1.534e-01
                                      ventilated_apache, #5.508e-01
                                    data = data_demo, family = "binomial")
summary(glm_mortalidad_demografica_7)

#aunentar Glasgow
#AIC: 34305
glm_mortalidad_demografica_6 <- glm(hospital_death ~ age + # 5.841e-02
                                      I(age^2)+ #-2.070e-04
                                      bmi + #-6.705e-02
                                      I(bmi^2) + # 9.271e-04
                                      elective_surgery +  # -1.095e+00 
                                      gender + # (Masculino) 9.196e-02
                                      icu_stay_type + #(typetransfer)-2.937e-01
                                      icu_type + # si hay asociacion
                                      preIcuLosDays_cat +  #(L) 7.513e-01
                                      apache_post_operative + #-2.349e-01
                                      heart_rate_apache + #-1.550e-02
                                      I(heart_rate_apache^2) + #1.292e-04
                                      map_apache + #-4.799e-02
                                      I(map_apache^2)+ #2.103e-04
                                      resprate_apache + #2.726e-02
                                      I(resprate_apache^2)+ #-1.953e-04
                                      temp_apache+ #-4.879e+00
                                      I(temp_apache^2)+ #6.419e-02
                                      gcs_eyes_apache + #-1.994e-01
                                      gcs_motor_apache + #-2.065e-01
                                      gcs_verbal_apache, #-1.553e-01
                                    data = data_demo, family = "binomial")
summary(glm_mortalidad_demografica_6)

#aunentar signos vitales
#AIC: 37949
glm_mortalidad_demografica_5 <- glm(hospital_death ~ age + # 5.417e-02
                                      I(age^2)+ #-2.088e-04
                                      bmi + #-6.999e-02
                                      I(bmi^2) + # 9.483e-04
                                      elective_surgery +  # -1.160e+00 
                                      gender + # (Masculino) 9.944e-0
                                      icu_stay_type + #si hay asociacion
                                      icu_type + # si hay asociacion
                                      preIcuLosDays_cat +  #(L) 7.575e-01
                                      apache_post_operative + #no se ve asociacion
                                      heart_rate_apache + #-1.535e-02
                                      I(heart_rate_apache^2) + #1.359e-04
                                      map_apache + #-5.693e-02
                                      I(map_apache^2)+ #2.542e-04
                                      resprate_apache + #2.096e-02
                                      I(resprate_apache^2)+ #-1.194e-04
                                      temp_apache+ #-9.051e+00
                                      I(temp_apache^2), #1.205e-01
                                    data = data_demo, family = "binomial")
summary(glm_mortalidad_demografica_5)

#pareceido al modelo 3 pero las variables 
#pero las variables continuas en lugar de hacerlas categoricas se las hizo polinomiales
#AIC: 45176
glm_mortalidad_demografica_4 <- glm(hospital_death ~ age + # 4.124e-02
                                      I(age^2)+ #-1.152e-04
                                      bmi + #-8.365e-02
                                      I(bmi^2) + # 1.102e-03
                                      elective_surgery +  # -1.392e+00 
                                      gender + # (Masculino) 3.849e-02
                                      icu_stay_type + #si hay asociacion
                                      icu_type + # si hay asociacion
                                      preIcuLosDays_cat +  #(L) 8.473e-01
                                      apache_post_operative,
                                    data = data_demo, family = "binomial")
summary(glm_mortalidad_demografica_4)




# Modelamiento de variables demograficas ----------------------------------
#AIC: 45212
glm_mortalidad_demografica_3 <- glm(hospital_death ~ age_cat + # asociacion positiva 2.7
                                      bmi_cat + # cambiarla a variable ordinar
                                      elective_surgery +  # asociacion negativa
                                      gender + # no hay asociacion
                                      icu_stay_type + #si hay asociacion
                                      icu_type + # si hay asociacion
                                      preIcuLosDays_cat +  #si hay asociacion
                                      apache_post_operative, # si hay asociacion
                                    data = data_demo, family = "binomial")
summary(glm_mortalidad_demografica_3)


# con la funcion estp se define que 12 variables seria las importantes para el modelo
# AIC Step:  AIC=45217.36
#hospital_death ~ hospital_id + age + elective_surgery + ethnicity + 
#  height + hospital_admit_source + icu_admit_source + icu_stay_type + 
#  icu_type + pre_icu_los_days + weight + apache_post_operative

glm_mortalidad_demografica_2 <- step(glm_mortalidad_demografica_1, direction = "backward")


# regresion con todas las variables demograficas
#AIC: 45213
glm_mortalidad_demografica_1 <- glm(hospital_death ~ hospital_id + 
                                    age + # asociacion positiva 2.7
                                    bmi + # cambiarla a variable ordinar
                                    elective_surgery +  # asociacion negativa
                                    ethnicity + #los hispanos y los nativos americanos
                                    gender + # no hay asociacion
                                    height + # no hay asocaicion
                                    hospital_admit_source + # asociacion con adminsion directa, emergencia, floor, otro hospital, otro UCI, step Down Unit
                                    icu_admit_source + # no hay asociacion
                                    icu_stay_type + #si hay asociacion
                                    icu_type + # si hay asociacion
                                    pre_icu_los_days +  #si hay asociacion
                                    weight + # poca asociacion
                                    apache_post_operative, # si hay asociacion
                                  data = data_demo, family = "binomial")
summary(glm_mortalidad_demografica_1)


# mide la colinelealidad basada en la inflacion de la varianza de los factores, 
#si es por encima de 5 hay problemas
#ninguno tiene valor por encima de 5

vif(glm_mortalidad_demografica_3) # mide la colinelealidad basada en la inflacion de la varianza de los factores, si es por encima de 5 hay problemas

#El kappa evalua multicolinealidad
# menor a 100 no hay multicolinealiridad
# entre 100 a 1000 hay poca multicolinealidad
# mas de mil hay severa multicolinealidad
# en este tenemos severa porque dio un valor kappa de 13669982
kappa(glm_mortalidad_demografica_3)

# se eliminana columans con NA para poder tener igual numero de variables y las del modelo 
data_demo <-data[!(is.na(data_demo$age_cat) | 
                is.na(data_demo$bmi_cat) | 
                is.na(data_demo$elective_surger) | 
                is.na(data_demo$gender)|
                is.na(data_demo$icu_stay_typ)|
                is.na(data_demo$icu_type)|
                is.na(data_demo$preIcuLosDays_cat) |
                is.na(data_demo$apache_post_operative)),]

#este comando sirve para predecir la mortalidad de cada uno de los pacientes
predict_glm_demografica = predict(glm_mortalidad_demografica, type = "response")

#pendiente aprender a calcular curva roc
roc_demograficas <- pROC::roc(data$hospital_death, predict_glm_demografica)
plot(roc_demograficas) # crea el grafico de la curva roc
auc(roc_demograficas)

#calculo de pseudo r
#libreria DescTool
PseudoR2(glm_mortalidad_demografica_3, c("McFadden", "Nagel", "Efron"))

#Calculo de McFadden's Pseudo R2
ll.null <- glm_mortalidad_demografica_3$null.deviance/-2 #log-likelihoof of de null model -> la desviacion nula dividida para -2
ll.proposed <- glm_mortalidad_demografica_3$deviance/-2 #log-likelihood del fancy model -> desviacion residual dividida para -2
#Pseudo R2 es 0.058
(ll.null-ll.proposed)/ll.null

#calculo del valor p del McFadden's pseudo Rs es <0.0001 
1 - pchisq(2*(ll.proposed -ll.null), df = (length(glm_mortalidad_demografica_3$coefficients) - 1))



# MODELAMIENTO CON TIDYMODEL -------------------------

# Crear base de datos
data_modelo <- data.frame(data_demo$hospital_death,
                          data_demo$gender,
                          data_demo$age_cat, 
                          data_demo$age_cat2,
                          data_demo$elective_surgery,
                          data_demo$preIcuLosDays_cat,
                          data_demo$icu_stay_type,
                          data_demo$icu_type,
                          data_demo$apache_post_operative)
str(data_modelo)

# Preprosezamiento con tidymodel
rct_dataModelo <- data_modelo %>% recipe(data_demo.hospital_death ~ . ) %>%
  step_normalize( all_numeric(), -all_outcomes()) %>% # Normalizacion
  step_other(all_nominal(), -all_outcomes() ) %>% #tienen menos de un 5%
  step_dummy(all_nominal(), -all_outcomes() ) %>% # Dummy
  step_nzv(all_predictors()) %>%
  themis::step_upsample(data_demo.hospital_death, over_ratio = 0.9, skip= TRUE, seed= 123)

##  
table(data_modelo$data_demo.hospital_death)
recetaoreoarada <- prep(rct_dataModelo, training = data_modelo)
trainpreprocesado <- bake(recetaoreoarada, new_data = NULL)
table(trainpreprocesado$data_demo.hospital_death)

names(trainpreprocesado)

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

#entrenamiento del modelo
reglog_fitted <- fit(reglog_wflow, data = data_modelo)

reglog_fitted %>% tidy

data_modelo %>%
  predict(reglog_fitted, new_data = . ) %>%
  mutate(Real= data_modelo$data_demo.hospital_death) %>%
  conf_mat(truth = Real, estimate = .pred_class ) %>%
  summary
