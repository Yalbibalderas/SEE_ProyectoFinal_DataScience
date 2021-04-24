#
# Importar librerias generales --------------------------------------------

library(openxlsx) # Importar y exportar a excel
library(tidyverse) # Manipulación de datos
library(magrittr) # Pipe %>%, %$%
library(ggplot2) # Gráficos estáticos
library(DataExplorer)
library(tibble)
library(dplyr)
library(mlogit)# para regresion loggistica
library(tidyr)
library(psych)
library(pROC) #para curva roc
library(car) #para ver grafico de residuos
library(lmtest) # para ver residuos, esta la preuba de bptest


# Generar ambiente --------------------------------------------------------


getwd()
setwd("C:/Users/DELL/OneDrive/Escritorio/clasesShinySEE/datathon/Datos")

# Bajar base de datos -----------------------------------------------------


training_v2 <- read.csv(file = "training_v2.csv")
unlabeled <- read.csv(file = "unlabeled.csv")


# Limpiesa de la base de datos --------------------------------------------

#elective_surgery= The common national or cultural tradition which the person belongs to

attach(data) #para no tener que siempre estar poniendo data antes de las variables

data <- training_v2
data$encounter_id <- NULL
data$patient_id <- NULL

# Falla de SNC ------------------------------------------------------------


#crear variable del escaka de glasgow para valorar todo el SNC
data <- mutate(data, escala_glasgow = gcs_eyes_apache + gcs_motor_apache + gcs_verbal_apache)

hist(data$escala_glasgow)
table(data$escala_glasgow)
multi.hist(data$escala_glasgow)

#Crear la variable de escala de glasgow - 15 para que el valor 0 sea todo bien y el valor 12 sea todo mal
data <- mutate(data, escala_glasgow_15 = 15-escala_glasgow)
data <- mutate(data, escala_glasgow_16 = 16-escala_glasgow)
data <- mutate(data, escala_glasgow_16log = log(escala_glasgow_16))

multi.hist(data$escala_glasgow_16log)
data <- data[!is.na(data$escala_glasgow_16log),] #para eliminar las filas con NA


# Crear modelos de regresion con variables del SNC ------------------------

#El primero es el mejor modelo
# La escala de glasgow se la trasnforma a logi 16 para que el menor valor sea 1 (asi no se tiene -inf con valores cero)
# la escala glasgow se le hace un polinomio de 3, que es donde menor ACI tiene
# junto a la interaccion con motor, el deterioro motor parece ser el que mas influye
#ACI 46380


glm_mortalidad_glasgow <- glm(hospital_death ~ poly(escala_glasgow_16log, 3, raw = TRUE) * (gcs_motor_apache), data = data, family = "binomial")

summary(glm_mortalidad_glasgow)


# modelos que tivieron menor ACI----

# La escala de glasgow se la trasnforma a logi 16 para que el menor valor sea 1 (asi no se tiene -inf con valores cero)
# la escala glasgow se le hace un polinomio de 3, que es donde menor ACI tiene
# junto a la interaccion entre ojo y motor
#ACI 46405
glm_mortalidad_glasgow_5 <- glm(hospital_death ~ poly(escala_glasgow_16log, 3, raw = TRUE), data = data, family = "binomial")
summary(glm_mortalidad_glasgow_5)

# interaccion entre las variables
# la inica interaccion que aparece existir es entre gcs_eyes_apache * gcs_motor_apache
#ACI 46506
glm_mortalidad_glasgow_4 <- glm(hospital_death ~ (gcs_eyes_apache * 
                                                    gcs_motor_apache *
                                                    gcs_verbal_apache  * 
                                                    gcs_unable_apache),
                                data = data, family = "binomial")

summary(glm_mortalidad_glasgow_4)

#la escala glasgow con plinomio 4 presenta mejor ACI con sig en todos sus polinomias
#ACI 46417
glm_mortalidad_glasgow_3 <- glm(hospital_death ~ poly(escala_glasgow, 4, raw = TRUE), data = data, family = "binomial")
summary(glm_mortalidad_glasgow_3)

#Regersion simple de escala glasgow 
#ACI 46531
glm_mortalidad_glasgow_5 <- glm(hospital_death ~ escala_glasgow_15, data = data, family = "binomial")
summary(glm_mortalidad_glasgow_2)

#se prueba un modelo con las variables de SNC por separado y no hay mucha diferencia con sumarlas
# la variable gcs_unable_apache parece que no aporta nada al modelo
# la poner los ojo, verval y motor separado no se observo que alguno de los subrupos tiviese mayor beta que los demas, eran similares
# ACI 46530
glm_mortalidad_glasgow_1 <- glm(hospital_death ~ gcs_eyes_apache + 
                                  gcs_motor_apache +
                                  gcs_verbal_apache  + 
                                  gcs_unable_apache,
                                data = data, family = "binomial")

summary(glm_mortalidad_glasgow_1)



#regresion logistica simple
#AIC 46531
glm_mortalidad_glasgow_0 <- glm(hospital_death ~ escala_glasgow, data = data, family = "binomial")
summary(glm_mortalidad_glasgow_0)


# Analisis de los errores del modelo escogido -----------------------------


# la escala de glasgow se asocia estadisticamente significativo a la mortaliad
anova(glm_mortalidad_glasgow, test= "Chisq") #me dice si las variables predictivas son estadisticamente significativas

#a menor menor escala de glasgow mayor mortalidad, un coeficiente beta de -0.2
coef(glm_mortalidad_glasgow) #mide la direccion de la asociacion


# el intervalo de confianza del intersector es de -0.03 a 0.08
# el intervalo de confianza de la escala de glasgow es de -0.21 a -0.2
confint(glm_mortalidad_glasgow, level = 0.95)

#Calculo de McFadden's Pseudo R2
ll.null <- glm_mortalidad_glasgow$null.deviance/-2 #log-likelihoof of de null model -> la desviacion nula dividida para -2
ll.proposed <- glm_mortalidad_glasgow$deviance/-2 #log-likelihood del fancy model -> desviacion residual dividida para -2
#Pseudo R2 es 0.10
(ll.null-ll.proposed)/ll.null

#calculo del valor p del McFadden's pseudo Rs es <0.0001 
1 - pchisq(2*(ll.proposed -ll.null), df = (length(glm_mortalidad_glasgow$coefficients) - 1))

predicted.data <- data.frame(probability.of.hospital_death = glm_mortalidad_glasgow$fitted.values, hospital_dead= data$hospital_death)

#este comando sirve para predecir la mortalidad de cada uno de los pacientes
predict_glm_glasgow = predict(glm_mortalidad_glasgow, type = "response")
round(prop.table(table(predicted.data), margin = 1)*100, digits = 2)
#                     hospital_dead
# probability           0      1
# 0.0399601887060991  96.00   4.00
# 0.0641563258416079  94.46   5.54
# 0.0746362125808144  92.75   7.25
# 0.0773836330402866  91.82   8.18
# 0.0798166047407705  90.52   9.48
# 0.0867163113307332  91.48   8.52
# 0.0970404405624415  88.66  11.34
# 0.0989616607124292  92.35   7.65
# 0.105781019633197   90.05   9.95
# 0.110841934626365   88.24  11.76
# 0.117278939879175   88.27  11.73
# 0.11900331185956    87.40  12.60
# 0.121035383513854   93.10   6.90
# 0.132565823433986   86.02  13.98
# 0.138916584641543   81.40  18.60
# 0.140645964646906   91.40   8.60
# 0.142462049842368   83.46  16.54
# 0.142682820098899   85.88  14.12
# 0.149507808748592   84.39  15.61
# 0.152886450791231   84.26  15.74
# 0.16368045698777    83.33  16.67
# 0.168195158622605   80.00  20.00
# 0.169654617133081   75.00  25.00
# 0.175079032747012   80.36  19.64
# 0.176869812382498  100.00   0.00
# 0.18033848762064   100.00   0.00
# 0.181301774688164   82.94  17.06
# 0.186311461865893   80.81  19.19
# 0.187093817207248  100.00   0.00
# 0.188700043721165  100.00   0.00
# 0.191427208777395   83.33  16.67
# 0.196649476137728   80.00  20.00
# 0.199733480429478   91.67   8.33
# 0.200822077032161  100.00   0.00
# 0.201978622809063   66.67  33.33
# 0.211070387342147  100.00   0.00
# 0.217728998803054   81.95  18.05
# 0.218105863238354   56.52  43.48
# 0.218483197797715   73.68  26.32
# 0.21886100243713    79.25  20.75
# 0.220034432204447   50.00  50.00
# 0.23608736197654    91.67   8.33
# 0.247990352993801   81.25  18.75
# 0.252893840919998   84.75  15.25
# 0.257861039138566   67.90  32.10
# 0.294672600967912   75.00  25.00
# 0.302869445159859   62.25  37.75
# 0.359674419436069   64.07  35.93
#pendiente aprender a calcular curva roc

roc_glasgow <- pROC::roc(data$hospital_death, predict_glm_glasgow)
plot(roc_glasgow) # crea el grafico de la curva roc
plot.roc(roc_glasgow, thresholds = "best") #revisar como hacer este grafico, no me sale
auc(roc_glasgow) #area debajo de la curva 0.7207
coords(roc_glasgow, transpose = FALSE) #crea una tabla con sensiulidad y especificidad de cada punto
power.roc.test(roc_glasgow) # estinma el poder, si es cercano al 1 tiene buen poder

roc_glm_glasgow <- pROC::roc(data$hospital_death, predict_glm_glasgow)

plot(roc_glm_glasgow)
multi.hist(escalo_glm_glasgow)
auc(roc_glm_glasgow) #Area under the curve: 0.7213
coords(roc_glm_glasgow, transpose = FALSE) #sencibilidad y especificidad igual con escala de glasgow. crea una tabla con sensiulidad y especificidad de cada punto

#escalar el resultado de mortalidad por glasgow para determinar riesgo que un paciente tieno de morir por problemas nuerologicos
escalar_glm_glasgow <- (predict_glm_glasgow - min(predict_glm_glasgow))/ (max(predict_glm_glasgow)-min(predict_glm_glasgow))
roc_glm_glasgow_escalar <- pROC::roc(data$hospital_death, escalo_glm_glasgow)
plot(roc_glm_glasgow_escalar)
table(round(escalar_glm_glasgow, digits = 1))

#grafico del residuo, tambien calcula el pr test stat, que mientras sea mayor a 0.05 habla que el modelo no necesita mas variables
residualPlots(glm_mortalidad_glasgow, tipe= "divance")

# mide la colinelealidad basada en la inflacion de la varianza de los factores, 
#si es por encima de 5 hay problemas
# este modelo tiene un problema de colinealidad con el polinimo (obvio es un polinomio)
# este modelo tiene un problema de colinealidad con gcs_motor_apache (obvio es parte del otro)
# este modelo tiene un problema de colinealidad con el polinimo * gcs_motor_apache (obvio es un polinomio y parte del otro)
vif(glm_mortalidad_glasgow) # mide la colinelealidad basada en la inflacion de la varianza de los factores, si es por encima de 5 hay problemas
#poly(escala_glasgow_16log, 3, raw = TRUE)                         22.25574
#gcs_motor_apache                                                 138.42421
#poly(escala_glasgow_16log, 3, raw = TRUE):gcs_motor_apache        15.51617



#El kappa evalua multicolinealidad
# menor a 100 no hay multicolinealiridad
# entre 100 a 1000 hay poca multicolinealidad
# mas de 1000 hay severa multicolinealidad
kappa(glm_mortalidad_glasgow)
# en este tenemos severa porque dio un valor kappa de 6945

#Breusch-Pagan Test para medir homocedasticidad vs heterocedasticidad
# que los reciduos tenganl la misma varianza a lo largo de todos los datos
bptest(glm_mortalidad_glasgow)
# p-value < 2.2e-16 sale que son heterocestasticicos 

#como manejar la heterocestacidad, una forma es transformando la variable otucome a logaritmo
# otra forma es transformando la variable outcome con la funcion BoxCox y escoger un lambda para la misma


install.packages("ResourceSelection")
library(ResourceSelection)
# sirve para predecir si el modelo predice mortalidad,
# valor por encima de 0,05 dice que el modelo si predice la los datos (outcome)
hoslem.test(data$hospital_death, fitted(glm_mortalidad_glasgow)) 
#p-value = 0.9616

#grafico para ver autocorrelacion
acf(residuals(glm_mortalidad_glasgow))
#no hay autocorrelacion
#da un grafico donde la primera linea en grande y las otras son chiquitas dentro de las lineas azules

install.packages("DescTools")
library(DescTools)
#calculo de pseudo r
#libreria DescTool
PseudoR2(glm_mortalidad_glasgow, c("McFadden", "Nagel", "Efron", "AIC"))

#datos de los residuos
par(mfrow = c(2,2))
plot(glm_mortalidad_glasgow_0)
par(mfrow = c(1,1))

barplot(escala_glasgow ~ hospital_death, data)

attach(data) #para no tener que siempre estar poniendo data antes de las variables

mutar_caracter = function(x) {
  data <- data %>% mutate(x= as.character(x))
}

mutar_caracter(hospital_death)
mutar_caracter(elective_surgery)

#data <- data %>% mutate(hospital_death= as.numeric(hospital_death))
#data <- data %>% mutate(elective_surgery= as.character(elective_surgery))



data <- relevel(hospital_death, 0)




# Estadistica descriptiva -------------------------------------------------



summary(data$hospital_admit_source)

table(data$elective_surgery)
barplot(table(data$hospital_death))



View(training_v2)

create_report(training_v2)
glimpse(training_v2)

table(data$elective_surgery)
barplot(table(data$hospital_death))

# regresion logistica -----------------------------------------------------

mortalidad_Model1 = glm(formula = hospital_death ~ age, data = data, family = binomial())
mortalidad_Model2 = glm(formula = hospital_death ~ age + elective_surgery, data = data, family = binomial())


summary(mortalidad_Model2)                        
                  