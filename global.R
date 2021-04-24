# Por último, la función shinyApp crea objetos 
# de la aplicación shiny a partir de ui y de server.
# library(shiny)
library(ggplot2)
library(shinythemes)
library(vioplot)
library(rsconnect)
library(stats)
library(tidyverse) # Manipulación de datos y ggplot2
library(magrittr) # pipe
library(dplyr)
#library(sm)
#library(zoo)
library(rsample)


#setear la carpeta donde estan los datos
#setwd("C:/Users/DELL/OneDrive/Escritorio/clasesShinySEE/datathon/Shiny")


#setwd(directory/in/my/computer)

training_v2 <- read.csv(file = "training_v2.csv")
#unlabeled <- read.csv(file = "unlabeled.csv")

fpe <-  training_v2

# Limpieza de base de datos -----------------------------------------------

fpe$encounter_id <- NULL #se retira porque es un identificador
fpe$patient_id <- NULL #se retira porque es un identificador
fpe$icu_id <- NULL #se retira porque es un identificador
fpe$readmission_status <- NULL # se retira porque todos son 0
fpe$icu_admit_type <- NULL # se retira porque no tiene elementos
fpe$ethnicity  # No se usa porque parece que esta variable no influye, hispanos son 3.600 y nativos 770, son los que tienen mayor prevalencia de mortalidad pero es muy baja
fpe$height  # se retira porque parece ser que no influye 
fpe$weight  # se retira porque su influencia parece ya estar recogida en BMI
fpe$pre_icu_los_days <- abs(fpe$pre_icu_los_days) #se transforman los numeros negativos en absolutos porque asumo error al digitar y en algunos centros lo reportaron como negativo
fpe$pre_icu_los_days <- round(fpe$pre_icu_los_days, digits = 0) #se le quita los decimales al los dias de hospitalizacion
fpe$hospital_death <- factor(fpe$hospital_death,
                              labels = c("Sobrevivió", 
                                         "Falleció"))

fpe$elective_surgery <- factor(fpe$elective_surgery,
                                labels = c("No", 
                                           "Si"))
fpe[fpe$gender == "",] <- NA #transformar los espacios vacios en NA

#crear nuevas variables categoricas
fpe <- mutate(fpe,
               age_cat = cut(fpe$age, breaks = c(0, 30, 50, 60, 70, 80, Inf),
                             levels(c) <- c("Menor de 30 años", 
                                            "De 31 a 50 años", 
                                            "De 51 a 60 años", 
                                            "De 61 a 70 años", 
                                            "De 71 a 80 años", 
                                            "Mas de 80 años"),
                             ordered_result = TRUE)
)

fpe <- mutate(fpe,
               bmi_cat = cut(fpe$bmi, breaks = c(0, 18.5, 25, 30, 35., 40, Inf),
                             levels(c) <- c("Bajo peso",
                                            "Peso normal", 
                                            "Sobrepeso", 
                                            "Obesidad grado I", 
                                            "Obesidad grado II", 
                                            "Obesidad grado III"),
                             ordered_result = TRUE)
)

fpe <- mutate(fpe,
               preIcuLosDays_cat = cut(fpe$pre_icu_los_days, breaks = c(-Inf, 1, 7, Inf),
                                       levels(c) <- c("Menos de 2 días",
                                                      "De 2 a una semana", 
                                                      "Mas de una semana"),
                                       ordered_result = TRUE)
)

fpe <- mutate(fpe, escala_glasgow = gcs_eyes_apache + gcs_motor_apache + gcs_verbal_apache)



xyclick <- function(e){
  e$x 
  e$y
}


