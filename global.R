# Por último, la función shinyApp crea objetos 
# de la aplicación shiny a partir de ui y de server.
#library(shiny)

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
#setwd("C:/Users/DELL/OneDrive/Escritorio/clasesShinySEE/datathon/Datos")
#setwd(directory/in/my/computer)

training_v2 <- read.csv(file = "training_v2.csv")
#unlabeled <- read.csv(file = "unlabeled.csv")

fpe <-  training_v2

fpe <- fpe %>% 
  mutate(`hospital_death` = factor(`hospital_death`, levels= c(0,1), labels = c("Sobrevivió", "Falleció")))


#fpe <-  read.table("https://drive.google.com/drive/trash?fbclid=IwAR35_tKG4ulIQEdORdvwF6co5Xbn8ZCm8AiLy7_DOxBjYHO2aGADCsrXPZ4")



xyclick <- function(e){
  e$x 
  e$y
}


