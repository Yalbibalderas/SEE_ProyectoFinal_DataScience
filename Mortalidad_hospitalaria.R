library(psych)
library(graphics)
library(stats)
#Existe diferencia en incidencia de mortalidad
# en diferentes UCI

#Crear data frame de la tabla con la info de hopsital_id y mortalidad
#opcion una para crear mortalidad: Mortalidad_hospitalaria <- as.data.frame.matrix(table(fpe$hospital_id, fpe$hospital_death))

Mortalidad_hospitalaria <- fpe %>% 
  group_by(hospital_id, hospital_death) %>% 
  summarise(Cant_sobrevivio = n()) %>%
  pivot_wider(names_from = hospital_death, 
              values_from= Cant_sobrevivio)


# Crear data frame con apacue_4a_icu_death_prob para ver riesgo de mortalidad
Apache_mortalidad <- fpe %>% 
  group_by(hospital_id) %>% 
  summarise(Mediana_Apache_4a = median(apache_4a_icu_death_prob, na.rm = TRUE))


# aumentar la columna con la incidencia de mortalidad en porcentaje
Mortalidad_hospitalaria <- round(mutate(Mortalidad_hospitalaria, 
                                        porcentaje_mortalidad = (Falleció * 100)/(Falleció + Sobrevivió)), 
                                 digits = 2)


# Unir mortalidad hospitalaria y apache_mortalidad

Mortalidad_hospitalaria_apache_4a <- merge(x =  Mortalidad_hospitalaria, y = Apache_mortalidad, by.x = "hospital_id", by.y = "hospital_id")



# Se eliminan a los UCI que atendieron menos de 25 pacientes
# se los elimina porque muchos de estas UCI tenian no teneian mortalidad (porque eran pocos pacientes)
# mientras que otros veian dos pacientes, uno fallecia y ya tenian mortalidad del 50%
Mortalidad_hospitalaria_apache_4a <-filter(Mortalidad_hospitalaria_apache_4a, Sobrevivió >= 25)

#Se crea grafico de densidad para encontrar la Bimodalidad en la curva
# Algunas UCI tienen mortalidad de un 6.5% 
# mientras que otras tienen mortalidad del 11.5%
ggplot(Mortalidad_hospitalaria_apache_4a)+ 
  aes(porcentaje_mortalidad)+
  geom_density()

# La mediana de Apache 4a tiene una distribicion tambien binomal
ggplot(Mortalidad_hospitalaria_apache_4a)+ 
  aes(Mediana_Apache_4a)+
  geom_density()

#xiste correlacion entre la probabilidad de fallcer y el reisgo de fallecer medido por APACHE 4a en los diferentes hospitales
  # si existe esta correlacion y es de un r= 68
pairs.panels(Mortalidad_hospitalaria_apache_4a)

#analisis de regresion lineal simple
lm_mortalidad_apachea4 <- lm(data = Mortalidad_hospitalaria_apache_4a, formula = porcentaje_mortalidad ~ Mediana_Apache_4a)
summary(lm_mortalidad_apachea4)

#grafico de regresion lineal simple
plot(Mortalidad_hospitalaria_apache_4a$Mediana_Apache_4a, Mortalidad_hospitalaria_apache_4a$porcentaje_mortalidad)
abline(lm_mortalidad_apachea4, lwd = 3, col = "red")


#graficos para analizar residuos
par(mfrow = c(2,2))
plot(lm_mortalidad_apachea4)

ar(mfrow = c(1,1))

# se realizo modelo de regresion possion y no se encontro buena correlacion
glm_mortalidad_apachea4 <- glm(data = Mortalidad_hospitalaria_apache_4a, formula = porcentaje_mortalidad ~ Mediana_Apache_4a, family = poisson())
summary(glm_mortalidad_apachea4)

plot(Mortalidad_hospitalaria_apache_4a$Mediana_Apache_4a, Mortalidad_hospitalaria_apache_4a$porcentaje_mortalidad)
abline(glm_mortalidad_apachea4, lwd = 3, col = "red")

par(mfrow = c(2,2))
plot(glm_mortalidad_apachea4)

par(mfrow = c(1,1))


