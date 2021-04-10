# La función server contiene las instrucciones
# que su computadora necesita para crear su aplicación.

# Definir el código que el servidor necesita
# para dibujar un histograma ----
server <- function(input, output) { # server es una función
  # con argumentos de entrada y de salida
  
  
  
  # arreglo <- names(fpe)
  # 
  # crear_inputDatos = function(nombre){
  #   nombre = fpe$nombre
  # }
  
  # for (i in 1:length(arreglo)){
  #   x <- i
  #   nombres_datos<- paste("'", arreglo, "'", " = ", "fpe$", arreglo, sep = "" )
  # }

# Analisis de univariables ------------------------------------------------

  
  # Devolver el conjunto de datos solicitado basado en las opcionse a analizar ----
  datos_entrada <- reactive({
    switch(input$datos,  
           "encounter_id" = fpe$encounter_id,
           "patient_id" = fpe$patient_id,
           "hospital_id" = fpe$hospital_id,
           "hospital_death" = fpe$hospital_death,
           "age" = fpe$age,
           "bmi" = fpe$bmi,
           "elective_surgery" = fpe$elective_surgery,
           "ethnicity" = fpe$ethnicity,
           "gender" = fpe$gender,
           "height" = fpe$height,
           "ospital_admit_source" = fpe$ospital_admit_source,
           "icu_admit_source" = fpe$icu_admit_source, #12
           "icu_id" = fpe$icu_id,
           "icu_stay_type" = fpe$icu_stay_type,
           "icu_type" = fpe$icu_type,
           "pre_icu_los_days" = fpe$pre_icu_los_days,
           "readmission_status" = fpe$readmission_status,
           "weight" = fpe$weight,
           "albumin_apache" = fpe$albumin_apache,
           "apache_2_diagnosis" = fpe$apache_2_diagnosis,
           "apache_3j_diagnosis" = fpe$apache_3j_diagnosis,
           "apache_post_operative" = fpe$apache_post_operative,
           "arf_apache" = fpe$arf_apache,
           "bilirubin_apache" = fpe$bilirubin_apache, #24
           "bun_apache" = fpe$bun_apache,
           "creatinine_apache" = fpe$creatinine_apache,
           "fio2_apache" = fpe$fio2_apache,
           "gcs_eyes_apache" = fpe$gcs_eyes_apache,
           "gcs_motor_apache" = fpe$gcs_motor_apache,
           "gcs_unable_apache" = fpe$gcs_unable_apache,
           "gcs_verbal_apache" = fpe$gcs_verbal_apache,
           "glucose_apache" = fpe$glucose_apache,
           "heart_rate_apache" = fpe$heart_rate_apache,
           "hematocrit_apache" = fpe$hematocrit_apache,
           "intubated_apache" = fpe$intubated_apache, #35
           "map_apache" = fpe$map_apache,
           "paco2_apache" = fpe$paco2_apache,
           "paco2_for_ph_apache" = fpe$paco2_for_ph_apache,
           "pao2_apache" = fpe$pao2_apache,
           "ph_apache" = fpe$ph_apache,
           "resprate_apache" = fpe$resprate_apache,
           "sodium_apache" = fpe$sodium_apache,
           "temp_apache" = fpe$temp_apache,
           "urineoutput_apache" = fpe$urineoutput_apache,
           "ventilated_apache" = fpe$ventilated_apache,
           "wbc_apache" = fpe$wbc_apache, #46
           "d1_diasbp_invasive_max" = fpe$d1_diasbp_invasive_max,
           "d1_diasbp_invasive_min" = fpe$d1_diasbp_invasive_min,
           "d1_diasbp_max" = fpe$d1_diasbp_max,
           "d1_diasbp_min" = fpe$d1_diasbp_min,
           "d1_diasbp_noninvasive_max" = fpe$d1_diasbp_noninvasive_max,
           "d1_diasbp_noninvasive_min" = fpe$d1_diasbp_noninvasive_min,
           "d1_heartrate_max" = fpe$d1_heartrate_max,
           "d1_heartrate_min" = fpe$d1_heartrate_min,
           "d1_mbp_invasive_max" = fpe$d1_mbp_invasive_max,
           "d1_mbp_invasive_min" = fpe$d1_mbp_invasive_min,
           "d1_mbp_max" = fpe$d1_mbp_max,
           "d1_mbp_min" = fpe$d1_mbp_min,
           "d1_mbp_noninvasive_max" = fpe$d1_mbp_noninvasive_max,
           "d1_mbp_noninvasive_min" = fpe$d1_mbp_noninvasive_min, #60
           "d1_resprate_max" = fpe$d1_resprate_max,
           "d1_resprate_min" = fpe$d1_resprate_min,
           "d1_spo2_max" = fpe$d1_spo2_max,
           "d1_spo2_min" = fpe$d1_spo2_min,
           "d1_sysbp_invasive_max" = fpe$d1_sysbp_invasive_max,
           "d1_sysbp_invasive_min" = fpe$d1_sysbp_invasive_min,
           "d1_sysbp_max" = fpe$d1_sysbp_max,
           "d1_sysbp_min" = fpe$d1_sysbp_min,
           "d1_sysbp_noninvasive_max" = fpe$d1_sysbp_noninvasive_max,
           "d1_sysbp_noninvasive_min" = fpe$d1_sysbp_noninvasive_min,
           "d1_temp_max" = fpe$d1_temp_max,
           "d1_temp_min" = fpe$d1_temp_min,
           "h1_diasbp_invasive_max" = fpe$h1_diasbp_invasive_max,
           "h1_diasbp_invasive_min" = fpe$h1_diasbp_invasive_min,
           "h1_diasbp_max" = fpe$h1_diasbp_max,
           "h1_diasbp_min" = fpe$h1_diasbp_min,
           "h1_diasbp_noninvasive_max" = fpe$h1_diasbp_noninvasive_max,
           "h1_diasbp_noninvasive_min" = fpe$h1_diasbp_noninvasive_min,
           "h1_heartrate_max" = fpe$h1_heartrate_max,
           "h1_heartrate_min" = fpe$h1_heartrate_min,
           "h1_sysbp_invasive_max" = fpe$h1_sysbp_invasive_max,
           "h1_sysbp_invasive_min" = fpe$h1_sysbp_invasive_min,
           "h1_sysbp_max" = fpe$h1_sysbp_max,
           "h1_sysbp_min" = fpe$h1_sysbp_min,
           "h1_sysbp_noninvasive_max" = fpe$h1_sysbp_noninvasive_max,
           "h1_sysbp_noninvasive_min" = fpe$h1_sysbp_noninvasive_min,
           "h1_temp_max" = fpe$h1_temp_max,
           "h1_temp_min" = fpe$h1_temp_min,
           "d1_albumin_max" = fpe$d1_albumin_max,
           "d1_albumin_min" = fpe$d1_albumin_min, #100
           "d1_bilirubin_max" = fpe$d1_bilirubin_max,
           "d1_bilirubin_min" = fpe$d1_bilirubin_min,
           "d1_bun_max" = fpe$d1_bun_max,
           "d1_bun_min" = fpe$d1_bun_min,
           "d1_calcium_max" = fpe$d1_calcium_max,
           "d1_calcium_min" = fpe$d1_calcium_min,
           "d1_creatinine_max" = fpe$d1_creatinine_max,
           "d1_creatinine_min" = fpe$d1_creatinine_min,
           "d1_glucose_max" = fpe$d1_glucose_max,
           "d1_glucose_min" = fpe$d1_glucose_min,
           "d1_hco3_max" = fpe$d1_hco3_max,
           "d1_hco3_min" = fpe$d1_hco3_min,
           "d1_hemaglobin_max" = fpe$d1_hemaglobin_max,
           "d1_hemaglobin_min" = fpe$d1_hemaglobin_min,
           "d1_hematocrit_max" = fpe$d1_hematocrit_max,
           "d1_hematocrit_min" = fpe$d1_hematocrit_min,
           "d1_inr_max" = fpe$d1_inr_max,
           "d1_inr_min" = fpe$d1_inr_min,
           "d1_lactate_max" = fpe$d1_lactate_max,
           "d1_lactate_min" = fpe$d1_lactate_min, #120
           "d1_platelets_max" = fpe$d1_platelets_max,
           "d1_platelets_min" = fpe$d1_platelets_min,
           "d1_potassium_max" = fpe$d1_potassium_max,
           "d1_potassium_min" = fpe$d1_potassium_min,
           "d1_sodium_max" = fpe$d1_sodium_max,
           "d1_sodium_min" = fpe$d1_sodium_min,
           "d1_wbc_max" = fpe$d1_wbc_max,
           "d1_wbc_min" = fpe$d1_wbc_min,
           "h1_albumin_max" = fpe$h1_albumin_max,
           "h1_albumin_min" = fpe$h1_albumin_min, #130
           "h1_bilirubin_max" = fpe$h1_bilirubin_max,
           "h1_bilirubin_min" = fpe$h1_bilirubin_min,
           "h1_bun_max" = fpe$h1_bun_max,
           "h1_bun_min" = fpe$h1_bun_min,
           "h1_calcium_max" = fpe$h1_calcium_max,
           "h1_calcium_min" = fpe$h1_calcium_min,
           "h1_creatinine_max" = fpe$h1_creatinine_max,
           "h1_creatinine_min" = fpe$h1_creatinine_min,
           "h1_glucose_max" = fpe$h1_glucose_max,
           "h1_glucose_min" = fpe$h1_glucose_min,
           "h1_hco3_max" = fpe$h1_hco3_max,
           "h1_hco3_min" = fpe$h1_hco3_min,
           "h1_hemaglobin_max" = fpe$h1_hemaglobin_max,
           "h1_hemaglobin_min" = fpe$h1_hemaglobin_min,
           "h1_inr_max" = fpe$h1_inr_max,
           "h1_inr_min" = fpe$h1_inr_min,
           "h1_lactate_max" = fpe$h1_lactate_max,
           "h1_lactate_min" = fpe$h1_lactate_min, #150
           "h1_platelets_max" = fpe$h1_platelets_max,
           "h1_platelets_min" = fpe$h1_platelets_min,
           "h1_potassium_max" = fpe$h1_potassium_max,
           "h1_potassium_min" = fpe$h1_potassium_min,
           "h1_sodium_max" = fpe$h1_sodium_max,
           "h1_sodium_min" = fpe$h1_sodium_min,
           "h1_wbc_max" = fpe$h1_wbc_max,
           "h1_wbc_min" = fpe$h1_wbc_min,
           "d1_arterial_pco2_max" = fpe$d1_arterial_pco2_max,
           "d1_arterial_pco2_min" = fpe$d1_arterial_pco2_min, #160
           "d1_arterial_ph_max" = fpe$d1_arterial_ph_max,
           "d1_arterial_ph_min" = fpe$d1_arterial_ph_min,
           "d1_arterial_po2_max" = fpe$d1_arterial_po2_max,
           "d1_arterial_po2_min" = fpe$d1_arterial_po2_min,
           "d1_pao2fio2ratio_max" = fpe$d1_pao2fio2ratio_max,
           "d1_pao2fio2ratio_min" = fpe$d1_pao2fio2ratio_min,
           "h1_arterial_pco2_max" = fpe$h1_arterial_pco2_max,
           "h1_arterial_pco2_min" = fpe$h1_arterial_pco2_min,
           "h1_arterial_ph_max" = fpe$h1_arterial_ph_max,
           "h1_arterial_ph_min" = fpe$h1_arterial_ph_min,
           "h1_arterial_po2_max" = fpe$h1_arterial_po2_max,
           "h1_arterial_po2_min" = fpe$h1_arterial_po2_min,
           "h1_pao2fio2ratio_max" = fpe$h1_pao2fio2ratio_max,
           "h1_pao2fio2ratio_min" = fpe$h1_pao2fio2ratio_min,
           "apache_4a_hospital_death_prob" = fpe$apache_4a_hospital_death_prob,
           "apache_4a_icu_death_prob" = fpe$apache_4a_icu_death_prob,
           "aids" = fpe$aids,
           "cirrhosis" = fpe$cirrhosis,
           "diabetes_mellitus" = fpe$diabetes_mellitus,
           "hepatic_failure" = fpe$hepatic_failure,
           "immunosuppression" = fpe$immunosuppression,
           "leukemia" = fpe$leukemia,
           "lymphoma" = fpe$lymphoma,
           "solid_tumor_with_metastasis" = fpe$solid_tumor_with_metastasis,
           "apache_3j_bodysystem" = fpe$apache_3j_bodysystem,
           "apache_2_bodysystem" = fpe$apache_2_bodysystem
           )
  })
  
  output$summary <- renderPrint({
    summary(datos_entrada())
  })
  
  # mostrar las primeras "n" observaciones ----
  output$view <- renderTable({
    head(datos_entrada(), n = input$obs)
  })
  
  # Histograma de los datos  ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # para renderPlot para indicar que:
  #
  # 1. Es "reactivo" y por lo tanto se actualiza
  # automáticamente
  # 2. Su tipo de salida es un grafico

  output$bar <- renderPlot({
    barplot(table(datos_entrada()))
    title(paste("Gráfico de caja de la variable",input$datos), col.main="red")
  })
    
  output$caja <- renderPlot({
    boxplot(datos_entrada(), col="lavender")
    title(paste("Gráfico de caja de la variable",input$datos), col.main="red")
  })


# Analisis bivariado con variable cuantitativa ----------------------------

  
  
  datos_entrada2 <- reactive({
    switch(input$datos2,
           "encounter_id" = fpe$encounter_id,
           "patient_id" = fpe$patient_id,
           "hospital_id" = fpe$hospital_id,
           "hospital_death" = fpe$hospital_death,
           "age" = fpe$age,
           "bmi" = fpe$bmi,
           "elective_surgery" = fpe$elective_surgery,
           "ethnicity" = fpe$ethnicity,
           "gender" = fpe$gender,
           "height" = fpe$height,
           "ospital_admit_source" = fpe$ospital_admit_source,
           "icu_admit_source" = fpe$icu_admit_source, #12
           "icu_id" = fpe$icu_id,
           "icu_stay_type" = fpe$icu_stay_type,
           "icu_type" = fpe$icu_type,
           "pre_icu_los_days" = fpe$pre_icu_los_days,
           "readmission_status" = fpe$readmission_status,
           "weight" = fpe$weight,
           "albumin_apache" = fpe$albumin_apache,
           "apache_2_diagnosis" = fpe$apache_2_diagnosis,
           "apache_3j_diagnosis" = fpe$apache_3j_diagnosis,
           "apache_post_operative" = fpe$apache_post_operative,
           "arf_apache" = fpe$arf_apache,
           "bilirubin_apache" = fpe$bilirubin_apache, #24
           "bun_apache" = fpe$bun_apache,
           "creatinine_apache" = fpe$creatinine_apache,
           "fio2_apache" = fpe$fio2_apache,
           "gcs_eyes_apache" = fpe$gcs_eyes_apache,
           "gcs_motor_apache" = fpe$gcs_motor_apache,
           "gcs_unable_apache" = fpe$gcs_unable_apache,
           "gcs_verbal_apache" = fpe$gcs_verbal_apache,
           "glucose_apache" = fpe$glucose_apache,
           "heart_rate_apache" = fpe$heart_rate_apache,
           "hematocrit_apache" = fpe$hematocrit_apache,
           "intubated_apache" = fpe$intubated_apache, #35
           "map_apache" = fpe$map_apache,
           "paco2_apache" = fpe$paco2_apache,
           "paco2_for_ph_apache" = fpe$paco2_for_ph_apache,
           "pao2_apache" = fpe$pao2_apache,
           "ph_apache" = fpe$ph_apache,
           "resprate_apache" = fpe$resprate_apache,
           "sodium_apache" = fpe$sodium_apache,
           "temp_apache" = fpe$temp_apache,
           "urineoutput_apache" = fpe$urineoutput_apache,
           "ventilated_apache" = fpe$ventilated_apache,
           "wbc_apache" = fpe$wbc_apache, #46
           "d1_diasbp_invasive_max" = fpe$d1_diasbp_invasive_max,
           "d1_diasbp_invasive_min" = fpe$d1_diasbp_invasive_min,
           "d1_diasbp_max" = fpe$d1_diasbp_max,
           "d1_diasbp_min" = fpe$d1_diasbp_min,
           "d1_diasbp_noninvasive_max" = fpe$d1_diasbp_noninvasive_max,
           "d1_diasbp_noninvasive_min" = fpe$d1_diasbp_noninvasive_min,
           "d1_heartrate_max" = fpe$d1_heartrate_max,
           "d1_heartrate_min" = fpe$d1_heartrate_min,
           "d1_mbp_invasive_max" = fpe$d1_mbp_invasive_max,
           "d1_mbp_invasive_min" = fpe$d1_mbp_invasive_min,
           "d1_mbp_max" = fpe$d1_mbp_max,
           "d1_mbp_min" = fpe$d1_mbp_min,
           "d1_mbp_noninvasive_max" = fpe$d1_mbp_noninvasive_max,
           "d1_mbp_noninvasive_min" = fpe$d1_mbp_noninvasive_min, #60
           "d1_resprate_max" = fpe$d1_resprate_max,
           "d1_resprate_min" = fpe$d1_resprate_min,
           "d1_spo2_max" = fpe$d1_spo2_max,
           "d1_spo2_min" = fpe$d1_spo2_min,
           "d1_sysbp_invasive_max" = fpe$d1_sysbp_invasive_max,
           "d1_sysbp_invasive_min" = fpe$d1_sysbp_invasive_min,
           "d1_sysbp_max" = fpe$d1_sysbp_max,
           "d1_sysbp_min" = fpe$d1_sysbp_min,
           "d1_sysbp_noninvasive_max" = fpe$d1_sysbp_noninvasive_max,
           "d1_sysbp_noninvasive_min" = fpe$d1_sysbp_noninvasive_min,
           "d1_temp_max" = fpe$d1_temp_max,
           "d1_temp_min" = fpe$d1_temp_min,
           "h1_diasbp_invasive_max" = fpe$h1_diasbp_invasive_max,
           "h1_diasbp_invasive_min" = fpe$h1_diasbp_invasive_min,
           "h1_diasbp_max" = fpe$h1_diasbp_max,
           "h1_diasbp_min" = fpe$h1_diasbp_min,
           "h1_diasbp_noninvasive_max" = fpe$h1_diasbp_noninvasive_max,
           "h1_diasbp_noninvasive_min" = fpe$h1_diasbp_noninvasive_min,
           "h1_heartrate_max" = fpe$h1_heartrate_max,
           "h1_heartrate_min" = fpe$h1_heartrate_min,
           "h1_sysbp_invasive_max" = fpe$h1_sysbp_invasive_max,
           "h1_sysbp_invasive_min" = fpe$h1_sysbp_invasive_min,
           "h1_sysbp_max" = fpe$h1_sysbp_max,
           "h1_sysbp_min" = fpe$h1_sysbp_min,
           "h1_sysbp_noninvasive_max" = fpe$h1_sysbp_noninvasive_max,
           "h1_sysbp_noninvasive_min" = fpe$h1_sysbp_noninvasive_min,
           "h1_temp_max" = fpe$h1_temp_max,
           "h1_temp_min" = fpe$h1_temp_min,
           "d1_albumin_max" = fpe$d1_albumin_max,
           "d1_albumin_min" = fpe$d1_albumin_min, #100
           "d1_bilirubin_max" = fpe$d1_bilirubin_max,
           "d1_bilirubin_min" = fpe$d1_bilirubin_min,
           "d1_bun_max" = fpe$d1_bun_max,
           "d1_bun_min" = fpe$d1_bun_min,
           "d1_calcium_max" = fpe$d1_calcium_max,
           "d1_calcium_min" = fpe$d1_calcium_min,
           "d1_creatinine_max" = fpe$d1_creatinine_max,
           "d1_creatinine_min" = fpe$d1_creatinine_min,
           "d1_glucose_max" = fpe$d1_glucose_max,
           "d1_glucose_min" = fpe$d1_glucose_min,
           "d1_hco3_max" = fpe$d1_hco3_max,
           "d1_hco3_min" = fpe$d1_hco3_min,
           "d1_hemaglobin_max" = fpe$d1_hemaglobin_max,
           "d1_hemaglobin_min" = fpe$d1_hemaglobin_min,
           "d1_hematocrit_max" = fpe$d1_hematocrit_max,
           "d1_hematocrit_min" = fpe$d1_hematocrit_min,
           "d1_inr_max" = fpe$d1_inr_max,
           "d1_inr_min" = fpe$d1_inr_min,
           "d1_lactate_max" = fpe$d1_lactate_max,
           "d1_lactate_min" = fpe$d1_lactate_min, #120
           "d1_platelets_max" = fpe$d1_platelets_max,
           "d1_platelets_min" = fpe$d1_platelets_min,
           "d1_potassium_max" = fpe$d1_potassium_max,
           "d1_potassium_min" = fpe$d1_potassium_min,
           "d1_sodium_max" = fpe$d1_sodium_max,
           "d1_sodium_min" = fpe$d1_sodium_min,
           "d1_wbc_max" = fpe$d1_wbc_max,
           "d1_wbc_min" = fpe$d1_wbc_min,
           "h1_albumin_max" = fpe$h1_albumin_max,
           "h1_albumin_min" = fpe$h1_albumin_min, #130
           "h1_bilirubin_max" = fpe$h1_bilirubin_max,
           "h1_bilirubin_min" = fpe$h1_bilirubin_min,
           "h1_bun_max" = fpe$h1_bun_max,
           "h1_bun_min" = fpe$h1_bun_min,
           "h1_calcium_max" = fpe$h1_calcium_max,
           "h1_calcium_min" = fpe$h1_calcium_min,
           "h1_creatinine_max" = fpe$h1_creatinine_max,
           "h1_creatinine_min" = fpe$h1_creatinine_min,
           "h1_glucose_max" = fpe$h1_glucose_max,
           "h1_glucose_min" = fpe$h1_glucose_min,
           "h1_hco3_max" = fpe$h1_hco3_max,
           "h1_hco3_min" = fpe$h1_hco3_min,
           "h1_hemaglobin_max" = fpe$h1_hemaglobin_max,
           "h1_hemaglobin_min" = fpe$h1_hemaglobin_min,
           "h1_inr_max" = fpe$h1_inr_max,
           "h1_inr_min" = fpe$h1_inr_min,
           "h1_lactate_max" = fpe$h1_lactate_max,
           "h1_lactate_min" = fpe$h1_lactate_min, #150
           "h1_platelets_max" = fpe$h1_platelets_max,
           "h1_platelets_min" = fpe$h1_platelets_min,
           "h1_potassium_max" = fpe$h1_potassium_max,
           "h1_potassium_min" = fpe$h1_potassium_min,
           "h1_sodium_max" = fpe$h1_sodium_max,
           "h1_sodium_min" = fpe$h1_sodium_min,
           "h1_wbc_max" = fpe$h1_wbc_max,
           "h1_wbc_min" = fpe$h1_wbc_min,
           "d1_arterial_pco2_max" = fpe$d1_arterial_pco2_max,
           "d1_arterial_pco2_min" = fpe$d1_arterial_pco2_min, #160
           "d1_arterial_ph_max" = fpe$d1_arterial_ph_max,
           "d1_arterial_ph_min" = fpe$d1_arterial_ph_min,
           "d1_arterial_po2_max" = fpe$d1_arterial_po2_max,
           "d1_arterial_po2_min" = fpe$d1_arterial_po2_min,
           "d1_pao2fio2ratio_max" = fpe$d1_pao2fio2ratio_max,
           "d1_pao2fio2ratio_min" = fpe$d1_pao2fio2ratio_min,
           "h1_arterial_pco2_max" = fpe$h1_arterial_pco2_max,
           "h1_arterial_pco2_min" = fpe$h1_arterial_pco2_min,
           "h1_arterial_ph_max" = fpe$h1_arterial_ph_max,
           "h1_arterial_ph_min" = fpe$h1_arterial_ph_min,
           "h1_arterial_po2_max" = fpe$h1_arterial_po2_max,
           "h1_arterial_po2_min" = fpe$h1_arterial_po2_min,
           "h1_pao2fio2ratio_max" = fpe$h1_pao2fio2ratio_max,
           "h1_pao2fio2ratio_min" = fpe$h1_pao2fio2ratio_min,
           "apache_4a_hospital_death_prob" = fpe$apache_4a_hospital_death_prob,
           "apache_4a_icu_death_prob" = fpe$apache_4a_icu_death_prob,
           "aids" = fpe$aids,
           "cirrhosis" = fpe$cirrhosis,
           "diabetes_mellitus" = fpe$diabetes_mellitus,
           "hepatic_failure" = fpe$hepatic_failure,
           "immunosuppression" = fpe$immunosuppression,
           "leukemia" = fpe$leukemia,
           "lymphoma" = fpe$lymphoma,
           "solid_tumor_with_metastasis" = fpe$solid_tumor_with_metastasis,
           "apache_3j_bodysystem" = fpe$apache_3j_bodysystem,
           "apache_2_bodysystem" = fpe$apache_2_bodysystem
           )
  })
  
  output$analisis1 <- renderPrint({
    (wilcox.test(datos_entrada2() ~ fpe$hospital_death))$p.value
  })
  
  output$dispers <- renderPlot({
    ggplot(fpe, aes(fpe$datos_entrada2())) + 
      geom_histogram() + 
      facet_grid(fpe$hospital_death ~ .)
  }) 
  output$dispersion <- renderPlot({
    ggplot(fpe, aes(datos_entrada2())) + 
      geom_density() + 
      facet_grid(fpe$hospital_death ~ .)
  })  
  output$dispe <- renderPlot({
     hist(datos_entrada2() ~ fpe$hospital_death, horizontal = TRUE, #y =datos_entrada2() ,
         main = paste("Gráfico de cajas de hospital_death vs",input$datos2) #,
         # ylab = names(fpe)[3],xlab = "",
         # sub="Valor 0 son los que sobrevivieron y valor 1 fallecieron",
         # pch=20,cex = 1.5,# pch se cambia para elegir tipo de punti y cex su tamaño
         # col =  topo.colors(nrow(fpe)),#, # color de los puntos
         # col.axis="blue", # color de los ejes
         # col.lab="dark violet", # color de las etiquetas
         # col.main="red", # color del titulo
         # col.sub="orange", # color del subtitulo
         # fg="turquoise",
         # 
         # 
         # 
         # font.axis=3,
         # font.lab=4,
         # font.main=1,
         # font.sub=2    
         ) 
    
  })

  output$dispersion1 <- renderPlot({
    boxplot(datos_entrada2() ~ fpe$hospital_death,  #y =datos_entrada2() ,
            main = paste("Gráfico de cajas de hospital_death vs",input$datos2),
            xlab = names(fpe)[3],ylab = "Frecuencia",
            sub="Valor 0 son los que sobrevivieron y valor 1 fallecieron",
            pch=20,cex = 1.5,# pch se cambia para elegir tipo de punti y cex su tamaño
            col =  topo.colors(nrow(fpe)),#, # color de los puntos
            col.axis="blue", # color de los ejes
            col.lab="dark violet", # color de las etiquetas
            col.main="red", # color del titulo
            col.sub="orange", # color del subtitulo
            fg="turquoise",
            
            
            
            font.axis=3,
            font.lab=4,
            font.main=1,
            font.sub=2    ) 
    
  })    


# Analisis bivariado con variable cualitativa -----------------------------

  
  
  datos_entrada3 <- reactive({
    switch(input$datos3,
           "encounter_id" = fpe$encounter_id,
           "patient_id" = fpe$patient_id,
           "hospital_id" = fpe$hospital_id,
           "hospital_death" = fpe$hospital_death,
           "age" = fpe$age,
           "bmi" = fpe$bmi,
           "elective_surgery" = fpe$elective_surgery,
           "ethnicity" = fpe$ethnicity,
           "gender" = fpe$gender,
           "height" = fpe$height,
           "ospital_admit_source" = fpe$ospital_admit_source,
           "icu_admit_source" = fpe$icu_admit_source, #12
           "icu_id" = fpe$icu_id,
           "icu_stay_type" = fpe$icu_stay_type,
           "icu_type" = fpe$icu_type,
           "pre_icu_los_days" = fpe$pre_icu_los_days,
           "readmission_status" = fpe$readmission_status,
           "weight" = fpe$weight,
           "albumin_apache" = fpe$albumin_apache,
           "apache_2_diagnosis" = fpe$apache_2_diagnosis,
           "apache_3j_diagnosis" = fpe$apache_3j_diagnosis,
           "apache_post_operative" = fpe$apache_post_operative,
           "arf_apache" = fpe$arf_apache,
           "bilirubin_apache" = fpe$bilirubin_apache, #24
           "bun_apache" = fpe$bun_apache,
           "creatinine_apache" = fpe$creatinine_apache,
           "fio2_apache" = fpe$fio2_apache,
           "gcs_eyes_apache" = fpe$gcs_eyes_apache,
           "gcs_motor_apache" = fpe$gcs_motor_apache,
           "gcs_unable_apache" = fpe$gcs_unable_apache,
           "gcs_verbal_apache" = fpe$gcs_verbal_apache,
           "glucose_apache" = fpe$glucose_apache,
           "heart_rate_apache" = fpe$heart_rate_apache,
           "hematocrit_apache" = fpe$hematocrit_apache,
           "intubated_apache" = fpe$intubated_apache, #35
           "map_apache" = fpe$map_apache,
           "paco2_apache" = fpe$paco2_apache,
           "paco2_for_ph_apache" = fpe$paco2_for_ph_apache,
           "pao2_apache" = fpe$pao2_apache,
           "ph_apache" = fpe$ph_apache,
           "resprate_apache" = fpe$resprate_apache,
           "sodium_apache" = fpe$sodium_apache,
           "temp_apache" = fpe$temp_apache,
           "urineoutput_apache" = fpe$urineoutput_apache,
           "ventilated_apache" = fpe$ventilated_apache,
           "wbc_apache" = fpe$wbc_apache, #46
           "d1_diasbp_invasive_max" = fpe$d1_diasbp_invasive_max,
           "d1_diasbp_invasive_min" = fpe$d1_diasbp_invasive_min,
           "d1_diasbp_max" = fpe$d1_diasbp_max,
           "d1_diasbp_min" = fpe$d1_diasbp_min,
           "d1_diasbp_noninvasive_max" = fpe$d1_diasbp_noninvasive_max,
           "d1_diasbp_noninvasive_min" = fpe$d1_diasbp_noninvasive_min,
           "d1_heartrate_max" = fpe$d1_heartrate_max,
           "d1_heartrate_min" = fpe$d1_heartrate_min,
           "d1_mbp_invasive_max" = fpe$d1_mbp_invasive_max,
           "d1_mbp_invasive_min" = fpe$d1_mbp_invasive_min,
           "d1_mbp_max" = fpe$d1_mbp_max,
           "d1_mbp_min" = fpe$d1_mbp_min,
           "d1_mbp_noninvasive_max" = fpe$d1_mbp_noninvasive_max,
           "d1_mbp_noninvasive_min" = fpe$d1_mbp_noninvasive_min, #60
           "d1_resprate_max" = fpe$d1_resprate_max,
           "d1_resprate_min" = fpe$d1_resprate_min,
           "d1_spo2_max" = fpe$d1_spo2_max,
           "d1_spo2_min" = fpe$d1_spo2_min,
           "d1_sysbp_invasive_max" = fpe$d1_sysbp_invasive_max,
           "d1_sysbp_invasive_min" = fpe$d1_sysbp_invasive_min,
           "d1_sysbp_max" = fpe$d1_sysbp_max,
           "d1_sysbp_min" = fpe$d1_sysbp_min,
           "d1_sysbp_noninvasive_max" = fpe$d1_sysbp_noninvasive_max,
           "d1_sysbp_noninvasive_min" = fpe$d1_sysbp_noninvasive_min,
           "d1_temp_max" = fpe$d1_temp_max,
           "d1_temp_min" = fpe$d1_temp_min,
           "h1_diasbp_invasive_max" = fpe$h1_diasbp_invasive_max,
           "h1_diasbp_invasive_min" = fpe$h1_diasbp_invasive_min,
           "h1_diasbp_max" = fpe$h1_diasbp_max,
           "h1_diasbp_min" = fpe$h1_diasbp_min,
           "h1_diasbp_noninvasive_max" = fpe$h1_diasbp_noninvasive_max,
           "h1_diasbp_noninvasive_min" = fpe$h1_diasbp_noninvasive_min,
           "h1_heartrate_max" = fpe$h1_heartrate_max,
           "h1_heartrate_min" = fpe$h1_heartrate_min,
           "h1_sysbp_invasive_max" = fpe$h1_sysbp_invasive_max,
           "h1_sysbp_invasive_min" = fpe$h1_sysbp_invasive_min,
           "h1_sysbp_max" = fpe$h1_sysbp_max,
           "h1_sysbp_min" = fpe$h1_sysbp_min,
           "h1_sysbp_noninvasive_max" = fpe$h1_sysbp_noninvasive_max,
           "h1_sysbp_noninvasive_min" = fpe$h1_sysbp_noninvasive_min,
           "h1_temp_max" = fpe$h1_temp_max,
           "h1_temp_min" = fpe$h1_temp_min,
           "d1_albumin_max" = fpe$d1_albumin_max,
           "d1_albumin_min" = fpe$d1_albumin_min, #100
           "d1_bilirubin_max" = fpe$d1_bilirubin_max,
           "d1_bilirubin_min" = fpe$d1_bilirubin_min,
           "d1_bun_max" = fpe$d1_bun_max,
           "d1_bun_min" = fpe$d1_bun_min,
           "d1_calcium_max" = fpe$d1_calcium_max,
           "d1_calcium_min" = fpe$d1_calcium_min,
           "d1_creatinine_max" = fpe$d1_creatinine_max,
           "d1_creatinine_min" = fpe$d1_creatinine_min,
           "d1_glucose_max" = fpe$d1_glucose_max,
           "d1_glucose_min" = fpe$d1_glucose_min,
           "d1_hco3_max" = fpe$d1_hco3_max,
           "d1_hco3_min" = fpe$d1_hco3_min,
           "d1_hemaglobin_max" = fpe$d1_hemaglobin_max,
           "d1_hemaglobin_min" = fpe$d1_hemaglobin_min,
           "d1_hematocrit_max" = fpe$d1_hematocrit_max,
           "d1_hematocrit_min" = fpe$d1_hematocrit_min,
           "d1_inr_max" = fpe$d1_inr_max,
           "d1_inr_min" = fpe$d1_inr_min,
           "d1_lactate_max" = fpe$d1_lactate_max,
           "d1_lactate_min" = fpe$d1_lactate_min, #120
           "d1_platelets_max" = fpe$d1_platelets_max,
           "d1_platelets_min" = fpe$d1_platelets_min,
           "d1_potassium_max" = fpe$d1_potassium_max,
           "d1_potassium_min" = fpe$d1_potassium_min,
           "d1_sodium_max" = fpe$d1_sodium_max,
           "d1_sodium_min" = fpe$d1_sodium_min,
           "d1_wbc_max" = fpe$d1_wbc_max,
           "d1_wbc_min" = fpe$d1_wbc_min,
           "h1_albumin_max" = fpe$h1_albumin_max,
           "h1_albumin_min" = fpe$h1_albumin_min, #130
           "h1_bilirubin_max" = fpe$h1_bilirubin_max,
           "h1_bilirubin_min" = fpe$h1_bilirubin_min,
           "h1_bun_max" = fpe$h1_bun_max,
           "h1_bun_min" = fpe$h1_bun_min,
           "h1_calcium_max" = fpe$h1_calcium_max,
           "h1_calcium_min" = fpe$h1_calcium_min,
           "h1_creatinine_max" = fpe$h1_creatinine_max,
           "h1_creatinine_min" = fpe$h1_creatinine_min,
           "h1_glucose_max" = fpe$h1_glucose_max,
           "h1_glucose_min" = fpe$h1_glucose_min,
           "h1_hco3_max" = fpe$h1_hco3_max,
           "h1_hco3_min" = fpe$h1_hco3_min,
           "h1_hemaglobin_max" = fpe$h1_hemaglobin_max,
           "h1_hemaglobin_min" = fpe$h1_hemaglobin_min,
           "h1_inr_max" = fpe$h1_inr_max,
           "h1_inr_min" = fpe$h1_inr_min,
           "h1_lactate_max" = fpe$h1_lactate_max,
           "h1_lactate_min" = fpe$h1_lactate_min, #150
           "h1_platelets_max" = fpe$h1_platelets_max,
           "h1_platelets_min" = fpe$h1_platelets_min,
           "h1_potassium_max" = fpe$h1_potassium_max,
           "h1_potassium_min" = fpe$h1_potassium_min,
           "h1_sodium_max" = fpe$h1_sodium_max,
           "h1_sodium_min" = fpe$h1_sodium_min,
           "h1_wbc_max" = fpe$h1_wbc_max,
           "h1_wbc_min" = fpe$h1_wbc_min,
           "d1_arterial_pco2_max" = fpe$d1_arterial_pco2_max,
           "d1_arterial_pco2_min" = fpe$d1_arterial_pco2_min, #160
           "d1_arterial_ph_max" = fpe$d1_arterial_ph_max,
           "d1_arterial_ph_min" = fpe$d1_arterial_ph_min,
           "d1_arterial_po2_max" = fpe$d1_arterial_po2_max,
           "d1_arterial_po2_min" = fpe$d1_arterial_po2_min,
           "d1_pao2fio2ratio_max" = fpe$d1_pao2fio2ratio_max,
           "d1_pao2fio2ratio_min" = fpe$d1_pao2fio2ratio_min,
           "h1_arterial_pco2_max" = fpe$h1_arterial_pco2_max,
           "h1_arterial_pco2_min" = fpe$h1_arterial_pco2_min,
           "h1_arterial_ph_max" = fpe$h1_arterial_ph_max,
           "h1_arterial_ph_min" = fpe$h1_arterial_ph_min,
           "h1_arterial_po2_max" = fpe$h1_arterial_po2_max,
           "h1_arterial_po2_min" = fpe$h1_arterial_po2_min,
           "h1_pao2fio2ratio_max" = fpe$h1_pao2fio2ratio_max,
           "h1_pao2fio2ratio_min" = fpe$h1_pao2fio2ratio_min,
           "apache_4a_hospital_death_prob" = fpe$apache_4a_hospital_death_prob,
           "apache_4a_icu_death_prob" = fpe$apache_4a_icu_death_prob,
           "aids" = fpe$aids,
           "cirrhosis" = fpe$cirrhosis,
           "diabetes_mellitus" = fpe$diabetes_mellitus,
           "hepatic_failure" = fpe$hepatic_failure,
           "immunosuppression" = fpe$immunosuppression,
           "leukemia" = fpe$leukemia,
           "lymphoma" = fpe$lymphoma,
           "solid_tumor_with_metastasis" = fpe$solid_tumor_with_metastasis,
           "apache_3j_bodysystem" = fpe$apache_3j_bodysystem,
           "apache_2_bodysystem" = fpe$apache_2_bodysystem
           )
  })

  output$analisis3 <- renderPrint({
    chisq.test(table(fpe$hospital_death, datos_entrada3()))$p.value
  })  
    
  output$tablaFrecCategorica <- renderPrint({
    table(datos_entrada3(), fpe$hospital_death)
  })
  
  output$dispersion2 <- renderPlot({
    barplot(table(fpe$hospital_death, datos_entrada3()), beside = TRUE, 
            main = paste("Gráfico de barras entre hospital_death y ",input$datos3),
            xlab = "hospital_death", ylab = "Frecuencia",
            sub="Valor 0 son los que sobrevivieron y valor 1 fallecieron"
            )
         # main = paste("Gráfico de dispersión de cambio vs",input$datos3),
         # sub="Las cantidades numéricas representan unidades monetarias.",
         # pch=20,cex = 1.5,# pch se cambia para elegir tipo de punti y cex su tamaño
         # col =  topo.colors(nrow(fpe)),#, # color de los puntos
         # col.axis="blue", # color de los ejes
         # col.lab="dark violet", # color de las etiquetas
         # col.main="red", # color del titulo
         # col.sub="orange", # color del subtitulo
         # fg="turquoise",
         # 
         # font.axis=3,
         # font.lab=4,
         # font.main=1,
         # font.sub=2    ) 
   
    
  })

  output$tablaPorcCategorica <- renderPrint({
    round(prop.table(table(datos_entrada3(), fpe$hospital_death), margin = 1)*100, digits = 2)
  })  
  
  output$dispersion3 <- renderPlot({
    barplot(prop.table(table(fpe$hospital_death, datos_entrada3()), margin = 2)*100, beside = TRUE, 
            main = paste("Gráfico de barras entre hospital_death y ",input$datos3),
            xlab = "hospital_death", ylab = "Porcentaje",
            sub="Valor 0 son los que sobrevivieron y valor 1 fallecieron"
    )
    # main = paste("Gráfico de dispersión de cambio vs",input$datos3),
    # sub="Las cantidades numéricas representan unidades monetarias.",
    # pch=20,cex = 1.5,# pch se cambia para elegir tipo de punti y cex su tamaño
    # col =  topo.colors(nrow(fpe)),#, # color de los puntos
    # col.axis="blue", # color de los ejes
    # col.lab="dark violet", # color de las etiquetas
    # col.main="red", # color del titulo
    # col.sub="orange", # color del subtitulo
    # fg="turquoise",
    # 
    # font.axis=3,
    # font.lab=4,
    # font.main=1,
    # font.sub=2    ) 
    
    
  })  
    
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=",input$plot_click$y)
  })
  
  
  datos_entrada4 <- reactive({
    switch(input$datos4,
           "ajuste" = fpe$ajuste,
           "cambio" = fpe$cambio)
  })
  
  output$dispersion4 <- renderPlot({
    plot(x = fpe$producto,y =datos_entrada4() ,
         main = paste("Gráfico de dispersión de producto vs",input$datos4),
         sub="Las cantidades numéricas representan unidades monetarias.",
         pch=20,cex = 1.5,# pch se cambia para elegir tipo de punti y cex su tamaño
         col =  topo.colors(nrow(fpe)),#, # color de los puntos
         col.axis="blue", # color de los ejes
         col.lab="dark violet", # color de las etiquetas
         col.main="red", # color del titulo
         col.sub="orange", # color del subtitulo
         fg="turquoise",
         
         font.axis=3,
         font.lab=4,
         font.main=1,
         font.sub=2    ) 
    
  })
  
  dato_dependiente <- reactive({
    switch(input$dependiente,
           "ajuste" = fpe$ajuste,
           "cambio" = fpe$cambio,
           "producto" = fpe$producto)
  })

  
  output$independiente <- renderUI({
    selectInput("independiente", 
                label = h6("Variable independiente:"), 
                choices = names(fpe)[names(fpe)!= input$dependiente],
                selected = names(fpe)[names(fpe)!=input$dependiente][1])
  })
  
  
  dato_independiente <- reactive({
    switch(input$independiente,
           "ajuste" = fpe$ajuste,
           "cambio" = fpe$cambio,
           "producto" = fpe$producto)
  })
  
  
      
output$gmodelo1 <- renderPlot({
  g <- ggplot(data = NULL, aes(x=dato_independiente(), y=dato_dependiente()))
  # + para añadir más capas de gráficos
  g + geom_point(size=2, color="green",show.legend=T)
})


output$gmodelo2 <- renderPlot({
  g <- ggplot(data = NULL, aes(x=dato_independiente(), y=dato_dependiente()))
  # + para añadir más capas de gráficos
  g + geom_point(size=2, color="green",show.legend=T)+stat_smooth(data = data.frame(dato_independiente(),dato_dependiente()), aes(x=dato_independiente(),y=dato_dependiente()))
})
 

output$gmodelo3 <- renderPlot({
  g <- ggplot(data = NULL, aes(x=dato_independiente(), y=dato_dependiente()))
  # + para añadir más capas de gráficos
  g + geom_point(size=2, color="green",show.legend=T)+ geom_quantile(size=1, color="orange",show.legend=T)
})

dato_dependiente1 <- reactive({
  switch(input$dependiente1,
         "ajuste" = fpe$ajuste,
         "cambio" = fpe$cambio,
         "producto" = fpe$producto)
})

#output$independiente1 guarda la opcion selecccionada (de las no seleccionadas en el primer widget)
output$independiente1 <- renderUI({ # no imprime ningun resultado, ni tiene conexion con ui, crea el objeto que vamos a usar dentro
  selectInput("independiente1", 
              label = h6("Variable independiente:"), 
              choices = names(fpe)[names(fpe)!= input$dependiente1],
              selected = names(fpe)[names(fpe)!=input$dependiente1][1])
})

dato_independiente1 <- reactive({
  switch(input$independiente1,
         "ajuste" = fpe$ajuste,
         "cambio" = fpe$cambio,
         "producto" = fpe$producto)
})

output$texto <- renderText({
  paste(  "Modelo lineal de ",input$dependiente1," vs ",input$independiente1)
})
output$modelo <-  renderPrint({
  summary(lm(dato_dependiente1()~dato_independiente1()))
})

output$texto1 <- renderText({
  modelo <- lm(dato_dependiente1()~dato_independiente1())
  paste(input$dependiente1," = ",round(modelo$coefficients[1],2),
        " + ",round(modelo$coefficients[2],2),input$independiente1)
})

output$residuos <- renderTable({
  modelo <- lm(dato_dependiente1()~dato_independiente1())
  head(modelo$residuals,n=input$obs1)})

output$histograma <- renderPlot({
  modelo <- lm(dato_dependiente1()~dato_independiente1())
  hist(head(modelo$residuals,n=input$obs1),xlab ="residuos",
       col = "violetred4",
       main = paste("Histograma de los residuales de ",input$dependiente1,
       " vs ",input$independiente1))})
output$tabla_resultado <- renderDataTable({
  modelo <- lm(dato_dependiente1()~dato_independiente1())
  data.frame("x"=dato_dependiente1(),
             "y"=dato_independiente1(),
             "valores ajustados"=modelo$fitted.values,
             "residuales"=modelo$residuals)
})




}