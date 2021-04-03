# El objeto de interfaz de usuario (ui) 
# controla el diseño y la apariencia de la aplicación.

shinyUI(navbarPage(theme = shinytheme("cerulean"),
  # titulo de la App  ----
  "Datathom UCI",
  
  # Diseño de la barra lateral con definiciones de entrada
  # y salida ----
  tabPanel( "Descripción de los datos",
  sidebarLayout(
    # Panel de la barra lateral para entradas ----
    sidebarPanel(#"Cargar archivo",
                 #fileInput("GetFile", "Cargar archivo"),
      
      # Entrada: deslizador para el número de entradas ----
      # Entrada: Seleccionador para elegir un elemento
      # de la base de datos ----
      selectInput(inputId = "datos",
                  label = "Escoja una variable:",
                  choices = names(fpe)),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Numero de observaciones a mostrar:",
                   value = 5)# en que valor aparecerá al comienzo
    ),
    
    # Panel principal para mostrar resultados ----
    mainPanel(
      
      # resultado: resumen de los datos ----
      verbatimTextOutput("summary"),
      
      # Salida: tabla HTML con el número 
      # solicitado de observaciones ----
      tableOutput ("view"),

      # Resultado: bar ----
      plotOutput(outputId = "bar"), # tipo de resultado
      # outputId muestra el nombre del objeto creado en server
      # del que se va a imprimir el resultado      
            
      # Resultado: Histograma ----
      plotOutput(outputId = "caja"), # tipo de resultado
      # outputId muestra el nombre del objeto creado en server
      # del que se va a imprimir el resultado
      

      
      # Salida: Tablas / graficos, summarys ----
      tabsetPanel(type = "tabs",
                  tabPanel("Gráfico de violin", #Ajuste
                           selectInput(inputId = "datos2",
                                       label = "Escoja una variable continua para comparar con ajuste:",
                                       choices = names(fpe)),
                           verbatimTextOutput("info"),
                           plotOutput("dispersion1", click = "plot_click"),
                           plotOutput("dispersion", click = "plot_click")
                           ),
                  
                  tabPanel("Gráfico de barras",
                           selectInput(inputId = "datos3",
                                       label = "Escoja una variable categórica para comparar con cambio:",
                                       choices = names(fpe)),
                           plotOutput("dispersion2"),
                           plotOutput("dispersion3")),
                  
                  tabPanel("Producto", 
                           selectInput(inputId = "datos4",
                                       label = "Escoja una variable para comparar con producto:",
                                       choices = c(names(fpe)[1],names(fpe)[3])),
                           
                           plotOutput("dispersion4"))
                  
      )
      
      
      
      
    ))),
  navbarMenu("Modelo Lineal Ajustado",# sirve para generar varias pestañas en una sola
  tabPanel("Análisis exploratorio de Datos",
  fluidRow(h6("Escoja las variables para realizar análisis exploratorio:"),
    column(2,
           selectInput("dependiente", 
                              label = h6("Variable dependiente:"), 
                              choices = names(fpe),
                              selected = names(fpe)[1])),
    column(2,uiOutput("independiente")
           )),
    p(),
plotOutput("gmodelo1"),
p(),
plotOutput("gmodelo2"),
p(),
plotOutput("gmodelo3")
),
 
tabPanel("Ajuste de un Modelo",
  fluidRow(h6("Escoja las variables para ajustar el modelo:"),
           column(2,
                  selectInput("dependiente1", 
                              label = h6("Variable dependiente:"), 
                              choices = names(fpe),
                              selected = names(fpe)[1])),
           column(2,uiOutput("independiente1") # sirve para crear widgets cuyas opciones dependen de la seleccion de otro widget
           )),
textOutput("texto"),
textOutput("texto1"),# imprimir la formula resultante del modelo lineal
  verbatimTextOutput("modelo"),
numericInput("obs1","Número de residuos mostrados",5),
tableOutput("residuos"),
plotOutput("histograma"),
dataTableOutput("tabla_resultado")
  
  
)


 
)

))