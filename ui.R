# El objeto de interfaz de usuario (ui) 
# controla el diseño y la apariencia de la aplicación.

shinyUI(navbarPage(theme = shinytheme("cerulean"), #cerulean es el Fondo de la pantalla
  # titulo de la App  ----
  "Datathom UCI",
  
  # Diseño de la barra lateral con definiciones de entrada
  # y salida ----
  tabPanel( "Análisis exploratorio de los datos",
  sidebarLayout(
    # Panel de la barra lateral para entradas ----
    sidebarPanel(h4("Equipo de Científicos de Datos"),
                 
                 img(src='https://553801.smushcdn.com/1938437/wp-content/uploads/2020/08/Jaime_Soria_Viteri-400x-300x400.jpg?lossy=1&strip=1&webp=1', height = 100, width = 75),
                 p("Dr. Jaime Soria"),
                 img(src='https://pbs.twimg.com/profile_images/1010320061192552450/Kw7s7nTu_400x400.jpg', height = 100, width = 75),
                 p("Ing. Yalbi Balderas"),
                 img(src='https://media-exp1.licdn.com/dms/image/C4E03AQHC95126yf7bQ/profile-displayphoto-shrink_200_200/0/1517435583627?e=1620864000&v=beta&t=3wkN8RecgLdc_9EOIpCxeckpwiZZ7uwpt7yKnn4vPuU', height = 100, width = 75),
                 p("Ing. Miriam Lupercio"),
                 img(src='', height = 100, width = 75),
                 p("Ing. Belen Escola"),
                 hr(),
      
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
      h2("Bienvenido al analisis exploratorio del Datathon 2020"),
      p("Cuando un paciente es trasladado a la Unidad de Cuidados Intensivos (UCI) 
      es porque su salud podría deteriorarse rápidamente. Si basados en los datos de varias mediciones de estos pacientes 
      logramos predecir la probabilidad de mortalidad y el grado de deterioro de 
      los diferentes órganos, podríamos responder la amenaza con anticipación y 
      darle al paciente la forma más avanzada de cuidados posibles para disminuir 
      su riesgo de mortalidad."),
      br(),
      p("Es con esto en mente que se desarrollo la siguiente applicación para
        que todos los médicos del mundo puedan revisarla y analizar. De esta forma 
        podran como las diferentes carateristicas clínicas y demográficas, signos vitales, 
        resultados de laboratorio y comorbilidades pueden influir en la mortalidad
        del paciente."),
      
      br(),
      
      h3("Análisis estadistico descriptivo univariado"),
      
      p("A continuacion presentamos de forma dinamica el analisis estadistico 
        desciptivo univariado de todas las variables de la base de datos"), 
      br(),
      
      strong("Se presenta la tabla con las caracteristicas descriptivas de la variable:"),
      verbatimTextOutput("summary"),
      
      # Salida: tabla HTML con el número 
      # solicitado de observaciones ----
      tableOutput("view"),

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
                  tabPanel("Variables continuas", #Ajuste
                           selectInput(inputId = "datos2",
                                       label = "Escoja una variable continua para comparar con ajuste:",
                                       choices = names(fpe)),
                           strong("Al realizar la prueba de Wilcoxon dio un valor p de:"),
                           verbatimTextOutput("analisis1"),
                           plotOutput("dispersion1", click = "plot_click"),
                           plotOutput("dispersion", click = "plot_click"),
                           #verbatimTextOutput("info"),
                           #label = ,
                           # strong("Al realizar la prueba de Wilcoxon dio un valor p de:"),
                           # verbatimTextOutput("analisis1"),
                           # plotOutput("dispersion1", click = "plot_click"),
                           # plotOutput("dispersion", click = "plot_click")
                           ),
                  
                  tabPanel("Variables categoricas",
                           selectInput(inputId = "datos3",
                                       label = "Escoja una variable categórica para comparar con cambio:",
                                       choices = names(fpe)),
                           strong("Al realizar la prueba de Chi2 dio un valor p de:"),
                           verbatimTextOutput("analisis3"),
                           verbatimTextOutput("tablaFrecCategorica"),
                           plotOutput("dispersion2"),
                           verbatimTextOutput("tablaPorcCategorica"),
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