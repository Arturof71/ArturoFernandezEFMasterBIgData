#
# MASTER UNED BIG DATA & BUSINESS ANALYTICS
# Módulo 4.5 visualización avanzada
#
# Ejercicio Final del Módulo
#
# Arturo Fernández Martínez


#Cargamos las librerias necesariasLibrerias 

library(shiny)
library(ggplot2)
library(readxl)
library(d3heatmap)

#Datos Obtenidos de United Nations (Develoment Programe: http://hdr.undp.org/es)



#Carga de datos para el gráfico principal
HDI2015Data <- get(load(url("https://github.com/Arturof71/ArturoFernandezEFMasterBIgData/blob/master/HDI2015Data.Rda?raw=true")))

# distinguimos variables continuas
nums <- sapply(HDI2015Data, is.numeric)
continuas <- names(HDI2015Data)[nums]

# y las variables discretas
cats <- sapply(HDI2015Data, is.character)
categoricas <- names(HDI2015Data)[cats]

shinyUI(
  navbarPage("Programa de Desarrollo de las Naciones Unidas",
             tabPanel("El Índice de Desarrollo Humano (HDI)",
                      mainPanel(
                        h2("¿Qué es el Índice HDI?"),
                        p("Desarrollado dentro de las Naciones Unidas por la Oficina del Informe sobre Desarrollo Humano (HDRO), El HDI
                          se creó para hacer hincapié en que las personas y sus capacidades —y no el crecimiento económico por sí solo— 
                          deben ser el criterio más importante para evaluar el desarrollo de un país. El HDI también puede usarse para cuestionar
                          las decisiones normativas nacionales, comparando cómo dos países con el mismo nivel de ingreso nacional bruto (GNI) per cápita
                          obtienen resultados diferentes en materia de desarrollo humano, con el objeto de impulsar el debate sobre las prioridades
                          normativas de los gobiernos."),
                        p("El índice de Desarrollo Humano (HDI) es un indicador sintético de los logros medios obtenidos en las dimensiones fundamentales
                          del desarrollo humano, a saber: tener una vida larga y saludable, adquirir conocimientos y disfrutar de un nivel de vida digno. 
                          El HDI es la media geométrica de los índices normalizados de cada una de estas tres dimensiones"),
                        p(""),
                        img(src='https://github.com/Arturof71/ArturoFernandezEFMasterBIgData/blob/master/hdi.png?raw=true', align = "center"),
                        p(""),
                        p(""),
                        em("Nota2: Para más información técnica sobre Índice HDI "),
                        a(href="http://hdr.undp.org/sites/default/files/hdr2016_technical_notes.pdf", "Click Aqui"),
                        p(""),
                        p(""),
                        h2("Parámetros analizados y Conclusiones"),
                        h3("Análisis nº1: ¿Que y Como influye en el índice HDI?"),
                        p("Siendo el bienestar un concepto dificilmente medible, el hecho de contar con un set de datos alrededor de un estudio 
                          como el indice HDI (Indice de Desarrollo Humano), me parecio un concepto que podía ayudar a comprender mejor que parametros y con 
                          que correlación influyen principalmente en la felicidad de la gente, o siendo más preciso, en su índice de desarrollo."),
                        h4("Análisis de los 4 parametros utilizados para determinar el HDI"),
                        p("1.- Life Expentancy at Birth"),
                        p("2.- Expected years at School"),
                        p("3.- Mean years of Schooling"),
                        p("2.- National Income per Capita"),
                        p("Es evidente que si el indice HDI se crea a partir de estas 4 variables principales, debe existir una correlación, y
                          efectivamente existe. Pero aqui es donde aparece la primera sorpresa desde mi punto de vista. A primera vista se podría
                          suponer que la mayor correlación deberia ser con los Ingresos per Capita, ya que un mayor nivel de ingresos por ciudadano 
                          permite invertir más en Sanidad y Educación mejorando por tanto la esperanza de vida y el número de años de escolarización"),
                        p("Pues no es exactamente así, como puede apreciarse en los graficos que presentan estas variables como función de HDI. 
                           Mientras que para los 3 primeros la correlación lineal se aprecía rapidamente (y estadisticamente esta en torno al 0.9),
                          en el caso de los Ingresos per capita esta correlación es menor (0.7) y ademas en el grafico se puede apreciar como países con
                          similar nivel de ingresos pueden tener hasta 20 puntos básicos de diferencia en el índice HDI."),
                        p("Una vez detectado esta situación, era necesario realizar dos comprobaciones: (1) ¿Utilizan los gobiernos un mayor ingreso en 
                          tener un mayor ratio de gasto publico en Sanidad (PublicHEalthExpenditureRate) y Educación(Education Expenditure Rate)?, 
                          y (2) ¿Existe una correlación que determine que a mayor gasto público en Educación y Sanidad, los ratios de años medios de Educación
                          (Mean years of Schooling) y esperanza de vida (Life Expentancy at Birth) mejoran?"),
                        p("Utilizando el mismo gráfico, llegamos a la conclusión que existe de nuevo mucha dispersión de los datos y no una clara correlación.
                          Podía ser de esperar para la primera pregunta entre el nivel de ingresos y el ratio de gasto en sanidad y educación, 
                          pero es más dificilmente explicable para la segunda. Este análisis permite llegar a dos conclusiones: (1)No siempre los gobiernos inverten  
                          lo suficiente en conceptos como sanidad y educación, y (2) como y en que se utiliza el presupuesto y no sólo la cantidad, definen como 
                          se impacta en el bienestar de los ciudadanos de un país"),
                        h3("Análisis nº2: ¿Como se posiciona cada país?"),
                        p("Con el objeto de poder analizar la situación concreta de un país en relación al global de paises y a los paises de su región, he
                           desarrollado este conjunto de gráficas que permite ver graficamente al posición concreta de un país con respecto a 6 de las variables
                           más importantes del estudio.Esto permite definir aquellas áreas en las que se debe incidir más para mejorar el ratio de desarrollo"),
                        p("Tomando como ejemplo España, se puede apreciar caundo se compara con el global de países que tiene una posición muy saludable, estando sólo
                           por debajo del percentil 75 en los años medios de escolarización. Haciendo la comparativa con los países de su región, lo cual premite hacer
                           una comparativa con paises de similar desarrollo, vemos que las áreas de actuación por criticidad son:"),
                        p("1.- Los años medios de escolarización, donde nos encontramos por debajo del percentil 25"),
                        p("2.- El ingreso nacional bruto per capita, donde nos encontramos por debajo del percentil 75"),
                        h3("Análisis nº3: Evolución histórica del Índice HDI"),
                        p("Comenzando por el año 1990, se ha creado un grafico animado que representa como se han posicionado los paises relacionado la evolución del HDI
                          en función de la evolución del Ingreso Nacional Bruto per Capita. Se prsenta tanto la evolución global como por región"),
                        p("Las conclusiones principales que se pueden deducir son:"),
                        p("1.- EL HDI mejora de manera global, pero las diferencias entre paises paises probre y ricos apenas se han reducido"),
                        p("2.- Las diferencias entre paises no sólo ocurren entre paises de regiones ricas y pobres, sino que existe mucha diferencia dentro de las
                           propias regiones."),
                        h3("Otros gráficos"),
                        p("Finalmente he añadido otros tres gráficos utilizando los datos del estudio HDI. Realmente es sorprendente la cantidad de cosas que se pueden
                          realizar desde el punto de vista de los gráficos y la cantidad de información disponible. Me queda claro que este es un area fundamental del área
                          de Big Data en el que me gustaría profundizar. Recomendaria ahora que se estan formado equipos de Big Data en las compañías, contar con al menos un 
                          recurso especializado en esta materia y solo encargado de buscar la mejor representación de los datos.")
                        )
                       ),
             tabPanel("Análisis Global Variables",
                      sidebarPanel(
                        
                        selectInput('x', 'Elige variable para eje X', continuas, continuas[[3]]),
                        selectInput('y', 'Elige variable para eje Y', continuas, continuas[[2]]),
                        selectInput('color', 'Color', c('None', 'Region','HDIGroup')),
                        strong("Dibujar Tendencia"),
                        checkboxInput('lm', 'Línea de Regresión'),
                        checkboxInput('smooth', 'Suavizado LOESS'),
                        
                        selectInput('facet_row', 'Agrupamiento', c(None='.', categoricas))
                      ),
         
                      mainPanel(
                        plotOutput('plot',
                                   height=800)
                        )
                      ),
             tabPanel("Situación por País",
                      sidebarPanel(
                        selectInput('c', 'País Análisis', c(None='.',HDI2015Data$Country)),
                        selectInput('a', 'Ámbito de Análisis', c(Global='g',Regional='r')),
                        p(""),
                        strong("Simbologia"),
                        p(""),
                        img(src='https://github.com/Arturof71/ArturoFernandezEFMasterBIgData/blob/master/Leyenda.png?raw=true')
                      ),
                      
                      mainPanel(
                        fluidRow(
                          column(3,
                            plotOutput('plot21',
                                   height=300,width=200)
                          ),
                          column(4,offset = 1,
                            plotOutput('plot22',
                                   height=300,width=200)
                          )
                        ),
                        fluidRow(
                          column(3,
                                 plotOutput('plot23',
                                            height=300,width=200)
                          ),
                          column(4,offset = 1,
                                 plotOutput('plot24',
                                            height=300,width=200)
                          )
                        ),
                          fluidRow(
                            column(3,
                                   plotOutput('plot25',
                                              height=300,width=200)
                            ),
                            column(4,offset = 1,
                                   plotOutput('plot26',
                                              height=300,width=200)
                            )  
                        )
                      )
                    ),
             tabPanel("Evolución Índice HDI",
                      mainPanel(
                        plotOutput('plot3',height=100),
                        img(src='HDIevolution.gif'),
                        img(src='HDIevolutionPerRegion.gif')
                      )
             ),
             tabPanel("Otros Gráficos",
                      mainPanel(
                        h3("Correlograma de variables de Estudio"),
                        plotOutput('plot4',height=800),
                        h3("Mapa de Calor Paises Europa y Asia Central"),
                        d3heatmapOutput('plot5',height=1000),
                        h3("Mapa del Mundo con Indice HDI"),
                        plotOutput('plot6',height=800)
                      )
             )
      ))
