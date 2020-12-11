library(shiny)
library(shinyjs)
library(shinyalert)
library(plotly)
library(DT)
library(openxlsx)


# 1) CONEXIÓN BBDD
db          <- 'datawarehouse'
host_db     <- '116.203.142.113'
db_port     <- '5432'
db_user     <- 'postgres'
db_password <- 'postgressysadmin_2019'

con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
print(con@bigint)


df_preguntas <- read.csv2("total_questions_es.csv",encoding = "UTF-8")
df_resultados <- read.csv2("total_results_es.csv",encoding = "UTF-8")
df_acciones <- read.csv2("total_actions_es.csv",encoding = "UTF-8",sep = ",")
df_acciones$Paso <- rep(c(1,2,3,4),9)

df_acciones$Recursos <- paste(df_acciones$Recursos,"-",df_acciones$Detalle,sep = "")
for(i in 1:nrow(df_acciones)){
  if(df_acciones$Herramientas[i] != "-"){
    df_acciones$Herramientas[i] <- paste0("<a href='", df_acciones$Links[i],"' target='_blank'>", df_acciones$Herramientas[i],"</a>")
  }
}



preguntas <- unique(df_preguntas$question_titl)
preguntas <- preguntas %>%
  gsub("Cuán","Cómo de",.) %>%
  gsub("Qué tan","Cómo de",.) %>%
  gsub("recien","recientemente",.)
niveles_preguntas <- unique(df_preguntas$question_lvds)

# ==============================================================================
ui <- fluidPage(style = "width: 100%; height: 100%;",
                
                #use_busy_spinner(spin = "fading-circle"),
                
                # Inicialización shinyjs
                useShinyjs(),
                
                titlePanel(title=div(
                  a(href="https://techfriendly.es",
                    img(src="img/techfriendly.png",style = 'width: 10vw; high: 60vw;')
                  ),
                  p("ESTADO Y HOJA DE RUTA DE LA DIGITALIZACIÓN",style = 'font-size:2vw; display: inline; text-align: center; margin-left: 13%;'),
                  div(style = "float: right;",
                      a(href="http://www.clusteralimentacion.com/es",
                        img(src="img/logo_cluster.jpg",style = 'width: 12vw; high: 60vw;')
                      )
                  )
                )),
                
                navbarPage(id ="menu", NULL,
                           
                           # 0) PÁGINA DE BIENVENIDA
                           tabPanel("REGISTRO", 
                                    sidebarPanel(style = "width: 100%; height: 100%; margin-left: 100%; margin-top: 15%;",
                                      textInput("empresa", "Empresa"),
                                      textInput("sector", "Sector profesional"),
                                      actionButton('suiguiente_0', 'ENTRAR'),
                                    )
                           ),
                           
                           # 1) 1 RECURSOS
                           tabPanel("1 RECURSOS", 
                                    sidebarLayout(
                                      sidebarPanel(style = "width: 100%; height: 100%; margin-left: 50%;",
                                        h4(preguntas[1]),
                                        sliderInput("actual_1_1", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_1_1", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[5],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[4],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[3],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[2],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[1],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[2]),
                                        sliderInput("actual_1_2", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_1_2", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[10],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[9],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[8],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[7],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[6],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[3]),
                                        sliderInput("actual_1_3", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_1_3", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[15],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[14],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[13],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[12],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[11],sep="")),
                                        actionButton('suiguiente_1', 'SIGUIENTE'),
                                        width = 6,
                                      ),

                                      mainPanel(
                                        tabsetPanel(id = "tab_1 RECURSOS"
                                                    
                                        )
                                      )
                                    )
                           ),
                           
                           # 2) 2 EXPERIENCIA DEL CLIENTE
                           tabPanel("2 EXPERIENCIA DEL CLIENTE",
                                    sidebarLayout(
                                      sidebarPanel(style = "width: 100%; height: 100%; margin-left: 50%;",
                                        
                                        h4(preguntas[4]),
                                        sliderInput("actual_2_1", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_2_1", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[20],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[19],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[18],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[17],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[16],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[5]),
                                        sliderInput("actual_2_2", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_2_2", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[25],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[24],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[23],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[22],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[21],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[6]),
                                        sliderInput("actual_2_3", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_2_3", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[30],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[29],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[28],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[27],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[26],sep="")),
                                        actionButton('suiguiente_2', 'SIGUIENTE'),
                                        width = 6,
                                      ),

                                      mainPanel(
                                        tabsetPanel(id = "tab_experiencia"
                                        )
                                      )
                                    )
                           ),
                           
                           # 3) 3 MARKETING MULTICANAL
                           tabPanel("3 MARKETING MULTICANAL",
                                    sidebarLayout(
                                      sidebarPanel(style = "width: 100%; height: 100%; margin-left: 50%;",
                                        h4(preguntas[7]),
                                        sliderInput("actual_3_1", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_3_1", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[35],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[34],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[33],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[32],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[31],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[8]),
                                        sliderInput("actual_3_2", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_3_2", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[40],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[39],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[38],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[37],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[36],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[9]),
                                        sliderInput("actual_3_3", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_3_3", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[45],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[44],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[43],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[42],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[41],sep="")),
                                        actionButton('suiguiente_3', 'SIGUIENTE'),
                                        width = 6,
                                      ),

                                      mainPanel(
                                        tabsetPanel(id = "tab_multicanal"
                                        )
                                      )
                                    )
                           ),
                           
                           # 4) 4 SOCIAL
                           tabPanel("4 SOCIAL",
                                    sidebarLayout(
                                      sidebarPanel(style = "width: 100%; height: 100%; margin-left: 50%;",
                                        h4(preguntas[10]),
                                        sliderInput("actual_4_1", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_4_1", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[50],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[49],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[48],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[47],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[46],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[11]),
                                        sliderInput("actual_4_2", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_4_2", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[55],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[54],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[53],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[52],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[51],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[12]),
                                        sliderInput("actual_4_3", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_4_3", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[60],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[59],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[58],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[57],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[56],sep="")),
                                        actionButton('suiguiente_4', 'SIGUIENTE'),
                                        width = 6,
                                      ),

                                      mainPanel(
                                        tabsetPanel(id = "tab_4 SOCIAL"
                                        )
                                      )
                                    )
                           ),
                           
                           # 5) 5 MÓVIL
                           tabPanel("5 MÓVIL",
                                    sidebarLayout(
                                      sidebarPanel(style = "width: 100%; height: 100%; margin-left: 50%;",
                                        h4(preguntas[13]),
                                        sliderInput("actual_5_1", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_5_1", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[65],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[64],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[63],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[62],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[61],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[14]),
                                        sliderInput("actual_5_2", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_5_2", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[70],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[69],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[68],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[67],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[66],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[15]),
                                        sliderInput("actual_5_3", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_5_3", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[75],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[75],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[73],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[72],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[71],sep="")),
                                        actionButton('suiguiente_5', 'SIGUIENTE'),
                                        width = 6,
                                      ),

                                      mainPanel(
                                        tabsetPanel(id = "tab_5 MÓVIL"
                                        )
                                      )
                                    )
                           ),
                           
                           # 6) 6 COMERCIO DIGITAL
                           tabPanel("6 COMERCIO DIGITAL",
                                    sidebarLayout(
                                      sidebarPanel(style = "width: 100%; height: 100%; margin-left: 50%;",
                                        h4(preguntas[14]),
                                        sliderInput("actual_6_1", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_6_1", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[80],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[79],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[78],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[77],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[76],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[15]),
                                        sliderInput("actual_6_2", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_6_2", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[85],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[84],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[83],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[82],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[81],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[16]),
                                        sliderInput("actual_6_3", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_6_3", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[90],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[89],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[88],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[87],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[86],sep="")),
                                        actionButton('suiguiente_6', 'SIGUIENTE'),
                                        width = 6,
                                      ),

                                      mainPanel(
                                        tabsetPanel(id = "tab_comercio"
                                        )
                                      )
                                    )
                           ),
                           
                           # 7) 7 DATA DRIVEN
                           tabPanel("7 DATA DRIVEN",
                                    sidebarLayout(
                                      sidebarPanel(style = "width: 100%; height: 100%; margin-left: 50%;",
                                        h4(preguntas[17]),
                                        sliderInput("actual_7_1", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_7_1", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[95],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[94],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[93],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[92],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[91],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[18]),
                                        sliderInput("actual_7_2", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_7_2", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[100],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[99],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[98],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[97],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[96],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[19]),
                                        sliderInput("actual_7_3", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_7_3", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[105],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[104],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[103],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[102],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[101],sep="")),
                                        actionButton('suiguiente_7', 'SIGUIENTE'),
                                        width = 6,
                                      ),

                                      mainPanel(
                                        tabsetPanel(id = "tab_data"
                                        )
                                      )
                                    )
                           ),
                           
                           # 8) 8 OPERACIONES
                           tabPanel("8 OPERACIONES",
                                    sidebarLayout(
                                      sidebarPanel(style = "width: 100%; height: 100%; margin-left: 50%;",
                                        h4(preguntas[20]),
                                        sliderInput("actual_8_1", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_8_1", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[110],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[109],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[108],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[107],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[106],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[21]),
                                        sliderInput("actual_8_2", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_8_2", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[115],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[114],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[113],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[112],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[111],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[22]),
                                        sliderInput("actual_8_3", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_8_3", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[120],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[119],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[118],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[117],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[116],sep="")),
                                        actionButton('suiguiente_8', 'SIGUIENTE'),
                                        width = 6,
                                      ),

                                      mainPanel(
                                        tabsetPanel(id = "tab_8 OPERACIONES"
                                        )
                                      )
                                    )
                           ),
                           
                           # 9) 9 INNOVACIÓN
                           tabPanel("9 INNOVACIÓN",
                                    sidebarLayout(
                                      sidebarPanel(style = "width: 100%; height: 100%; margin-left: 50%;",
                                        h4(preguntas[23]),
                                        sliderInput("actual_9_1", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_9_1", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[125],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[124],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[123],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[122],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[121],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[24]),
                                        sliderInput("actual_9_2", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_9_2", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[130],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[129],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[128],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[127],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[126],sep="")),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        h4(preguntas[25]),
                                        sliderInput("actual_9_3", "Nivel actual",1,5,1,step = 1, width = "50%"),
                                        sliderInput("deseado_9_3", "Nivel deseado",1,5,1,step = 1, width = "50%"),
                                        h5(paste("Nivel 1 - ",niveles_preguntas[135],sep="")),
                                        h5(paste("Nivel 2 - ",niveles_preguntas[134],sep="")),
                                        h5(paste("Nivel 3 - ",niveles_preguntas[133],sep="")),
                                        h5(paste("Nivel 4 - ",niveles_preguntas[132],sep="")),
                                        h5(paste("Nivel 5 - ",niveles_preguntas[131],sep="")),
                                        actionButton('suiguiente_9', 'VER RESULTADO'),
                                        width = 6,
                                      ),

                                      mainPanel(
                                        tabsetPanel(id = "tab_innovacion"
                                        )
                                      )
                                    )
                           ),
                           
                           # 10) RESULTADOS
                           tabPanel("RESULTADOS",
                                      mainPanel(style = "margin-left: 16%;",
                                        tags$div(id = "1",tags$h2(tags$b("Resumen de evaluación"))),
                                        plotlyOutput("radial_total", height = 500),
                                        dataTableOutput("tabla_resumen"),
                                        br(),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        tags$div(id = "1",tags$h2(tags$b("Estado de digitalización actual"))),
                                        plotlyOutput("radial_resultados", height = 500),
                                        dataTableOutput("tabla_resultados"),
                                        br(),
                                        br(),
                                        HTML('<hr style="border-color: #868686;">'),
                                        br(),
                                        tags$div(id = "1",tags$h2(tags$b("Acciones a emprender"))),
                                        plotlyOutput("radial_acciones", height = 500),
                                        dataTableOutput("tabla_acciones"),
                                        br(),
                                        br(),
                                        tags$div(downloadButton("descarga_datos_xlsx", "Descargar datos")),
                                        br(),
                                        br()
                                      )
                           )
                )# Cierre navbar
                           
)

# ==============================================================================
server <- function(input, output, session) {
  
  observeEvent(input$menu, {
    if(input$menu == "REGISTRO"){
      hideTab(inputId = "menu", target = "1 RECURSOS")
      hideTab(inputId = "menu", target = "2 EXPERIENCIA DEL CLIENTE")
      hideTab(inputId = "menu", target = "3 MARKETING MULTICANAL")
      hideTab(inputId = "menu", target = "4 SOCIAL")
      hideTab(inputId = "menu", target = "5 MÓVIL")
      hideTab(inputId = "menu", target = "6 COMERCIO DIGITAL")
      hideTab(inputId = "menu", target = "7 DATA DRIVEN")
      hideTab(inputId = "menu", target = "8 OPERACIONES")
      hideTab(inputId = "menu", target = "RESULTADOS")
      hideTab(inputId = "menu", target = "9 INNOVACIÓN")
    }else{
      showTab(inputId = "menu", target = "1 RECURSOS")
      showTab(inputId = "menu", target = "2 EXPERIENCIA DEL CLIENTE")
      showTab(inputId = "menu", target = "3 MARKETING MULTICANAL")
      showTab(inputId = "menu", target = "4 SOCIAL")
      showTab(inputId = "menu", target = "5 MÓVIL")
      showTab(inputId = "menu", target = "6 COMERCIO DIGITAL")
      showTab(inputId = "menu", target = "7 DATA DRIVEN")
      showTab(inputId = "menu", target = "8 OPERACIONES")
      showTab(inputId = "menu", target = "RESULTADOS")
      showTab(inputId = "menu", target = "9 INNOVACIÓN")
      hideTab(inputId = "menu", target = "REGISTRO")
    }
    
  })
  
  observeEvent(input$suiguiente_0, {
    if(input$sector != "" & input$empresa != ""){
      
      data <- data.frame(input$sector,input$empresa,Sys.time())
      colnames(data) <- c("Sector","Empresa","Timestamp")

      #dbWriteTable(con, 'usuarios',data, temporary = FALSE)
      dbWriteTable(con, 'usuarios',data, append = TRUE)
      
      updateTabsetPanel(session, "menu",
                        selected = "1 RECURSOS")
    }
  })
  observeEvent(input$suiguiente_1, {
    updateTabsetPanel(session, "menu",
                      selected = "2 EXPERIENCIA DEL CLIENTE")
  })
  observeEvent(input$suiguiente_2, {
    updateTabsetPanel(session, "menu",
                      selected = "3 MARKETING MULTICANAL")
  })
  observeEvent(input$suiguiente_3, {
    updateTabsetPanel(session, "menu",
                      selected = "4 SOCIAL")
  })
  observeEvent(input$suiguiente_4, {
    updateTabsetPanel(session, "menu",
                      selected = "5 MÓVIL")
  })
  observeEvent(input$suiguiente_5, {
    updateTabsetPanel(session, "menu",
                      selected = "6 COMERCIO DIGITAL")
  })
  observeEvent(input$suiguiente_6, {
    updateTabsetPanel(session, "menu",
                      selected = "7 DATA DRIVEN")
  })
  observeEvent(input$suiguiente_7, {
    updateTabsetPanel(session, "menu",
                      selected = "8 OPERACIONES")
  })
  observeEvent(input$suiguiente_8, {
    updateTabsetPanel(session, "menu",
                      selected = "9 INNOVACIÓN")
  })
  observeEvent(input$suiguiente_9, {
    updateTabsetPanel(session, "menu",
                      selected = "RESULTADOS")
  })
  
  
  #=======================================
  # RESULTADOS
  # ======================================
  resultados <- reactive({
    
    resultados_1 <- ceiling(mean(as.numeric(input$actual_1_1),as.numeric(input$actual_1_2),as.numeric(input$actual_1_3)))
    resultados_2 <- ceiling(mean(as.numeric(input$actual_2_1),as.numeric(input$actual_2_2),as.numeric(input$actual_2_3)))
    resultados_3 <- ceiling(mean(as.numeric(input$actual_3_1),as.numeric(input$actual_3_2),as.numeric(input$actual_3_3)))
    resultados_4 <- ceiling(mean(as.numeric(input$actual_4_1),as.numeric(input$actual_4_2),as.numeric(input$actual_4_3)))
    resultados_5 <- ceiling(mean(as.numeric(input$actual_5_1),as.numeric(input$actual_5_2),as.numeric(input$actual_5_3)))
    resultados_6 <- ceiling(mean(as.numeric(input$actual_6_1),as.numeric(input$actual_6_2),as.numeric(input$actual_6_3)))
    resultados_7 <- ceiling(mean(as.numeric(input$actual_7_1),as.numeric(input$actual_7_2),as.numeric(input$actual_7_3)))
    resultados_8 <- ceiling(mean(as.numeric(input$actual_8_1),as.numeric(input$actual_8_2),as.numeric(input$actual_8_3)))
    resultados_9 <- ceiling(mean(as.numeric(input$actual_9_1),as.numeric(input$actual_9_2),as.numeric(input$actual_9_3)))
    
    
    df_1 <- df_resultados[df_resultados$segment_title == df_resultados$segment_title[1] & df_resultados$current_level == resultados_1,c(1,3,4)]
    df_2 <- df_resultados[df_resultados$segment_title == df_resultados$segment_title[2] & df_resultados$current_level == resultados_2,c(1,3,4)]
    df_3 <- df_resultados[df_resultados$segment_title == df_resultados$segment_title[3] & df_resultados$current_level == resultados_3,c(1,3,4)]
    df_4 <- df_resultados[df_resultados$segment_title == df_resultados$segment_title[4] & df_resultados$current_level == resultados_4,c(1,3,4)]
    df_5 <- df_resultados[df_resultados$segment_title == df_resultados$segment_title[5] & df_resultados$current_level == resultados_5,c(1,3,4)]
    df_6 <- df_resultados[df_resultados$segment_title == df_resultados$segment_title[6] & df_resultados$current_level == resultados_6,c(1,3,4)]
    df_7 <- df_resultados[df_resultados$segment_title == df_resultados$segment_title[7] & df_resultados$current_level == resultados_7,c(1,3,4)]
    df_8 <- df_resultados[df_resultados$segment_title == df_resultados$segment_title[8] & df_resultados$current_level == resultados_8,c(1,3,4)]
    df_9 <- df_resultados[df_resultados$segment_title == df_resultados$segment_title[9] & df_resultados$current_level == resultados_9,c(1,3,4)]
    
    df_1$valor <- resultados_1
    df_2$valor <- resultados_2
    df_3$valor <- resultados_3
    df_4$valor <- resultados_4
    df_5$valor <- resultados_5
    df_6$valor <- resultados_6
    df_7$valor <- resultados_7
    df_8$valor <- resultados_8
    df_9$valor <- resultados_9
    
    df_resultados <- rbind(df_1,df_2,df_3,df_4,df_5,df_6,df_7,df_8,df_9)
    colnames(df_resultados) <- c("Área", "Nivel", "Aclaración", "Valor")
    
    df_resultados$Área <- gsub("Conducida por datos","Data driven",df_resultados$Área)
    
    df_resultados
  })
  
  #=======================================
  # ACCIONES
  # ======================================
  acciones <- reactive({
    
    acciones_1 <- ceiling(mean(as.numeric(input$deseado_1_1),as.numeric(input$deseado_1_2),as.numeric(input$deseado_1_3)))
    acciones_2 <- ceiling(mean(as.numeric(input$deseado_2_1),as.numeric(input$deseado_2_2),as.numeric(input$deseado_2_3)))
    acciones_3 <- ceiling(mean(as.numeric(input$deseado_3_1),as.numeric(input$deseado_3_2),as.numeric(input$deseado_3_3)))
    acciones_4 <- ceiling(mean(as.numeric(input$deseado_4_1),as.numeric(input$deseado_4_2),as.numeric(input$deseado_4_3)))
    acciones_5 <- ceiling(mean(as.numeric(input$deseado_5_1),as.numeric(input$deseado_5_2),as.numeric(input$deseado_5_3)))
    acciones_6 <- ceiling(mean(as.numeric(input$deseado_6_1),as.numeric(input$deseado_6_2),as.numeric(input$deseado_6_3)))
    acciones_7 <- ceiling(mean(as.numeric(input$deseado_7_1),as.numeric(input$deseado_7_2),as.numeric(input$deseado_7_3)))
    acciones_8 <- ceiling(mean(as.numeric(input$deseado_8_1),as.numeric(input$deseado_8_2),as.numeric(input$deseado_8_3)))
    acciones_9 <- ceiling(mean(as.numeric(input$deseado_9_1),as.numeric(input$deseado_9_2),as.numeric(input$deseado_9_3)))
    
    acciones_1_seleccion <- ifelse(acciones_1 == 1,1,acciones_1-1)
    acciones_2_seleccion <- ifelse(acciones_2 == 1,1,acciones_2-1)
    acciones_3_seleccion <- ifelse(acciones_3 == 1,1,acciones_3-1)
    acciones_4_seleccion <- ifelse(acciones_4 == 1,1,acciones_4-1)
    acciones_5_seleccion <- ifelse(acciones_5 == 1,1,acciones_5-1)
    acciones_6_seleccion <- ifelse(acciones_6 == 1,1,acciones_6-1)
    acciones_7_seleccion <- ifelse(acciones_7 == 1,1,acciones_7-1)
    acciones_8_seleccion <- ifelse(acciones_8 == 1,1,acciones_8-1)
    acciones_9_seleccion <- ifelse(acciones_9 == 1,1,acciones_9-1)

    df_1 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[1] & df_acciones$Paso == acciones_1_seleccion,c(1,2,3)]
    df_2 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[2] & df_acciones$Paso == acciones_2_seleccion,c(1,2,3)]
    df_3 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[3] & df_acciones$Paso == acciones_3_seleccion,c(1,2,3)]
    df_4 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[4] & df_acciones$Paso == acciones_4_seleccion,c(1,2,3)]
    df_5 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[5] & df_acciones$Paso == acciones_5_seleccion,c(1,2,3)]
    df_6 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[6] & df_acciones$Paso == acciones_6_seleccion,c(1,2,3)]
    df_7 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[7] & df_acciones$Paso == acciones_7_seleccion,c(1,2,3)]
    df_8 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[8] & df_acciones$Paso == acciones_8_seleccion,c(1,2,3)]
    df_9 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[9] & df_acciones$Paso == acciones_9_seleccion,c(1,2,3)]
    
    df_1$valor <- acciones_1
    df_2$valor <- acciones_2
    df_3$valor <- acciones_3
    df_4$valor <- acciones_4
    df_5$valor <- acciones_5
    df_6$valor <- acciones_6
    df_7$valor <- acciones_7
    df_8$valor <- acciones_8
    df_9$valor <- acciones_9
    
    df_acciones <- rbind(df_1,df_2,df_3,df_4,df_5,df_6,df_7,df_8,df_9)
    colnames(df_acciones) <- c("Área", "Nivel", "Aclaración", "Valor")
    
    df_acciones$Área <- gsub("Conducida por datos","Data driven",df_acciones$Área)
    
    df_acciones 
  })
  
  datos_tabla_acciones <- reactive({
    
    acciones_1 <- ceiling(mean(as.numeric(input$deseado_1_1),as.numeric(input$deseado_1_2),as.numeric(input$deseado_1_3)))
    acciones_2 <- ceiling(mean(as.numeric(input$deseado_2_1),as.numeric(input$deseado_2_2),as.numeric(input$deseado_2_3)))
    acciones_3 <- ceiling(mean(as.numeric(input$deseado_3_1),as.numeric(input$deseado_3_2),as.numeric(input$deseado_3_3)))
    acciones_4 <- ceiling(mean(as.numeric(input$deseado_4_1),as.numeric(input$deseado_4_2),as.numeric(input$deseado_4_3)))
    acciones_5 <- ceiling(mean(as.numeric(input$deseado_5_1),as.numeric(input$deseado_5_2),as.numeric(input$deseado_5_3)))
    acciones_6 <- ceiling(mean(as.numeric(input$deseado_6_1),as.numeric(input$deseado_6_2),as.numeric(input$deseado_6_3)))
    acciones_7 <- ceiling(mean(as.numeric(input$deseado_7_1),as.numeric(input$deseado_7_2),as.numeric(input$deseado_7_3)))
    acciones_8 <- ceiling(mean(as.numeric(input$deseado_8_1),as.numeric(input$deseado_8_2),as.numeric(input$deseado_8_3)))
    acciones_9 <- ceiling(mean(as.numeric(input$deseado_9_1),as.numeric(input$deseado_9_2),as.numeric(input$deseado_9_3)))
    
    resultados_1 <- ceiling(mean(as.numeric(input$actual_1_1),as.numeric(input$actual_1_2),as.numeric(input$actual_1_3)))
    resultados_2 <- ceiling(mean(as.numeric(input$actual_2_1),as.numeric(input$actual_2_2),as.numeric(input$actual_2_3)))
    resultados_3 <- ceiling(mean(as.numeric(input$actual_3_1),as.numeric(input$actual_3_2),as.numeric(input$actual_3_3)))
    resultados_4 <- ceiling(mean(as.numeric(input$actual_4_1),as.numeric(input$actual_4_2),as.numeric(input$actual_4_3)))
    resultados_5 <- ceiling(mean(as.numeric(input$actual_5_1),as.numeric(input$actual_5_2),as.numeric(input$actual_5_3)))
    resultados_6 <- ceiling(mean(as.numeric(input$actual_6_1),as.numeric(input$actual_6_2),as.numeric(input$actual_6_3)))
    resultados_7 <- ceiling(mean(as.numeric(input$actual_7_1),as.numeric(input$actual_7_2),as.numeric(input$actual_7_3)))
    resultados_8 <- ceiling(mean(as.numeric(input$actual_8_1),as.numeric(input$actual_8_2),as.numeric(input$actual_8_3)))
    resultados_9 <- ceiling(mean(as.numeric(input$actual_9_1),as.numeric(input$actual_9_2),as.numeric(input$actual_9_3)))
    
    df_1 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[1] & df_acciones$Paso <= acciones_1 & df_acciones$Paso >= resultados_1,c(1,2,3,4,6)]
    df_2 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[2] & df_acciones$Paso <= acciones_2 & df_acciones$Paso >= resultados_2,c(1,2,3,4,6)]
    df_3 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[3] & df_acciones$Paso <= acciones_3 & df_acciones$Paso >= resultados_3,c(1,2,3,4,6)]
    df_4 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[4] & df_acciones$Paso <= acciones_4 & df_acciones$Paso >= resultados_4,c(1,2,3,4,6)]
    df_5 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[5] & df_acciones$Paso <= acciones_5 & df_acciones$Paso >= resultados_5,c(1,2,3,4,6)]
    df_6 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[6] & df_acciones$Paso <= acciones_6 & df_acciones$Paso >= resultados_6,c(1,2,3,4,6)]
    df_7 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[7] & df_acciones$Paso <= acciones_7 & df_acciones$Paso >= resultados_7,c(1,2,3,4,6)]
    df_8 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[8] & df_acciones$Paso <= acciones_8 & df_acciones$Paso >= resultados_8,c(1,2,3,4,6)]
    df_9 <- df_acciones[df_acciones$segment_title == unique(df_acciones$segment_title)[9] & df_acciones$Paso <= acciones_9 & df_acciones$Paso >= resultados_9,c(1,2,3,4,6)]
    
    shiny::validate(
      need(nrow(df_1) != 0 & nrow(df_2) != 0 & nrow(df_3) != 0 & nrow(df_4) != 0 & nrow(df_5) != 0 & nrow(df_6) != 0 & nrow(df_7) != 0 & nrow(df_9) != 0 & nrow(df_9) != 0,
           "¡El nivel actual no debe ser superior al nivel deseado!\nPor favor, revise sus valoraciones y modifique los valores\n")
    )
    
    df_1$valor <- acciones_1
    df_2$valor <- acciones_2
    df_3$valor <- acciones_3
    df_4$valor <- acciones_4
    df_5$valor <- acciones_5
    df_6$valor <- acciones_6
    df_7$valor <- acciones_7
    df_8$valor <- acciones_8
    df_9$valor <- acciones_9
    
    df_acciones <- rbind(df_1,df_2,df_3,df_4,df_5,df_6,df_7,df_8,df_9)
    colnames(df_acciones) <- c("Área", "Nivel", "Acción", "Herramienta", "Recursos")
    
    df_acciones$Área <- gsub("Conducida por datos","Data driven",df_acciones$Área)
    
    df_acciones 
    
  })
  
  resumen <- reactive({
    
    resultados_1 <- ceiling(mean(as.numeric(input$actual_1_1),as.numeric(input$actual_1_2),as.numeric(input$actual_1_3)))
    resultados_2 <- ceiling(mean(as.numeric(input$actual_2_1),as.numeric(input$actual_2_2),as.numeric(input$actual_2_3)))
    resultados_3 <- ceiling(mean(as.numeric(input$actual_3_1),as.numeric(input$actual_3_2),as.numeric(input$actual_3_3)))
    resultados_4 <- ceiling(mean(as.numeric(input$actual_4_1),as.numeric(input$actual_4_2),as.numeric(input$actual_4_3)))
    resultados_5 <- ceiling(mean(as.numeric(input$actual_5_1),as.numeric(input$actual_5_2),as.numeric(input$actual_5_3)))
    resultados_6 <- ceiling(mean(as.numeric(input$actual_6_1),as.numeric(input$actual_6_2),as.numeric(input$actual_6_3)))
    resultados_7 <- ceiling(mean(as.numeric(input$actual_7_1),as.numeric(input$actual_7_2),as.numeric(input$actual_7_3)))
    resultados_8 <- ceiling(mean(as.numeric(input$actual_8_1),as.numeric(input$actual_8_2),as.numeric(input$actual_8_3)))
    resultados_9 <- ceiling(mean(as.numeric(input$actual_9_1),as.numeric(input$actual_9_2),as.numeric(input$actual_9_3)))
    
    resultados <- c(resultados_1,resultados_2,resultados_3,resultados_4,resultados_5,resultados_6,resultados_7,resultados_8,resultados_9)
    
    acciones_1 <- ceiling(mean(as.numeric(input$deseado_1_1),as.numeric(input$deseado_1_2),as.numeric(input$deseado_1_3)))
    acciones_2 <- ceiling(mean(as.numeric(input$deseado_2_1),as.numeric(input$deseado_2_2),as.numeric(input$deseado_2_3)))
    acciones_3 <- ceiling(mean(as.numeric(input$deseado_3_1),as.numeric(input$deseado_3_2),as.numeric(input$deseado_3_3)))
    acciones_4 <- ceiling(mean(as.numeric(input$deseado_4_1),as.numeric(input$deseado_4_2),as.numeric(input$deseado_4_3)))
    acciones_5 <- ceiling(mean(as.numeric(input$deseado_5_1),as.numeric(input$deseado_5_2),as.numeric(input$deseado_5_3)))
    acciones_6 <- ceiling(mean(as.numeric(input$deseado_6_1),as.numeric(input$deseado_6_2),as.numeric(input$deseado_6_3)))
    acciones_7 <- ceiling(mean(as.numeric(input$deseado_7_1),as.numeric(input$deseado_7_2),as.numeric(input$deseado_7_3)))
    acciones_8 <- ceiling(mean(as.numeric(input$deseado_8_1),as.numeric(input$deseado_8_2),as.numeric(input$deseado_8_3)))
    acciones_9 <- ceiling(mean(as.numeric(input$deseado_9_1),as.numeric(input$deseado_9_2),as.numeric(input$deseado_9_3)))
    
    acciones <- c(acciones_1,acciones_2,acciones_3,acciones_4,acciones_5,acciones_6,acciones_7,acciones_8,acciones_9)
    
    df_resumen <- data.frame(unique(df_resultados[,1]),resultados,acciones)
    colnames(df_resumen) <- c("Área", "Nivel actual", "Nivel deseado")
    
    df_resumen$Área <- gsub("Conducida por datos","Data driven",df_resumen$Área)
    
    df_resumen
  })
  
  output$tabla_resumen <- renderDataTable({
    df <- resumen()
    df <- datatable(df, extensions = c('FixedHeader','Buttons'), options = list(fixedHeader = TRUE,dom = 'lBfrtip', 
                                                                                buttons = c('copy', 'csv', 'excel', 'pdf')),rownames = FALSE,escape = F)
  })
  
  output$tabla_resultados <- renderDataTable({
    df <- resultados()[,c(1,2,3)]
    df <- datatable(df, extensions = c('FixedHeader','Buttons'), options = list(fixedHeader = TRUE,dom = 'lBfrtip', 
                                                                                buttons = c('copy', 'csv', 'excel', 'pdf')),rownames = FALSE,escape = F)
  })
  
  output$tabla_acciones <- renderDataTable({
    df <- datos_tabla_acciones()[,-6]
    df$Acción <- gsub("[.]",".\n",df$Acción)

    
    df <- datatable(df, extensions = c('FixedHeader','Buttons'), options = list(fixedHeader = TRUE,dom = 'lBfrtip', 
                                                                                buttons = c('copy', 'csv', 'excel', 'pdf')),rownames = FALSE,escape = F)
  })
  
  
  #======================================
  # GRÁFICOS
  #=====================================
  add_closed_trace <- function(p, r, theta, ...) 
  {
    plotly::add_trace(p, r = c(r, r[1]), theta = c(theta, theta[1]), ...)
  }
  
  output$radial_total <- renderPlotly({
    df_resumen <- resumen()
    
    fig <- plot_ly(
      type = 'scatterpolar',
      mode = 'lines',
    )
    fig <- fig %>%
      add_closed_trace(
        r = as.vector(as.numeric(df_resumen$`Nivel actual`)),
        theta = as.vector(as.character(df_resumen$Área)),
        name = "Nivel actual",
        line = list(color = rgb(0.8,0,0,1), width = 3.5),
        hovertemplate = '<b>%{theta}</b> <br><br>Nivel actual: %{r}'
      )
    fig <- fig %>%
      add_closed_trace(
        r = as.vector(as.numeric(df_resumen$`Nivel deseado`)),
        theta = as.vector(as.character(df_resumen$Área)),
        name = "Nivel deseado",
        line = list(color = rgb(0,0.6,0,1), width = 3.5, dash = 'dash'),
        hovertemplate = '<b>%{theta}</b> <br><br>Nivel actual: %{r}'
      )
    fig <- fig %>%
      layout(title = list(text = "Estado de digitalización actual vs deseado", y = -5),
             polar = list(
               radialaxis = list(
                 visible = T,
                 range = c(0,5)
               ),
               angularaxis = list(tickfont = list(size = 13))
             ),
             margin = list(
               l = -5,
               r = 100,
               b = -5,
               t = -25,
               pad = -25
             ),
             annotations = 
               list(x = 1, y = -0.25, text = "    Nivel 1: Naciente
      Nivel 2: Desarrollo
        Nivel 3: Intermedio 
     Nivel 4: Avanzado
   Nivel 5: Maestro", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=10)
               )
      )
    
  })
  
  output$radial_resultados <- renderPlotly({
    df_resultados <- resultados()
    
    fig <- plot_ly(
      type = 'scatterpolar',
      mode = 'lines',
    )
    fig <- fig %>%
      add_closed_trace(
        r = as.vector(as.numeric(df_resultados$Valor)),
        theta = as.vector(as.character(df_resultados$Área)),
        name = "Resultados",
        line = list(color = rgb(0.8,0,0,1), width = 3.5),
        hovertemplate = '<b>%{theta}</b> <br><br>Nivel actual: %{r}'
      )
    fig <- fig %>%
      layout(title = list(text = "Estado de digitalización actual", y = -5),
             polar = list(
               radialaxis = list(
                 visible = T,
                 range = c(0,5)
               ),
               angularaxis = list(tickfont = list(size = 13))
             ),
             margin = list(
               l = -5,
               r = 100,
               b = -5,
               t = -25,
               pad = -25
             ),
             annotations = 
               list(x = 1, y = -0.25, text = "    Nivel 1: Naciente
      Nivel 2: Desarrollo
        Nivel 3: Intermedio 
     Nivel 4: Avanzado
   Nivel 5: Maestro", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=10)
               )
      )
    
  })
  
  output$radial_acciones <- renderPlotly({
    df_acciones <- acciones()
    
    fig <- plot_ly(
      type = 'scatterpolar',
      mode = 'lines',
    )
    fig <- fig %>%
      add_closed_trace(
        r = as.vector(as.numeric(df_acciones$Valor)),
        theta = as.vector(as.character(df_acciones$Área)),
        name = "Acciones",
        line = list(color = rgb(0,0.6,0,1), width = 3.5),
        hovertemplate = '<b>%{theta}</b> <br><br>Nivel deseado: %{r}'
      ) 
    fig <- fig %>%
      layout(title = list(text = "Estado de digitalización deseado", y = -5),
             polar = list(
               radialaxis = list(
                 visible = T,
                 range = c(0,5)
               ),
               angularaxis = list(tickfont = list(size = 13))
             ),
             margin = list(
               l = -5,
               r = -5,
               b = -5,
               t = -25,
               pad = -25
             ),
             annotations = 
               list(x = 1, y = -0.25, text = "    Nivel 1: Naciente
      Nivel 2: Desarrollo
        Nivel 3: Intermedio 
     Nivel 4: Avanzado
   Nivel 5: Maestro", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=10)
               )
      )
  })
  
  output$descarga_datos_xlsx <- downloadHandler(
    
    filename = paste0("Estado_y_hoja_de_ruta_digital_",Sys.Date(),".xlsx"),
    content  = function(file) {
      
      df_estado_actual <- resultados()[,c(1,2,3)]
      df_estado_deseado <- datos_tabla_acciones()[,c(1,2,3)]
      
      wb <- createWorkbook()
      addWorksheet(wb, sheetName = "Estado actual")
      writeData(wb, sheet = 1, x = df_estado_actual)
      addWorksheet(wb, sheetName = "Estado deseado")
      writeData(wb, sheet = 2, x = df_estado_deseado)
      saveWorkbook(wb, file)
    }
  )

  
}

# ==============================================================================
# Run the application
shinyApp(ui = ui, server = server)
