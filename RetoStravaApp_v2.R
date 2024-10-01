# Cargar librerías necesarias
# ---------------------------
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(bslib)
library(DT)
# ---------------------------

# Se define la interfaz de usuario
# --------------------------------

# UI con navbarPage para separar las secciones en páginas distintas:
ui <- navbarPage(
  title = "Reto Strava El Tricicle",
  id = "navbar",  # ID del navegador => Para asegurar que cada objeto se muestra solo donde se quiere.

  # Carga de datos / Actualización de parámetros:
  tabPanel("Carga de archivos / Parámetros del reto",
           fluidPage(theme = shinytheme("united"), # shinythemes::themeSelector()
                     fluidRow( # Se añade un layout fluido con 2 columnas, una para cada "card".
                       # "Card" para la carga de archivos:
                       column(6, offset = 0, align = "center",
                              card(card_header(HTML("Carga y procesamiento de los archivos personales 
                                                    <i>*.csv</i> exportados desde Strava")),
                                   hr(),
                                   # Introducción de los datos para cada usuario:
                                   fileInput("Aitor", HTML("Aitor, selecciona tu archivo <i>activities.csv:</i>"), 
                                             accept = ".csv", width = "100%"),
                                   fileInput("Carlos", HTML("Carlos, selecciona tu archivo <i>activities.csv:</i>"), 
                                             accept = ".csv", width = "100%"),
                                   fileInput("Ignacio", HTML("Ignacio, selecciona tu archivo <i>activities.csv:</i>"), 
                                             accept = ".csv", width = "100%"),
                                   style = "text-align: center;"  # Centra el contenido dentro de la "card".
                                   )
                              ),
                         
                       # "Card" para los parámetros del reto:
                       column(6, offset = 0, align = "center",
                              card(card_header("Parámetros del reto"),
                                   hr(),
                                   dateInput("fecha_inicio", "Fecha de inicio del reto:", value = "2024-09-01", width = "100%"),
                                   dateInput("fecha_fin", "Fecha de fin del reto:", value = "2025-07-31", width = "100%"),
                                   numericInput("factor_correccion", "Factor de esfuerzo calculado:", 
                                                value = 10, min = 0, step = 10, width = "100%"),
                                   style = "text-align: center;"  # Centra el contenido dentro de la "card".
                                   )
                              )
                       ),
                     
                     # "Centrado del botón "Agregar / Actualizar", y del mensaje, debajo de las "cards":
                     fluidRow(
                       column(12, align = "center", 
                              actionButton("agregar_actualizar_param", "Agregar / Actualizar"),
                              hr(),
                              textOutput("mensaje"),
                              style = "margin-top: 20px;"  # Añade el espacio necesario entre las "cards" y el botón.
                              )
                       )
                     )
           ),
  
  # Menú desplegable para las tablas:
  navbarMenu("Tablas de datos",
             tabPanel("Datos mensuales para cada ciclista",
                      fluidPage(theme = shinytheme("united"), # shinythemes::themeSelector()
                                # Se garantiza el centrado del texto:
                                tags$style(type = "text/css", "h4 { text-align: center; }
                                table { margin-left: auto; margin-right: auto; }th, td { text-align: center; }"),
                                conditionalPanel(
                                  condition = "input.navbar == 'Datos mensuales para cada ciclista'",
                                  h4("Datos mensuales para cada ciclista a lo largo del reto"),
                                  tableOutput("df_final_mensual")  # Muestra el DF (DF o df) final en una pestaña separada.
                                  )
                                )
                      ),
             
             tabPanel("Datos totales acumulados",
                      fluidPage(theme = shinytheme("united"), # shinythemes::themeSelector()
                                # Se garantiza el centrado del texto:
                                tags$style(type = "text/css", "h4 { text-align: center; }
                                table { margin-left: auto; margin-right: auto; }th, td { text-align: center; }"),
                                conditionalPanel(
                                  condition = "input.navbar == 'Datos totales acumulados'",
                                  h4("Datos totales acumulados para cada ciclista a lo largo del reto"),
                                  tableOutput("df_final_total_acumulado")  # Muestra el DF (DF o df) final en una pestaña separada.
                                )
                      )
             )
             ),
  
  # Menú desplegable para los gráficos:
  navbarMenu("Gráficos",
             tabPanel("Gráfico 3D Distancia vs. Desnivel vs. Mes",
                      fluidPage(theme = shinytheme("united"), # shinythemes::themeSelector()
                                # Se garantiza el centrado del texto:
                                tags$style(type = "text/css", "h4 { text-align: center; }"),
                                conditionalPanel(
                                  condition = "input.navbar == 'Gráfico 3D Distancia vs. Desnivel vs. Mes'",
                                  h4(HTML("Gráfico 3D Distancia vs. Desnivel vs. Mes (Intensidad dada por el <i>Esfuerzo calculado<i>)")),
                                  plotlyOutput("grafico_burbujas", height = "800px")
                                )
                      )
             ),
             
             tabPanel("Gráfico 2D Mes vs. Distancia",
                      fluidPage(theme = shinytheme("united"), # shinythemes::themeSelector()
                                # Se garantiza el centrado del texto:
                                tags$style(type = "text/css", "h4 { text-align: center; }"),
                                conditionalPanel(
                                  condition = "input.navbar == 'Gráfico 2D Mes vs. Distancia'",
                                  h4(HTML("Gráfico 2D Mes vs. Distancia (Intensidad dada por el <i>Esfuerzo calculado<i>)")),
                                  plotlyOutput("bubble_plot_2d", height = "600px")
                                )
                      )
             ),
             
             tabPanel("Gráfico 2D Distancia vs. Desnivel total acumulado",
                      fluidPage(theme = shinytheme("united"), # shinythemes::themeSelector()
                                # Se garantiza el centrado del texto:
                                tags$style(type = "text/css", "h4 { text-align: center; }"),
                                conditionalPanel(
                                  condition = "input.navbar == 'Gráfico 2D Distancia vs. Desnivel total acumulado'",
                                  h4(HTML("Gráfico 2D Distancia vs. Desnivel (Intensidad dada por el <i>Esfuerzo calculado<i>)")),
                                  plotlyOutput("bubble_plot_2d_acumulado", height = "600px")
                                )
                      )
             )
             )
  )

# --------------------------------

# Adecuación del servidor para almacenar los datos cargados
# ---------------------------------------------------------

# Se define/crea la carpeta en el servidor donde se guardarán los archivos:
data_folder <- "data_uploaded/"

if (!dir.exists(data_folder)) {
  dir.create(data_folder) # Crea la carpeta si esta no existe.
}

# Función que guarda el archivo en la carpeta del servidor:
guardar_archivo_en_servidor <- function(file_input, nombre_ciclista) {
  
  # Se define la ruta donde se guardará el archivo:
  ruta_guardado <- file.path(data_folder, paste0(nombre_ciclista, "_activities.csv"))
  
  # Y se mueve el archivo subido a la carpeta del servidor, sobreescribiéndolo si ya existe:
  file.copy(file_input$datapath, ruta_guardado, overwrite = TRUE)
}

# Función que carga el archivo desde el servidor:
cargar_archivo_desde_servidor <- function(nombre_ciclista) {
  ruta_archivo <- file.path(data_folder, paste0(nombre_ciclista, "_activities.csv"))
  if (file.exists(ruta_archivo)) {
    return(read.csv(ruta_archivo))
  } else {
    return(NULL)
  }
}

# Función que procesará los archivos:
procesar_archivo <- function(ruta_archivo, nombre_ciclista,
                             fecha_inicio, fecha_fin, factor_correccion) {
  
  df <- read.csv(ruta_archivo)  # Lee el archivo *.csv como DF.
  
  # # Tratamiento del DF (conversiones, formatos, filtrados y cálculos):
  # DF <- data.frame(df$Fecha.de.la.actividad, df$Tiempo.en.movimiento,
  #                  df$Distancia , df$Ritmo.cardiaco.promedio,
  #                  df$Desnivel.positivo, df$Esfuerzo.Relativo)
  # colnames(DF) <- c("Fecha", "Tiempo_en_movimiento", "Distancia",
  #                   "Ritmo_cardiaco_promedio", "Desnivel_positivo",
  #                   "Esfuerzo_Relativo")
  
  # Tratamiento del DF (conversiones, formatos, filtrados y cálculos):
  DF <- data.frame(df$Fecha.de.la.actividad, df$Distancia ,
                   df$Desnivel.positivo, df$Esfuerzo.Relativo)
  colnames(DF) <- c("Fecha", "Distancia", "Desnivel_positivo",
                    "Esfuerzo_Relativo")
  
  # Se reemplazan los posibles NA con el promedio de su columna respectiva:
  DF <- DF %>%
    mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  
  # Conversión de la columna "Fecha":
  Month_dict <- c(" ene. " = "/01/", " feb. " = "/02/", " mar. " = "/03/",
                  " abr. " = "/04/", " may. " = "/05/", " jun. " = "/06/",
                  " jul. " = "/07/", " ago. " = "/08/", " sept. " = "/09/",
                  " oct. " = "/10/", " nov. " = "/11/", " dic. " = "/12/")
  
  DF$Fecha <- as.Date(sub(" .*", "", str_replace_all(DF$Fecha, Month_dict)),
                      format = "%d/%m/%Y")
  
  # Conversión de las columnas numéricas "Tiempo_en_movimiento", "Distancia",
  # "Ritmo_cardiaco_promedio", "Desnivel_positivo" y "Esfuerzo_Relativo":
  # DF$Tiempo_en_movimiento <- round(as.numeric(gsub(",", ".", DF$Tiempo_en_movimiento)), 0)
  DF$Distancia <- round(as.numeric(gsub(",", ".", DF$Distancia)), 0)
  # DF$Ritmo_cardiaco_promedio <- round(as.numeric(gsub(",", ".", DF$Ritmo_cardiaco_promedio)), 0)
  DF$Desnivel_positivo <- round(as.numeric(gsub(",", ".", DF$Desnivel_positivo)), 0)
  DF$Esfuerzo_Relativo <- round(as.numeric(gsub(",", ".", DF$Esfuerzo_Relativo)), 0)
  
  # Filtrado del DF según las fechas del reto:
  DF <- DF %>%
    # filter(Fecha >= "2023-09-01" & Fecha <= "2024-07-31") # Se fija el reto actual.
    filter(Fecha >= fecha_inicio & Fecha <= fecha_fin)
  
  
  # Cálculo del esfuerzo usando el factor de corrección proporcionado:
  DF <- DF %>%
    mutate(
      Ciclista = rep(nombre_ciclista, n()),  # Identificación del ciclista.
      Esfuerzo_calculado = as.numeric(Distancia) * (1 + (as.numeric(Desnivel_positivo)
                                                         / (as.numeric(Distancia) * 1000)) 
                                                    * as.numeric(factor_correccion))
    )
  
  # Se mueve la columna "Ciclista" a la primera posición:
  DF <- DF[c("Ciclista", setdiff(names(DF), "Ciclista"))]
  
  # Se reemplazan los posibles NA o NAn con el promedio de su columna respectiva:
  DF <- DF %>%
    mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  
  # Redondeamos las columnas necesarias:
  DF$Esfuerzo_calculado <- round(as.numeric(gsub(",", ".", DF$Esfuerzo_calculado)), 0)
  
  # Aseguramos la clase de la columna "Fecha":
  DF$Fecha <- as.Date(DF$Fecha, format = "%d/%m/%Y")

  # Útiles:
  # colSums(is.na(DF)) # Cuenta los NA de cada columna ≅ Verificación que no hay NA.
  # sapply(DF, class) # Da cuenta del tipo de variavle de cada columna del DF.
  
  return(DF)  # Devuelve el DF procesado.
}

# ---------------------------------------------------------

# Definir la lógica del servidor
# ------------------------------
server <- function(input, output, session) {
  
  # Se crea un objeto reactivo que almacena los datos:
  valores <- reactiveValues(df_list = NULL)
  
  # Eventos al hacer clic en "Agregar archivos al servidor" o "Actualizar parámetros":
  # ----------------------------------------------------------------------------------
  
  # Manejo del evento de agregar archivos a la lista:
  observeEvent(input$agregar_actualizar_param, {
    if (!is.null(input$Aitor)) {
      guardar_archivo_en_servidor(input$Aitor, "Aitor")
    }
    if (!is.null(input$Carlos)) {
      guardar_archivo_en_servidor(input$Carlos, "Carlos")
    }
    if (!is.null(input$Ignacio)) {
      guardar_archivo_en_servidor(input$Ignacio, "Ignacio")
    }
    
    # Mostrar un mensaje de éxito:
    output$mensaje <- renderText({"Archivos guardados exitosamente / Parámetros actualizados correctamente."})
    
    # Actualizar los datos:
    actualizar_datos()
  })
  
  # Actualización automática cada vez que cambian los parámetros del reto
  observeEvent(input$agregar_actualizar_param, {
    # Actualizar los datos:
    actualizar_datos()
    
    # Mostrar un mensaje de éxito:
    output$mensaje <- renderText({"Archivos guardados exitosamente / Parámetros actualizados correctamente."})
  })
  
  # Función para actualizar los datos en base a los archivos cargados y parámetros
  actualizar_datos <- function() {
    # Parámetros del reto:
    fecha_inicio <- input$fecha_inicio
    fecha_fin <- input$fecha_fin
    factor_correccion <- input$factor_correccion
    
    # Procesar los archivos de cada ciclista si existen:
    df_Aitor <- if (!is.null(cargar_archivo_desde_servidor("Aitor"))) {
      procesar_archivo(file.path(data_folder, "Aitor_activities.csv"), "Aitor", fecha_inicio, fecha_fin, factor_correccion)
    }
    
    df_Carlos <- if (!is.null(cargar_archivo_desde_servidor("Carlos"))) {
      procesar_archivo(file.path(data_folder, "Carlos_activities.csv"), "Carlos", fecha_inicio, fecha_fin, factor_correccion)
    }
    
    df_Ignacio <- if (!is.null(cargar_archivo_desde_servidor("Ignacio"))) {
      procesar_archivo(file.path(data_folder, "Ignacio_activities.csv"), "Ignacio", fecha_inicio, fecha_fin, factor_correccion)
    }
    
    # Combinar los datos en una lista reactiva
    valores$df_list <- list(df_Aitor, df_Carlos, df_Ignacio) %>%
      purrr::compact() %>%  # Elimina NULL en caso de no existir algún archivo.
      bind_rows()
  }
  
  # ----------------------------------------------------------------------------------
  
  # Se crea el reactivo para el DF final:
  # -------------------------------------
  df_final <- reactive({
    req(valores$df_list)
    valores$df_list
  })
  # -------------------------------------
  
  # Se crea el DF final con los acumulados por mes y ciclista:
  # ----------------------------------------------------------
  df_final_mensual <- reactive({
    req(valores$df_list)
    valores$df_list %>%
      mutate(Mes = format(Fecha, "%Y-%m")) %>%
      group_by(Ciclista, Mes) %>%
      summarise(
        Distancia_acumulada = round(as.numeric(sum(Distancia)), 0),
        Desnivel_acumulado = round(as.numeric(sum(Desnivel_positivo)), 0),
        Esfuerzo_promedio = round(as.numeric(mean(Esfuerzo_calculado)), 0),
        .groups = 'drop'
      )
      
  })
  # ----------------------------------------------------------
  
  # Se crea el DF final con los acumulados en todo el reto por ciclista:
  # --------------------------------------------------------------------
  df_final_total_acumulado <- reactive({
    req(valores$df_list)
    valores$df_list %>%
      group_by(Ciclista) %>%
      summarise(
        Distancia_total_acumulada = sum(Distancia),
        Desnivel_total_acumulado = sum(Desnivel_positivo),
        Esfuerzo_total_promedio = mean(Esfuerzo_calculado),
        .groups = 'drop'
      )
  })
  # --------------------------------------------------------------------
  
  # Se muestra la tabla de datos mensuales por ciclista con las columnas renombradas:
  # ---------------------------------------------------------------------------------
  output$df_final_mensual <- renderTable({
    req(df_final_mensual())
    df <- df_final_mensual()  # Se obtiene el DF.
    
    # Verificar si el DF es NULL o está vacío:
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    # Redondeamos las columnas necesarias, convirtiendo a enteros sin decimales utilizando "format()":
    df$Distancia_acumulada <- format(round(df$Distancia_acumulada, 0), nsmall = 0)
    df$Desnivel_acumulado <- format(round(df$Desnivel_acumulado, 0), nsmall = 0)
    df$Esfuerzo_promedio <- format(round(df$Esfuerzo_promedio, 0), nsmall = 0)
    
    # Se renombran adecuadamente las columnas:
    colnames(df) <- c("Ciclista", "Mes", "Distancia acumulada (km)",
                      "Desnivel acumulado (m)", "Esfuerzo calculado promedio")
    df  # Devuelve el DF si tiene datos.
  })
  # ---------------------------------------------------------------------------------
  
  # Se muestra la tabla de datos acumulados durante el reto por ciclista con las columnas renombradas:
  # --------------------------------------------------------------------------------------------------
  output$df_final_total_acumulado <- renderTable({
    req(df_final_total_acumulado())
    df <- df_final_total_acumulado()  # Se obtiene el DF.
    
    # Verificar si el DF es NULL o está vacío:
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    # Redondeamos las columnas necesarias, convirtiendo a enteros sin decimales utilizando "format()":
    df$Distancia_total_acumulada <- format(round(df$Distancia_total_acumulada, 0), nsmall = 0)
    df$Desnivel_total_acumulado <- format(round(df$Desnivel_total_acumulado, 0), nsmall = 0)
    df$Esfuerzo_total_promedio <- format(round(df$Esfuerzo_total_promedio, 0), nsmall = 0)
    
    # Se renombran adecuadamente las columnas:
    colnames(df) <- c("Ciclista", "Distancia acumulada (km)", 
                      "Desnivel acumulado (m)", "Esfuerzo calculado promedio")
    df  # Devuelve el DF si tiene datos.
  })
  # --------------------------------------------------------------------------------------------------
  
  # Gráfico 3D Distancia-Desnivel-Mes  (Esfuerzo calculado):
  # --------------------------------------------------------
  output$grafico_burbujas <- renderPlotly({
    req(df_final_mensual())
    df <- df_final_mensual()
    if (is.null(df) || nrow(df) == 0) return(NULL)  # Verificación que el DF no esté vacío.

    # Se convierte el Mes a formato numérico para el eje X:
    df$Mes_num <- as.numeric(as.factor(df$Mes))

    # Gráfico de burbujas 3D: color asociado a cada ciclista, tamaño de burbujas asociado con el esfuerzo.
    p <- plot_ly(df, x = ~Distancia_acumulada, y = ~Desnivel_acumulado, z = ~Mes_num,
                 color = ~Ciclista, size = ~Esfuerzo_promedio, type = 'scatter3d', mode = 'markers',
                 marker = list(sizemode = 'diameter', opacity = 0.5)) %>%
      layout(
        scene = list(
          xaxis = list(title = 'Distancia acumulada (km)'),
          yaxis = list(title = 'Desnivel acumulado (m)'),
          zaxis = list(title = 'Mes', tickvals = df$Mes_num, ticktext = df$Mes)
        ),
        legend = list(title = list(text = "Ciclista"), orientation = 'v')
      )

    return(p)
  })
  # --------------------------------------------------------
  
  # Gráfico 2D Mes-Distancia (Esfuerzo calculado):
  # ----------------------------------------------
  output$bubble_plot_2d <- renderPlotly({
    req(df_final_mensual())
    df <- df_final_mensual()
    if (is.null(df) || nrow(df) == 0) return(NULL)  # Verificación que el DF no esté vacío.
    
    # Se convierte el Mes a formato numérico para el eje X:
    df$Mes_num <- as.factor(df$Mes)
    
    df %>%
      group_by(Ciclista, Mes) %>%
      ungroup()  # Se asegura que no haya agrupamientos persistentes.
    
    # Gráfico de burbujas 2D: color asociado a cada ciclista
    plot_ly(df, x = ~Mes, y = ~Distancia_acumulada,
            color = ~Ciclista, size = ~Esfuerzo_promedio, type = 'scatter', mode = 'markers',
            marker = list(sizemode = 'diameter', opacity = 0.5)) %>%
      layout(
        xaxis = list(title = 'Mes', tickvals = df$Mes_num, ticktext = df$Mes),
        yaxis = list(title = 'Distancia acumulada (km)')
      )
  })
  # ----------------------------------------------
  
  # Gráfico 2D Distancia-Desnivel total acumulado (Esfuerzo calculado):
  # -------------------------------------------------------------------
  output$bubble_plot_2d_acumulado <- renderPlotly({
    req(df_final_total_acumulado())
    df <- df_final_total_acumulado()
    if (is.null(df) || nrow(df) == 0) return(NULL)  # Verificación que el DF no esté vacío.
    
    df %>%
      group_by(Ciclista) %>%
      ungroup()  # Se asegura que no haya agrupamientos persistentes.
    
    # Gráfico de burbujas 2D: color asociado a cada ciclista
    plot_ly(df, x = ~Distancia_total_acumulada, y = ~Desnivel_total_acumulado,
            color = ~Ciclista, size = ~Esfuerzo_total_promedio, type = 'scatter', mode = 'markers',
            marker = list(sizemode = 'diameter', opacity = 0.5)) %>%
      layout(
        xaxis = list(title = 'Distancia acumulada total durante el reto (km)'),
        yaxis = list(title = 'Desnivel acumulado total durante el reto (km)')
      )
  })
  # ------------------------------------------------------------------- 
  
  # ------------------------------------------------------------------
}
# ------------------------------

# Ejecutar la aplicación Shiny
# ----------------------------
shinyApp(ui = ui, server = server)
# ----------------------------


# Utilidades
# ----------

# Consola:

# Deploy de la App en los servidores de Shiny:
# rsconnect::deployApp("ruta")

# ----------
