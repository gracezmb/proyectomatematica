library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)

# Definir las carreras y sus características
carreras <- c("Ingeniería", "Medicina", "Derecho", "Artes", "Ciencias", "Economía", "Psicología", "Arquitectura")
caracteristicas <- c("Matemáticas", "Ciencias", "Lectura", "Creatividad", "Análisis", "Comunicación", "Empatía", "Diseño")

# Matriz de pesos
pesos <- matrix(c(
  9, 8, 6, 5, 9, 6, 4, 7,  # Ingeniería
  7, 9, 8, 5, 8, 8, 9, 4,  # Medicina
  6, 5, 9, 6, 9, 9, 7, 4,  # Derecho
  5, 4, 7, 10, 6, 8, 7, 9, # Artes
  9, 9, 7, 6, 8, 7, 5, 5,  # Ciencias
  8, 6, 8, 7, 9, 8, 6, 5,  # Economía
  6, 7, 8, 7, 8, 9, 10, 5, # Psicología
  7, 5, 6, 9, 7, 7, 6, 10  # Arquitectura
), nrow = 8, byrow = TRUE) 

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Asesor Avanzado de Elección de Carrera"),
  sidebarLayout(
    sidebarPanel(
      h4("Evalúa tus habilidades e intereses"),
      lapply(caracteristicas, function(caract) {
        sliderInput(caract, label = caract, min = 0, max = 10, value = 5)
      })
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Resultados", 
                 h3("Compatibilidad con carreras"),
                 plotlyOutput("grafico_resultados"),
                 h3("Ranking de carreras"),
                 tableOutput("tabla_resultados")
        ),
        tabPanel("Perfil personal", 
                 h3("Tu perfil de habilidades"),
                 plotlyOutput("grafico_radar"),
                 verbatimTextOutput("analisis_perfil")
        ),
        tabPanel("Información de carreras", 
                 h3("Detalles de la carrera"),
                 selectInput("carrera_info", "Selecciona una carrera:", 
                             choices = c("Selecciona una carrera" = "", carreras)),
                 verbatimTextOutput("info_carrera")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  resultados <- reactive({
    puntajes_usuario <- sapply(caracteristicas, function(caract) input[[caract]])
    puntajes_carreras <- pesos %*% puntajes_usuario
    resultados <- data.frame(
      Carrera = carreras,
      Puntaje = puntajes_carreras
    ) %>% 
      mutate(Compatibilidad = round(Puntaje / max(Puntaje) * 100, 1)) %>%
      arrange(desc(Compatibilidad))
    resultados
  })
  
  output$grafico_resultados <- renderPlotly({
    datos <- resultados()
    plot_ly(datos, x = ~Carrera, y = ~Compatibilidad, type = "bar", 
            marker = list(color = "rgba(50, 171, 96, 0.7)")) %>%
      layout(title = "Compatibilidad con carreras",
             xaxis = list(title = ""),
             yaxis = list(title = "Compatibilidad (%)", range = c(0, 100)))
  })
  
  output$tabla_resultados <- renderTable({
    resultados() %>% select(Carrera, Compatibilidad)
  })
  
  output$grafico_radar <- renderPlotly({
    valores <- sapply(caracteristicas, function(caract) input[[caract]])
    plot_ly(
      type = 'scatterpolar',
      r = valores,
      theta = caracteristicas,
      fill = 'toself',
      mode = 'lines'  
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0, 10)
          )
        ),
        showlegend = F
      )
  })
  
  output$analisis_perfil <- renderText({
    valores <- sapply(caracteristicas, function(caract) input[[caract]])
    fortalezas <- caracteristicas[valores >= 5]
    debilidades <- caracteristicas[valores <= 4]
    
    analisis <- "Análisis de tu perfil:\n\n"
    if (length(fortalezas) > 0) {
      analisis <- paste0(analisis, "Fortalezas: ", paste(fortalezas, collapse = ", "), "\n")
    }
    if (length(debilidades) > 0) {
      analisis <- paste0(analisis, "Áreas de mejora: ", paste(debilidades, collapse = ", "), "\n")
    }
    analisis
  })
  
  output$info_carrera <- renderText({
    carrera_seleccionada <- input$carrera_info
    if (carrera_seleccionada == "") {
      return("Selecciona una carrera para ver información detallada.")
    }
    
    info <- switch(carrera_seleccionada,
                   "Ingeniería" = "La ingeniería aplica principios científicos y matemáticos para resolver problemas prácticos. Campos incluyen civil, mecánica, eléctrica, entre otros.",
                   "Medicina" = "La medicina se enfoca en el diagnóstico, tratamiento y prevención de enfermedades. Requiere un largo periodo de estudio y práctica clínica.",
                   "Derecho" = "El derecho estudia las leyes y su aplicación en la sociedad. Los abogados pueden especializarse en áreas como penal, civil, corporativo, etc.",
                   "Artes" = "Las artes abarcan disciplinas creativas como pintura, música, teatro y danza. Fomenta la expresión personal y la innovación.",
                   "Ciencias" = "Las ciencias exploran el mundo natural a través de la observación y experimentación. Incluye física, química, biología y más.",
                   "Economía" = "La economía estudia la producción, distribución y consumo de bienes y servicios. Analiza mercados, políticas económicas y tendencias globales.",
                   "Psicología" = "La psicología estudia el comportamiento humano y los procesos mentales. Aplicaciones en clínica, educación, organizaciones y más.",
                   "Arquitectura" = "La arquitectura combina arte y ciencia para diseñar edificios y espacios. Requiere creatividad, habilidades técnicas y conciencia ambiental."
    )
    paste("Información sobre", carrera_seleccionada, ":\n\n", info)
  })
}

shinyApp(ui = ui, server = server)