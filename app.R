#
# Aplicación en Shiny para visualizaciones
#

library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(rjson)
library(mapSpain)
library(ggplot2)


load("INPUT/DATA/gbif_tabla.Rdata")
load("INPUT/DATA/gbif_meds.Rdata")
load("INPUT/DATA/tabla_casos.Rdata")

spain <- esp_get_prov()
can <- esp_get_can_box()
  
gbif_local <- merge(spain, gbif_tabla, by.x = "ine.prov.name", by.y = "Provincia")

ui <- page_sidebar(
  title = "Biodiversidad",
  sidebar = sidebar(
    title = "Dashboard",
    helpText("¡Estudia los efectos de la biodiversidad en la salud humana!"),
    selectInput(
      "var",
      label = "Elige qué quiere visualizar:",
      choices = list("Riqueza Específica", "Diversidad por kilómetro cuadrado")
    ),
    sliderInput(
      "year",
      label = "Selecciona un año:",
      min = 2000, max = 2020, value = 2010,
      sep = ""
    ),
  ),
  page_fillable(
    layout_columns(
      value_box(
        title = "TOTAL DE",
        value = textOutput("n_riqueza"),
        "ESPECIES REGISTRADAS EN EL AÑO SELECCIONADO",
        showcase = icon("otter", class = "light")
      ),
      value_box(
        title = "TOTAL DE",
        value = textOutput("pacientes"),
        "HOSPITALIZADOS POR INFECCIÓN O ENFERMEDAD RESPIRATORIA",
        showcase = icon("hospital", class = "regular")
      ),
      card(
        card_header("Visto en un Mapa..."),
        leafletOutput("map")
      ),
      card(
        card_header("Variación en el Consumo de Fármacos"),
        plotOutput("medicacion")
      ),
      col_widths = c(6,6,7,5),
      row_heights = c(1, 4)
    )
  )
)
  
server <- function(input, output){
  selectedYear <- reactive({
    gbif_local %>%
      filter(Año == input$year)
  })
  
  selectedYearPac <- reactive({
    df_casos %>%
      filter(Año == input$year)
  })
  
  output$n_riqueza = renderText({
    sum(selectedYear()$n_especies, na.rm = TRUE)
  })
  
  output$pacientes = renderText({
    sum(selectedYearPac()$Altas, na.rm = TRUE)
  })
  
  output$map = renderLeaflet({
    datos <- selectedYear()
    variable <- switch(
      input$var,
      "Riqueza Específica" = datos$n_especies,
      "Diversidad por kilómetro cuadrado"  = datos$indice_km2
    )
    datos$variable <- variable
    dominio <- range(variable)
    pal <- colorNumeric(palette = "Spectral", domain = dominio)
    leaflet(datos) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -3.5, lat = 40, zoom = 6) %>%
      addPolygons(
        fillColor = ~pal(variable),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.8,
        label = ~paste0(ine.prov.name, ": ", variable, " especies"),
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(
        pal = pal,
        values = dominio,
        title = input$var,
        position = "bottomright",
        opacity = 1
      )
  })
  
  output$medicacion = renderPlot({
    validate(
      need(input$year > 2005 & input$year <= 2018,
           "No hay datos de consumo de fármacos para este año")
    )
    
    datos <- gbif_meds %>%
      filter(Año == input$year) %>%
      mutate(
        delta_num = gsub(" %", "", `X..Δ`),        # Quitar " %"
        delta_num = gsub(",", ".", delta_num),     # Cambiar coma por punto
        delta_num = as.numeric(delta_num)          # Convertir a numérico
      ) %>%
      group_by(CCAA) %>%
      summarise(delta_promedio = mean(delta_num, na.rm = TRUE))
    
    ggplot(datos, aes(x = CCAA, y = delta_promedio)) +
    geom_bar(stat = "identity", fill = "deeppink") +
    scale_y_continuous(
      limits = c(-10, 10),           # De -10% a 10%
      breaks = seq(-10, 0, 2)      # Saltos cada 10%
    ) +
    labs(
      x = "Comunidad Autónoma",
      y = "X..Δ"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(size = 6),
    )
  })
}


shinyApp(ui = ui, server = server)
