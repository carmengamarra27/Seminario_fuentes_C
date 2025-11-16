#
# Aplicación en Shiny para visualizaciones
#

# install.packages("highcharter")

library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(rjson)
library(mapSpain)


gbif_df <- as.data.frame(fromJSON(file = "DATA/gbifJSON.json"))

spain <- esp_get_prov()
can <- esp_get_can_box()
  
gbif_local <- merge(spain, gbif_df, by.x = "ine.prov.name", by.y = "provincia")

ui <- page_sidebar(
  title = "Biodiversidad",
  sidebar = sidebar(
    title = "Opciones",
    helpText("Crea mapas de biodiversidad con distintas variables."),
    selectInput(
      "var",
      label = "Elige qué quiere visualizar:",
      choices = list("Riqueza Específica", "Diversidad por kilómetro cuadrado", "Hospitalizados")
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
      card("Total de Especies"),
      card("Total de Hospitalizados por Infección"),
      card("Fármacos más Empleados"),
      col_widths = c(3, 3, 5.5),
      row_heights = c(1, 1, 2)
    ),
    layout_columns(
      card(
        card_header("Visto en un Mapa..."),
        leafletOutput("map")
      ),
      card(
        card_header("Índices de Diversidad"),
        leafletOutput("gbif_tabla")
      ),
      col_widths = c(7, 5),
      row_heights = c(2, 1)
    )
  )
)
  
server <- function(input, output){
  selectedYear <- reactive({
    gbif_local %>%
      filter(year == input$year)
  })
  output$map = renderLeaflet({
    datos <- selectedYear()
    variable <- switch(
      input$var,
      "Riqueza Específica" = datos$n_especies,
      "Diversidad por kilómetro cuadrado"  = datos$indice_km2
      #"Hospitalizados"     = dat$pacientes
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
        popup = ~paste0("<b>Provincia:</b> ", ine.prov.name, "<br>", 
                        " · ", variable, " especies"),
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
}


shinyApp(ui = ui, server = server)
