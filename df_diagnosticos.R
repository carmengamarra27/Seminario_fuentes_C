#Data frame con las altas por diagnóstico y provincia desde 2000 a 2020
library(pxR)
library(dplyr)
library(stringr)
library(tidyverse)

#Ruta incompleta para hacer bucle por año, dado que los archivos tienen como nombre: morb_año
ruta <- "INPUT/datos_morbilidad/diagnostico/"

#Vector de años
años <- c(2000:2020)

#Lista vacía donde ir almacenando los df de cada año
lista_df <- list()

#bucle para importar datos de cada año
for (año in años) {
  nombre_archivo <- paste0(ruta, "morb_",año,".px")
  
  datos_anuales <- read.px(nombre_archivo)
  df_anual <- as.data.frame(datos_anuales)
  
#matches busca coincidencias en los nombres (varían nombres de las columnas según el año aunque se refieran a lo mismo)
  df_anual <- df_anual %>%
    rename(Diagnostico_principal = matches("Diagnóstico"),
           Lugar_hospitalizacion = matches("Provincia"),
           Altas = matches("value")
    ) %>%
    mutate(
      Lugar_hospitalizacion = case_when(
        str_detect(Lugar_hospitalizacion, "Álava|Araba|Alava|ALAVA") ~ "Araba/Álava",
        str_detect(Lugar_hospitalizacion, "Albacete") ~ "Albacete",
        str_detect(Lugar_hospitalizacion, "Alicante|Alacant") ~ "Alicante/Alacant",
        str_detect(Lugar_hospitalizacion, "Almería|Almeria") ~ "Almería",
        str_detect(Lugar_hospitalizacion, "Asturias|Asturies|Oviedo") ~ "Asturias",
        str_detect(Lugar_hospitalizacion, "Ávila|Avila") ~ "Ávila",
        str_detect(Lugar_hospitalizacion, "Badajoz") ~ "Badajoz",
        str_detect(Lugar_hospitalizacion, "Barcelona|Barcelonés") ~ "Barcelona",
        str_detect(Lugar_hospitalizacion, "Burgos") ~ "Burgos",
        str_detect(Lugar_hospitalizacion, "Cáceres|Caceres") ~ "Cáceres",
        str_detect(Lugar_hospitalizacion, "Cádiz") ~ "Cádiz",
        str_detect(Lugar_hospitalizacion, "Cantabria|Cantabric|Santander") ~ "Cantabria",
        str_detect(Lugar_hospitalizacion, "Castellón|Castelló") ~ "Castellón/Castelló",
        str_detect(Lugar_hospitalizacion, "Ceuta") ~ "Ceuta",
        str_detect(Lugar_hospitalizacion, "Ciudad Real") ~ "Ciudad Real",
        str_detect(Lugar_hospitalizacion, "Córdoba|Cordoba") ~ "Córdoba",
        str_detect(Lugar_hospitalizacion, "Coruña|coruña|Coruna|coruna") ~ "Coruña, A",
        str_detect(Lugar_hospitalizacion, "Cuenca") ~ "Cuenca",
        str_detect(Lugar_hospitalizacion, "Gerona|Girona") ~ "Girona",
        str_detect(Lugar_hospitalizacion, "Granada") ~ "Granada",
        str_detect(Lugar_hospitalizacion, "Guadalajara") ~ "Guadalajara",
        str_detect(Lugar_hospitalizacion, "Guipúzcoa|Guipuzcoa|Gipuzkoa") ~ "Gipuzkoa",
        str_detect(Lugar_hospitalizacion, "Huelva") ~ "Huelva",
        str_detect(Lugar_hospitalizacion, "Huesca") ~ "Huesca",
        str_detect(Lugar_hospitalizacion, "Baleares|Balearic|Balears|Menorca|Mallorca") ~ "Balears, Illes",
        str_detect(Lugar_hospitalizacion, "Gran Canaria|Lanzarote|Gomera|Fuerteventura|Palma") ~ "Palmas, Las",
        str_detect(Lugar_hospitalizacion, "Santa Cruz|Tenerife|Hierro") ~ "Santa Cruz de Tenerife",
        str_detect(Lugar_hospitalizacion, "Jaén|Jaen") ~ "Jaén",
        str_detect(Lugar_hospitalizacion, "Lléida|Lérida|LLeida|Lerida") ~ "Lleida",
        str_detect(Lugar_hospitalizacion, "Lugo") ~ "Lugo",
        str_detect(Lugar_hospitalizacion, "Madrid") ~ "Madrid",
        str_detect(Lugar_hospitalizacion, "Málaga|Malaga") ~ "Málaga",
        str_detect(Lugar_hospitalizacion, "Melilla") ~ "Melilla",
        str_detect(Lugar_hospitalizacion, "Murcia") ~ "Murcia",
        str_detect(Lugar_hospitalizacion, "Navarra|Nafarroa|Navarre|Pamplona") ~ "Navarra",
        str_detect(Lugar_hospitalizacion, "Orense|Ourense") ~ "Ourense",
        str_detect(Lugar_hospitalizacion, "Palencia") ~ "Palencia",
        str_detect(Lugar_hospitalizacion, "Pontevedra|Pontevedro") ~ "Pontevedra",
        str_detect(Lugar_hospitalizacion, "Rioja|Logroño|Logrono") ~ "Rioja, La",
        str_detect(Lugar_hospitalizacion, "Salamanca") ~ "Salamanca",
        str_detect(Lugar_hospitalizacion, "Segovia") ~ "Segovia",
        str_detect(Lugar_hospitalizacion, "Sevilla") ~ "Sevilla",
        str_detect(Lugar_hospitalizacion, "Soria") ~ "Soria",
        str_detect(Lugar_hospitalizacion, "Tarragona|Alfacs|Fangar") ~ "Tarragona",
        str_detect(Lugar_hospitalizacion, "Teruel") ~ "Teruel",
        str_detect(Lugar_hospitalizacion, "Toledo") ~ "Toledo",
        str_detect(Lugar_hospitalizacion, "Valencia") ~ "Valencia/València",
        str_detect(Lugar_hospitalizacion, "Valladolid") ~ "Valladolid",
        str_detect(Lugar_hospitalizacion, "Vizcaya|Bizcaya|Bizkaia|Viscay|Bilbao") ~ "Bizkaia",
        str_detect(Lugar_hospitalizacion, "Zamora") ~ "Zamora",
        str_detect(Lugar_hospitalizacion, "Zaragoza") ~ "Zaragoza",
        str_detect(Lugar_hospitalizacion, "León|Leon") ~ "León"
      )
    )
  
  #Para añadir columna del año
  df_anual$Año <- año
  
  lista_df[[as.character(año)]] <- df_anual
}

#Unir todos los data frames en uno
df_total <- bind_rows(lista_df)
df_total <- select(df_total,Lugar_hospitalizacion, Diagnostico_principal, Altas, Año) #La columna "Sexo" no nos interesa
df_total <- df_total %>%
  group_by(Año, Lugar_hospitalizacion) %>%
  summarise(Pacientes = sum(Altas, na.rm = TRUE)) %>%
  select(Año, Lugar_hospitalizacion, Pacientes)
View(df_total)




#TABLA POBLACIÓN POR AÑO
poblacion <- read.px("INPUT/datos_morbilidad/datos_poblacion.px")
df_pob <- as.data.frame(poblacion)

df_anual <- df_pob %>%
  rename(Poblacion = value) %>%
  mutate(
    Provincias = case_when(
      str_detect(Provincias, "Álava|Araba|Alava|ALAVA") ~ "Araba/Álava",
      str_detect(Provincias, "Albacete") ~ "Albacete",
      str_detect(Provincias, "Alicante|Alacant") ~ "Alicante/Alacant",
      str_detect(Provincias, "Almería|Almeria") ~ "Almería",
      str_detect(Provincias, "Asturias|Asturies|Oviedo") ~ "Asturias",
      str_detect(Provincias, "Ávila|Avila") ~ "Ávila",
      str_detect(Provincias, "Badajoz") ~ "Badajoz",
      str_detect(Provincias, "Barcelona|Barcelonés") ~ "Barcelona",
      str_detect(Provincias, "Burgos") ~ "Burgos",
      str_detect(Provincias, "Cáceres|Caceres") ~ "Cáceres",
      str_detect(Provincias, "Cádiz") ~ "Cádiz",
      str_detect(Provincias, "Cantabria|Cantabric|Santander") ~ "Cantabria",
      str_detect(Provincias, "Castellón|Castelló") ~ "Castellón/Castelló",
      str_detect(Provincias, "Ceuta") ~ "Ceuta",
      str_detect(Provincias, "Ciudad Real") ~ "Ciudad Real",
      str_detect(Provincias, "Córdoba|Cordoba") ~ "Córdoba",
      str_detect(Provincias, "Coruña|coruña|Coruna|coruna") ~ "Coruña, A",
      str_detect(Provincias, "Cuenca") ~ "Cuenca",
      str_detect(Provincias, "Gerona|Girona") ~ "Girona",
      str_detect(Provincias, "Granada") ~ "Granada",
      str_detect(Provincias, "Guadalajara") ~ "Guadalajara",
      str_detect(Provincias, "Guipúzcoa|Guipuzcoa|Gipuzkoa") ~ "Gipuzkoa",
      str_detect(Provincias, "Huelva") ~ "Huelva",
      str_detect(Provincias, "Huesca") ~ "Huesca",
      str_detect(Provincias, "Baleares|Balearic|Balears|Menorca|Mallorca") ~ "Balears, Illes",
      str_detect(Provincias, "Gran Canaria|Lanzarote|Gomera|Fuerteventura|Palma") ~ "Palmas, Las",
      str_detect(Provincias, "Santa Cruz|Tenerife|Hierro") ~ "Santa Cruz de Tenerife",
      str_detect(Provincias, "Jaén|Jaen") ~ "Jaén",
      str_detect(Provincias, "Lléida|Lérida|LLeida|Lerida") ~ "Lleida",
      str_detect(Provincias, "Lugo") ~ "Lugo",
      str_detect(Provincias, "Madrid") ~ "Madrid",
      str_detect(Provincias, "Málaga|Malaga") ~ "Málaga",
      str_detect(Provincias, "Melilla") ~ "Melilla",
      str_detect(Provincias, "Murcia") ~ "Murcia",
      str_detect(Provincias, "Navarra|Nafarroa|Navarre|Pamplona") ~ "Navarra",
      str_detect(Provincias, "Orense|Ourense") ~ "Ourense",
      str_detect(Provincias, "Palencia") ~ "Palencia",
      str_detect(Provincias, "Pontevedra|Pontevedro") ~ "Pontevedra",
      str_detect(Provincias, "Rioja|Logroño|Logrono") ~ "Rioja, La",
      str_detect(Provincias, "Salamanca") ~ "Salamanca",
      str_detect(Provincias, "Segovia") ~ "Segovia",
      str_detect(Provincias, "Sevilla") ~ "Sevilla",
      str_detect(Provincias, "Soria") ~ "Soria",
      str_detect(Provincias, "Tarragona|Alfacs|Fangar") ~ "Tarragona",
      str_detect(Provincias, "Teruel") ~ "Teruel",
      str_detect(Provincias, "Toledo") ~ "Toledo",
      str_detect(Provincias, "Valencia") ~ "Valencia/València",
      str_detect(Provincias, "Valladolid") ~ "Valladolid",
      str_detect(Provincias, "Vizcaya|Bizcaya|Bizkaia|Viscay|Bilbao") ~ "Bizkaia",
      str_detect(Provincias, "Zamora") ~ "Zamora",
      str_detect(Provincias, "Zaragoza") ~ "Zaragoza",
      str_detect(Provincias, "León|Leon") ~ "León"
    )
  )

View(df_anual)
