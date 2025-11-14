library(pxR)
library(dplyr)

#Ruta incompleta para hacer bucle por año, dado que los archivos tienen como nombre: morb_año
ruta <- "./datos_morbilidad/grupo_edad/"

#Vector de años
años <- c(2005:2015)

#Lista vacía donde ir almacenando los df de cada año
lista_df <- list()

#bucle para importar datos de cada año
for (año in años) {
  nombre_archivo <- paste0(ruta, "morb_",año,".px")
  
  datos_anuales <- read.px(nombre_archivo)
  df_anual <- as.data.frame(datos_anuales)

  #matches busca coincidencias en los nombres (a partir de 2010 cambiaban los nombres de las columnas)
  df_anual <- df_anual %>%
    rename(Grupo_edad = matches("Grupos.de.edad"),
           Diagnostico_principal = matches("Diagnóstico.principal"),
           Lugar_hospitalización = matches("Provincia"),
           Altas = value)
  
  #Para añadir columna del año
  df_anual$año <- año
  
  lista_df[[as.character(año)]] <- df_anual
}

#Unir todos los data frames en uno
df_total <- bind_rows(lista_df)

View(df_total)

