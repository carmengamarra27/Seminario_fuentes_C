#Data frame con las altas por diagnóstico y provincia desde 2000 a 2020
library(pxR)
library(dplyr)

#Ruta incompleta para hacer bucle por año, dado que los archivos tienen como nombre: morb_año
ruta <- "./datos_morbilidad/diagnostico/"

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
    rename(Grupo_edad = matches("Grupos.de.edad"),
           Diagnostico_principal = matches("Diagnóstico"),
           Lugar_hospitalizacion = matches("Provincia"),
           Altas = value)
  
  #Para añadir columna del año
  df_anual$Año <- año
  
  lista_df[[as.character(año)]] <- df_anual
}

#Unir todos los data frames en uno
df_total <- bind_rows(lista_df)
df_total <- select(df_total,Lugar_hospitalizacion, Diagnostico_principal, Altas, Año) #La columna "Sexo" no nos interesa
View(df_total)
