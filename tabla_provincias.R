library(pdftools)
library(tidyverse)
library(stringr)
#lee las páginas del pdf una a una y las separa por filas
ficha_pdf <- pdf_text("extension_provincias.pdf") %>%
  str_split("\n")
#La info correspondiente a las provincias está en la 3 primeras páginas del pdf
ficha_provincias <- ficha_pdf[1:3]
ficha_provincias

#Vamos a unir todas las líneas de las páginas en un sólo vector 
#Unlist: convierte lista anidada en un vector atómico
#Eliminamos las lineas de la cabecera que no nos interesan, lineas en blanco y última linea que tampoco nos interesa
#Eliminamos también las filas que están vacías
lineas <- unlist(ficha_provincias)[-(1:10)]
lineas <- lineas[trimws(lineas) != ""]
lineas
#Eliminamos la linea en la que indica la fuente
lineas_a_eliminar <- str_detect(lineas, "Fuente:")
datos_provinciales <- lineas[!lineas_a_eliminar]
#Corregimos nombre que se había dividido en dos lineas
datos_provinciales[40] <- "CASTILLA LA MANCHA            79.461       –     9.982   53.091   16.379    9" 
datos_provinciales <- datos_provinciales[-41]

datos_provinciales <- datos_provinciales %>% # <-- INICIAMOS LA TUBERÍA CON EL OBJETO
  # Reemplazar el guion ('–') por el valor faltante NA
  str_replace_all("–", "NA") %>%
  
  # Eliminar los puntos (separadores de miles)
  str_replace_all("\\.", "")
datos_provinciales


#  Limpieza de caracteres
datos_finales <- datos_provinciales %>% 
  stringr::str_remove("^\\[\\d+\\].*?\\s+") %>%   # Quitar índices
  stringr::str_replace_all("–", "NA") %>%        # Reemplazar guiones por NA
  stringr::str_replace_all("\\.", "")             # Quitar puntos (miles)

# Separar cada línea por 2 o más espacios y convertir en data frame
lista <- strsplit(datos_finales, "\\s{2,}")
lista <- lapply(lista, function(x) x[x != ""])   # Quitar elementos vacíos
df_provincias <- as.data.frame(do.call(rbind, lista), stringsAsFactors = FALSE)

#  Renombrar columnas y tipar
df_provincias <- df_provincias %>%
  rename(
    Provincia = V1,
    Total_km2 = V2,
    Columna_A = V3,
    Columna_B = V4,
    Columna_C = V5,
    Columna_D = V6
  ) %>%
  mutate(across(Total_km2:Columna_D, ~ as.integer(.x)))
df_provincias

df_total <- select(df_provincias, Provincia,Total_km2)
df_total
