library(readr)
library(dplyr)
library(tidyverse)
library(stringr)
library(pdftools)
library(rjson)
library(ggplot2)
library(scales)

ficha_pdf <- pdf_text("INPUT/GBIF/extension_provincias.pdf") %>%
  str_split("\n")

ficha_provincias <- ficha_pdf[1:3]
lineas <- unlist(ficha_provincias)[-(1:10)]

lineas <- lineas[trimws(lineas) != ""]

lineas_a_eliminar <- str_detect(lineas, "Fuente:")

datos_provinciales <- lineas[!lineas_a_eliminar]
datos_provinciales[40] <- "CASTILLA LA MANCHA            79.461       –     9.982   53.091   16.379    9" 

datos_provinciales <- datos_provinciales[-41]

datos_provinciales <- datos_provinciales %>% # <-- INICIAMOS LA TUBERÍA CON EL OBJETO
  # Reemplazar el guion ('–') por el valor faltante NA
  str_replace_all("–", "NA") %>%
  # Eliminar los puntos (separadores de miles)
  str_replace_all("\\.", "")


datos_finales <- datos_provinciales %>% 
  stringr::str_remove("^\\[\\d+\\].*?\\s+") %>%   # Quitar índices
  stringr::str_replace_all("–", "NA") %>%         # Reemplazar guiones por NA
  stringr::str_replace_all("\\.", "")             # Quitar puntos (miles)
lista <- strsplit(datos_finales, "\\s{2,}")

lista <- lapply(lista, function(x) x[x != ""])   # Quitar elementos vacíos

df_provincias <- as.data.frame(do.call(rbind, lista), stringsAsFactors = FALSE)
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
df_total <- select(df_provincias, Provincia,Total_km2) %>%
  filter(!(str_detect(Provincia, '[:upper:]') & !str_detect(Provincia, '[:lower:]') & !(Provincia %in% 
                                                                                          c("BALEARES", "S C Tenerife", "P DE ASTURIAS", "CANTABRIA", "NAVARRA", "LA RIOJA", "MADRID", "R DE MURCIA")))) %>%
  mutate(
    Provincia = case_when(
      Provincia == "A Coruña" ~ "Coruña, A",
      Provincia == "Alava" ~ "Araba/Álava",
      Provincia == "Guipúzcoa" ~ "Gipuzkoa",
      Provincia == "Vizcaya" ~ "Bizkaia",
      Provincia == "BALEARES" ~ "Balears, Illes",
      Provincia == "Avila" ~ "Ávila",
      Provincia == "Alicante" ~ "Alicante/Alacant",
      Provincia == "Castellón" ~ "Castellón/Castelló",
      Provincia == "Valencia" ~ "Valencia/València",
      Provincia == "Palmas (Las)" ~ "Palmas, Las",
      Provincia == "S C Tenerife" ~ "Santa Cruz de Tenerife",
      Provincia == "P DE ASTURIAS" ~ "Asturias",
      Provincia == "CANTABRIA" ~ "Cantabria",
      Provincia == "NAVARRA" ~ "Navarra",
      Provincia == "LA RIOJA" ~ "Rioja, La",
      Provincia == "MADRID" ~ "Madrid",
      Provincia == "R DE MURCIA" ~ "Murcia",
      TRUE ~ Provincia
    )
  )


df_total


gbif <- as.data.frame(fromJSON(file = "INPUT/GBIF/gbif_datos.json"))

gbif_tabla <- gbif %>%
  filter(!is.na(provincia)) %>%
  left_join(df_total, by = c("provincia" = "Provincia")) %>%
  mutate(indice_km2 = round(n_especies/Total_km2, digits = 3)) %>%
  select(provincia, year, n_especies, var_especies, indice_km2)


gbif_tabla

medicacion <- read.delim(file = "INPUT/Medicamentos/Recetas_facturadas_SNS.csv", sep = ",")
unique(medicacion$CCAA)


# Nos fijamos que, contrario a los casos anteriores, esta vez no estamos trabajando con provincias, sino con comunidades autónomas. Por ello, en primer lugar, agruparemos los datos obtenidos en la tabla de biodiversidad.

gbif_meds <- gbif_tabla %>%
  mutate(ccaa = case_when(
    provincia %in% c("Almería", "Cádiz", "Córdoba", "Granada", "Huelva", "Jaén", "Málaga", "Sevilla") ~ "ANDALUCÍA",
    provincia %in% c("Huesca", "Teruel", "Zaragoza") ~ "ARAGÓN",
    provincia %in% c("Asturias") ~ "ASTURIAS",
    provincia %in% c("Balears, Illes") ~ "BALEARES",
    provincia %in% c("Palmas, Las", "Santa Cruz de Tenerife") ~ "CANARIAS",
    provincia %in% c("Cantabria") ~ "CANTABRIA",
    provincia %in% c("Ávila", "Burgos", "León", "Palencia", "Salamanca", "Segovia", "Soria", "Valladolid", "Zamora") ~ "CASTILLA Y LEÓN",
    provincia %in% c("Albacete", "Ciudad Real", "Cuenca", "Guadalajara", "Toledo") ~ "CASTILLA - LA MANCHA",
    provincia %in% c("Barcelona", "Girona", "Lleida", "Tarragona") ~ "CATALUÑA",
    provincia %in% c("Alicante/Alacant", "Castellón/Castelló", "Valencia/València") ~ "COMUNIDAD VALENCIANA",
    provincia %in% c("Badajoz", "Cáceres") ~ "EXTREMADURA",
    provincia %in% c("Coruña, A", "Lugo", "Ourense", "Pontevedra") ~ "GALICIA",
    provincia %in% c("Madrid") ~ "COMUNIDAD DE MADRID",
    provincia %in% c("Murcia") ~ "MURCIA",
    provincia %in% c("Navarra") ~ "NAVARRA",
    provincia %in% c("Araba/Álava", "Bizkaia", "Gipuzcoa") ~ "PAÍS VASCO",
    provincia %in% c("Rioja, La") ~ "LA RIOJA",
    provincia %in% c("Ceuta") ~ "CEUTA",
    provincia %in% c("Melilla") ~ "MELILLA")
  ) %>%
  select(ccaa, year, indice_km2) %>%
  left_join(medicacion, by = c("ccaa" = "CCAA", "year" = "Año")) %>%
  filter(!is.na(Nº.de.Recetas.CNAC))

gbif_meds


gbif_meds %>%
  filter(year == 2013) %>%
  mutate(
    delta_num = gsub(" %", "", `X..Δ`),       # Quitar " %"
    delta_num = gsub(",", ".", delta_num),     # Cambiar coma por punto
    delta_num = as.numeric(delta_num)          # Convertir a numérico
  ) %>%
  group_by(ccaa) %>%
  summarise(delta_promedio = mean(delta_num, na.rm = TRUE)) %>%
  ggplot(aes(x = ccaa, y = delta_promedio)) +
  geom_bar(stat = "identity", fill = "deeppink") +
  scale_y_continuous(
    limits = c(-10, 0),           # De -100% a 0%
    breaks = seq(-10, 0, 2)      # Saltos cada 10%
  ) +
  labs(
    title = "Variación de medicamentos por Comunidad Autónoma en 2013",
    x = "Comunidad Autónoma",
    y = "X..Δ"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 6),
  )

