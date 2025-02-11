library(rvest)
library(dplyr)

# Lista tablas
todas_las_tablas <- list()

# Iterar en las 10 paginas
for (i in 1:10) {
  url_tabla <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i, ".html")
  
  pagina_tabla <- read_html(url_tabla)
  
  # Extraer tabla
  tabla <- pagina_tabla %>% html_element("table") %>% html_table()
  
  # Agregar la tabla a la lista
  todas_las_tablas[[i]] <- tabla
}

# Unir todas las tablas en un solo dataframe
df_final <- bind_rows(todas_las_tablas)

head(df_final)

df_final %>%
  select(age, ocu, dsi) %>%  # Seleccionar solo las columnas clave
  sample_n(10)  # Mostrar 10 filas aleatorias

#Filtrar personas mayores de 18 años que están empleadas
df_filtrado <- df_final %>%
  filter(age >= 18, ocu == "1") 

head(df_filtrado)


faltantes <- colSums(is.na(df_filtrado)) / nrow(df_final) * 100
faltantes <- sort(faltantes, decreasing = TRUE)  # Ordena de mayor a menor

print(faltantes)

#Imputación de datos faltantes con la media
datos_imputados_media <- df_filtrado %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

sum(is.na(datos_imputados_media))
sum(is.na(df_filtrado))

colSums(is.na(datos_imputados_media)) %>% 
  sort(decreasing = TRUE) %>% 
  .[. > 0]

df_filtrado %>% select(where(is.character)) %>% str()

#Denominacion de las variables
#6050: Cual es el parentesco; 6090: Afiliado a SSS; 6100:Regimen de Seguridad