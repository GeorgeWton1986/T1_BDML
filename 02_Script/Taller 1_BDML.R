#1.1 General Instructions----

#1. Introduction.----
"The introduction briefly states the problem and if there are any antecedents. 
It briefly describes the data and its suitability to address the problem set question. 
It contains a preview of the results and main takeaways."

#2. Data----
"We will use data for Bogota from the 2018 Medicion de Pobreza Monetaria
y Desigualdad Report that takes information from the GEIH.

The data set contains all individuals sampled in Bogota and is available at the following 
website https://ignaciomsarmiento.github.io/GEIH2018 sample/. To obtain the data, you must scrape 
the website.

In this problem set, we will focus only on employed individuals older than eighteen (18) years old. 
Restrict the data to these individuals and perform a descriptive analysis of the variables used in the problem set. 
Keep in mind that in the data, there are many observations with missing data or 0 wages. I leave it to you to find 
a way to handle this data.

When writing this section up, you must:
(a) Describe the data briefly, including its purpose, and any other relevant information.
(b) Describe the process of acquiring the data and if there are any restrictions to accessing/scraping these data."



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

