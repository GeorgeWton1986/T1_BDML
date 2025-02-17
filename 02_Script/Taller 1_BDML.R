# carga de paquetes

#if(!require(pacman)) install.packages("pacman") ; require(pacman)

p_load(rvest,     # permite realizar web scraping en R
       dplyr,     # Permite manipulación y transformación de los datos
       skimr,     # resumen estadístico
       visdat,    # visualización de missing values
       corrplot,  # correlation plots
       stargazer, # Tables/outputs to tex
       ggplot2    # graficación
       )

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
df_GEIH <- bind_rows(todas_las_tablas)

# Resumen de la Base de datos df_GEIH
skim(df_GEIH) %>% head()

df_GEIH %>%
  select(age, ocu) %>%  # Visualizar solo las columnas clave
  sample_n(10)  # Mostrar 10 filas aleatorias

# Verificar estructura de las variables clave
str(df_GEIH[, c("age", "ocu")])

# Filtrar personas mayores de 18 años que están empleadas
df_filtrado <- df_filtrado %>%
  filter(age >= 18, ocu == 1) 

head(df_filtrado)

# Nuevo data frame
df_salario <- df_filtrado %>%
  select(y_total_m_ha, age, ocu, sex, maxEducLevel, p6426, p6870, depto, formal, p6100)

# cambio del nombre de algunas variables
df_salario <-  df_salario %>% 
  rename(salario_hora = y_total_m_ha,
         max_nivel_educ = maxEducLevel,
         tiempo_empresa = p6426,
         tamaño_empresa = p6870,
         segu_social = p6100
  )

# estadísticas de df_salario
skim(df_salario)
str(df_salario)

# datos faltantes con skim
base_faltantes <- skim(df_salario) %>% select( skim_variable, n_missing)

# calcular el porcentaje de valores faltantes
obs <- nrow(df_salario) 

base_faltantes<- base_faltantes %>% mutate(p_missing = n_missing/obs)
head(base_faltantes)

# ordenar de forma descendente
base_faltantes <- base_faltantes %>% arrange(-n_missing)

# mantener solo las variables con valores faltantes
base_faltantes<- base_faltantes %>% filter(n_missing!= 0)
base_faltantes

# gráfica de los valores faltantes
ggplot(base_faltantes, aes(x = reorder(skim_variable, -n_missing), y = p_missing, fill = skim_variable)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +  
  scale_y_continuous(labels = scales::percent) +  
  scale_fill_manual(values = c("lightsalmon", "lightpink", "lightblue")) +  
  labs(title = "Porcentaje de valores faltantes por variables", 
       x = "Variables",
       y = "Porcentaje de valores faltantes") + 
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

# Imputación de valores faltantes con el Método 1: Media/Mediana

## Imputación de variables categóricas: max_nievl_educ y segu_social

### Calcular los valores mas comunes
educacion <- as.numeric(names(sort(table(df_salario$max_nivel_educ), decreasing = TRUE)[1]))

seg_soc <- as.numeric(names(sort(table(df_salario$segu_social), decreasing = TRUE)[1]))

### Imputación de los valores faltantes para ambas variables
df_salario <- df_salario  %>%
  mutate(max_nivel_educ = ifelse(is.na(max_nivel_educ) == TRUE, educacion , max_nivel_educ)) %>% 
  mutate(segu_social = ifelse(is.na(segu_social) == TRUE, seg_soc , segu_social))

## Imputación de variable continua: salario por hora

### Gráfica de la distribución del salario por hora
ggplot(df_salario, aes(salario_hora)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(df_salario$salario_hora, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(df_salario$salario_hora, na.rm = TRUE), linetype = "dashed", color = "blue") +  
  ggtitle(" Ingreso Total por hora") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#### Debido a que la distribución del ingreso total por hora tiene un cola a la derecha, se deicide utilizar la mediana para imputar los valores faltantes.

### Imputación de valores faltantes para variable continua
df_salario <- df_salario %>% 
  mutate(salario_hora = ifelse(is.na(salario_hora) == TRUE, median(df_salario$salario_hora, na.rm = TRUE) , salario_hora))

# Validación de la correcta imputación de valroes faltantes en el data frame de salario
sum(is.na(df_salario))

