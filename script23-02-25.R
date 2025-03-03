if(!require(pacman)) install.packages("pacman") ; require(pacman)

p_load(rvest,     # permite realizar web scraping en R
       dplyr,     # Permite manipulación y transformación de los datos
       skimr,     # resumen estadístico
       visdat,    # visualización de missing values
       corrplot,  # correlation plots
       stargazer, # Tables/outputs to tex
       ggplot2,    # graficación
       boot
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
df_filtrado <- df_GEIH %>%
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

#Estimacion de regresion log (salario)
modelo_age_wage <- lm(log(salario_hora) ~ age + I(age^2), data = df_salario) 

summary(modelo_age_wage)


# Gráfico del perfil estimado de ingresos y edad

edades <- unique(df_salario$age)  

# Calcular los log salarios predichos por edad
log_salarios_predichos <- coef(modelo_age_wage)["(Intercept)"] +
  coef(modelo_age_wage)["age"] * edades +
  coef(modelo_age_wage)["I(age^2)"] * edades^2

# Data frame con salario log y reales
df_pred <- data.frame(
  edad = edades,
  log_salario = log_salarios_predichos,
  salario = exp(log_salarios_predichos)  
)

# Graficar el perfil estimado de ingresos y edad
ggplot(df_pred, aes(x = edad, y = salario)) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = df_pred %>% filter(edad %% 10 == 0),  
             aes(x = edad, y = salario), color = "red", size = 2) +  # Puntos cada 10 años
  labs(title = "Perfil estimado de ingresos por hora según la edad",
       x = "Edad",
       y = "Salario promedio estimado") +
  theme_minimal()

# Bootstrap para construir los intervalos de confianza

#https://rpubs.com/andrew32118/Bootstrap

# Realizar el remuestreo (bootstrap) de tamaño 1000 con reemplazos sobre la variable age
boot <- sample(df_salario$age,1000, replace = TRUE)

#Crear una matriz de 1000 filas y 1 columna a partir del remuestreo
m_boots <- matrix(boot, nrow = 1000, ncol = 1, byrow = TRUE)

#Calcular los cuantiles de confianza de la matriz
quantiles <- quantile(m_boots, probs = c(0.025,0.975))

#Ver los resultados de los cuantiles en la variable age.
quantiles

#Calcular los intervalos de confianza de la matriz
intervalos <- c(2*mean(m_boots)-quantiles[2], 2*mean(m_boots)-quantiles[1])

#Ver los resultados de los intervalos en la variable age.
intervalos

#Graficar el histograma

hist(m_boots, las=1,
     main = "Distribución de muestra",
     xlim = c(0,100),
     xlab = "Edades",
     ylim = c(0,200),
     ylab = "Frecuencia",
     col = "blue")

abline(v=intervalos, col = "red", lwd = 2, lty = 2)
abline(v=quantiles, col = "green", lwd =2, lty=2)
legend("topright", legend = c("Intervalos", "Cuantiles"), col = c("red","green"), lwd = 2, lty = 2)

#Se crea la variable female en nuestra base

df_salario <- df_salario %>%
  mutate(female = ifelse(sex == 0, 1, 0))

#Estimación de regresion log (salario) con female
reg_female <- lm(formula = log(salario_hora) ~ female, data =df_salario)

stargazer(reg_female, type = "text")
# PASO 1
# Estimación de regresion log (salario) con female y otros controles
reg_female_controles <- lm(formula = log(salario_hora) ~ female + max_nivel_educ + tiempo_empresa + age, 
                           data =df_salario)

# PASO 2
# Regresión auxiliar para obtener los residuos u
reg_female_residuos <- lm(formula = female  ~ max_nivel_educ + tiempo_empresa + age, 
                          data =df_salario)

residuos_female <- residuals(reg_female_residuos)

# PASO 3
#Correr la primera regresión con los residuos de female 
reg_residuos_female <- lm(log(salario_hora) ~ residuos_female, data = df_salario)


stargazer(reg_female_controles , reg_residuos_female, type = "text",
          column.labels = c("Modelo Condicional", "Modelo Incondicional"))

#Se comparan los coeficientes de female

beta_female_controles <- coef(reg_female_controles)["female"]
beta_femal_residuos <- coef(reg_residuos_female)["residuos_female"]

beta_female_controles 
beta_femal_residuos

# Función para sacar residuales
fwl_boot <- function(data ,index) {
  
  coef(lm(log(salario_hora) ~ female + max_nivel_educ + tiempo_empresa + age, 
          data = df_salario, 
          subset = index))[2]
}

# Validación
fwl_boot(df_salario, 1:nrow(df_salario))

# Semilla para hacerlo reproducible
set.seed(123)

# Estimaciones y errores estandar
errorestandar_boot <-  boot(df_salario, fwl_boot, R = 1000)
errorestandar_boot


