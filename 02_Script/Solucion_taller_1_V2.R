#Autores ----
#Nicolás Jácome Velasco
#Jorge Viáfara Morales

#Inicio de Código Inicial ----

#Limpiar la consola
cat("\f")
#Limpiar el Global Environment
rm(list = ls())
#Instalar los paquetes a usar
install.packages("data table")
install.packages("tidyverse")
install.packages("docstring")
install.packages("dplyr")
install.packages("stargazer")

#Cargar los paquetes
library(data.table)
library(tidyverse)
library(docstring)
library(dplyr)
library(stargazer)

#Fijar el directorio de trabajo
getwd()
setwd('/Users/jorgeviafara/Documents/Talleres R y Python/Taller R')

#Inspeccionar el directorio de trabajo
list.files()
list.dirs()

#Crear carpetas de input y output
dir_inputs <- '/Users/jorgeviafara/Documents/Talleres R y Python/Taller R/Taller_1/Input'
dir_outputs <- '/Users/jorgeviafara/Documents/Talleres R y Python/Taller R/Taller_1/Output'
setwd(dir_inputs)
getwd()

#1.Puntos a desarrollar----

##1.1 Definir la semilla----

set.seed(123)

"La semilla es un instrumento que se usa con el objeto de generar números seudoaleatorios,
en este sentido, tener un mismo número de semilla permite que dos modelos obtengan los mismos
números seudoaletorios, esto permite que exista la posibilidad de realizar una comparación entre dos modelos."

##1.2 Creación de 4 vectores ----
ident_individuo <- seq(from = 1,to = 50, by= 1) #Vector1: números secuenciales
edad_individuo <- as(runif(50, min = 5, max=50),'integer','true') #Vector2: Convertir la distribución en un integer
print(edad_individuo)
caracter_anos <- rep("años", 50) #Vector3: Repite 50 veces la palabra años
# 'print(caracter_anos)'
#tabla_nombres <- read.table("Nombres.csv", sep = ";") #Vector4: Los nombres de personas
#nombre_individuo <- tabla_nombres [2:51,2] #Extracción de nombres de la lista desde la fila 2 hasta la 50 de la columna 2.

nombre_individuo <- c('Adel','Adonis','Ajaz','Akos','Aldo','Amets','Amaro','Aquiles','Algimantas','Alpidio',
                      'Amrane','Anish','Arián','Ayun','Azariel','Bagrat','Bencomo','Bertino','Candi','Cesc',
                      'Cirino','Crisólogo','Cruz','Danilo','Dareck','Dariel','Darin','Delmiro','Damen','Dilan',
                      'Dipa','Doménico','Drago','Edivaldo','Elvis','Elyan','Emeric','Engracio','Ensa','Eñaut',
                      'Eleazar','Eros','Eufemio','Feiyang','Fiorenzo','Foudil','Galo','Gastón','Giulio','Gautam',
                      'Gentil','Gianni','Gianluca','Giorgio','Gourav','Grober','Guido','Guifre','Guim','Hermes',
                      'Inge','Irai','Iraitz','Iyad','Iyán','Jeremías','Joao','Jun','Jorge','Julian','Javier','Juan',
                      'Jairo','Lucián','Mael','Misael','Moad','Munir','Nael','Najim','Neo','Neil','Nikita',
                      'Nilo','Otto','Pep','Policarpo','Radu','Ramsés','Rómulo','Roy','Severo','Sidi','Simeón',
                      'Taha','Tao','Vadim','Vincenzo','Zaid','Zigor','Zouhair')

nombres_aleatorios <- sample(nombre_individuo, 50, replace = TRUE)
print(nombres_aleatorios)

#print(paste("La longitud del Identificador del individuo, es de:",length(ident_individuo),"posiciones"))
#print(paste("La longitud de las edades, es de:",length(edad_individuo),"posiciones"))
#print(paste("La cantidad de veces que se repiete la palabra años, es de:",length(caracter_anos)))
#print(paste("La longitud de los nombres, es de:",length(nombre_individuo)))

#view(nombre_individuo)

##1.3 Elaboración oraciones ----

for (i in 1:length(nombres_aleatorios)) {
  print(paste(i,"",nombres_aleatorios[i],"Tiene",edad_individuo[i],"años"))  
}

##1.4 Elaboracion oraciones sin J y número par ----

iniciales_nombre <- substr(nombres_aleatorios, start = 1, stop = 1)
print(iniciales_nombre)

for (i in 1:length(nombres_aleatorios)){
  if(iniciales_nombre [i] != 'J' & !edad_individuo [i] %% 2 == 0){
    #print("Edad Par")
    print(paste(i,"",nombres_aleatorios[i],"Tiene",edad_individuo[i],"años"))  
  }
}

# 'print("distinto")'
# 
# #Recorre el vector de las inicales del nombre e imprime solo las "J"
# 'for (i in 1:length(iniciales_nombre)){
#   if (iniciales_nombre [i] == "J"){
#     print(iniciales_nombre[i])
#   }
# }'

##1.5 Función Promedio y Desviación Estándar----

##Función externa para calcular el promedio y la desviacion estandar
promedio <- function(edades){
  edades <- (edad_individuo)
  suma_acum <-0
for (i in 1:length(edades)){
  suma_acum <- edades[i]+suma_acum
  # 'print(suma_acum)'
}
  
  prom_edad <- suma_acum/length(edades)
  print(paste("El promedio de edad es:", prom_edad))

return(prom_edad)
}

#Llamado de la funcion promedio desviacion esta
prom_edad <- promedio(edad_individuo)


desv <- function(edades, prom_edad){
  prom_edad <- promedio(edad_individuo) #Llamado a la función promedio y asignacion de la variable
  diferencia_acum <- 0

for (i in 1:length(edades)){
  diferencia <- (edades [i] - prom_edad)**2
  # 'print(diferencia)'
  diferencia_acum <- diferencia_acum + (edades [i] - prom_edad)**2
  # 'print(diferencia_acum)'
}

desv_edad <- (diferencia_acum/(length(edades)-1))**(1/2)
print(paste("La desviacion estandar es:", desv_edad))

return(desv_edad)

}

#Llamado de la función desviación estándar
desv_edad <- desv(edad_individuo, prom_edad)

#Verificación de los valores con R
mean(edad_individuo)
sd(edad_individuo)  

##1.6 y 1.7 Estandarizar una serie de valores-----

#Funcion interna para estandarizar un muestra del vector edad y crear el vector 
estand_muestra <- function(desv, promedio, edades){
  edadstdi <- ((edad_individuo - prom_edad)/desv_edad)
  print(edadstdi) 
  return(edadstdi)
}


##1.8 Crear una lista Outcomes_nominales----

set.seed(123)
edadstdi <- estand_muestra(desv = desv_edad, promedio = prom_edad, edades = edad_individuo)
ei <- rnorm(50, mean = 0, sd = 1) #Vector de error para 50 datos con media 0 y desv. estándar 1
print(ei)
salarioi <- 2 + 3*edadstdi + ei
index_salud <- 5 - 3*edadstdi - edadstdi**2 + ei
xper_labor <- 2 + ei

outcomes_nominales <- list(salario = salarioi, indic_salud = index_salud, expe_labo = xper_labor)
print(outcomes_nominales)

# View(outcomes_nominales)

##1.9 y 1.10 Crear un matriz a partir de un vector----

matriz_regrsimple <- function(edadstdi){
  vect_edadstd <- matrix( data = edadstdi, nrow = 50, ncol = 1, byrow = FALSE)
  'print(vect_edadstd)'
  vector1 <- matrix( data = 1, nrow = 50, ncol = 1, byrow = FALSE)
  'print(vector1)'
  m_regsimple <- cbind(vector1,vect_edadstd) #Es la matriz "x" a partir de los dos vectores de edadstdi y el vector de 1's
  nombres_columnas <- c('Vect1','edadstdi') #Vector que contiene los nombres de las columnas
  colnames(m_regsimple) <- nombres_columnas #La asignacion de los nombres a la matriz por medio del comando colnames
  print(m_regsimple)
  return(m_regsimple) #Retorna de la función la matriz con edadstdi y vect1
}

m_regsimple <- matriz_regrsimple(edadstdi) #Llamado de la funcion matriz_regrsimple y Asignacion de la matriz de 1 y edadstdi

#2. Segundo Punto -----

##2.1 Estimadores y errores por MCO----
###2.1.1 beta's = (X'X)**-1X'y >>> OK ----
estimadores_mco <- function(yi, m_matriz){
  mult_matrices <- t(m_matriz) %*% m_matriz #Multiplicacion de x' y la x
  inversa_matrices <- solve(mult_matrices) #Inversa de la multiplicacion entre x' y la x
  traspuesta_matrices <- inversa_matrices %*% t(m_matriz)
  est_beta <- traspuesta_matrices %*% yi
  #print(est_beta)
  return(est_beta) #Retorna los betas estimados
}

###2.1.2 El vector de residuales >>>OK ----
estimador_residuales <- function(yi, m_matriz){
  est_beta <- estimadores_mco(yi, m_matriz)
  #print(est_beta)
  m_predicha <- m_matriz %*% est_beta
  v_observado <- yi
  esti_residuales <- v_observado - m_predicha
  #head(esti_residuales, n=5)
  
  return(esti_residuales)
}

###2.1.3 Varianza estimada del termino error >>>OK----
varianza_error <- function(yi, v_residual_error){
  var_error <- as.numeric(t(v_residual_error)%*%v_residual_error/(50-2-1))
  #print(var_error)
  return(var_error)
}

###2.1.4 Matriz Varcovar betas estimados ----
m_varcovar <- function(est_varianza_error, m_matriz){
  sigma_cuadr <- var_error
  #print(var_error)
  varcovar_betas <- var_error *solve(t(m_matriz)%*%m_matriz)
  #print(varcovar_betas)
  
  return(varcovar_betas)
}

###2.1.5 Desviacion estandar betas estimados ----
desviacion_estd <- function(varcovar){
  varcovar_betas <- m_varcovar(varcovar_betas, m_matriz)
  desv_std_betas <- sqrt(diag(varcovar_betas))
  #print(desv_std_betas)
  
  return(desv_std_betas)
}

###2.1.6 Resultado individual Prueba----

yi <- salarioi #Variables intercambiables (salarioi, index_salud, xper_labor)
m_matriz<- m_regsimple

est_beta <- estimadores_mco(yi, m_matriz)
print(est_beta)

esti_residuales <- estimador_residuales(yi, m_matriz)
head(esti_residuales, n=5)

var_error <- varianza_error(yi, v_residual_error = esti_residuales)
print(var_error)

varcovar_betas <- m_varcovar(est_varianza_error = var_error, m_matriz)
print(varcovar_betas)

desv_std_betas <- desviacion_estd(varcovar = varcovar_betas)
print(desv_std_betas)

##2.2 Iteración de matrices y obtencion de estimadores MCO----

class(m_regsimple) #Clase matriz o array
class(outcomes_nominales) #Clase list
vec_salud<- unlist(outcomes_nominales[2])
class(vec_salud)
m_matriz<- m_regsimple
m_resumen <- list() #Creacion de una lista que almacena los datos calculados

#Este ciclo recorre la lista outcomes. Pero, primero convierte cada uno de los elemento en un vector.
#Creacion de una matriz para organizacion de los datos.
#Columna 1: Nombres de yi
#Columna 2: Beta 0 - Intercepto
#Columna 3: Beta 1 - Variable 1
#Columna 4: Desv. estándar de Beta 1

  for (i in 1:length(outcomes_nominales)){ #Entra a la lista outcomes_nominales desde la posicion 1 hasta terminar
    yi <- unlist(outcomes_nominales[i]) #Conversion de una lista a un vector.
    
    est_beta <- estimadores_mco(yi, m_matriz) #Estimacion de los betas
    #print(est_beta)
    
    esti_residuales <- estimador_residuales(yi, m_matriz) #Calculos de los residuales
    #head(esti_residuales, n=5)

    var_error <- varianza_error(yi, v_residual_error = esti_residuales) #Calculo de la varianza del error
    #print(var_error)
    
    varcovar_betas <- m_varcovar(est_varianza_error = var_error, m_matriz) #Calculo de la matriz VarcoVar
    #print(varcovar_betas)
    
    desv_std_betas <- desviacion_estd(varcovar = varcovar_betas) #Calculo de la desvicion estandar
    #print(desv_std_betas)
    
    titulos <- c(betas = est_beta, desviacionestadar = desv_std_betas) #Es un vector que se le asignan los titulos
    m_resumen [[i]] <- titulos #Asigna a la lista m_resumen los titulos
    
  }

dataframe1 <- do.call(rbind, m_resumen) #Creacion del dataframe a traves de la funcion rdind desde la lista m_resumen
nombre_filas <- c("Salarioi", "Index_Salud","Xper_Labor") #Inclusion de los nombres a la filas
rownames (dataframe1) <- nombre_filas 
m_resultado <- as.matrix(dataframe1) #Convetir la lista m_resultado en un matriz que tenga como argumento el dataframe
m_final <- dataframe1[,c("betas1","betas2","desviacionestadar.edadstdi")] #Seleccion de la informacion que se quiere mostrar del dataframe
print(m_final)

#rm(dataframe1)
#rm(m_resultado)
#rm(m_final)

##2.3 Interpretación de resultados----

#La interpretación de los resultados se presenta a continuacion por cada una de las
#variables estudiadas.

###2.3.1 Salario -----
"La regresión lineal denominada Salario, está compuesta por 3 elementos, 2 de ellos variables, tales 
como la edad estandarizada y un error con distribución uniforme que tiene media 0 y  desviación estándar 1. 
Luego de realizar la estimación de los coeficientes de la variable edad estandarizada, se logró establecer 
que, el intercepto estimado b0 tiene un valor de 2,03 unidades estándar, el b1 estimado se calculó en 2,97 
unidades estándar y la desviación estándar sobre el estimado b1 es de 0,134. Los coeficientes estimados se 
pueden interpretar de la siguiente manera, el valor esperado del salario es de 2.03 unidades de salario 
manteniendo constante la variaciones en el estimador B1. Por otra parte, si aumenta en una desviación 
estándar la edad estandariza, el valor esperado del salario aumentará en 2,97 unidades de salario. 
En conclusión, los betas estimados empíricamente, se acercan a las betas presentados en el punto 1.8"

###2.3.2 Indice de Salud -----

"La regresión lineal denominada índice de salud, está compuesta por 4 elementos, 3 de ellos variables, 
tales como la edad estandarizada y un error con distribución uniforme que tiene media 0 y desviación estándar 1.
Luego de realizar la estimación de los coeficientes de la variable edad estandarizada, se logró establecer que, 
el intercepto estimado b0 tiene un valor de 4,05 unidades estándar, el b1 estimado se calculó en -3,064 unidades 
estándar y el error estándar sobre el estimado b1 es de 0,185. Los coeficientes estimados se pueden interpretar 
de la siguiente manera, el valor esperado del salario es de 4.05 unidades de salario manteniendo constante la 
variaciones en el estimador B1. Por otra parte, si aumenta en una desviación estándar la edad estandariza, el 
valor esperado en el índice de salud disminuye en 3,06 unidades el índice de salud. En conclusión, los betas 
estimados empíricamente, se acercan en magnitud y mantienen los signos presentados en el punto 1.8"

###2.3.3 Expericia laboral -----

"La regresión lineal denominada experiencia laboral, está compuesta por 2 elementos, 1 de ellos variables, 
en un error con distribución uniforme que tiene media 0 y desviación estándar 1. Luego de realizar la estimación 
de los coeficientes de la variable edad estandarizada, se logró establecer que, el intercepto estimado b0 tiene 
un valor de 2,03 unidades estándar,  el error estándar sobre el estimado b1 es de 0,185. En conclusión, 
el intercepto estimado empíricamente, se acercan en magnitud a los presentados en el punto 1.8"

##2.4 Verificación ----

# beta <- solve(t(m_regsimple)%*%m_regsimple)%*%t(m_regsimple)%*%salarioi
# print(beta)
# 
# uhat <- salarioi - (m_regsimple%*%beta)
# head(uhat, n= 5)
# 
# varianza_cuadrada <- as.numeric((t(uhat)%*%uhat)/(50-2-1))
# print(varianza_cuadrada)
# 
# varcovabeta <- varianza_cuadrada*solve(t(m_regsimple)%*%m_regsimple)
# print(varcovabeta)
# 
# esta_desv <- sqrt(diag(varcovabeta))
# print(esta_desv)
# 
yi <- salarioi
reg_prueba <- lm(formula = yi ~ m_regsimple)
summary(reg_prueba)
  
