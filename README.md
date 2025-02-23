# Problem Set 1: Predicting Income 
## Integrantes:
Nicolas Jácome 
Zaira García
Jorge Viáfara
Laura Rivera

## Introducción:
La Dirección de Impuestos y Aduanas Nacionales (DIAN) es fundamental para reacudar el financiamiento del Gobierno Colombiano mediante los ingresos tributarios, aunque la economía enfrenta un déficit fiscal debido a una baja recaudación, por debajo de las proyecciones del Marco Fiscal de Mediano Plazo. La evasión fiscal es uno de los principales problemas, impulsada por factores como el bajo nivel educativo, la desconfianza en el gobierno y la falta de valores, lo que afecta la competitividad del país y los ingresos destinados a servicios públicos esenciales. Para mitigar esta evasión, se proponen herramientas como modelos predictivos de ingresos, que pueden identificar fraudes.

El estudio se enfoca en un análisis econométrico utilizando datos de la Gran Encuesta Integrada de Hogares 2018 (GEIH) de Bogotá, con el objetivo de analizar las condiciones salariales de los trabajadores mayores de 18 años. Se exploraron variables como ingresos, edad, género, educación, experiencia laboral y formalidad del empleo. El proceso incluyó la limpieza de los datos, identificando y corrigiendo valores faltantes, y seleccionando variables clave que influyen en el salario. Este análisis permite comprender los factores que afectan el ingreso y contribuir a mejorar la gestión tributaria y económica del país.

## Contenido
En la primera sección, se describe el proceso de adquisición y limpieza de la base de datos, que se encuentra disponible en línea. Para obtener los datos, se realizó un scraping del sitio web https://ignaciomsarmiento.github.io/GEIH2018_sample/.
En la segunda sección, se analiza la relación entre la edad y el salario utilizando intervalos de confianza construidos con el método de bootstrap.
En la tercera sección, se examina la brecha salarial por género, presentando las estimaciones obtenidas a partir del método FWL y FWL con bootstrap.
En la cuarta sección, se muestra la predicción de las ganancias mediante cinco modelos adicionales con diferentes niveles de complejidad y flexibilidad.

## 01_Documents
En esta carpeta se encuentra el documento necesario para desarrollar el problem set, junto con el informe de Medición de Pobreza Monetaria y Desigualdad 2018, publicado por el Departamento Administrativo Nacional de Estadística (DANE) y contiene el documento final en pdf.

## 02_Script
En esta carpeta se incluye el archivo en R Markdown donde se desarrolla el problem set y el script en R donde se deposita únicamente el código.

## 03_Stores
En esta carpeta se enceuntra la base de datos ya filtrada utilizada para realizar el problem set. 

## 04_Views
En esta carpeta se evidencian los gráficos obtenidos del código en formato JPG y las tablas con algunos de los resultados.

