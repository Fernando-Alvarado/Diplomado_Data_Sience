# Proyecto de fin de módulo

El proyecto final se realizará en **equipos de 3 personas**. Las tres personas tendrán que crear repositorios: dos personas en local y una en remoto. El repositorio remoto se considerará como un repositorio colaborativo, en el que las 3 personas trabajarán.

## Primera parte

Se documenta mediante screenshots en un archivo de Google Doc (i.e. un archivo de Word de Google).

Cada persona tomará uno de los siguientes roles:

1. Administrador del repositorio: Creará el repositorio en GitHub, añade contenido y resuelve posibles conflictos. Segunda persona del equipo ordenada alfabéticamente por apellido paterno.

2. Colaborador 1: Clona el repositorio, añade contenido y realiza una contribución. Tercera persona del equipo ordenada alfabéticamente por apellido paterno.

3. Colaborador 2: Modifica el contenido y resuelve posibles conflictos. Primera persona del equipo ordenada alfabéticamente por apellido paterno.

Instrucciones:

1. Configuración inicial (administrador del repositorio)

En GitHub, el administrador del repositorio crea un repositorio privado llamado diplo-modulo3-equipo-n, donde n es el número de su equipo. Por ejemplo, si mi equipo es el 5, mi repositorio privado se llamará "diplo-modulo3-equipo-equipo-5"

Agrega un archivo README.md con una breve descripción del proyecto.

Agrega a los otros dos colaboradores y al usuario EduardoSelimMM con permisos de escritura.

Comparte la URL del repositorio con los colaboradores.

Sube al repositorio el archivo "baseball.db"

Crea, desde GitHub, el archivo "proyecto_final.Rmd" con el siguiente contenido

```
---
title: "Proyecto final módulo 3"
author: "Equipo - n"
output: html_document
---

# Integrantes:

+ Integrante 1. Rol: Administrador
+ Integrante 2. Rol: Colaborador 1
+ Integrante 3. Rol: Colaborador 2

```{r}
library(DBI)
library(dbplyr)
library(RSQLite)

conn <- DBI::dbConnect(RSQLite::SQLite(), "baseball.db")
```

2. Clonar el repositorio (Colaboradores 1 y 2, Administrador)

Cada colaborador debe clonar el repositorio en su máquina:

```
git clone https://github.com/usuario/diplo-modulo3-equipo-n.git
```

3. Colaborador 1: Desde su máquina crear chunks de código R en el archivo "proyecto_final.Rmd" (que obtuvo del Administrador) que muestre el número de tablas y sus nombres de la base de datos "baseball"; además, crear un objeto en R, un por cada tabla de la base de datos. Hacer commit y push del código al repositorio del administrador y hacer un pull request.

4. Colaborador 2: Desde su máquina crear chunks de código R en el archivo "proyecto_final.Rmd" (que obtuvo del Administrador) que use las funciones `vis_dat()` y `vis_miss()` de la librería `{visdat}` para analizar los tipos de datos y datos faltantes de cada una de las tablas generadas por el Colaborador 1. Debe corregir la línea `author: "Equipo - n"` con el número correcto de equipo y agregar los nombres completos de los integrantes en la sección de `# Integrantes`. Hacer commit y push del código al repositorio del administrador y hacer un pull request.

5. Administrador del equipo. Desde su máquina crear chunks de código R en el archivo "proyecto_final.Rmd" (que obtuvo del Administrador) que use funciones de la librería {skimr} para obtener un resumen del tipo de variables, resumen de variables numéricas y resumen de variables categóricas para cada uno de los datasets que creo el Colaborador 1. Adicionalmente, hacer un diagrama de cómo se relacionan todas las tablas de la base de datos. Hacer commit y push del código al repositorio del administrador y revisar los pull requests de los colaboradores 1 y 2.

6. Obtener la versión actualizada del repositorio (Colaboradores 1 y 2, Administrador)

7. Cada colaborador debe hacer pull (jalar) el contenido actualizado del repositorio a su máquina. Los 3 archivos .Rmd de cada computadora deben ser idénticos entre sí.
   
9. El colaborador 2 debe ejecutar el código actualizado y dar conclusiones sobre cada uno de los subconjuntos de datos

## Segunda parte

Deberá realizar un **EDA detallado** que incluya tablas y gráficas que **al menos** respondan las siguientes preguntas:

+ ¿Cuál es la duración promedio de la carrera de un jugador de béisbol profesional según su posición?

+ ¿Cómo se comparan los promedios de bateo entre jugadores que asistieron a la universidad y los que no?

+ ¿Los jugadores con carreras más largas tienden a tener salarios más altos?

+ ¿Existe una diferencia significativa en el rendimiento entre jugadores de diferentes conferencias universitarias?

+ ¿Cómo se relaciona la altura y el peso del jugador con su rendimiento en bateo?

+ ¿Existe alguna tendencia de los salarios en la MLB en los últimos 50 años?

+ ¿Existen disparidades salariales entre jugadores con diferentes antecedentes educativos?

+ ¿La asistencia a la universidad influye en el salario del primer año de un jugador?

+ ¿Hay alguna relación entre las estadísticas de rendimiento (promedio de bateo, home runs) y el salario?

+ ¿Los lanzadores ganan salarios significativamente diferentes a los bateadores?

+ ¿Qué factores contribuyen a la inducción al Salón de la Fama?

+ ¿Existe una diferencia significativa en las tasas de entrada al Salón de la Fama entre lanzadores y bateadores?

+ ¿El número de home runs aumenta las probabilidades de ingresar al Salón de la Fama?

+ ¿Los jugadores de ciertas décadas tienen más probabilidades de ser incluidos al Salón de la Fama?

+ ¿Qué se puede decir del tiempo en el que un jugador empieza su carrera hasta que se vuelve miembro del Salón de la Fama?

+ ¿Cómo se relaciona el rendimiento en su primera temporada con la duración de su carrera?

+ ¿Las estadísticas de fielding se relacionan con la duración total de la carrera?

+ ¿Cuál es la probabilidad de que un jugador llegue a las Grandes Ligas según sus estadísticas universitarias?

+ ¿Existen grupos de jugadores con trayectorias profesionales similares según métricas de rendimiento?

+ ¿Cómo afecta el rendimiento de un jugador en sus mejores años a sus ganancias totales en la carrera?
