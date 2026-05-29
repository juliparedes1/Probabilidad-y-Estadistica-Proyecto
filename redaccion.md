# Análisis de muestra de vinos

El trabajo se centra en el análisis de una muestra de datos de vinos obtenida de una recolección automatizada entre febrero y marzo de 2022. La unidad de análisis es la calificación de vinos, y la población de interés está representada por un conjunto original mucho mayor de registros de vino y puntuaciones. La muestra que usamos comprende 100 vinos distintos y 1.000 calificaciones asociadas.

Esta información se presenta en dos tablas complementarias:

- **Tabla de vinos:** incluye 100 vinos distintos con atributos como nombre, país de origen (country), tipo de vino (type), graduación alcohólica (abv), variedades de uva y posibles maridajes.
- **Tabla de calificaciones:** contiene 1.000 registros de rating, cada uno con una puntuación entre 1 y 5 para los vinos de la muestra.

Los datos fueron recolectados automáticamente por otro equipo desde varios sitios web. El conjunto completo original alcanzaba los 100.000 vinos y 21.000.000 de calificaciones, y nuestro dataset es una versión resumida. Por esta razón, consideramos esta muestra como una herramienta práctica para explorar y probar inferencias sobre la población original, asumiendo que se trata de una muestra aleatoria.

## Variables de interés

Se seleccionaron cuatro variables principales para el análisis:

- **Country (país):** variable cualitativa nominal con 17 valores únicos.
- **Rating (puntuación):** variable cuantitativa de razón con rango de valores de 1 a 5.
- **ABV (alcohol por volumen):** variable cuantitativa continua de razón con rango de valores de 4 a 48.
- **Type (tipo de vino):** variable cualitativa nominal con 5 valores únicos.

## Descripción de la muestra

La muestra incluye 100 vinos y 1.000 calificaciones, y se la describe como una versión reducida del dataset original. No se expone un diseño muestral explícito, por lo que en este trabajo se asume que la muestra tiene carácter aleatorio y es representativa de la población mayor que se pretende estudiar.

## Análisis descriptivo

En la muestra se observan dos variables cualitativas y dos cuantitativas:

- Para **Country**, la diversidad de países sugiere que el análisis de frecuencia y proporciones podrá identificar distribuciones regionales relevantes.
- En **Type**, la clasificación nominal en cinco categorías permite comparar los distintos estilos de vino y su posible relación con la calidad percibida.
- **Rating** ofrece una escala de calificación de 1 a 5, útil para analizar niveles de satisfacción y detectar vinos con puntuaciones consistentemente altas o bajas.
- **ABV** proporciona información continua sobre graduación alcohólica, lo que permite estudiar su variabilidad y su posible vínculo con las calificaciones.

## Parámetros asociados al problema

El análisis pretende describir la muestra mediante parámetros estadísticos básicos como frecuencias, proporciones, medidas de tendencia central y dispersión. En las variables cuantitativas se pueden estimar medias, medianas y rangos, mientras que en las variables cualitativas se analizarán las categorías más frecuentes.

## Conclusión preliminar

Este trabajo presenta un análisis preliminar de un dataset de vinos seleccionado como muestra aleatoria de una población mayor. La información disponible es suficiente para comparar variables cualitativas y cuantitativas clave, pero la conclusión se mantiene como preliminar, ya que depende de la representatividad de la muestra respecto al conjunto original y de la ausencia de detalles sobre el diseño muestral.

La redacción toma como guía el contenido del `README.md` hasta antes del checklist, respetando su enfoque en la descripción de las tablas, las variables seleccionadas y la fecha de recolección de los datos.