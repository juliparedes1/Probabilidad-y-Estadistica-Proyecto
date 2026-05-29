install.packages("tidyverse")
install.packages("here")
library(here)
library(dplyr)
library(tidyverse)
library(ggplot2)


#Carga de Datos
if (!exists("df_ratings")) {
  ruta_ratings <- here("XWines_Test_1K_ratings.csv")
  df_ratings <- read.csv(ruta_ratings)
}

if (!exists("df_wines")) {
  ruta_wines <- here("XWines_Test_100_wines.csv")
  df_wines <- read.csv(ruta_wines)
}

#EDA
#Merge de Dataframes
df_completo <- merge(df_ratings, df_wines, by = "WineID")
#Eliminación de Dataframes ya unificados
rm(df_ratings, df_wines)

unique(df_completo$Country)

#Eliminación de columnas que no utilizamos mediante select
df_completo <- df_completo %>% select(-RatingID, -UserID, -Date,
                                      -WineryID, -WineryName, -Website,
                                      -Vintages, -RegionID, -RegionName)

paises <- df_completo$Country

#Definimos el mapeo "País" = "Continente"
mapeo_continentes <- c(
  "Brazil" = "South America", "Argentina" = "South America", "Chile" = "South America", "Uruguay" = "South America",
  "Portugal" = "Europe", "Germany" = "Europe", "France" = "Europe", "Italy" = "Europe", 
  "Spain" = "Europe", "Austria" = "Europe", "Russia" = "Europe", "Greece" = "Europe",
  "South Africa" = "Africa",
  "Australia" = "Oceania", "New Zealand" = "Oceania",
  "United States" = "North America", "Canada" = "North America"
)

#Mapeamos continentes según país
continentes <- mapeo_continentes[paises]


#Agregamos columna de continentes a df_completo
df_completo$Continente <- mapeo_continentes[df_completo$Country]

#Expandir rango [1,5] a [1,10]
puntaje_10 = round(((df_completo$Rating-1)*2.25)+1)
df_completo$Puntaje = puntaje_10

#Análisis de nulos - Observamos que no tenemos nulos en ninguna columna
tabla_nulos <- data.frame(
  Columna = names(df_completo),
  Conteo_NA = colSums(is.na(df_completo)),
  Proporcion = colMeans(is.na(df_completo))
)

#Combinamos "Dessert/Port" con "Dessert"
df_completo <- df_completo %>%
  mutate(Type = ifelse(Type == "Dessert/Port", "Dessert", Type))

#Observamos la frecuencia para futura referencia
#Teóricamente al tener más observaciones los intervalos de confianza
#van a ser mejores (más pequeños/precisos) para los tipos que tengamos más observaciones
frecuencias_por_tipo<- df_completo %>%
  count(Type, name = "Frecuencia", sort = TRUE)

#Caso de análisis: ¿Qué tipo de vino tiene el mejor promedio de rating?
#Análisis complementario: Dispersión de rating por tipo de vino.

#Gráfico - Boxplot para observar dispersión
ggplot(df_completo, aes(x = Type, y = Rating, fill = Type)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribución de Calificaciones por Tipo de Vino",
    x = "Tipo de Vino",
    y = "Calificación (Rating)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

df_promedios_tipo <- df_completo %>%
  group_by(Type) %>%
  summarise(Rating_Promedio = mean(Rating)) %>%
  arrange(desc(Rating_Promedio))

#Gráfico - Barras, promedios de Rating por tipo de vino
ggplot(df_promedios_tipo, aes(x = reorder(Type, -Rating_Promedio), y = Rating_Promedio, fill = Type)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "Rating Promedio por Tipo de Vino",
    x = "Tipo de Vino",
    y = "Promedio de Rating"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Análisis 2 - Proporción de niveles de alcohol por tipo de vino
#Discretización de la columna ABV - Niveles Alto, Moderado y Bajo.
df_completo <- df_completo %>%
  mutate(Nivel_Alcohol = case_when(
    ABV < 12 ~ "Bajo (<12%)",
    ABV >= 12 & ABV <= 14.5 ~ "Moderado (12-14.5%)",
    ABV > 14.5 ~ "Alto (>14.5%)",
  )) %>%
  mutate(Nivel_Alcohol = factor(Nivel_Alcohol, 
                                levels = c("Bajo (<12%)", "Moderado (12-14.5%)", "Alto (>14.5%)")))


df_proporciones <- df_completo %>%
  count(Type, Nivel_Alcohol) %>%
  group_by(Type) %>%
  mutate(Porcentaje = n / sum(n) * 100)


#===========================================================================
#Análisis final
df_promedios_continente <- df_completo %>%
  group_by(Continente) %>%
  summarise(Rating_Promedio = mean(Rating)) %>%
  arrange(Rating_Promedio)

#Observar si los vinos de difentes continentes tienen diferencias en puntaje significativas
#Gráfico - Boxplot para observar dispersión
ggplot(df_completo, aes(x = Continente, y = Rating, fill = Continente)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribución de Calificaciones por Continente",
    x = "Continente",
    y = "Calificación (Rating)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data = df_promedios_continente, 
       aes(x = reorder(Continente, -Rating_Promedio), 
           y = Rating_Promedio, 
           fill = Continente)) +
  geom_col(alpha = 0.8) +
  labs(
    title = "Rating Promedio por Continente",
    x = "Continente",
    y = "Rating Promedio"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p95 = qnorm(p=0.95,0,1)
p_que_estaba = 1.96

# Observar si hay diferencias de nivel de alcohól por tipo de vino. 
df_alcohol_tipo <- df_completo %>%
  group_by(Type) %>%
  summarise(
    media = mean(ABV),
    sd = sd(ABV),
    n = n(),
    se = sd / sqrt(n),                       # error estándar
    IC_inf = media - p95 * se,
    IC_sup = media + p95 * se
  )

#Gráfico - Boxplot para observar dispersión

#Consultar: ¿Sacar outliers de Dessert o normalizar?

ggplot(df_completo, aes(x = Type, y = ABV, fill = Type)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribución de Nivel de Alcohol por Tipo de Vino",
    x = "Tipo de Vino",
    y = "Nivel de Alcohol"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#=======================================================================
#Analisis sin outlier
analisis_sin_outlier = df_completo %>% filter(ABV < 40)

ggplot(analisis_sin_outlier, aes(x = Type, y = ABV, fill = Type)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribución de Nivel de Alcohol por Tipo de Vino",
    x = "Tipo de Vino",
    y = "Nivel de Alcohol"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

tapply(df_completo$ABV,df_completo$Type,median)
tapply(analisis_sin_outlier$ABV,analisis_sin_outlier$Type,median)
median(df_completo$T)



count(group_by(df_wines,Type))


#=======================================================================

ggplot(df_alcohol_tipo, 
       aes(x = reorder(Type, media), y = media)) +
  
  geom_pointrange(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2) +
  coord_flip() +
  labs(
    title = "Nivel Promedio de Alcohol por Tipo de Vino",
    x = "Tipo de Vino",
    y = "Promedio de Alcohol (ABV)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



#=======================================================
#Análisis de frecuencia (Tipo de Vino + Continente)
unique(df_completo$Continente)
unique(df_completo$Type)
#Sacar una medida resumen - (Encontrar medida resumen)
#Sacar los IC

#Preguntar: Si estamos tratando con una muestra de una encuesta,
#y generalizamos para la encuesta o para todos los vinos.

