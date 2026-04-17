#install.packages("tidyverse")
#install.packages("here")
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

#Eliminación de columnas que no utilizamos mediante select (librería dplyr)
df_completo <- df_completo %>% select(-RatingID, -UserID, -Date,
                                      -WineryID, -WineryName, -Website,
                                      -Vintages, -RegionID, -RegionName)

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

# 3. Gráfico con la leyenda ordenada
ggplot(df_proporciones, aes(x = Type, y = Porcentaje, fill = Nivel_Alcohol)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "YlOrRd") + 
  labs(
    title = "Distribución Proporcional de Niveles de Alcohol por Tipo de Vino",
    x = "Tipo de Vino",
    y = "Proporción (%)",
    fill = "Graduación (ABV)"
  ) +
  theme_minimal()

