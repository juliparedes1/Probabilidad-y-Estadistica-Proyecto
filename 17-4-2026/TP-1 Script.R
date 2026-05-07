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

#===========================================================================

df_promedios_continente <- df_completo %>%
  group_by(Continente) %>%
  summarise(Rating_Promedio = mean(Rating)) %>%
  arrange(Rating_Promedio)

#Observar si los vinos de difentes paises tienen diferencias en puntaje significativas

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

# Observar si hay diferencias de nivel de alcohól por tipo de vino. 
df_alcohol_tipo <- df_completo %>%
  group_by(Type) %>%
  summarise(
    media = mean(ABV),
    sd = sd(ABV),
    n = n(),
    se = sd / sqrt(n),                       # error estándar
    IC_inf = media - 1.96 * se,
    IC_sup = media + 1.96 * se
  )

ggplot(df_alcohol_tipo, 
       aes(x = reorder(Type, media), y = media, fill = Type)) +
  geom_col(alpha = 0.8) +
  #geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2) +
  coord_flip() +
  labs(
    title = "Nivel Promedio de Alcohol por Tipo de Vino",
    x = "Tipo de Vino",
    y = "Promedio de Alcohol (ABV)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#=======================================================

