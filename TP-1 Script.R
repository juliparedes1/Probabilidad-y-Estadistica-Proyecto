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

#Definimos el mapeo "País" -> "Continente"
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

#eliminamos el outlier ya que no corresponde a un vino sino a un agua ardiente
df_completo = df_completo %>% filter(ABV < 40)

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


#=======================================================================
#Caso de análisis: ¿Es posible identificar diferencias en la calidad del vino según la procedencia?
df_promedios_continente <- df_completo %>%
  group_by(Continente) %>%
  summarise(Rating_Promedio = mean(Puntaje)) %>%
  arrange(Rating_Promedio)

#Observar si los vinos de difentes continentes tienen diferencias en puntaje significativas
#Gráfico - Boxplot para observar dispersión
ggplot(df_completo, aes(x = Continente, y = Rating, fill = Continente)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribución de Calificaciones por Continente",
    x = "Continente",
    y = "Puntaje"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#=======================================================================
# Cálculo del cuantil en una distribución normal
# Se utiliza p = 0.975 para un nivel de confianza del 95% dejando dos colas de %2.5
z95 = qnorm(p = 0.975)

#Calculamos los promedios de puntaje por continente y guardamos el intervalo de confianza
df_promedios_continente <- df_completo %>%
  group_by(Continente) %>%
  summarise(
    Rating_Promedio = mean(Puntaje),
    sd = sd(Puntaje),
    n = n(),
    se = sd / sqrt(n),
    IC_inf = Rating_Promedio - z95 * se,
    IC_sup = Rating_Promedio + z95 * se
  ) %>%
  arrange(desc(Rating_Promedio))

#--- Gráficos finales para el informe ---
#Análisis N. 1
#Gráfico de Barras con IC 95%
ggplot(data = df_promedios_continente, 
       aes(x = reorder(Continente, -Rating_Promedio), 
           y = Rating_Promedio, 
           fill = Continente)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Puntaje Promedio por Continente (con IC 95%)",
    x = "Continente",
    y = "Puntaje Promedio"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

#Geom_pointrange con IC 95%
ggplot(data = df_promedios_continente, 
       aes(x = reorder(Continente, Rating_Promedio), 
           y = Rating_Promedio, 
           color = Continente)) +
  geom_pointrange(aes(ymin = IC_inf, ymax = IC_sup), linewidth = 0.8, size = 0.5) +
  coord_flip() +
  labs(
    title = "Puntaje Promedio por Continente (con IC 95%)",
    x = "Continente",
    y = "Puntaje Promedio"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
#=======================================================================
#Caso de Análisis: 
#¿Existen diferencias observables en el contenido alcohólico promedio
#entre los distintos tipos de vino?

#Gráfico - Boxplot para observar dispersión
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
# Observar si hay diferencias de nivel de alcohol por tipo de vino. 
df_alcohol_tipo <- df_completo %>%
  group_by(Type) %>%
  summarise(
    media = mean(ABV),
    sd = sd(ABV),
    n = n(),
    se = sd / sqrt(n),
    IC_inf = media - z95 * se,
    IC_sup = media + z95 * se
  )


tapply(df_completo$ABV,df_completo$Type,median)
tapply(analisis_sin_outlier$ABV,analisis_sin_outlier$Type,median)
median(df_completo$T)

count(group_by(df_wines,Type))

#Gráfico de Barras con IC 95%
ggplot(df_alcohol_tipo, 
       aes(x = reorder(Type, media), y = media, fill = Type)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Nivel Promedio de Alcohol por Tipo de Vino (con IC 95%)",
    x = "Tipo de Vino",
    y = "Promedio de Alcohol (ABV)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Geom_pointrange con IC 95%
ggplot(df_alcohol_tipo, 
       aes(x = reorder(Type, media), y = media, color = Type)) +
  geom_pointrange(aes(ymin = IC_inf, ymax = IC_sup), size = 0.5, linewidth = 0.8) +
  coord_flip() +
  labs(
    title = "Nivel Promedio de Alcohol por Tipo de Vino (con IC 95%)",
    x = "Tipo de Vino",
    y = "Promedio de Alcohol (ABV)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
#======================================================================

