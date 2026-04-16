install.packages("tidyverse") # con la linea install.packages instalamos cualquier paquete. En algunas ocasiones hay que indicar desde qué repositorio se instala el paquete (CRAN by default)
library("tidyverse") # cargamos la librería

setwd("C:\\Users\\PC\\Desktop\\TUIA\\2do año\\probabilidad y estadistica\\Probabilidad-y-Estadistica-Proyecto/") # chequear previamente la dirección

#explotamos el df para poder ver los maridajes y luego los agrupamos en categorias mas especificas
#===========================================================================
df_informe_harmonize_exploded <- df_informe %>% 
  # Paso A: Limpiamos los caracteres indeseados usando expresiones regulares
  mutate(Harmonize_clean = str_remove_all(Harmonize, "\\['|'\\]|'")) %>% #la doble barra sirve para escapar la primera, ya que dentro de las comillas dobles existen simbolos como \t con significado
  
  # Paso B: Explotamos la columna separando por la coma y el espacio
  separate_longer_delim(cols = Harmonize_clean, delim = ", ")

df_informe_harmonize_exploded_clean <- df_informe_harmonize_exploded %>%
  mutate(
    general_category = case_when(
      Harmonize_clean %in% c("Beef", "Lamb", "Pork", "Chicken", "Veal", "Game Meat", "Poultry", "Cured Meat", "Barbecue", "Cold Cuts") ~ "Meat",
      Harmonize_clean %in% c("Blue Cheese", "Hard Cheese", "Soft Cheese", "Maturated Cheese", "Goat Cheese") ~ "Cheese",
      Harmonize_clean %in% c("Rich Fish", "Lean Fish", "Fish", "Shellfish", "Seafood") ~ "Seafood/Fish",
      Harmonize_clean %in% c("Fruit", "Fruit Dessert", "Appetizer", "Sweet Dessert", "Cake", "Aperitif", "Chocolate", "Cream", "Snack") ~ "Desserts",
      Harmonize_clean %in% c("Vegetarian", "Salad", "Tomato Dishes", "Mushrooms") ~ "Vegetables",
      Harmonize_clean %in% c("Pasta", "Pizza", "French Fries", "Light Stews", "Spicy Food", "Grilled", "Risotto" ) ~ "Specific foods",
      # El TRUE actúa como el "en cualquier otro caso" de nuestra fórmula matemática
      TRUE ~ Harmonize_clean 
    )
  )

#Analizamos cuales son los vinos 3 mejor calificados
#===========================================================================
mejor_arg = df_informe %>% filter( Country == "Argentina" )  %>% group_by( WineID, Country) %>%
  summarise(promedio_rating = mean(Rating, na.rm=TRUE), total_votos = n() , .groups = "drop")  %>% arrange(desc(promedio_rating))

mejor_arg = mejor_arg %>% left_join(Wines, by = "WineID") 

mejor_arg = mejor_arg %>% select(promedio_rating, WineName )

 ggplot(data =mejor_arg, aes(x=reorder(WineName, promedio_rating), y = promedio_rating) ) +
   geom_col(fill = "steelblue") +
   labs(title = "TOP 3" , x = "Vinos", y = "Calificacion" ) + 
   theme(axis.text.x = element_text(angle = 30, hjust = 1))



promedio_por_pais = df_informe %>% group_by(Country) %>% summarise(promedio_rating = mean(Rating, na.rm=TRUE), total_votos = n() , .groups = "drop") %>% arrange(desc(promedio_rating))

tabla = promedio_por_pais %>% select(Country, promedio_rating)


#que tan buenos son los vinos segun el pais
#===========================================================================
ggplot(data = tabla, aes(y = reorder(Country, promedio_rating), x = promedio_rating)) +
  geom_col(fill="steelblue", color="black")  +
  labs(
    title = "Promedios por pais",
    x = "Calificacion promedio",
    y = "Pais"
    )


# calculos para realizar relacion entre graduacion alcoholica y calificacion 
#===========================================================================
analisis_abv_rating <- df_informe %>%
  group_by(WineID, ABV) %>%
  summarise(
    Media_Rating = mean(Rating, na.rm = TRUE),
    .groups = "drop"
  )

##Histograma para ver si nos conviene utilizar la media o la mediana para el los ratings
#===========================================================================
ggplot(data = analisis_abv_rating, aes(x = Media_Rating)) +
  # geom_histogram divide el rango en barras. 
  # binwidth define el ancho de cada barra (ej: 0.2 puntos de rating)
  geom_histogram(binwidth = 0.2, fill = "steelblue", color = "white") +
  # Agreguemos una línea para la Media (roja) y la Mediana (amarilla) para comparar
  geom_vline(aes(xintercept = mean(Media_Rating)), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(Media_Rating)), color = "yellow", size = 1) +
  labs(
    title = "Histograma de Ratings Promedio",
    subtitle = "Línea roja: Media | Línea amarilla: Mediana",
    x = "Rating Promedio",
    y = "Cantidad de Vinos"
  )



#hay un vino con 48 grados, realmente no nos interesa ya que ni para vino sirve
#===========================================================================


analisis_abv_rating = analisis_abv_rating %>% filter(ABV < 28)



#Scatterplot
##Podemos ver que no hay corelacion notoria entre la graduacion alcoholica y el rating
#===========================================================================
# Paso 2: Crear el Scatterplot
ggplot(data = analisis_abv_rating, aes(x = ABV, y = Media_Rating)) +
  # geom_point() dibuja los puntos
  geom_point(color = "darkred", alpha = 0.6, size = 3) +
  labs(
    title = "Relación entre Graduación Alcohólica y Calificación",
    subtitle = "Cada punto representa un vino único",
    x = "ABV (%)",
    y = "Rating Promedio"
  )
#===========================================================================











