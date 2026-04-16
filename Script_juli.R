

install.packages("tidyverse") # con la linea install.packages instalamos cualquier paquete. En algunas ocasiones hay que indicar desde qué repositorio se instala el paquete (CRAN by default)
library("tidyverse") # cargamos la librería
setwd("C:\\Users\\PC\\Desktop\\TUIA\\2do año\\probabilidad y estadistica\\Probabilidad-y-Estadistica-Proyecto/") # chequear previamente la dirección

#read.csv("C:\\Users\\PC\\Desktop\\TUIA\\2do año\\probabilidad y estadistica\\Probabilidad-y-Estadistica-Proyecto\\XWines_Test_100_wines.csv") 


exploded_wines <- Wines %>% # el %>% sirve como un pipe "y luego..."
  # Paso A: Limpiamos los caracteres indeseados usando expresiones regulares
  mutate(Harmonize_clean = str_remove_all(Harmonize, "\\['|'\\]|'")) %>% #la doble barra sirve para escapar la primera, ya que dentro de las comillas dobles existen simbolos como \t con significado
  
  # Paso B: Explotamos la columna separando por la coma y el espacio
  separate_longer_delim(cols = Harmonize_clean, delim = ", ")

# 4. Inspeccionar el resultado
head(exploded_wines %>% select(WineID, WineName, Harmonize, Harmonize_clean))

nrow(exploded_wines)

frecuency_table <- exploded_wines %>% count(Harmonize_clean, name = "Frecuency", sort = TRUE)

frecuency_table

grouped_wines <- exploded_wines %>%
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


harmonize_frecuency = grouped_wines %>% count(general_category, name = "Frecuency", sort = TRUE)
harmonize_frecuency


pie(harmonize_frecuency$Frecuency, labels = harmonize_frecuency$general_category)


ggplot(data = harmonize_frecuency, aes(x = reorder(general_category, Frecuency), y = Frecuency) ) + geom_col(fill="steelblue", color="black") + labs(
  title = "Frecuencia de Categorías de Maridaje",
  x = "Cantidad de recomendaciones",
  y = "Categoría de Comida"
)

prueba = Wines %>% filter(ABV< 25)


ggplot(data = prueba, aes( x = ABV )) + geom_boxplot() 


df_informe = wines_tests %>% left_join(Wines %>% select(WineID, ABV, Country, Harmonize), by = "WineID")


head(df_informe)

