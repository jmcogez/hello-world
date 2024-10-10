library(tidyverse)
data(starwars)
#Ejemplos usando select
# Seleccionar todas las columnas menos el nombre
starwars %>% select(-name)
#Seleccionar sólo las columnas que tienen subraya (_)
starwars %>% select(contains("_"))
#Seleccionar sólo las columnas que empiezan con "s"
starwars %>% select(starts_with("s"))
#Crear un data frame con los nombres y planeta de origen (homeworld)
homeworld <- starwars %>% select(name, homeworld)
#Filtrar datos 
#Filtrar por especies: sólo humanos
human <- starwars %>% filter(species == "Human")
#Filtrar por especies: sólo humanos del planeta Tatooine
starwars %>% filter(species == "Human", homeworld == "Tatooine")
#Crear un nuevo datframe con todas las especies menos los Droides
starwars_nodroids <- starwars %>% filter(species != "Droid")

#¿Cuántos registros cumplen las condiciones finales? 
#En homeworld 87
#En human 35
#En starwars 87
#En starwars_nodroids 77

#Seleccionar y agrupar datos
#Usamos group_by y tally
starwars %>% group_by(species) %>% tally()

#Añadiendo otra variable
starwars %>% group_by(species, gender) %>% tally()

#Si lo quieres guardar en el environment recuerda asignarle un nombre
table_gender <- starwars %>% group_by(species, gender) %>% tally()

#Calcular algunos estadísticos: MEDIA Y DESVIACIÓN ESTÁNDAR
#na.rm=T quiere decir que elima los NA (valores No Asignados o sin datos)
starwars %>% group_by(species) %>% summarise(mean_height = mean(height, na.rm = T),mean_mass = mean(mass,na.rm = T))
#no hace falta calcular la media para cada especie ya que se separa directamente si 
#pongo group_by(species)

#¿Cómo calcularías la desviación estándar (sd) de esos parámetros? 
#Recuerda consultar con ? si no sabes como usar una función o comando. 
#Por ejemplo: ?summarise() ,?sd().

#Hemos calculado la media, ahora podemos ver la desviación estándar
starwars %>% group_by(species) %>% summarise(sd_height = sd(height, na.rm = T),mean_sd = sd(mass,na.rm = T))

#Crear y modificar algunos elementos
#Hacer un gráfico de la altura vs. la masa de los personajes
ggplot(starwars, aes(height, mass)) + geom_point()

#Puedes modificar el color 
ggplot(starwars, aes(height, mass)) + geom_point(colour = "red")

#Modificando el color y el punto
ggplot(starwars, aes(height, mass)) + geom_point(colour = "purple", pch = 3)

#Modificando el color y el fondo 
ggplot(starwars, aes(height, mass)) + geom_point(colour = "red") + theme_light()

#Vemos que hay un punto que correspone a un personaje con una masa muy grande
#creamos de nuevo el gráfico sin ese personaje

#Para eliminar el personaje que pese más de 1000
starwars_filtered <- starwars %>% filter(mass < 1000)

#Aquí hago la gráfica con el nuevo data set sin el personaje que pesa tanto
ggplot(starwars_filtered, aes(height, mass)) + 
  geom_point(colour = "red") + 
  theme_light()

#Ejercicio
toy <- read_csv("C:/Users/julia/Downloads/toy.csv")
#me voy a escritorio busco el documento y le doy a copiar ruta de acceso
#Si no funciona cambiar barras de sentido y poner comillas
#Otra manera de hacerlo sería darle a import y seleccionar documento

#Inspecciona el dataset, haz un resumen de la media (mean) de las variables 
#(Peso, Altura,IMC, IAS, CCintura). Agrupando por sexo.

#Inspeccionamos el data set
head(toy)
#Ya conozco el nombre de las columnas, así que puedo calcular las medias agrupando por sexo

#Agrupamos los datos por sexo, así las operaciones se hacen para hombres y mujeres.
toy_summary <- toy %>%
  group_by(Sex) %>%
  summarise(
    mean_weight = mean(Weight_Kg, na.rm = TRUE),
    mean_height = mean(Height_cm, na.rm = TRUE),
    mean_imc = mean(IMC, na.rm = TRUE),  
    mean_ias = mean(IAS, na.rm = TRUE),
    mean_waist = mean(Ccintura, na.rm = TRUE)
  )
#Haz una tabla sólo con los pacientes femeninos 
#¿Cuántos registros cumplen las condiciones? 
#¿De estos cuantos tienen Sobrepeso (Overweight)?  
#Usa select y filter.

#Usando filter podemos filtrar los pacientes femeninos
female <- toy %>% filter(Sex == "Women")

# Filtrar pacientes femeninos con sobrepeso
female_table <- female %>%
  select(Weight_Kg, Height_cm, IMC_clas, IMC_clas, IAS, IAS_Clas, Ccintura)

#¿Cuántos registros cumplen las condiciones?
nrow(female_table)
#58 cumplen las condiciones.

#¿De estos cuantos tienen Sobrepeso (Overweight)?  
pacientes_sobrepeso <- female %>% filter(IMC_clas == "Overweight")
nrow(pacientes_sobrepeso)
#9 tienen sobrepeso.

#Haz un gráfico usando ggplot relacionando el IMC con el peso de todos los pacientes.
ggplot(toy, aes(IMC, Weight_Kg)) + geom_point()

#Repítelo filtrando sólo los pacientes categorizados como "Overweight" y "Obesity".
ggplot(toy %>% filter (IMC_clas %in% c("Overweight","Obesity")), aes(IMC, Weight_Kg)) + geom_point()

#Ejercicio
#Utiliza los comandos adecuados para instalar los paquetes de R ape phangorn y phytools
install.packages("ape")
install.packages("phangorn")
install.packages("phytools")
