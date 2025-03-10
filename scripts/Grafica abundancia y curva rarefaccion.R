#Cargar paquetes necesarios
library(vegan)
library(readr)
library(ggplot2)

#Grafica de abundancia y curva de rarefaccion
#Lectura de los datos necesarios (archivo CSV)
Datos_grafica<-read.csv("Datos/Abundancias por especie.csv")
Datos_grafica
#Colores a usar
colores1<-c("orange","green","blue","turquoise","deeppink","purple","lightblue","red","lightgreen","salmon","lightpink","yellow")
colores2<-c("cyan","pink","orange","green","steelblue","red","salmon","purple")


# Convertir en matriz de abundancia excluyendo la columna de población
Datos_grafica_wenos <- as.matrix(Datos_grafica[, -1])
Datos_grafica_wenos

# Suma de las abundancias por especie
Suma_pa_grafica <- colSums(Datos_grafica_wenos) 
Suma_pa_grafica
# Convertir a  un dataframe para ggplot
Abundancias_df <- data.frame(     #Se usa sort para ordenar los datos y decreasing para que sea de mayor a menor
  Especie = (names(sort(Suma_pa_grafica, decreasing = TRUE))),  #names ayuda a extraer los datos en el orden que estan
  Abundancia = sort(Suma_pa_grafica, decreasing = TRUE)
)
Abundancias_df
# Graficar con ggplot2
Grafica_abundancias<-ggplot(Abundancias_df, aes(x = reorder(Especie, -Abundancia), y = Abundancia,)) +   #se usa reorder para reordenar los valores y vayan de mayor a menor
  geom_bar(stat="identity", fill=colores1) +
  theme_minimal() +
  labs(title="Gráfico de Abundancia",
       x="Especies",
       y="Número de Individuos") 

Grafica_abundancias
###################
#############
###############
#Generar curva de rarefaccion usando funcion de vegan para asi obtener los datos necesarios
rarefaccion<-rarecurve(Datos_grafica_wenos, col=colores2, label=FALSE)

#hacer dataframe con los datos obtenidos de vegan
Datos_rarefaccion <- bind_rows(lapply(seq_along(rarefaccion), function(i) { #la secuencia genera tantos valores de rarefaccion como datos haya
  data.frame(Tamaño = attr(rarefaccion[[i]], "Subsample"),                #mientras que la funcion se encargara de asignarle un numero a cada dato generado
             Especies = rarefaccion[[i]],                                   #attr se encarga de extraer y asignar los valores(subsample) a cada dato de rarefaccion desde la funcion rarecurve
             Poblacion = paste("Población", i))                     #se le asigna una etiqueta a cada poblacion, dependiendo del numero de datos
}))

# Graficar con ggplot2
Curva_rarefaccion<-ggplot(Datos_rarefaccion, aes(x = Tamaño, y = Especies, color = Poblacion)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Curva de Rarefacción",
       x = "Tamaño de la Muestra",
       y = "Número de Especies") +
  scale_color_manual(values = colores2) 

Curva_rarefaccion
