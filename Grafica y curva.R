#Grafica de abundancia y curva de rarefaccion
#Lectura de los datos necesarios
Datos_grafica<-read.csv("Datos/Abundancias por especie.csv")
Datos_grafica
#Eliminacion de la columna con el nombre de las poblaciones
Datos_grafica_wenos <- as.matrix(Datos_grafica[, -1])
Datos_grafica_wenos
# Suma de los individuos por especie

Suma_pa_grafica <- colSums(Datos_grafica_wenos) 
#Creacion de la grafica de abundancias
#colores de las especies
colores1<-c("orange","green","blue","turquoise","deeppink","purple","lightblue","red","lightgreen","salmon","lightpink","yellow")

barplot(sort(Suma_pa_grafica, decreasing = TRUE), col=colores1,
        main="Gráfica de Abundancias",
        xlab="Especies", ylab="Número de Individuos")


#Curva de rarefaccion
#Instalamos y cargamos el paquete vegan, el cual
#tiene una funcion para hacer una cura de rarefaccion
install.packages("vegan")  
library(vegan)
#colores para diferenciar lineas
colores2<-c("cyan","pink","orange","green","steelblue","red","salmon","purple")
#Creacion de curva de rarefaccion
rarecurve(Datos_grafica_wenos, col=colores2, label=FALSE,ylab = "Especies" ,xlab = "Tamaño de muestra", main="Curva de Rarefacción")




