############################### JACCARD ######################################

Datos<-read.csv("Datos/Abundancias por especie.csv") # Cargamos nuestra base de datos de abundancias de especies para cada poblacion 

# Separamos nuestra base de datos en 7 poblaciones poblaciones 

class(Datos)

datos1<-as.data.frame(Datos)

class(datos1)

datos1[1,2:13]
poblaciones<-Datos

poblacion1<- poblaciones[1,2:13]

poblacion2<- poblaciones[2,2:13]

poblacion3<- poblaciones[3,2:13]

poblacion4<- poblaciones[4,2:13]

poblacion5<- poblaciones[5,2:13]

poblacion6<- poblaciones[6,2:13]

poblacion7<- poblaciones[7,2:13]

#######################################################################

intersecto1<-intersect(datos1[1,2:13],datos1[2,2:13])

intersecto1
######################################################################

datosb<-data.frame( poblacion1<- poblaciones[1,2:13],
  
  poblacion2<- poblaciones[2,2:13],
  
  poblacion3<- poblaciones[3,2:13],
  
  poblacion4<- poblaciones[4,2:13],
  
  poblacion5<- poblaciones[5,2:13],
  
  poblacion6<- poblaciones[6,2:13],
  
  poblacion7<- poblaciones[7,2:13],
)
######################################################################

############ Version funcional, asignar 1 o 0 a cada población ###############

for ( i in 1:length(poblacion1)){
  
  if ( poblacion1[i] != 0){
    
    poblacion1[i]<-1
    
  } else if ( poblacion1[i] == 0){
    
    poblacion1[i]<-0
  }
  
  
}


poblacion1

####################################################################################################

nrow(Datos)

##################### version 2 asignar cambios a todas las poblaciones ##############

for ( i in 1:length(poblacion1)){
  
  if ( poblacion1[i] != 0){
    
    poblacion1[i]<-1
    
  } else if ( poblacion1[i] == 0){
    
    poblacion1[i]<-0
  }
###################################
  
  if ( poblacion2[i] != 0){
    
    poblacion2[i]<-1
    
  } else if ( poblacion2[i] == 0){
    
    poblacion2[i]<-0
  }
##################################
  
  if ( poblacion3[i] != 0){
    
    poblacion3[i]<-1
    
  } else if ( poblacion3[i] == 0){
    
    poblacion3[i]<-0
  }
######################################
  
  if ( poblacion4[i] != 0){
    
    poblacion4[i]<-1
    
  } else if ( poblacion4[i] == 0){
    
    poblacion4[i]<-0
  }
  
  
  
} # El ultimo

poblacion1
poblacion2
poblacion3

jaccar
######################################################################


