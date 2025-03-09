############################### JACCARD ######################################

#Este indice no requiere cargar ninguna libreria

Datos<-read.csv("Datos/Abundancias por especie.csv") # Cargamos nuestra base de datos de abundancias de especies para cada poblacion 

# Cambiamos nuestra matriz de abundancia a presencia-ausencia, esto es posible debido a que el indice Jaccard no considera abundancias

for ( n in 1:nrow(Datos)){ # n indica la fila de nuestro data.frame (se ajusta a la base de datos del usuario) 
  
  for (p in 2:length(Datos) ){ # p indica el intervalo de elementos de la fila que serán transformados, omitimos el 1 debido a que este es el ID de la fila
    
    if ( Datos[n,p] != 0 ){ # Si la celda es distinta a o quiere decir que la especie esta presente, por lo tanto se le asigna un 1
      
      Datos[n,p]<- 1 
      
    } else if ( Datos[n,p] == 0){ # Si la celda es igual a 0 quiere decir que la especie NO esta presente, por lo tanto se le asigna un 0
      
      Datos[n,p]<- 0
    }
  } 
}  

 Datos 
######### Determinar interseccion y union de los conjuntos seleccionados #######

# "u" e "i" serán los elementos en los que se almacenara cada iteracion del ciclo que cumpla la condicion de cada uno de los objetos 
# Deben estar fuera del ciclo siguiente, pues, de lo contrario esto se reinician en cada iteracion 
 
u<-0
i<-0
  
  for (c in 2:length(Datos)) { # c determina el rango de los elementos de cada conjunto (fila del data frame) que seran comparados, este numero dependerá de los datos del usuario
    
    if (Datos[1, c] == 1 & Datos[2, c] == 1) { # Determinamos la intersección por aquellos elementos que estan presentes en ambos conjuntos, por ello usamos el conector logico &, ya que ambos conjuntos deben cumplirt dicha condicion 
      
      i <- i + 1  # Cada que un elemento cumple la condicion anterior se suma 1 al objeto de interseccion
      
    }
    
    if (Datos[1, c] == 1 || Datos[2, c] == 1) { # Determinamos la union como los elementos que están presentes en uno u otro conjunto, por lo tanto, usamos el conector logico "|" ya que este implica que al menos uno debe cumplir la condicion
      
      u <- u + 1  #Cada que un elemento cumple la condicion anterior se suma 1 al objeto de union
      
    }
  }

################# Crear la funcion ##################

TablaJaccard<-data.frame(Especie_A = integer() , Especie_B = integer(), Indice_Jaccard = numeric()) # Creamos este data frame para almacenar cada vez que se ejecute nuestra funcion 

# El data frame debe contar con el tipo de objeto que ocupara esa posición, de lo contrario no se podrá crear 

Jaccard<-function(x,y){
  
a<- as.numeric (readline (prompt = "Coloca el numero de fila en tu dataframe de tu primera poblacion: ")) # De esta forma el usuario elige que poblaciones desea comparar 

b<- as.numeric (readline (prompt = "Coloca el numero de fila en tu dataframe de tu segunda poblacion: "))

  for ( n in 1:nrow(Datos)){ 
    
    for (p in 2:length(Datos) ){ 
      
      if ( Datos[n,p] != 0 ){ 
        
        Datos[n,p]<- 1 
        
      } else if ( Datos[n,p] == 0){ 
        
        Datos[n,p]<- 0
      }
    } 
  }
  
  u<-0
  i<-0
  
  for (c in 2:length(Datos)) { 
    
    if (Datos[a, c] == 1 & Datos[b, c] == 1) { 
      
      i <- i + 1  
      
    }
    
    if (Datos[a, c] == 1 || Datos[b, c] == 1) { 
      
      u <- u + 1  
      
    }
  }
  
  indice<- i/u
  
print(indice)

comparacion<-data.frame(Especie_A = a , Especie_B = b, Indice_Jaccard = indice)

TablaJaccard<<-rbind(TablaJaccard, comparacion)

View(TablaJaccard)
  
}

Jaccard()

###############################

############################################## Bray - curtis ######################################

# Rescatamos parte del codigo Jaccard para comparar la intersección entre las dos poblaciones seleccionadas

# Debido a que el indice Bray-Curtis si requiere de abundancias en este caso la base de datos no será modificada 

Datos<-read.csv("Datos/Abundancias por especie.csv") # Cargamos nuestra base de datos 

ea<-0# El objeto "ea" almacenara la suma de los valores minimos de abundancia entre cada poblacion 

for (c in 2:ncol(Datos)) { # c determina el rango de los elementos de cada conjunto (fila del data frame) que seran comparados, este numero dependerá de los datos del usuario 
  
  if (Datos[1, c] > 0 & Datos[2, c] > 0) { # En este caso queremos identificar todas aquellas posiciones en las que ambas poblaciones tienen presencia de la misma especie 
    
    # Cualquier valor mayor a 0 indica presencia de especies, se usa el conector logico & debido a que ambas poblaciones deben cumplir la condicion de tener abundancia de dicha especie
    
    # Se uso ">" en lugar de "!=" debido a que en el caso de "!=" si al hacer la base de datos el usuario coloca por error un valor negativo este cumpliria la condicion "!= 0", esto alteraria el resultado 
    
    ea <- ea + min(Datos[1, c], Datos[2, c]) # Usamos min para seleccionar el numero más chico en c posicion de cada vector 
  }
}

print(ea)

################# Creamos objetos Si e Sj 

si<<-sum(Datos[a,2:length(Datos)]) # Determinamos las columnas usando el numero de columnas de la base de datos

sj<<-sum(Datos[b,2:length(Datos)]) # Partimos del 2 debido a que el 1 corresponde al ID 

########## Indice Bray- Curtis ############

indicebc<- 1 - ((2 * (ea)) / (si + sj))




################################# Creamos la funcion ###########################

TablaBC<-data.frame(Especie_A = integer() , Especie_B = integer(), Indice_Bray_Curtis = numeric()) # Al igual que con Jaccard creamos una tabla que guarde nuestras comparaciones 

Bray_curtis<-function(x,y){
  
  a<<- as.numeric (readline (prompt = "Coloca el numero de fila en tu dataframe de tu primera poblacion: ")) # De esta forma el usuario elige que poblaciones desea comparar 
  
  b<<- as.numeric (readline (prompt = "Coloca el numero de fila en tu dataframe de tu segunda poblacion: "))
  
  ea<-0
  
  for (c in 2:ncol(Datos)) {
    
    if (Datos[a, c] > 0 & Datos[b, c] > 0) { 
      
      ea <- ea + min(Datos[a, c], Datos[b, c]) 
      
    }
  } 
  
  si<<-sum(Datos[a,2:length(Datos)])
  
  sj<<-sum(Datos[b,2:length(Datos)])
  
  indicebc<- 1 - ((2 * (ea)) / (si + sj))
  
  print(indicebc)
  
  comparacionbc<-data.frame(Especie_A = a , Especie_B = b, Indice_Bray_Curtis = indicebc)
  
  TablaBC<<-rbind(TablaBC, comparacionbc)
  
  View(TablaBC)
  
}

Bray_curtis()


