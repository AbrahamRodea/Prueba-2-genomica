#Leémos los datos de la tabla 
datos_poblacion <- read.csv("Datos/Abundancias por especie.csv")
datos_poblacion


#Cálculo de índices de Shannon de todas las poblaciones

shannonH <- function(x) { 
  x <- x[x > 0]  
  p <- x / sum(x)  
  return(-sum(p * log(p)))
}

# Aplicar la función a cada fila (población)
# Excluir la primera columna (Poblacion) y la última columna (n_especies)
indices_shannon <- apply(datos_poblacion[, 2:(ncol(datos_poblacion)-1)], 1, shannonH)

# Mostrar los resultados
resultados <- data.frame(Población = datos_poblacion$Poblacion, Shannon = indices_shannon)
print(resultados)
#Cálculo de índice de Pielou
mi_pielou <- function(abundances){
  shannonH(abundances)/log(length(abundances))
}

indices_pielou <- apply(datos_poblacion[, 2:(ncol(datos_poblacion)-1)], 1, mi_pielou)

resultado_pielou <- data.frame(Población = datos_poblacion$Poblacion, Pielou = indices_pielou)
print(resultado_pielou)


#Cálculo de índice de Simpson
SimpsonI <- function(abundances){
  prob <- abundances/sum(abundances)
  sum(prob*prob)
}
indices_simpson <- apply(datos_poblacion[, 2:(ncol(datos_poblacion)-1)], 1, SimpsonI)

resultado_simpson <- data.frame(Población = datos_poblacion$Poblacion, Simpson = indices_simpson)
print(resultado_simpson)


#Cálculo de índice de Simpson Inverso 
SimpsonInverse <- function(abundances){
  prob <- abundances/sum(abundances)
  1 - sum(prob^2)
}
indices_simpsoninv <- apply(datos_poblacion[, 2:(ncol(datos_poblacion)-1)], 1, SimpsonInverse)

resultado_simpsoninv <- data.frame(Población = datos_poblacion$Poblacion, SimpsonInverso = indices_simpsoninv)
print(resultado_simpsoninv)

#Cálculo de índice de Gini
GiniI <- function(abundances) {
  abundances <- abundances[abundances > 0]  
  n <- length(abundances)  
  abundances <- sort(abundances)  
  gini <- sum((2 * 1:n - n - 1) * abundances) / (n * sum(abundances))  # Fórmula simplificada
  return(gini)
}
indices_gini <- apply(datos_poblacion[, 2:(ncol(datos_poblacion)-1)], 1, GiniI)

resultados_gini <- data.frame(Población = datos_poblacion$Poblacion, Gini = indices_gini)
print(resultados_gini)

#Cálculo de índice de Chao1

Chao1 <- function(abundances) {
  abundances <- abundances[abundances > 0]  
  S_obs <- length(abundances)  # Número de especies observadas
  F1 <- sum(abundances == 1)  # Número de singletons (especies con abundancia 1)
  F2 <- sum(abundances == 2)  # Número de doubletons (especies con abundancia 2)
  
  # Calcular el índice de Chao1
  if (F2 == 0) {
    chao1 <- S_obs + (F1 * (F1 - 1)) / 2  # Sirve para evitar división por 0
  } else {
    chao1 <- S_obs + (F1^2) / (2 * F2)  # Fórmula normal de Chao1
  }
  
  return(chao1)
}

indices_chao1 <- apply(datos_poblacion[, 2:(ncol(datos_poblacion)-1)], 1, Chao1)
resultado_chao1 <- data.frame(Población = datos_poblacion$Poblacion, Chao1 = indices_chao1)
print(resultado_chao1)