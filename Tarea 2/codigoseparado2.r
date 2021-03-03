#PRACTICA NUMERO 2
#AUTOMATA CELULAR
#JOSE ADRIAN GARCIA FUENTES
library(parallel)
terreno <- 12  #numero de dimensiones del experimento en este caso se multiplicara por el mispo para tener un un numero total de celdas
hectareascuadradas <-  terreno^2

nido <- matrix(round(runif(hectareascuadradas)), nrow=terreno, ncol=terreno, byrow=TRUE)
#punto de origen esta creado con una rutina llamada matrix en donde intervienen el entero mas cercano de las posiciones aleatorios del numero total de celdas del experimento numeros aleatorios entre 0 y 1
#byrow=true cambiara el orden de llenado de la matrix los espacios ,,,forma de la matrix nrow y ncol indican el numero de filas y columnas
suppressMessages(library("sna"))  #se utiliza la libreria sna para visualizar crear la matrix en dibujo para cambiar el color a negro en los que esten renacio
png("practica2_t0_r.png")
plot.sociomatrix(nido, diaglab=FALSE, main="Inicio")  #main nombre dela primera imagen o punto origen diaglab pone un falso o blanco para los que esten muertos
graphics.off()

Año <- function(posicionvector) {
  fila <- floor((posicionvector - 1) / terreno) + 1
  columna <- ((posicionvector - 1) %% terreno) + 1
  vecinos <-  nido[max(fila - 1, 1) : min(fila + 1 , terreno),
                      max(columna - 1, 1): min(columna + 1, terreno)]
  return(1 *((sum(vecinos) - nido[fila, columna]) == 2)) #numero a modificar 3 para el cambio de reglas
   
  }
print(nido)
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "terreno")
clusterExport(cluster, "Año")

for(iteracion in 1:30){
  clusterExport(cluster, "nido")
  siguiente<- parSapply(cluster, 1:hectareascuadradas, Año)
  renacio<- sum(siguiente)
  cat(iteracion, renacio, '\n')
  if (renacio == 0) { 
    print("renacio el fenix")
    break;
  } 
  nido <- matrix(siguiente, nrow=terreno, ncol=terreno, byrow=TRUE)
  print(nido)
  salida = paste("practica2_t", iteracion, "_r.png", sep="")
  tiempo = paste("Año", iteracion)
  png(salida)
  plot.sociomatrix(nido, diaglab=FALSE, main=tiempo)
  graphics.off()
}
