#PRACTICA NUMERO 2
#AUTOMATA CELULAR
#JOSE ADRIAN GARCIA FUENTES
#LIBRERIA PARALELA A REPOSITORIO DE GITHUB ELISA STU

library(parallel)
dimensiondereja <- 12
numeroceldas <-  dimensiondereja^2
estadoinicial <- matrix(round(runif(numeroceldas)), nrow=dimensiondereja, ncol=dimensiondereja, byrow=TRUE)
suppressMessages(library("sna"))
png("p2_t0_r.png")
plot.sociomatrix(estadoinicial, diaglab=FALSE, main="Inicio")
graphics.off()

paso <- function(posicionvector) {
  fila <- floor((posicionvector - 1) / dimensiondereja) + 1
  columna <- ((posicionvector - 1) %% dimensiondereja) + 1
  vecinos <-  estadoinicial[max(fila - 1, 1) : min(fila + 1, dimensiondereja),
                      max(columna - 1, 1): min(columna + 1, dimensiondereja)]
  return(1 * ((sum(vecinos) - estadoinicial[fila, columna]) == 1))
  print("1 vecino le dio covid")
}
paso <- function(posicionvector) {
  fila <- floor((posicionvector - 1) / dimensiondereja) + 1
  columna <- ((posicionvector - 1) %% dimensiondereja) + 1
  vecinos <-  estadoinicial[max(fila - 1, 1) : min(fila + 1, dimensiondereja),
                            max(columna - 1, 1): min(columna + 1, dimensiondereja)]
  (return(1 * ((sum(vecinos) - estadoinicial[fila, columna]) == 1))*5)
  print("1 vecino le dio covid pero los demas seguimos vivos")
}

print(estadoinicial)
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dimensiondereja")
clusterExport(cluster, "paso")

for (iteracion in 1:30) {
  clusterExport(cluster, "estadoinicial")
  siguiente <- parSapply(cluster, 1:numeroceldas, paso)
  vivos = sum(siguiente)
  cat(iteracion, vivos, '\n')
  if (vivos == 0) { 
    print("la pandemia los mato")
    break;
  }
  estadoinicial <- matrix(siguiente, nrow=dimensiondereja, ncol=dimensiondereja, byrow=TRUE)
  print(estadoinicial)
  salida = paste("p2-t", iteracion, "_r.png", sep="")
  tiempo = paste("Paso", iteracion)
  png(salida)
  plot.sociomatrix(estadoinicial, diaglab=FALSE, main=tiempo)
  graphics.off()
}
stopCluster(cluster)

