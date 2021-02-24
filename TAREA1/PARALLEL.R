repetir <- 30
duracion <- 2**4:9

eucl <-  FALSE
library(parallel)

datos <-  data.frame()

experimento <- function(replica) {
  posicioninicial <- rep(0, dimension)
  mayor <- 0
  for (t in 1:duracion) {
    cambiar <- sample(1:dimension, 1)
    cambio <- 1
    if (runif(1) < 0.5) {
      cambio <- -1
    }
    posicioninicial[cambiar] <- posicioninicial[cambiar] + cambio
    if (eucl) {
      d <- sqrt(sum(posicioninicial**2))
    } else { # Manhattan
      d <- sum(abs(posicioninicial))
    }
    if (d > mayor) {
      mayor <- d
    }
  }
  return(mayor)
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "duracion")
clusterExport(cluster, "eucl")
clusterExport(cluster, "experimento")

for (dimension in 1:5) {
  clusterExport(cluster, "dimension")
  resultado <- parSapply(cluster, 1:repetir, experimento)
  datos <- rbind(datos, resultado)
}
stopCluster(cluster)
print(resultado) 

