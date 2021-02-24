#PRACTICA 1
#MOVIMIENTO BROWNIANO
#JOSE ADRIAN GARCIA FUENTES
dimension<- 5
posicioninicial<- 0*dimension   #posicion donde se encuentra mi bolita modifique y le puse rep para repetir el cero por 5 que son las dimensiones en nuestro caso nos pidieron 5
runif(1)             #numero pseudeoaleatorio del 0 a 1
duracioncaminata<- 30   #numero de pasos de la caminata
mayorvaloralcanzadodurantelacaminata<-0  #que tan lejos llego la particula desde el origen en funcion del numero de pasos checar despues de el primer for 
for (dimension in 1:5)
  {
  clusterExport(cluster, "dimension")
  resultado <- parSapply(cluster, 1:repetir,experimento)
                         function(r){
                         posicioninicial <- rep(0, dimension)
                         mayorvaloralcanzadodurantelacaminata<- 0
                         
    for (t in 1:duracioncaminata) {
        if (runif(1) < 0.5) {
    posicioninicial<- posicioninicial + 1 
    } else {
    posicioninicial<- posicioninicial - 1
    }
distanciaalcanzada <- abs(posicioninicial)
#la distancia alcanzada es igual al absoluto de la posicion inicial
  if (distanciaalcanzada > mayorvaloralcanzadodurantelacaminata){
    mayorvaloralcanzadodurantelacaminata<- distanciaalcanzada
  }
  }
                         }
print(mayorvaloralcanzadodurantelacaminata)
    #ciclo for si el tiempo en mi duracion de caminata es menor a o.5 de un numero aleatoriola posicion sumara uno de lo contrario se restara 1 se imprimen resultados
paso= function(posicioninicial, dimension){
  d= sample(1:dimension,5);
  if(runif(1) < 0.5) {
    posicioninicial[d] = posicioninicial[d] - 1;
  } else {
    posicioninicial[d] = posicioninicial[d] + 1;
  }
  return(posicioninicial);
}
source("distance.R")
source("caminata.R")
caminata(5, 30, ed.origen)
caminata(5, 30, md.origen)
if (eucl) {
  png("p1er.png")
  boxplot(data.matrix(datos), use.cols=FALSE, 
          xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima", 
          main="Euclideana")
} else {
  png("p1mr.png")
  boxplot(data.matrix(datos), use.cols=FALSE, 
          xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima", 
          main="Manhattan")
}
graphics.off()

