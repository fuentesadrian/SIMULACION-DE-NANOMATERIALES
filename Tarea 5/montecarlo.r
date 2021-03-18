library(distr)
library(ggplot2)

wolf=0.048834 #wolfram alpha
rate=0.80 #tasa exito
exitos=as.data.frame(matrix(nrow = 5, ncol = 1))  #matriz para guardado y grafico final
wolfram=cbind(round(wolf,digits=2),round(wolf,digits=3),round(wolf,digits=4),round(wolf,digits=5),wolf)#matriz de decimales

montecarlosf<-function(n) {
  desde <- 3
  hasta <- 7
  f <- function(x) { return(1 / (exp(x) + exp(-x))) }
  g <- function(x) { return((2 / pi) * f(x)) }
  suppressMessages(library(distr))
  generador  <- r(AbscontDistribution(d = g)) # creamos un generador            
  valores <- generador(n)
  mc=sum(valores >= desde & valores <= hasta)
  integral <- sum(mc) / n
  resultado=(pi / 2) * integral
  return(resultado)
}

replica <- function() { replicate(reps, montecarlosf(nsample))  } #repite la funcion montecarlo


for (c in 2:6) {  #bucle por cada decimal
  reps=20 #repeticiones
  nsample=5000  #muestras
  paso=1000 #pasos
  #bucle en caso una repeticion no consiga la tasa de exito, repetir proceso
  for (i in 1:10) {
    resultado<-round(replica(),digits=c)  #guarda en resultado las replicas
    #en caso el vector resultado no encuentre a la tasa fijada de exito los decimales iguales a wolfram,
    #sumar pasos y muestras y repite, caso contrario, sale del bucle
    if (as.numeric(length(resultado[resultado == wolfram[1,c-1] ])/reps) <= rate){
      paso=paso+paso
      nsample=nsample+paso
    } else {
      break
    }
  }
  #guarda la muestra en la cual fue exitosa la tasa para el vector resultado
  exitos[c-1,1]=nsample
}

#grafico
ggplot()+geom_line(data=exitos,aes(x=c(2:6),y=exitos$V1))+labs(title="Montecarlo",y="n",x="decimales")
