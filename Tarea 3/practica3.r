library(parallel)
detectCores()

numprimos=read.csv("250numerosprimos.txt", header = FALSE)
n=dim(numprimos)
print(length(numprimos))
noprimos=numprimos + 1
tareas=c(numprimos,noprimos)


primo <- function(n) {
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

original <- tareas
invertido <- rev(tareas)
aleatorio <- sample(tareas) 
replicas <- 10
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
ot <-  numeric()
it <-  numeric()
at <-  numeric()
for (r in 1:replicas) {
  ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
  it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
  at <- c(at, system.time(foreach(n = aleatorio, .combine=c) %dopar% primo(n))[3]) # orden aleatorio
}
stopImplicitCluster()
summary(ot)
summary(it)
summary(at)

tiempo=t(rbind(ot,it,at))
#install.packages("vioplot")
library(vioplot)
tiff("3corregida.tif", width=12, height=12, units="cm", res=600, pointsize = 10)
vioplot(tiempo,xlab="Orden",ylab="Tiempo de ejecuci�n (seg)", col=palette("classic tableau"), border="black")
dev.off()

names(tiempo)=c("Original","Invertido","Aleatorio")
write.table(tiempo,file="t3r1-tiempo.txt",quote=FALSE,sep="\t",row.names=FALSE)
