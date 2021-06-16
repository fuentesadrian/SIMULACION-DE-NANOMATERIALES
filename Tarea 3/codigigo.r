 library("parallel")
library("foreach")
library("doParallel")
library("iterators")
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

divisores<-function(n){
  div<-numeric()
  for (i in 1:ceiling(sqrt(n))){
    if ((n%%i) == 0){
      div<-c(div,i)
      div<-c(div,n/i)
    }
  }
  return(sort(unique(div)))
}


fact_primos<-function(n){
  div<-numeric()
  if(n==1){
    return(1)
  }
  while(n%%2 == 0){
    div<-c(div,2)
    n = n/2
  }
  
  for(i in seq(3, max(3, ceiling(sqrt(n))), 2)){
    while(n%%i==0){
      div<-c(div,i)
      n=n/i
    }
  }
  if(n>2){
    div<-c(div,n)
  }
  return(table(div))
}

primes <- read.delim("primos2.txt", header=FALSE, sep=",")
x <- c()
for (i in 1:nrow(primes)) {
  x1 <- primes[i,]
  x <- c(x,x1)
}

x<- unlist(x)
x <- x[!is.na(x)]
x<-as.numeric(x)

hasta=x[length(x)]
desde=x[1]


noprimos=c()
for (i in desde:hasta){
  if(primo(i) == FALSE){
    noprimos=c(noprimos,i)
  }    
}

vectorprimos <- function (porcentajeprimos) {
  vector=c()
  m=(porcentajeprimos/100)*1000
  for (i in 1:m){
    vector=c(vector, primos[i]) 
  }
  for (j in 1:(1000-m)){
    vector=c(vector,noprimos[j])
  }
  return(sort(vector))
}

vectorprimos1 <- function (porcentajeprimos) {
  vector=c()
  m=(porcentajeprimos/100)*1000
  for (i in 1:m){
    vector=c(vector, primos[i]) 
  }
  for (j in 1:(1000-m)){
    vector=c(vector,noprimos[j])
  }
  return(vector)
}

vectorprimos2 <- function (porcentajeprimos) {
  vector=c()
  m=(porcentajeprimos/100)*1000
  
  for (j in 1:(1000-m)){
    vector=c(vector,noprimos[j])
  }
  for (i in 1:m){
    vector=c(vector, primos[i]) 
  }
  
  return(vector)
}

datos0 <- list(list(), list(), list())
for (j in 1:3) {
  original <- vectorprimos(50)
  invertido <- sort(original, decreasing = TRUE)
  aleatorio <- sample(original) # fijo
  primosfirst <- vectorprimos1(50)
  primoslast  <- vectorprimos2(50)
  replicas <- 10
  suppressMessages(library(doParallel))
  registerDoParallel(makeCluster(detectCores() - 1))
  ot <-  numeric()
  it <-  numeric()
  at <-  numeric()
  pf <-  numeric()
  pl <-  numeric()
  
  for (r in 1:replicas) {
    ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
    it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
    at <- c(at, system.time(foreach(n = aleatorio, .combine=c) %dopar% primo(n))[3]) # orden aleatorio
    pf <- c(at, system.time(foreach(n = primosfirst, .combine=c) %dopar% primo(n))[3]) 
    pl <- c(at, system.time(foreach(n = primoslast, .combine=c) %dopar% primo(n))[3]) 
  }
  stopImplicitCluster()
  
  datos0[[j]][[1]]  <- ot
  datos0[[j]][[2]]  <- it
  datos0[[j]][[3]]  <- at
  datos0[[j]][[4]]  <- pf
  datos0[[j]][[5]]  <- pl
}

y<-data.frame("Nucleos", "Orden", "Tiempo")
for(j in 1:3){
  for(k in 1:5){
    y<-rbind(y,c(4-j,k,round(mean(datos0[[j]][[k]]),digits = 4)))
  }
}
colnames(y)<-c("Nucleos","Orden", "Tiempo")
kruskal.test(y$Tiempo~y$Nucleos)
kruskal.test(y$Tiempo~y$Orden)

png(file="Nucleo1_primos.png",
    width=3000, height=3000, res = 400)
boxplot(datos0[[1]][[1]],datos0[[1]][[2]],datos0[[1]][[3]],
        datos0[[1]][[4]],datos0[[1]][[5]],col=palette("Pastel 2"),
        ylab="Tiempo (s)", xlab=" ",
        names=c("Ascendente", "Descendente", "Aleatorio", "PrimosPrimero", 
                "PrimosFinal"), las=1, cex.lab=1.2, cex.axis=0.9)
dev.off()


png(file="Nucleo2_primos.png",
    width=3000, height=3000, res = 400)
boxplot(datos0[[2]][[1]],datos0[[2]][[2]],datos0[[2]][[3]],
        datos0[[2]][[4]],datos0[[2]][[5]],col=palette("Pastel 2"),
        ylab="Tiempo (s)", xlab=" ",
        names=c("Ascendente", "Descendente", "Aleatorio", "Primos", 
                "No primos"), las=1, cex.lab=1.2, cex.axis=0.9)
dev.off()


png(file="Nucleo3_primos.png",
    width=3000, height=3000, res = 400)
boxplot(datos0[[3]][[1]],datos0[[3]][[2]],datos0[[3]][[3]],
        datos0[[3]][[4]],datos0[[3]][[5]],col=palette("Pastel 2"),
        ylab="Tiempo (s)", xlab=" ",
        names=c("Ascendente", "Descendente", "Aleatorio", "Primos", 
                "No primos"), las=1, cex.lab=1.2, cex.axis=0.9)
dev.off()

datos1 <- list(list(), list(), list())
for (j in 1:3){
  
  suppressMessages(library(doParallel))
  registerDoParallel(makeCluster(detectCores() - j))
  replicas<-30
  
  exp0   <-  numeric()
  exp10  <-  numeric()
  exp20  <-  numeric()
  exp30  <-  numeric()
  exp40  <-  numeric()
  exp50  <-  numeric()
  exp60  <-  numeric()
  exp70  <-  numeric()
  exp80  <-  numeric()
  exp90  <-  numeric()
  exp100 <-  numeric()
  for (r in 1:replicas) {
    exp0   <- c(exp0,   system.time(foreach(n = vectorprimos(0),   .combine=c) %dopar% primo(n))[3])
    exp10  <- c(exp10,  system.time(foreach(n = vectorprimos(10),  .combine=c) %dopar% primo(n))[3])
    exp20  <- c(exp20,  system.time(foreach(n = vectorprimos(20),  .combine=c) %dopar% primo(n))[3])
    exp30  <- c(exp30,  system.time(foreach(n = vectorprimos(30),  .combine=c) %dopar% primo(n))[3])
    exp40  <- c(exp40,  system.time(foreach(n = vectorprimos(40),  .combine=c) %dopar% primo(n))[3])
    exp50  <- c(exp50,  system.time(foreach(n = vectorprimos(50),  .combine=c) %dopar% primo(n))[3])
    exp60  <- c(exp60,  system.time(foreach(n = vectorprimos(60),  .combine=c) %dopar% primo(n))[3])
    exp70  <- c(exp70,  system.time(foreach(n = vectorprimos(70),  .combine=c) %dopar% primo(n))[3])
    exp80  <- c(exp80,  system.time(foreach(n = vectorprimos(80),  .combine=c) %dopar% primo(n))[3])
    exp90  <- c(exp90,  system.time(foreach(n = vectorprimos(90),  .combine=c) %dopar% primo(n))[3])
    exp100 <- c(exp100, system.time(foreach(n = vectorprimos(100), .combine=c) %dopar% primo(n))[3])
  }
  stopImplicitCluster()
  datos1[[j]][[1]]  <- exp0
  datos1[[j]][[2]]  <- exp10
  datos1[[j]][[3]]  <- exp20
  datos1[[j]][[4]]  <- exp30
  datos1[[j]][[5]]  <- exp40
  datos1[[j]][[6]]  <- exp50
  datos1[[j]][[7]]  <- exp60
  datos1[[j]][[8]]  <- exp70
  datos1[[j]][[9]]  <- exp80
  datos1[[j]][[10]] <- exp90
  datos1[[j]][[11]] <- exp100
  
}

png(file="porcentaje_1.png",
    width=3000, height=3000, res = 400)
boxplot(datos1[[1]][[1]],datos1[[1]][[2]],datos1[[1]][[3]],
        datos1[[1]][[4]],datos1[[1]][[5]],datos1[[1]][[6]],
        datos1[[1]][[7]],datos1[[1]][[8]],datos1[[1]][[9]],
        datos1[[1]][[10]],datos1[[1]][[11]],col=palette("Pastel 2"),
        ylab="Tiempo (s)", xlab="Porcentaje de primos en el vector",
        names=c("0%", "10%", "20%", "30%", "40%", "50%", "%60", "70%",
                "80%", "90%", "100%"))
dev.off()

png(file="porcentaje_2.png",
    width=3000, height=3000, res = 400)
boxplot(datos1[[2]][[1]],datos1[[2]][[2]],datos1[[2]][[3]],
        datos1[[2]][[4]],datos1[[2]][[5]],datos1[[2]][[6]],
        datos1[[2]][[7]],datos1[[2]][[8]],datos1[[2]][[9]],
        datos1[[2]][[10]],datos1[[2]][[11]],col=palette("Pastel 2"),
        ylab="Tiempo (s)", xlab="Porcentaje de primos en el vector",
        names=c("0%", "10%", "20%", "30%", "40%", "50%", "%60", "70%",
                "80%", "90%", "100%"))
dev.off()


png(file="porcentaje_3.png",
    width=3000, height=3000, res = 400)
boxplot(datos1[[3]][[1]],datos1[[3]][[2]],datos1[[3]][[3]],
        datos1[[3]][[4]],datos1[[3]][[5]],datos1[[3]][[6]],
        datos1[[3]][[7]],datos1[[3]][[8]],datos1[[3]][[9]],
        datos1[[3]][[10]],datos1[[3]][[11]],col=palette("Pastel 2"),
        ylab="Tiempo (s)", xlab="Porcentaje de primos en el vector",
        names=c("0%", "10%", "20%", "30%", "40%", "50%", "%60", "70%",
                "80%", "90%", "100%"))
dev.off()

z<-data.frame("Nucleos", "PorcentajePRIMOS", "Tiempo")
for(j in 1:3){
  for(k in 1:11){
    z<-rbind(z,c(4-j,k,round(mean(datos1[[j]][[k]]))))
  }
}
colnames(z)<-c("Nucleos","PorcentajePRIMOS", "Tiempo")
kruskal.test(z$Tiempo~z$Nucleos)
kruskal.test(z$Tiempo~z$PorcentajePRIMOS)

datos2 <- list(list(), list(), list())
for (j in 1:3){
  
  original    <-vectorprimos(50)
  invertido   <- sort(original, decreasing = TRUE)
  aleatorio   <- sample(original) 
  primosfirst <- vectorprimos1(50)
  primoslast  <- vectorprimos2(50)
  replicas    <- 30
  
  suppressMessages(library(doParallel))
  registerDoParallel(makeCluster(detectCores() - j))
  
  ot <-  numeric()
  it <-  numeric()
  at <-  numeric()
  pf <-  numeric()
  pl <-  numeric()
  
  for (r in 1:replicas) {
    ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% divisores(n))[3])
    it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% divisores(n))[3])
    at <- c(at, system.time(foreach(n = aleatorio, .combine=c) %dopar% divisores(n))[3]) 
    pf <- c(at, system.time(foreach(n = primosfirst, .combine=c) %dopar% divisores(n))[3]) 
    pl <- c(at, system.time(foreach(n = primoslast, .combine=c) %dopar% divisores(n))[3]) 
  }
  stopImplicitCluster()
  datos2[[j]][[1]]  <- ot
  datos2[[j]][[2]]  <- it
  datos2[[j]][[3]]  <- at
  datos2[[j]][[4]]  <- pf
  datos2[[j]][[5]]  <- pl
  
}


w<-data.frame("Nucleos", "Orden", "Tiempo")
for(j in 1:3){
  for(k in 1:5){
    w<-rbind(w,c(4-j,k,round(mean(datos2[[j]][[k]]))))
  }
}
colnames(w)<-c("Nucleos","Orden", "Tiempo")
kruskal.test(w$Tiempo~w$Orden)
kruskal.test(w$Tiempo~w$Nucleos)

png(file="divisores_1.png",
    width=3000, height=3000, res = 400)
boxplot(datos2[[1]][[1]],datos2[[1]][[2]],datos2[[1]][[3]],
        datos2[[1]][[4]],datos2[[1]][[5]],col=palette("Pastel 2"),
        ylab="Tiempo (s)", xlab=" ",
        names=c("Ascendente", "Descendente", "Aleatorio", "Primos", 
                "No primos"), las=1, cex.lab=1.2, cex.axis=0.9)
dev.off()

png(file="divisores_2.png",
    width=3000, height=3000, res = 400)
boxplot(datos2[[2]][[1]],datos2[[2]][[2]],datos2[[2]][[3]],
        datos2[[2]][[4]],datos2[[2]][[5]],col=palette("Pastel 2"),
        ylab="Tiempo (s)", xlab=" ",
        names=c("Ascendente", "Descendente", "Aleatorio", "Primos", 
                "No primos"), las=1, cex.lab=1.2, cex.axis=0.9)
dev.off()

png(file="divisores_3.png",
    width=3000, height=3000, res = 400)
boxplot(datos2[[3]][[1]],datos2[[3]][[2]],datos2[[3]][[3]],
        datos2[[3]][[4]],datos2[[3]][[5]],col=palette("Pastel 2"),
        ylab="Tiempo (s)", xlab=" ",
        names=c("Ascendente", "Descendente", "Aleatorio", "Primos", 
                "No primos"), las=1, cex.lab=1.2, cex.axis=0.9)
dev.off()

datos3 <- list(list(), list(), list())
for (j in 1:3){
  
  original    <-vectorprimos(50)
  invertido   <- sort(original, decreasing = TRUE)
  aleatorio   <- sample(original) 
  primosfirst <- vectorprimos1(50)
  primoslast  <- vectorprimos2(50)
  replicas    <- 30
  
  suppressMessages(library(doParallel))
  registerDoParallel(makeCluster(detectCores() - j))
  
  ot <-  numeric()
  it <-  numeric()
  at <-  numeric()
  pf <-  numeric()
  pl <-  numeric()
  
  for (r in 1:replicas) {
    ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% fact_primos(n))[3])
    it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% fact_primos(n))[3])
    at <- c(at, system.time(foreach(n = aleatorio, .combine=c) %dopar% fact_primos(n))[3]) 
    pf <- c(at, system.time(foreach(n = primosfirst, .combine=c) %dopar% fact_primos(n))[3]) 
    pl <- c(at, system.time(foreach(n = primoslast, .combine=c) %dopar% fact_primos(n))[3]) 
  }
  stopImplicitCluster()
  datos3[[j]][[1]]  <- ot
  datos3[[j]][[2]]  <- it
  datos3[[j]][[3]]  <- at
  datos3[[j]][[4]]  <- pf
  datos3[[j]][[5]]  <- pl
  
}

v<-data.frame("Nucleos", "Orden", "Tiempo")
for(j in 1:3){
  for(k in 1:5){
    v<-rbind(v,c(4-j,k,round(mean(datos3[[j]][[k]]))))
  }
}
colnames(v)<-c("Nucleos","Orden", "Tiempo")
kruskal.test(v$Tiempo~v$Orden)
kruskal.test(v$Tiempo~v$Nucleos)

png(file="factorizacion_1.png",
    width=3000, height=3000, res = 400)
boxplot(datos3[[1]][[1]],datos3[[1]][[2]],datos3[[1]][[3]],
        datos3[[1]][[4]],datos3[[1]][[5]],col=palette("Pastel 2"),
        ylab="Tiempo (s)", xlab=" ",
        names=c("Ascendente", "Descendente", "Aleatorio", "Primos", 
                "No primos"), las=1, cex.lab=1.2, cex.axis=0.9)
dev.off()

png(file="factorizacion_2.png",
    width=3000, height=3000, res = 400)
boxplot(datos3[[2]][[1]],datos3[[2]][[2]],datos3[[2]][[3]],
        datos3[[2]][[4]],datos3[[2]][[5]],col=palette("Pastel 2"),
        ylab="Tiempo (s)", xlab=" ",
        names=c("Ascendente", "Descendente", "Aleatorio", "Primos", 
                "No primos"), las=1, cex.lab=1.2, cex.axis=0.9)
dev.off()

png(file="factorizacion_3.png",
    width=3000, height=3000, res = 400)
boxplot(datos3[[3]][[1]],datos3[[3]][[2]],datos3[[3]][[3]],
        datos3[[3]][[4]],datos3[[3]][[5]],col=palette("Pastel 2"),
        ylab="Tiempo (s)", xlab=" ",
        names=c("Ascendente", "Descendente", "Aleatorio", "Primos", 
                "No primos"), las=1, cex.lab=1.2, cex.axis=0.9)
dev.off()
