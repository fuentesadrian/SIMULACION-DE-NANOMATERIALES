library(corrplot)
n <- 50
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n), m=runif(n,min=.001, max=1))
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
eps <- 0.001
fuerza <- function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  mi <- p[i,]$m
  fx <- 0
  fy <- 0
  for (j in 1:n) {
    cj <- p[j,]$c
    mj <- p[j,]$m
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    if (dir>0) {
      factor <- dir * abs(ci - cj) * ((1+mi * (1+mj)^3)^(1/8)) / (sqrt(dx^2 + dy^2) + eps)
    } else {
      factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
    }
    fx <- fx - dx * factor
    fy <- fy - dy * factor
  }
  return(c(fx, fy))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
  tl <- paste("0", tl, sep="")
}
plot(p$x, p$y, col=colores[p$g+6], pch=15, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
     main="Estado inicial", xlab="X", ylab="Y")
f.iter=matrix(ncol=tmax, nrow=n*2)
for (iter in 1:tmax) {
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
  delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i], 1), 0)
  for(i in 1:(n*2)) { f.iter[i,iter] <- f[i] * delta }
  tl <- paste(iter, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  plot(p$x, p$y, col=colores[p$g+6], pch=15, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
       main=paste("Paso", iter), xlab="X", ylab="Y")
}
stopImplicitCluster()
f.iter.distance=matrix(ncol=tmax, nrow=n)
f.iter.media ="0"
for (t in 1:tmax) {
  for (i in 1:n) {
    k = i*2
    f.iter.distance[i,t] <- sqrt(f.iter[k,t]^2 + f.iter[k-1,t]^2)
  }
}
for (i in 1:n) {
  p$v[i]<-mean(f.iter.distance[i,])
}
factores<-cbind(p$v,p$m, p$c)
colnames(factores) <- c("Velocidad","Masa", "Carga")
corrplot(cor(factores)) #Graficando los coeficientes de correlación de las variables para ver que tan relacionadas se encuentran entre si
