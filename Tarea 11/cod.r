library(ggplot2)
pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}
poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}
eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}
domin.by <- function(target, challenger) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}
vc <- 4 
md <- 3 
tc <- 5 
k <- 2 
obj <- list()
r <- 30 
vectorfunciones <- c(2,4,6,8)
poncentajefrente<-matrix(0, nrow=r, ncol=length(vectorfunciones))
for(f in vectorfunciones) {
k <- f o
for (q in 1:r) {
for (i in 1:k) {
  obj[[i]] <- poli(md, vc, tc)
}
minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim)
n <- 200 # cuantas soluciones aleatorias
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
for (i in 1:n) { 
  for (j in 1:k) { 
    val[i, j] <- eval(obj[[j]], sol[i,], tc)
  }
}
mejor1 <- which.max(sign[1] * val[,1])
mejor2 <- which.max(sign[2] * val[,2])
cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")

no.dom <- logical()
dominadores <- integer()
for (i in 1:n) {
  d <- logical()
  for (j in 1:n) {
    d <- c(d, domin.by(sign * val[i,], sign * val[j,]))
  }
  cuantos <- sum(d)
  dominadores <- c(dominadores, cuantos)
  no.dom <- c(no.dom, cuantos == 0) 
}
frente <- subset(val, no.dom) 
poncentajefrente[q,(f/2)]  <- (length(frente[,1])/n)*100
}
}
frentes<-c(poncentajefrente[,1], poncentajefrente[,2], poncentajefrente[,3], poncentajefrente[,4])
funciones<-c(rep(2, r), rep(4, r), rep(6, r), rep(8, r))
funciones<-as.factor(funciones)
data2 <- data.frame(pos=funciones, dom=frentes)

g2 <- ggplot(data2, aes(x=pos, y=dom)) + geom_violin(fill="orange", color="red") 
g2 + geom_boxplot(width=0.2, fill="blue", color="green", lwd=2) +
  xlab("") +
  ylab("Frecuencia") +
  ggtitle("Porcentaje de Frentes de Pareto entre las Soluciones Totales")
t.test(poncentajefrente[,1], poncentajefrente[,2])$p.value
t.test(poncentajefrente[,1], poncentajefrente[,3])$p.value
t.test(poncentajefrente[,1], poncentajefrente[,4])$p.value
t.test(poncentajefrente[,2], poncentajefrente[,3])$p.value
t.test(poncentajefrente[,2], poncentajefrente[,4])$p.value
t.test(poncentajefrente[,3], poncentajefrente[,4])$p.value
