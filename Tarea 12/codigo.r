binario <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

n_prob <- c(0.995, 0.98, 0.95, 0.30)
g_prob <- c(0.920, 0.90, 0.85, 0.50)
b_prob <- c(0.002, 0.01, 0.05, 0.80)

prom_fscore <- c()

for (iter in 1:4) {
  modelos <- read.csv("digits.txt", sep=" ", header=FALSE, stringsAsFactors=F)
modelos[modelos=='n'] <- n_prob[iter]
modelos[modelos=='g'] <- g_prob[iter]
modelos[modelos=='b'] <- b_prob[iter]

r <- 5
c <- 3
dim <- r * c

tasa <- 0.15
tranqui <- 0.99

tope <- 9
digitos <- 0:tope
k <- length(digitos)
contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
rownames(contadores) <- 0:tope
colnames(contadores) <- c(0:tope, NA)

n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones

for (t in 1:5000) { # entrenamiento
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,]
  correcto <- binario(d, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    if (deseada != resultado) {
      ajuste <- tasa * (deseada - resultado)
      tasa <- tranqui * tasa
      neuronas[i,] <- w + ajuste * pixeles
    }
  }
}

for (t in 1:300) { # prueba
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
  correcto <- binario(d, n)
  salida <- rep(FALSE, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    salida[i] <- resultado
  }
  r <- min(decimal(salida, n), k) # todos los no-existentes van al final
  contadores[d+1, r+1] <- contadores[d+1, r+1] + 1
}
print(contadores)
precision <- diag(contadores) / rowSums(contadores)
recall <- diag(contadores) / colSums(contadores)[1:(ncol(contadores)-1)]
fscore <- (2 * precision * recall) / (precision + recall)
prom_fscore[iter] <- mean(fscore)
}
prom_fscore

