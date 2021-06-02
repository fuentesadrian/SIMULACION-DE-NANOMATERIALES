
euclideana <- function(p1, p2) {
  return(sqrt(sum((p1 - p2)**2)))
}

manhattan <- function(p1, p2) {
  return(sum(abs(p1 - p2)))
}

ed.orig <- function(p) {
  dimension <- length(p)
  origen <- rep(0, dimension)
  return(euclideana(p, origen))
}

md.orig <- function(p) {
  dimension <- length(p)
  origen <- rep(0, dimension)
  return(manhattan(p, origen))
}

# Funcion Caminata
caminata <- function(dim, dur, dist) {
  pos <- rep(0, dim)
  mayor <- 0
  for (t in 1:dur) {
    cambiar <- sample(1:dim, 1)
    cambio <- 1
    if (runif(1) < 0.5) {
      cambio <- -1
    }
    pos[cambiar] <- pos[cambiar] + cambio
    d <- dist(pos)
    if (d > mayor) {
      mayor <- d
    }
  }
  return(mayor)
}
pos <- 0
runif(1)
dur <- 1000000
for (t in 1:dur) { 
  if (runif(1) < 0.5) {
    pos <- pos + 1
  } else {
    pos <- pos - 1 
  }
  print(pos)
}
pos <- 0
mayor <- 0
dur <- 1000000
for (t in 1:dur) {
  if (runif(1) < 0.5) {
    pos <- pos + 1
  } else {
    pos <- pos - 1
  }
  dist <- abs(pos)
  if (dist > mayor) {
    mayor <- dist
  }
}
print(mayor)
caminata(2, 1000000, ed.orig)
caminata(2, 1000000, md.orig)
