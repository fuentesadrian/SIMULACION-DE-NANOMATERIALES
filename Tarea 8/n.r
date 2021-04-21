k <- 1000
n <- 16,32,64,128
originales <- rnorm(k)
cumulos <- originales - min(originales) + 1
cumulos <- round(n * cumulos / sum(cumulos))
diferencia <- n - sum(cumulos)
if (diferencia > 0) {
    for (i in 1:diferencia) {
        p <- sample(1:k, 1)
        cumulos[p] <- cumulos[p] + 1
    }
} else if (diferencia < 0) {
    for (i in 1:-diferencia) {
        p <- sample(1:k, 1)
        if (cumulos[p] > 1) {
            cumulos[p] <- cumulos[p] - 1
        }
    }
}
c <- median(cumulos) # tamanio critico de cumulos
d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva
sigmoid <- function(x, c, d) {
    return (1 / (1 + exp((c - x) / d)))
}
png("p8_sigmoid.png")
low <- min(cumulos)
high <- max(cumulos)
x <- low:high
plot(x, sigmoid(x, c, d), ylim=c(-0.1, 1.1), xlim=c(low, high),
     xlab="Tama\u{00f1}o", ylab="Probabilidad de fragmentaci\u{00f3}n")
abline(v = c, col=2, lwd=3)
graphics.off()
