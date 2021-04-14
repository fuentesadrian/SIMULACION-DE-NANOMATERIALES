f <- function(x) {
  return(sin(-97*x - 0.3) + x * (x + 0.2) + 1.01)
}
x <- seq(-3, 3, 0.25)
png("p7_1d.png", width=500, height=400)
plot(x, f(x), type="l")
graphics.off()
