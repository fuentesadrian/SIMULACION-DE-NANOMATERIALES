g <- function(x, y) {
  return(sin(-97*x - 0.3) + x * (x + 0.2) + 1.01)
}
x <- seq(-6, 5, 0.25)
y <-  x
z <- outer(x, y, g)
png("p7_2d.png", width=700, height=700)
persp(x, y, z, shade=0.2, col='blue', theta=40, phi=30)
graphics.off()
