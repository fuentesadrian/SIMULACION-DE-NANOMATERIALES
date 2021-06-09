install.packages('gganimate')
install.packages('gifski')
install.packages('av')
install.packages('dplyr')
install.packages('ggthemes')
install.packages('gapminder')

library(gganimate)
library(ggplot2)
library(dplyr)
library(gapminder)
library(ggthemes)

T = 30 
t <- 0:1000
sig2 <- T/30
size.change <- 5

x <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
y <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
x <- c(runif(1, 1, 5), cumsum(x))
y <- c(runif(1, 1, 5), cumsum(y))

plot(x, y, type="l")


generate_size <- function(initial, increment) {
  size <- c(initial)
  for (i in 1:(length(t)-1)) {
    size <- c(size, increment + size[i])
  }
  return(round(size, digits = 0))
}


brown_one_particle <- function(identity) {
  initial_size <- runif(1, 100, 2000)
  if (initial_size < 1000) {
    size.change <- -size.change
  }
  size <- generate_size(initial_size, size.change)
  id <- list(rep(identity, length(t)))
  ## first, simulate a set of random deviates
  x <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
  y <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
  x <- c(runif(1, 1, 50), cumsum(x))
  y <- c(runif(1, 1, 50), cumsum(y))
  
  df <- data.frame(x, y, t, id, size)
  colnames(df) <- c('x', 'y', 'time', 'identity', 'size')
  return(as_tibble(df))
}


brown_multiple_particles <- function(number) {
  
  names <- c("x", "y","time","identity", "size")
  brownian_matrix <- data.frame()
  for (k in names) brownian_matrix[[k]] <- as.character()
  
  for (part in 1:number) {
    brownian_matrix <- rbind(brownian_matrix, brown_one_particle(part))
  }
  
  brownian_matrix$identity <- as.factor(brownian_matrix$identity)
  brownian_matrix$size <- as.integer(brownian_matrix$size)
  
  return(brownian_matrix)
}



brownian_data = brown_multiple_particles(8)

brownian_data

graph1 = brownian_data %>%
  ggplot(aes(x=x, y=y, color=identity, size=size)) +
  geom_point(alpha=0.7, stroke=0) +
  theme_fivethirtyeight() +
  labs(title = 'Brownian motion in 2D',
       x = "x axis",
       y = 'y axis',
       color = 'Identity',
       caption = 'Source') +
        theme(axis.title = element_text(),
        text = element_text(family = 'Rubik'),
        legend.text=element_text(size=10)) +
  scale_color_brewer(palette = 'Set2')

graph1

graph.animation = graph1 +
  transition_time(time) + 
  labs(subtitle = "Year: {frame_time}") +
  

animate(graph.animation, height=500, width=800, fps=30, duration = 10, end_pause = 60, res=100)

