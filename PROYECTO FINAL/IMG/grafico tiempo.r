brownian_data %>%
ggplot(aes(x=time, y=size, color=identity)) +
geom_line()

graph.animation = graph + transition_reveal(time)
anim_save('Particles_Size_Evolution.gif',graph.animation)
