
# load images ###################################

# plotexample Representation.png
image_1 <- image_read(paste0("../illustrations/plotexample ", levels(d3$condition)[1], ".png"))
image_2 <- image_read(paste0("../illustrations/plotexample ", levels(d3$condition)[2], ".png"))
image_3 <- image_read(paste0("../illustrations/plotexample ", levels(d3$condition)[3], ".png"))
image_4 <- image_read(paste0("../illustrations/plotexample ", levels(d3$condition)[4], ".png"))
# stimuli Representation.png
image_1_2 <- image_read(paste0("../illustrations/stimuli ", levels(d3$condition)[1], ".png"))
image_2_2 <- image_read(paste0("../illustrations/stimuli ", levels(d3$condition)[2], ".png"))
image_3_3 <- image_read(paste0("../illustrations/stimuli ", levels(d3$condition)[3], ".png"))
image_4_4 <- image_read(paste0("../illustrations/stimuli ", levels(d3$condition)[4], ".png"))
# create grobs
image_1_grob <- rasterGrob(image_1, interpolate = TRUE)
image_2_grob <- rasterGrob(image_2, interpolate = TRUE)
image_3_grob <- rasterGrob(image_3, interpolate = TRUE)
image_4_grob <- rasterGrob(image_4, interpolate = TRUE)
image_1_2_grob <- rasterGrob(image_1_2, interpolate = TRUE)
image_2_2_grob <- rasterGrob(image_2_2, interpolate = TRUE)
image_3_2_grob <- rasterGrob(image_3_3, interpolate = TRUE)
image_4_2_grob <- rasterGrob(image_4_4, interpolate = TRUE)

# plot 1
plot1 <- ggplot() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey70", alpha = 0.75) +
  
  geom_point(
    data = d3 %>% filter(condition == levels(condition)[1]),
    aes(x = ageinyears, y = mean, colour = colour),
    alpha = 0.5, shape = 1, size = 2
  ) +
  
  geom_smooth(
    data = f3 %>% filter(condition == levels(condition)[1]),
    aes(x = age, y = Estimate, ymin = Q2.5, ymax = Q97.5, fill = colour, colour = colour),
    stat = "identity", alpha = 0.2, linewidth = 0.8
  ) +
  
  # shape 21 uses both fill (inside) and colour (stroke)
  geom_point(
    data = p3 %>% filter(condition == levels(condition)[1]),
    aes(x = days/365.25, y = 0.5, fill = colour, colour = colour),
    size = 4, shape = 21, stroke = 1
  ) +
  
  # constant black overlay: set outside aes()
  geom_point(
    data = p3 %>% filter(condition == levels(condition)[1]),
    aes(x = days/365.25, y = 0.5),
    fill = "black", colour = "black", size = 0.5, shape = 21, stroke = 1
  ) +
  
  # text: move color/fontface outside aes()
  geom_text(
    data = p3 %>% filter(condition == levels(condition)[1]),
    aes(label = months, x = days/365.25, y = 0.13),
    color = "black", fontface = "bold",
    angle = 90, size = 3, vjust = 0.5
  ) +
  
  geom_text(
    data = p3 %>% filter(condition == levels(condition)[1]),
    aes(label = "months", x = days/365.25, y = .31),
    color = "black", fontface = "bold",
    angle = 90, size = 3, vjust = 0.5
  ) +
  
  scale_colour_identity() +
  scale_fill_identity() +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(x = "Age", y = "Proportion Correct") +
  coord_cartesian(ylim = c(0, 1), xlim = c(2.7, 7.3)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.border = element_rect(colour = "grey30", fill = NA, size = 1),
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.title = element_text(face = "bold", size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )


# plot 2
plot2 <- ggplot() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey70", alpha = 0.75) +
  
  geom_point(
    data = d3 %>% filter(condition == levels(condition)[2]),
    aes(x = ageinyears, y = mean, colour = colour),
    alpha = 0.5, shape = 1, size = 2
  ) +
  
  geom_smooth(
    data = f3 %>% filter(condition == levels(condition)[2]),
    aes(x = age, y = Estimate, ymin = Q2.5, ymax = Q97.5, fill = colour, colour = colour),
    stat = "identity", alpha = 0.2, linewidth = 0.8
  ) +
  
  # shape 21 uses both fill (inside) and colour (stroke)
  geom_point(
    data = p3 %>% filter(condition == levels(condition)[2]),
    aes(x = days/365.25, y = 0.5, fill = colour, colour = colour),
    size = 4, shape = 21, stroke = 1
  ) +
  
  # constant black overlay: set outside aes()
  geom_point(
    data = p3 %>% filter(condition == levels(condition)[2]),
    aes(x = days/365.25, y = 0.5),
    fill = "black", colour = "black", size = 0.5, shape = 21, stroke = 1
  ) +
  
  # text: move color/fontface outside aes()
  geom_text(
    data = p3 %>% filter(condition == levels(condition)[2]),
    aes(label = months, x = days/365.25, y = 0.13),
    color = "black", fontface = "bold",
    angle = 90, size = 3, vjust = 0.5
  ) +
  
  geom_text(
    data = p3 %>% filter(condition == levels(condition)[2]),
    aes(label = "months", x = days/365.25, y = .31),
    color = "black", fontface = "bold",
    angle = 90, size = 3, vjust = 0.5
  ) +
  
  scale_colour_identity() +
  scale_fill_identity() +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(x = "Age", y = "Proportion Correct") +
  coord_cartesian(ylim = c(0, 1), xlim = c(2.7, 7.3)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.border = element_rect(colour = "grey30", fill = NA, size = 1),
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.title = element_text(face = "bold", size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

# plot 3
plot3 <- ggplot() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey70", alpha = 0.75) +
  
  geom_point(
    data = d3 %>% filter(condition == levels(condition)[3]),
    aes(x = ageinyears, y = mean, colour = colour),
    alpha = 0.5, shape = 1, size = 2
  ) +
  
  geom_smooth(
    data = f3 %>% filter(condition == levels(condition)[3]),
    aes(x = age, y = Estimate, ymin = Q2.5, ymax = Q97.5, fill = colour, colour = colour),
    stat = "identity", alpha = 0.2, linewidth = 0.8
  ) +
  
  # shape 21 uses both fill (inside) and colour (stroke)
  geom_point(
    data = p3 %>% filter(condition == levels(condition)[3]),
    aes(x = days/365.25, y = 0.5, fill = colour, colour = colour),
    size = 4, shape = 21, stroke = 1
  ) +
  
  # constant black overlay: set outside aes()
  geom_point(
    data = p3 %>% filter(condition == levels(condition)[3]),
    aes(x = days/365.25, y = 0.5),
    fill = "black", colour = "black", size = 0.5, shape = 21, stroke = 1
  ) +
  
  # text: move color/fontface outside aes()
  geom_text(
    data = p3 %>% filter(condition == levels(condition)[3]),
    aes(label = months, x = days/365.25, y = 0.13),
    color = "black", fontface = "bold",
    angle = 90, size = 3, vjust = 0.5
  ) +
  
  geom_text(
    data = p3 %>% filter(condition == levels(condition)[3]),
    aes(label = "months", x = days/365.25, y = .31),
    color = "black", fontface = "bold",
    angle = 90, size = 3, vjust = 0.5
  ) +
  
  scale_colour_identity() +
  scale_fill_identity() +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(x = "Age", y = "Proportion Correct") +
  coord_cartesian(ylim = c(0, 1), xlim = c(2.7, 7.3)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.border = element_rect(colour = "grey30", fill = NA, size = 1),
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.title = element_text(face = "bold", size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )



# plot 4
plot4 <- ggplot() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey70", alpha = 0.75) +
  
  geom_point(
    data = d3 %>% filter(condition == levels(condition)[4]),
    aes(x = ageinyears, y = mean, colour = colour),
    alpha = 0.5, shape = 1, size = 2
  ) +
  
  geom_smooth(
    data = f3 %>% filter(condition == levels(condition)[4]),
    aes(x = age, y = Estimate, ymin = Q2.5, ymax = Q97.5, fill = colour, colour = colour),
    stat = "identity", alpha = 0.2, linewidth = 0.8
  ) +
  
  # shape 21 uses both fill (inside) and colour (stroke)
  geom_point(
    data = p3 %>% filter(condition == levels(condition)[4]),
    aes(x = days/365.25, y = 0.5, fill = colour, colour = colour),
    size = 4, shape = 21, stroke = 1
  ) +
  
  # constant black overlay: set outside aes()
  geom_point(
    data = p3 %>% filter(condition == levels(condition)[4]),
    aes(x = days/365.25, y = 0.5),
    fill = "black", colour = "black", size = 0.5, shape = 21, stroke = 1
  ) +
  
  # text: move color/fontface outside aes()
  geom_text(
    data = p3 %>% filter(condition == levels(condition)[4]),
    aes(label = months, x = days/365.25, y = 0.13),
    color = "black", fontface = "bold",
    angle = 90, size = 3, vjust = 0.5
  ) +
  
  geom_text(
    data = p3 %>% filter(condition == levels(condition)[4]),
    aes(label = "months", x = days/365.25, y = .31),
    color = "black", fontface = "bold",
    angle = 90, size = 3, vjust = 0.5
  ) +
  
  scale_colour_identity() +
  scale_fill_identity() +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(x = "Age", y = "Proportion Correct") +
  coord_cartesian(ylim = c(0, 1), xlim = c(2.7, 7.3)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.border = element_rect(colour = "grey30", fill = NA, size = 1),
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.title = element_text(face = "bold", size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )



# Combine plots #######################


S3_complete <- ggdraw() +
  draw_plot_label(label = paste(levels(d3$condition)[1]), size = 14, x = 0.027, y = 0.98) +
  draw_plot_label(label = paste(levels(d3$condition)[2]), size = 14, x = 0.265, y = 0.98) +
  draw_plot_label(label = paste(levels(d3$condition)[3]), size = 14, x = 0.487, y = 0.98) +
  draw_plot_label(label = paste(levels(d3$condition)[4]), size = 14, x = 0.73, y = 0.98) +
  draw_plot(image_1_grob, y = 0.77, x = 0.05, width = .2, height = .2) +
  draw_plot(image_2_grob, y = 0.77, x = 0.292, width = .2, height = .2) +
  draw_plot(image_3_grob, y = 0.77, x = 0.541, width = .2, height = .2) +
  draw_plot(image_4_grob, y = 0.77, x = 0.79, width = .2, height = .2) +
  draw_plot(image_1_2_grob, y = 0.47, x = -0.01, width = .32, height = .32) +
  draw_plot(image_2_2_grob, y = 0.47, x =  0.23, width = .32, height = .32) +
  draw_plot(image_3_2_grob, y = 0.47, x =  0.48, width = .32, height = .32) +
  draw_plot(image_4_2_grob, y = 0.47, x =  0.728, width = .32, height = .32) +
  draw_plot(plot1, y = 0, x =  0.01, width = .25, height = .44) +
  draw_plot(plot2, y = 0, x =  0.25, width = .25, height = .44) +
  draw_plot(plot3, y = 0, x =  0.5, width = .25, height = .44) +
  draw_plot(plot4, y = 0, x =  0.75, width = .25, height = .44) +
  draw_plot_label(label = "A", size = 14, x = 0, y = 0.89) +
  draw_plot_label(label = "B", size = 14, x = 0, y = 0.65) +
  draw_plot_label(label = "C", size = 14, x = 0, y = 0.26)


ggsave(
  filename = "./S3_complete.pdf",
  plot = S3_complete,
  width = 27,
  height = 16.9,
  units = "cm",
  dpi = 600   # or 600 for very high resolution
)

# shell.exec("S3_complete.pdf")

ggsave(
  filename = "./S3_complete.jpg",
  plot = S3_complete,
  width = 27,
  height = 16.9,
  units = "cm",
  dpi = 600   # or 600 for very high resolution
)
              