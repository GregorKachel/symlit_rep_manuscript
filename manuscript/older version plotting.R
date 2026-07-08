# load images ###################################


# plotexample Representation.png
image_1 <- image_read(paste0("../illustrations/plotexample ", levels(d1$condition)[1], ".png"))
image_2 <- image_read(paste0("../illustrations/plotexample ", levels(d1$condition)[2], ".png"))
image_3 <- image_read(paste0("../illustrations/plotexample ", levels(d1$condition)[3], ".png"))
image_4 <- image_read(paste0("../illustrations/plotexample ", levels(d1$condition)[4], ".png"))
# stimuli Representation.png
image_1_2 <- image_read(paste0("../illustrations/stimuli ", levels(d1$condition)[1], ".png"))
image_2_2 <- image_read(paste0("../illustrations/stimuli ", levels(d1$condition)[2], ".png"))
image_3_3 <- image_read(paste0("../illustrations/stimuli ", levels(d1$condition)[3], ".png"))
image_4_4 <- image_read(paste0("../illustrations/stimuli ", levels(d1$condition)[4], ".png"))
# create grobs
image_1_grob <- rasterGrob(image_1, interpolate = TRUE)
image_2_grob <- rasterGrob(image_2, interpolate = TRUE)
image_3_grob <- rasterGrob(image_3, interpolate = TRUE)
image_4_grob <- rasterGrob(image_4, interpolate = TRUE)
image_1_2_grob <- rasterGrob(image_1_2, interpolate = TRUE)
image_2_2_grob <- rasterGrob(image_2_2, interpolate = TRUE)
image_3_2_grob <- rasterGrob(image_3_3, interpolate = TRUE)
image_4_2_grob <- rasterGrob(image_4_4, interpolate = TRUE)

# plot 1 #####################
plot1 <- ggplot() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey70", alpha = 0.75) +
  
  geom_point(
    data = d1 %>% filter(condition == levels(condition)[1]),
    aes(x = ageinyears, y = mean, colour = colour),
    alpha = 0.5, shape = 1, size = 2
  ) +
  
  geom_smooth(
    data = f1 %>% filter(condition == levels(condition)[1]),
    aes(x = age, y = Estimate, ymin = Q2.5, ymax = Q97.5, fill = colour, colour = colour),
    stat = "identity", alpha = 0.2, linewidth = 0.8
  ) +
  
  # shape 21 uses both fill (inside) and colour (stroke)
  geom_point(
    data = p1 %>% filter(condition == levels(condition)[1]),
    aes(x = days/365.25, y = 0.5, fill = colour, colour = colour),
    size = 4, shape = 21, stroke = 1
  ) +
  
  # constant black overlay: set outside aes()
  geom_point(
    data = p1 %>% filter(condition == levels(condition)[1]),
    aes(x = days/365.25, y = 0.5),
    fill = "black", colour = "black", size = 0.5, shape = 21, stroke = 1
  ) +
  
  # text: move color/fontface outside aes()
  geom_text(
    data = p1 %>% filter(condition == levels(condition)[1]),
    aes(label = months, x = days/365.25, y = 0.13),
    color = "black", fontface = "bold",
    angle = 90, size = 3, vjust = 0.5
  ) +
  
  geom_text(
    data = p1 %>% filter(condition == levels(condition)[1]),
    aes(label = "months", x = days/365.25, y = .31),
    color = "black", fontface = "bold",
    angle = 90, size = 3, vjust = 0.5
  ) +
  
  scale_colour_identity() +
  scale_fill_identity() +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +labs(x = "Age", y = "Proportion Correct") +
  coord_cartesian(ylim = c(0, 1), xlim = c(2.7, 7.3)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.ticks.length = unit(0.0, "pt"),
    panel.border = element_rect(colour = "grey30", fill = NA, size = 1),
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.title = element_text(face = "bold", size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank())



plot1

# plot 2 ##################
plot2 <- ggplot() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey70", alpha = 0.75) +
  
  geom_point(
    data = d1 %>% filter(condition == levels(condition)[2]),
    aes(x = ageinyears, y = mean, colour = colour),
    alpha = 0.5, shape = 1, size = 2
  ) +
  
  geom_smooth(
    data = f1 %>% filter(condition == levels(condition)[2]),
    aes(x = age, y = Estimate, ymin = Q2.5, ymax = Q97.5, fill = colour, colour = colour),
    stat = "identity", alpha = 0.2, linewidth = 0.8
  ) +
  
  # shape 21 uses both fill (inside) and colour (stroke)
  geom_point(
    data = p1 %>% filter(condition == levels(condition)[2]),
    aes(x = days/365.25, y = 0.5, fill = colour, colour = colour),
    size = 4, shape = 21, stroke = 1
  ) +
  
  # constant black overlay: set outside aes()
  geom_point(
    data = p1 %>% filter(condition == levels(condition)[2]),
    aes(x = days/365.25, y = 0.5),
    fill = "black", colour = "black", size = 0.5, shape = 21, stroke = 1
  ) +
  
  # text: move color/fontface outside aes()
  geom_text(
    data = p1 %>% filter(condition == levels(condition)[2]),
    aes(label = months, x = days/365.25, y = 0.13),
    color = "black", fontface = "bold",
    angle = 90, size = 3, vjust = 0.5
  ) +
  
  geom_text(
    data = p1 %>% filter(condition == levels(condition)[2]),
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
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.ticks.length = unit(0.0, "pt"),
    panel.border = element_rect(colour = "grey30", fill = NA, size = 1),
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.title = element_text(face = "bold", size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank())

# plot 3 #########################
plot3 <- ggplot() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey70", alpha = 0.75) +
  
  geom_point(
    data = d1 %>% filter(condition == levels(condition)[3]),
    aes(x = ageinyears, y = mean, colour = colour),
    alpha = 0.5, shape = 1, size = 2
  ) +
  
  geom_smooth(
    data = f1 %>% filter(condition == levels(condition)[3]),
    aes(x = age, y = Estimate, ymin = Q2.5, ymax = Q97.5, fill = colour, colour = colour),
    stat = "identity", alpha = 0.2, linewidth = 0.8
  ) +
  
  # shape 21 uses both fill (inside) and colour (stroke)
  geom_point(
    data = p1 %>% filter(condition == levels(condition)[3]),
    aes(x = days/365.25, y = 0.5, fill = colour, colour = colour),
    size = 4, shape = 21, stroke = 1
  ) +
  
  # constant black overlay: set outside aes()
  geom_point(
    data = p1 %>% filter(condition == levels(condition)[3]),
    aes(x = days/365.25, y = 0.5),
    fill = "black", colour = "black", size = 0.5, shape = 21, stroke = 1
  ) +
  
  # text: move color/fontface outside aes()
  geom_text(
    data = p1 %>% filter(condition == levels(condition)[3]),
    aes(label = months, x = days/365.25, y = 0.13),
    color = "black", fontface = "bold",
    angle = 90, size = 3, vjust = 0.5
  ) +
  
  geom_text(
    data = p1 %>% filter(condition == levels(condition)[3]),
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
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.ticks.length = unit(0.0, "pt"),
    panel.border = element_rect(colour = "grey30", fill = NA, size = 1),
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.title = element_text(face = "bold", size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank())



# plot 4 #########################
plot4 <- ggplot() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey70", alpha = 0.75) +
  
  geom_point(
    data = d1 %>% filter(condition == levels(condition)[4]),
    aes(x = ageinyears, y = mean, colour = colour),
    alpha = 0.5, shape = 1, size = 2
  ) +
  
  geom_smooth(
    data = f1 %>% filter(condition == levels(condition)[4]),
    aes(x = age, y = Estimate, ymin = Q2.5, ymax = Q97.5, fill = colour, colour = colour),
    stat = "identity", alpha = 0.2, linewidth = 0.8
  ) +
  
  # shape 21 uses both fill (inside) and colour (stroke)
  geom_point(
    data = p1 %>% filter(condition == levels(condition)[4]),
    aes(x = days/365.25, y = 0.5, fill = colour, colour = colour),
    size = 4, shape = 21, stroke = 1
  ) +
  
  # constant black overlay: set outside aes()
  geom_point(
    data = p1 %>% filter(condition == levels(condition)[4]),
    aes(x = days/365.25, y = 0.5),
    fill = "black", colour = "black", size = 0.5, shape = 21, stroke = 1
  ) +
  
  # text: move color/fontface outside aes()
  geom_text(
    data = p1 %>% filter(condition == levels(condition)[4]),
    aes(label = months, x = days/365.25, y = 0.13),
    color = "black", fontface = "bold",
    angle = 90, size = 3, vjust = 0.5
  ) +
  
  geom_text(
    data = p1 %>% filter(condition == levels(condition)[4]),
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
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.ticks.length = unit(0.0, "pt"),
    panel.border = element_rect(colour = "grey30", fill = NA, size = 1),
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.title = element_text(face = "bold", size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank())


# Combine plots #######################

S1_complete <- ggdraw() +
  draw_plot_label(label = "A", size = 13, x = 0, y = 0.89) +
  draw_plot_label(label = "B", size = 13, x = 0, y = 0.67) +
  draw_plot_label(label = "C", size = 13, x = 0, y = 0.275) +
  draw_plot_label(label = "Target", size = 12, x = 0.02, y = 0.64, angle = 90 ) +
  draw_plot_label(label = "Cue", size = 12, x = 0.02, y = 0.53, angle = 90) +
  draw_plot_label(label = "Proportion Correct", size = 12, x = 0.02, y = 0.02, angle = 90) +
  draw_plot_label(label = "Age in Years", size = 12, x = 0.44, y = 0.038) +
  draw_plot_label(label = paste(levels(d1$condition)[1]), size = 13, x = 0.06, y = .99) +
  draw_plot_label(label = paste(levels(d1$condition)[2]), size = 13, x = 0.305, y = .99) +
  draw_plot_label(label = paste(levels(d1$condition)[3]), size = 13, x = 0.485, y = .99) +
  draw_plot_label(label = paste(levels(d1$condition)[4]), size = 13, x = 0.7, y = .99) + 
  # A - Top row example
  draw_plot(image_1_grob, y = 0.78, x = 0.085, width = .19, height = .2) +
  draw_plot(image_2_grob, y = 0.78, x = 0.32, width = .19, height = .2) +
  draw_plot(image_3_grob, y = 0.78, x = 0.565, width = .19, height = .2) +
  draw_plot(image_4_grob, y = 0.78, x = 0.8, width = .19, height = .2) +
  # B - Mid Row Stimuli
  draw_plot(image_1_2_grob, y = 0.49, x =  0.015, width = .32, height = .32) +
  draw_plot(image_2_2_grob, y = 0.49, x =  0.253, width = .32, height = .32) +
  draw_plot(image_3_2_grob, y = 0.49, x =  0.495, width = .32, height = .32) +
  draw_plot(image_4_2_grob, y = 0.49, x =  0.73, width = .32, height = .32) +
  # C - Bottom Results
  draw_plot(plot1, y = 0.03, x =  0.04, width = .235, height = .42) +
  draw_plot(plot2, y = 0.03, x =  0.28, width = .235, height = .42) +
  draw_plot(plot3, y = 0.03, x =  0.522,  width = .235, height = .42) +
  draw_plot(plot4, y = 0.03, x =  0.76, width = .235, height = .42) 


S1_complete + canvas(width=27, height= 17, units="cm", dpi = 600)




# saving images ###############################

ggsave(
  filename = "../illustrations/S1_complete.pdf",
  plot = S1_complete,
  width = 27,
  height = 16.9,
  units = "cm",
  dpi = 600)

# shell.exec("../illustrations/S1_complete.pdf")

ggsave(
  filename = "./../illustrations/S1_complete.jpg",
  plot = S1_complete,
  width = 27,
  height = 16.9,
  units = "cm",
  dpi = 600)

ggsave(
  filename = "./../illustrations/S1_complete.png",
  plot = S1_complete,
  width = 27,
  height = 16.9,
  units = "cm",
  dpi = 600)

ggsave(
  filename = "./../illustrations/S1_complete.svg",
  plot = S1_complete,
  width = 27,
  height = 16.9,
  units = "cm",
  dpi = 600)
