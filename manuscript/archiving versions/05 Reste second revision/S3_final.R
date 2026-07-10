
# plotexample Representation.png
image_1 <- image_read(paste0("../illustrations/plotexample ", levels(d3$condition)[1], ".png")) 
image_2 <- image_read(paste0("../illustrations/plotexample ", levels(d3$condition)[2], ".png"))
image_3 <- image_read(paste0("../illustrations/plotexample ", levels(d3$condition)[3], ".png")) 
image_4 <- image_read(paste0("../illustrations/plotexample ", levels(d3$condition)[4], ".png")) 

image_1_grob <- rasterGrob(image_1, interpolate = TRUE)
image_2_grob <- rasterGrob(image_2, interpolate = TRUE)
image_3_grob <- rasterGrob(image_3, interpolate = TRUE)
image_4_grob <- rasterGrob(image_4, interpolate = TRUE)

# plot1 -----------------------------------------

# 1. Label definieren
label <- paste(levels(d3$condition)[1])

# 2. Plot erstellen
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
  
  geom_point(
    data = p3 %>% filter(condition == levels(condition)[1]),
    aes(x = days/365.25, y = 0.5, fill = colour, colour = colour),
    size = 4, shape = 21, stroke = 1
  ) +
  
  geom_point(
    data = p3 %>% filter(condition == levels(condition)[1]),
    aes(x = days/365.25, y = 0.5),
    fill = "black", colour = "black", size = 0.5, shape = 21, stroke = 1
  ) +
  
  geom_text(
    data = p3 %>% filter(condition == levels(condition)[1]),
    aes(label = months, x = days/365.25, y = 0.13),
    color = "black", fontface = "bold",
    angle = 90, size = 4, vjust = 0.5
  ) +
  
  geom_text(
    data = p3 %>% filter(condition == levels(condition)[1]),
    aes(label = "months", x = days/365.25, y = .31),
    color = "black", fontface = "bold",
    angle = 90, size = 4, vjust = 0.5
  ) +
  
  scale_colour_identity() +
  scale_fill_identity() +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  
  labs(title = label, x = "Age", y = "Proportion Correct") + 
  
  coord_cartesian(ylim = c(0, 1), xlim = c(2.7, 7.3), clip = "off") +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.ticks.length = unit(0.0, "pt"),
    panel.border = element_rect(colour = "grey30", fill = NA, linewidth = 1), # Hinweis: size ist in neueren ggplot-Versionen 'linewidth'
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.title = element_text(face = "bold", size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    # axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14) 
    
  ) +
  annotation_custom(
    grob = image_1_grob, 
    # Positionierung im Koordinatensystem des Hauptplots (x, y)
    # Positionierung im Koordinatensystem des Hauptplots (x, y)
    xmin = 3, xmax = 7,  
    ymin = 1.45)

# plot2 ---------------

# 1. Label definieren
label <- paste(levels(d3$condition)[2])
# 2. Plot erstellen
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
  
  geom_point(
    data = p3 %>% filter(condition == levels(condition)[2]),
    aes(x = days/365.25, y = 0.5, fill = colour, colour = colour),
    size = 4, shape = 21, stroke = 1
  ) +
  
  geom_point(
    data = p3 %>% filter(condition == levels(condition)[2]),
    aes(x = days/365.25, y = 0.5),
    fill = "black", colour = "black", size = 0.5, shape = 21, stroke = 1
  ) +
  
  geom_text(
    data = p3 %>% filter(condition == levels(condition)[2]),
    aes(label = months, x = days/365.25, y = 0.13),
    color = "black", fontface = "bold",
    angle = 90, size = 4, vjust = 0.5
  ) +
  
  geom_text(
    data = p3 %>% filter(condition == levels(condition)[2]),
    aes(label = "months", x = days/365.25, y = .31),
    color = "black", fontface = "bold",
    angle = 90, size = 4, vjust = 0.5
  ) +
  
  scale_colour_identity() +
  scale_fill_identity() +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  
  labs(title = label, x = "Age", y = "Proportion Correct") + 
  
  coord_cartesian(ylim = c(0, 1), xlim = c(2.7, 7.3), clip = "off") +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.margin = margin(0, 0, 0, 10, "pt"),
    axis.ticks.length = unit(0.0, "pt"),
    panel.border = element_rect(colour = "grey30", fill = NA, linewidth = 1), # Hinweis: size ist in neueren ggplot-Versionen 'linewidth'
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.title = element_text(face = "bold", size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    # axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14) 
    
  ) +
  annotation_custom(
    grob = image_2_grob, 
    # Positionierung im Koordinatensystem des Hauptplots (x, y)
    xmin = 3, xmax = 7,  
    ymin = 1.45)

# plot3 ----------------------

# 1. Label definieren
label <- paste(levels(d3$condition)[3])
# 2. Plot erstellen
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
  
  geom_point(
    data = p3 %>% filter(condition == levels(condition)[3]),
    aes(x = days/365.25, y = 0.5, fill = colour, colour = colour),
    size = 4, shape = 21, stroke = 1
  ) +
  
  geom_point(
    data = p3 %>% filter(condition == levels(condition)[3]),
    aes(x = days/365.25, y = 0.5),
    fill = "black", colour = "black", size = 0.5, shape = 21, stroke = 1
  ) +
  
  geom_text(
    data = p3 %>% filter(condition == levels(condition)[3]),
    aes(label = months, x = days/365.25, y = 0.13),
    color = "black", fontface = "bold",
    angle = 90, size = 4, vjust = 0.5
  ) +
  
  geom_text(
    data = p3 %>% filter(condition == levels(condition)[3]),
    aes(label = "months", x = days/365.25, y = .31),
    color = "black", fontface = "bold",
    angle = 90, size = 4, vjust = 0.5
  ) +
  
  scale_colour_identity() +
  scale_fill_identity() +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  
  labs(title = label, x = "Age", y = "Proportion Correct") + 
  
  coord_cartesian(ylim = c(0, 1), xlim = c(2.7, 7.3), clip = "off") +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.margin = margin(0, 0, 0, 10, "pt"),
    axis.ticks.length = unit(0.0, "pt"),
    panel.border = element_rect(colour = "grey30", fill = NA, linewidth = 1), # Hinweis: size ist in neueren ggplot-Versionen 'linewidth'
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.title = element_text(face = "bold", size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    # axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    # HIER OPTIONAL: Styling für den Titel (z. B. fett und zentriert
    plot.title = element_text(
      face = "bold", 
      hjust = 0.5, 
      size = 14,
      # t = top, r = right, b = bottom, l = left
      margin = margin(t = 0, r = 0, b = 60, l = 0, unit = "pt") 
    )
  ) +
  annotation_custom(
    grob = image_3_grob, 
    # Positionierung im Koordinatensystem des Hauptplots (x, y)
    xmin = 3, xmax = 7,  
    ymin = 1.45)

# plot4 --------------------

# 1. Label definieren
label <- paste(levels(d3$condition)[4])

# 2. Plot erstellen
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
  
  geom_point(
    data = p3 %>% filter(condition == levels(condition)[4]),
    aes(x = days/365.25, y = 0.5, fill = colour, colour = colour),
    size = 4, shape = 21, stroke = 1
  ) +
  
  geom_point(
    data = p3 %>% filter(condition == levels(condition)[4]),
    aes(x = days/365.25, y = 0.5),
    fill = "black", colour = "black", size = 0.5, shape = 21, stroke = 1
  ) +
  
  geom_text(
    data = p3 %>% filter(condition == levels(condition)[4]),
    aes(label = months, x = days/365.25, y = 0.13),
    color = "black", fontface = "bold",
    angle = 90, size = 4, vjust = 0.5
  ) +
  
  geom_text(
    data = p3 %>% filter(condition == levels(condition)[4]),
    aes(label = "months", x = days/365.25, y = .31),
    color = "black", fontface = "bold",
    angle = 90, size = 4, vjust = 0.5
  ) +
  
  scale_colour_identity() +
  scale_fill_identity() +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  
  labs(title = label, x = "Age", y = "Proportion Correct") + 
  
  coord_cartesian(ylim = c(0, 1), xlim = c(2.7, 7.3), clip = "off") +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.margin = margin(0, 0, 0, 10, "pt"),
    axis.ticks.length = unit(0.0, "pt"),
    panel.border = element_rect(colour = "grey30", fill = NA, linewidth = 1), # Hinweis: size ist in neueren ggplot-Versionen 'linewidth'
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.title = element_text(face = "bold", size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    # axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14) 
    
  ) +
  annotation_custom(
    grob = image_4_grob, 
    # Positionierung im Koordinatensystem des Hauptplots (x, y)
    xmin = 3, xmax = 7,  
    ymin = 1.45)

# combine -----------------
library(patchwork)
S3_complete <- plot1 | plot2 | plot3 | plot4 + 
  plot_layout(guides = "collect", axes = "collect")

S3_complete[[2]] <- S3_complete[[2]] + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())
S3_complete[[3]] <- S3_complete[[3]] + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())
S3_complete[[4]] <- S3_complete[[4]] + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())


S3_complete <- S3_complete +
  plot_annotation(
    caption = "Age in Years", # This acts as your global X label
    theme = theme(
      plot.caption = element_text(size = rel(0.9), face = "bold", hjust = 0.52, margin = margin(t = 0))))

S3_complete + canvas(width=27, height= 10, units="cm", dpi = 600)


ggsave(
  filename = "../illustrations/S3_complete.pdf",
  plot = S3_complete,
  width = 27,
  height = 10,
  units = "cm",
  dpi = 600)

ggsave(
  filename = "./../illustrations/S3_complete.jpg",
  plot = S3_complete,
  width = 27,
  height = 10,
  units = "cm",
  dpi = 600)

ggsave(
  filename = "./../illustrations/S3_complete.png",
  plot = S3_complete,
  width = 27,
  height = 10,
  units = "cm",
  dpi = 600)

ggsave(
  filename = "./../illustrations/S3_complete.svg",
  plot = S3_complete,
  width = 27,
  height = 10,
  units = "cm",
  dpi = 600)



