
library(cowplot)

plot1 <- ggplot() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey70", alpha = 0.75) +
  
  geom_point(data = d1 %>% filter(condition == condition[1]), 
             aes(x = ageinyears, y = mean, colour = colour), alpha = 0.5, shape = 1, size = 2) +
  
  geom_smooth(data = f1 %>% filter(condition == condition[1]), 
              aes(x = age, y = Estimate, ymin = Q2.5, ymax = Q97.5, fill = colour, colour = colour),
              stat = "identity", alpha = 0.2, linewidth = 0.8) +
  
  geom_point(data = p1 %>% filter(condition == condition[1]), 
             aes(x = days/365.25, y = 0.5, fill = colour, colour = colour), size = 4, shape = 21, stroke = 1) +
  geom_point(data = p1 %>% filter(condition == condition[1]), 
             aes(x = days/365.25, y = 0.5, fill = "black", colour = "black"), size = 0.5, shape = 21, stroke = 1) +
  geom_text(data = p1 %>% filter(condition == condition[1]), 
            aes(label = months, x = days/365.25, y = 0.13, colour = "black", fontface = "bold"),
            angle = 90, size = 3, vjust = 0.5) +
  
  geom_text(data = p1 %>% filter(condition == condition[1]), 
            aes(label = "months", x = days/365.25, y = .31, colour = "black", fontface = "bold"), 
            angle = 90, size = 3, vjust = 0.5) +
  
  scale_colour_identity() +
  scale_fill_identity() +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) + # Multiply by 100 & add %  
  # facet_grid(cols = vars(condition)) +
  labs(x = "Age", y = "Proportion Correct") +
  
  coord_cartesian(ylim = c(0, 1), xlim = c(2.7, 7.3)) +   # Allow more Y space for image
  
  theme_minimal(base_size = 13) +
  theme(
    panel.border = element_rect(colour = "grey30", fill = NA, size = 1),
    strip.text = element_blank(),
    strip.background = element_blank(),
    # strip.text = element_text(face = "bold", size = rel(0.8),
    #                           margin = margin(t = 5, b = 145)), # HIER STRIPE SPACING via b =
    # # strip.switch.pad.grid = unit(0, "cm"),
    axis.title = element_text(face = "bold", size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

plot2 <- plot1 + plottheme(axis.title.y = element_blank()),
plot2

plot1 + theme(axis.text.y = element_blank())

plot_row <- plot_grid(plot1, 
                      plot1 + theme(axis.text.y = element_blank()),
                      plot1 + theme(axis.text.y = element_blank()),
                      plot1 + theme(axis.text.y = element_blank()),
                      nrow = 1)




# adding images -----
# load images
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

top_row <- plot_grid(image_1_grob, image_2_grob, image_3_grob, image_4_grob, nrow = 1)

bottom_row <- plot_grid(image_1_2_grob, image_2_2_grob, image_3_2_grob, image_4_2_grob, nrow = 1)
# 
# plot_row <- plot_grid(plot1, 
#                       plot1,
#                       plot1,
#                       plot1,
#                       nrow = 1)
# 

plot_row <- plot_grid(plot1 + theme(axis.text.y = element_blank()), 
                      plot1 + theme(axis.text.y = element_blank()),
                      plot1 + theme(axis.text.y = element_blank()),
                      plot1 + theme(axis.text.y = element_blank()),
                      nrow = 1)

S1_complete <- plot_grid(
  top_row,
  nullGrob(),    # <-- adds vertical space
  bottom_row,
  nrow = 3,
  align ="v",
  axis = "r",
  rel_heights = c(0.26, 0.035, 1))
 
S1_complete


# ggsave(
#   filename = "../illustrations/S1_complete.pdf",
#   plot = S1_complete,
#   width = 16,
#   height = 12,
#   units = "cm"
# )

main_plot

ggsave(
  filename = "../illustrations/S1_complete.png",
  plot = S1_complete,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600   # or 600 for very high resolution
)

ggsave(
  filename = "./S1_complete.jpg",
  plot = S1_complete,
  width = 27,
  height = 16.9,
  units = "cm",
  dpi = 600   # or 600 for very high resolution
)
















# # JUST THE FUCKING STIMULI
# 
# # adding images -----
# # load images
# # plotexample Representation.png
# image_1 <- image_read(paste0("../illustrations/plotexample ", levels(d1$condition)[1], ".png"))
# image_2 <- image_read(paste0("../illustrations/plotexample ", levels(d1$condition)[2], ".png"))
# image_3 <- image_read(paste0("../illustrations/plotexample ", levels(d1$condition)[3], ".png"))
# image_4 <- image_read(paste0("../illustrations/plotexample ", levels(d1$condition)[4], ".png"))
# # stimuli Representation.png
# image_1_2 <- image_read(paste0("../illustrations/stimuli ", levels(d1$condition)[1], ".png"))
# image_2_2 <- image_read(paste0("../illustrations/stimuli ", levels(d1$condition)[2], ".png"))
# image_3_3 <- image_read(paste0("../illustrations/stimuli ", levels(d1$condition)[3], ".png"))
# image_4_4 <- image_read(paste0("../illustrations/stimuli ", levels(d1$condition)[4], ".png"))
# # create grobs
# image_1_grob <- rasterGrob(image_1, interpolate = TRUE)
# image_2_grob <- rasterGrob(image_2, interpolate = TRUE)
# image_3_grob <- rasterGrob(image_3, interpolate = TRUE)
# image_4_grob <- rasterGrob(image_4, interpolate = TRUE)
# image_1_2_grob <- rasterGrob(image_1_2, interpolate = TRUE)
# image_2_2_grob <- rasterGrob(image_2_2, interpolate = TRUE)
# image_3_2_grob <- rasterGrob(image_3_3, interpolate = TRUE)
# image_4_2_grob <- rasterGrob(image_4_4, interpolate = TRUE)
# 
# 
# top_row <- plot_grid(image_1_grob, image_2_grob, image_3_grob, image_4_grob, nrow = 1)
# 
# bottom_row <- plot_grid(image_1_2_grob, image_2_2_grob, image_3_2_grob, image_4_2_grob, nrow = 1)
# 
# 
# 
# s1_stimuli <- plot_grid(top_row,
#                         bottom_row,
#                         rel_heights = c(0.3, 1),
#                         nrow = 2)
# 
# plot_grid(
#   top_row,
#   nullGrob(),    # <-- adds vertical space
#   bottom_row,
#   nrow = 3,
#   align ="v",
#   axis = "r",
#   rel_heights = c(0.3, 0.0001, 1))
# 
# 
