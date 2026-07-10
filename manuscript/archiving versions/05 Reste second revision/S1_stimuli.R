
image_1 <- image_read(paste0("../illustrations/plotexample ", levels(d1$condition)[1], ".png")) %>%
  image_ggplot() +
  labs(
    title = paste(levels(d1$condition)[1]),
    tag = "A"                                    # 1. Define your label as a tag
  ) +
  theme(
    # 2. Add extra left padding (45pt) so the tag doesn't touch the image
    plot.margin = margin(10, 10, 10, 10, "pt"),

    plot.title = element_text(face = "bold", hjust = 0.5, size = 16,
                              margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),

    # 3. Position the tag relative to the entire plot window
    plot.tag.position = c(-0.08, 0.35),            # c(X, Y) -> 2% from left, 50% up (perfectly centered)
    plot.tag = element_text(face = "bold", size = 14, color = "black"))
 

image_2 <- image_read(paste0("../illustrations/plotexample ", levels(d1$condition)[2], ".png")) %>%
  image_ggplot() + 
  labs(title = paste(levels(d1$condition)[2])) +  
  theme(
    plot.margin = margin(10, 10, 10, 10, "pt"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16,
                              margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")))
  
image_3 <- image_read(paste0("../illustrations/plotexample ", levels(d1$condition)[3], ".png")) %>%
  image_ggplot() + 
  labs(title = paste(levels(d1$condition)[3])) +  
  theme(
    plot.margin = margin(10, 10, 10, 10, "pt"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16,
                              margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")))

image_4 <- image_read(paste0("../illustrations/plotexample ", levels(d1$condition)[4], ".png")) %>%
  image_ggplot() + 
  labs(title = paste(levels(d1$condition)[4])) +  
  theme(
    plot.margin = margin(10, 10, 10, 10, "pt"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16,
                              margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")))
 

# stimuli Representation.png
image_1_1 <- image_read(paste0("../illustrations/stimuli ", levels(d1$condition)[1], ".png")) %>%
  image_ggplot() +
  labs(tag = "B") +
  theme(
    # 2. Add extra left padding (45pt) so the tag doesn't touch the image
    plot.margin = margin(0, 0, 0, 0, "pt"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16,
                              margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
    
    # 3. Position the tag relative to the entire plot window
    plot.tag.position = c(-0.08, 0.5),            # c(X, Y) -> 2% from left, 50% up (perfectly centered)
    plot.tag = element_text(face = "bold", size = 14, color = "black"))


image_2_2 <- image_read(paste0("../illustrations/stimuli ", levels(d1$condition)[2], ".png")) %>%
  image_ggplot()
image_3_3 <- image_read(paste0("../illustrations/stimuli ", levels(d1$condition)[3], ".png")) %>%
  image_ggplot()
image_4_4 <- image_read(paste0("../illustrations/stimuli ", levels(d1$condition)[4], ".png")) %>%
  image_ggplot()

S1_Stimuli <- 
  image_1 / image_1_1 | 
  image_2 / image_2_2 | 
  image_3 / image_3_3 | 
  image_4 / image_4_4 
  
  
S1_Stimuli + canvas(width=27, height= 9.4, units="cm", dpi = 600)


# saving images ###############################

ggsave(
  filename = "../illustrations/S1_Stimuli.pdf",
  plot = S1_Stimuli,
  width = 27,
  height = 9.4,
  units = "cm",
  dpi = 600)

ggsave(
  filename = "./../illustrations/S1_Stimuli.jpg",
  plot = S1_Stimuli,
  width = 27,
  height = 9.4,
  units = "cm",
  dpi = 600)

ggsave(
  filename = "./../illustrations/S1_Stimuli.png",
  plot = S1_Stimuli,
  width = 27,
  height = 9.4,
  units = "cm",
  dpi = 600)

ggsave(
  filename = "./../illustrations/S1_Stimuli.svg",
  plot = S1_Stimuli,
  width = 27,
  height = 9.4,
  units = "cm",
  dpi = 600)

