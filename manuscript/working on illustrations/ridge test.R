library(tidyverse)
library(ggridges)

# 1. Data Prep: Ensure subid is a factor ordered by age
# Note: To have youngest in FRONT, they must be the LAST levels in the factor
# because ggridges draws from the bottom (Level 1) up.
plot_data <- ridge_data_s1 %>%
  filter(agem <= 36 ) %>% 
  mutate(condition_num = as.numeric(as.factor(condition))) %>%
  # Reverse age ordering so youngest children are drawn last (on top)
  mutate(subid = fct_reorder(as.character(subid), agem, .desc = TRUE))

# 2. Control the "Mountain Height"
# If 'mean' is 0-1, try a scale of 5-10. If 'mean' is 0-100, try 0.1.
overlap_height <- 3 

ggplot(plot_data, aes(x = condition_num, y = subid, height = mean, group = subid)) +
  # fill = "white" is crucial for the 3D effect so front mountains hide back ones
  geom_ridgeline(
    scale = overlap_height, 
    fill = "white", 
    color = "black", 
    alpha = 0.9,      # Slight transparency helps see the 'valley' behind
    min_height = -100 # Ensures the 'base' of the mountain stays visible
  ) +
  # Add the points on the peaks
  geom_point(
    aes(y = as.numeric(subid) + (mean * overlap_height)), 
    size = 1, 
    color = "royalblue"
  ) +
  scale_x_continuous(
    breaks = 1:length(unique(plot_data$condition)),
    labels = levels(as.factor(ridge_data_s1$condition)),
    expand = c(0.1, 0.1)
  ) +
  labs(x = "Condition", y = "Participants (Youngest at Bottom/Front)") +
  theme_ridges() +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank() # Cleans up the background for the 3D look
  )