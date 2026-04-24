library(tidyverse)
library(ggridges)

# 1. Prepare the data
plot_data <- ridge_data_s1 %>%
  # Ensure condition is a factor and convert to numeric for the x-axis
  mutate(condition = as.factor(condition),
         cond_num = as.numeric(condition)) %>%
  # Reorder subid based on age (Youngest = Level 1 = Bottom/Front)
  mutate(subid = fct_reorder(as.character(subid), agem))

# 2. Define a scaling factor
# This determines how much the ridges overlap. 
# Increase this value if you want the ridges to be "taller."
my_scale <- 1.5 

# 3. Create the plot
ggplot(plot_data, aes(x = cond_num, y = subid, height = mean, group = subid)) +
  # geom_ridgeline draws the connected lines and fills the area below to create depth
  geom_ridgeline(scale = my_scale, fill = "white", color = "black", alpha = 0.8) +
  # Add dots on top of each ridge peak
  # We calculate the Y position manually: Baseline (as.numeric(subid)) + (height * scale)
  geom_point(aes(y = as.numeric(subid) + (mean * my_scale)), size = 1.5) +
  # Map the numeric x-axis back to condition labels
  scale_x_continuous(breaks = 1:length(levels(plot_data$condition)), 
                     labels = levels(plot_data$condition)) +
  # Standard labeling and theme
  labs(title = "Individual Performance Profiles by Age",
       subtitle = "Youngest children in front, older in back",
       x = "Condition", 
       y = "Participants (Ordered by Age)") +
  theme_ridges() +
  theme(axis.text.y = element_blank()) # Remove y-labels if there are too many subjects