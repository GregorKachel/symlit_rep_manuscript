main_plot <- ggplot() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey70", alpha = 0.75) +
  
  geom_point(data = d3, aes(x = ageinyears, y = mean, colour = colour), alpha = 0.5, shape = 1, size = 2) +
  
  geom_smooth(data = f3, aes(x = age, y = Estimate, ymin = Q2.5, ymax = Q97.5, fill = colour, colour = colour),
              stat = "identity", alpha = 0.2, linewidth = 0.8) +
  
  geom_point(data = p3, aes(x = days/365.25, y = 0.5, fill = colour, colour = colour), size = 4, shape = 21, stroke = 1) +
  geom_point(data = p3, aes(x = days/365.25, y = 0.5, fill = "black", colour = "black"), size = 0.5, shape = 21, stroke = 1) +
  geom_text(data = p3, aes(label = months, x = days/365.25, y = 0.32, colour = "black", fontface = "bold"),
            angle = 90, size = 3.2, vjust = 0.5) +
  
  geom_text(data = p3, aes(label = "months", x = days/365.25, y = .4, colour = "black", fontface = "bold"), 
            angle = 90, size = 3.2, vjust = 0.5) +
  
  scale_colour_identity() +
  scale_fill_identity() +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) + # Multiply by 100 & add %  
  
  
  facet_grid(cols = vars(condition)) +
  
  labs(x = "Age", y = "Proportion Correct") +
  
  coord_cartesian(ylim = c(0, 1), xlim = c(2.7, 7.3)) +   # Allow more Y space for image
  
  theme_minimal(base_size = 13) +
  theme(
    panel.border = element_rect(colour = "grey30", fill = NA, size = 1),
    strip.background = element_rect(colour = "grey30", size = 1),
    strip.text = element_text(face = "bold", size = rel(0.8),
                              margin = margin(t = 5, b = 5)), # HIER STRIPE SPACING via b =
    strip.switch.pad.grid = unit(0, "cm"),
    axis.title = element_text(face = "bold", size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )


main_plot
