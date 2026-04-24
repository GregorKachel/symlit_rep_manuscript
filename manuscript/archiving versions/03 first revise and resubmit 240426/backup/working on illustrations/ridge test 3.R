




library(tidyverse)
install.packages("ggridges")
library(ggridges)

head(rep.S1.bayes.data)


ridge_data_s1 <- rep.S1.bayes.data %>% 
  group_by(subid, agem, condition) %>% 
  summarize(mean = mean(correct))

head(ridge_data_s1)


# 1. Daten vorbereiten: subid als Faktor sortiert nach Alter (agem)
ridge_data_prepared <- ridge_data_s1 %>%
  mutate(subid = fct_reorder(as.factor(subid), agem))

# 2. Den Ridge Plot erstellen
ggplot(ridge_data_prepared, aes(x = mean, y = subid, fill = agem)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01, alpha = 0.7) +
  # Eine schöne Farbskala für das Alter
  scale_fill_viridis_c(name = "Alter (Monate)") +
  theme_ridges() + 
  labs(
    title = "Testleistung pro Proband sortiert nach Alter",
    x = "Durchschnittliche Leistung",
    y = "Proband (ID)"
  ) +
  theme(axis.text.y = element_text(size = 6)) # Falls du viele Probanden hast

