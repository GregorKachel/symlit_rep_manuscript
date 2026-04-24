

library(tidyverse)
library(patchwork)
library(ggview)


# Full data set including all participants (i.e. including dropped participants)
rep.data <- readRDS("../data/symlitrep_final_data.rds")

# test  <- rep.data %>%
#   #filter(valid != "drop") %>% # valid participants only 
#   filter(trial != "fam") %>% 
#   group_by(subid, agem) %>% 
#   summarize(participants = n_distinct(subid),
#             trials = n())

# Study1 ----------------------------------------------
rep.S1.bayes.data  <- rep.data %>%
  filter(valid != "drop") %>% # valid participants only 
  filter(study == "study1") %>% # in study one
  filter(trial != "fam") %>% 
  select(condition, subid, sex, aged, agey, agem, correct, trial, rt, cue) %>%
  mutate(condition = factor(condition, levels = c(
    "Representation", # first level becomes reference
    "Pars Pro Toto", 
    "Simple Form Analogy", 
    "Complex Form Analogy"))) %>%  
  mutate(sex = factor(sex, levels = c(
    "0", # first level becomes reference
    "1"))) %>%  
  mutate(z.trial = scale(as.numeric(trial)),
         ageinyears = aged/365.25, 
         z.age = ageinyears - mean(ageinyears),
         z.sex = scale(as.numeric(sex))) %>% 
  mutate(item = gsub("_A.png", "", cue)) %>% 
  mutate(item = gsub("_B.png", "", item)) 

rep.S1.order <- rep.S1.bayes.data %>% 
  mutate(trial = as.integer(trial)) %>% 
  mutate(block = case_when(
    trial %in% 1:4 ~ "block1",
    trial %in% 5:8 ~ "block2",
    trial %in% 9:12 ~ "block3",
    trial %in% 13:16 ~ "block4")) %>% 
  group_by(condition, block) %>% 
  summarize(correct = mean(correct),
            n = n())

rep.S1.sequ <- rep.S1.bayes.data %>%
  mutate(trial = as.integer(trial)) %>%
  arrange(subid, trial) %>%
  group_by(subid) %>%
  mutate(
    change = condition != lag(condition, default = first(condition)),
    last_block_val = if_else(change, lag(condition), NA_character_)) %>%
  fill(last_block_val, .direction = "down") %>%
  rename(preceded_by = last_block_val) %>%
  select(-change) %>%
  ungroup() %>% 
  mutate(preceded_by = 
           if_else(is.na(preceded_by), "None", preceded_by)) 

rep.S1.sequ.plot1 <- rep.S1.sequ %>%
group_by(condition, preceded_by) %>% 
  summarize(correct = mean(correct, na.rm = TRUE), .groups = "drop") %>% 
  mutate(
    # X-Achse: Von links nach rechts
    preceded_by = fct_relevel(preceded_by, 
                              "None", 
                              "Representation", 
                              "Pars Pro Toto", 
                              "Simple Form Analogy", 
                              "Complex Form Analogy"),
    condition = fct_relevel(condition, 
                            "Representation", 
                            "Pars Pro Toto", 
                            "Simple Form Analogy", 
                            "Complex Form Analogy") %>% fct_rev())

heat_plot1 <- ggplot(rep.S1.sequ.plot1, 
                    aes(x = preceded_by, y = condition, fill = correct)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#d7191c", mid = "#ffffbf", high = "#1a9641", 
                       midpoint = 0.5, limit = c(0,1), name = "Genauigkeit") +
  geom_text(aes(label = paste0(round(correct * 100, 1), "%")), color = "black") +
  labs(
       y = "Condition") +
  theme_minimal() +
  theme(legend.position = "none")

heat_plot1


# Study2 ----------------------------------
rep.S2.bayes.data  <- rep.data %>%
  filter(valid != "drop") %>% # valid participants only
  filter(study == "study2") %>%
  filter(trial != "fam") %>%
  select(condition, subid, sex, aged, agey, agem, correct, trial, rt, cue) %>%
  mutate(condition = factor(condition, levels = c(
    "Absolute Position", # first level becomes reference
    "Relative Position",
    "Orientation of Object",
    "Orientation of Feature"))) %>%
  mutate(sex = factor(sex, levels = c(
    "0", # first level becomes reference
    "1"))) %>%
  mutate(z.trial = scale(as.numeric(trial)),
         ageinyears = aged/365.25,
         z.age = ageinyears - mean(ageinyears),
         z.sex = scale(as.numeric(sex))) %>%
  mutate(item = gsub("_A.png", "", cue)) %>%
  mutate(item = gsub("_B.png", "", item))





rep.S2.sequ <- rep.S2.bayes.data %>%
  mutate(trial = as.integer(trial)) %>%
  arrange(subid, trial) %>%
  group_by(subid) %>%
  mutate(
    change = condition != lag(condition, default = first(condition)),
    last_block_val = if_else(change, lag(condition), NA_character_)) %>%
  fill(last_block_val, .direction = "down") %>%
  rename(preceded_by = last_block_val) %>%
  select(-change) %>%
  ungroup() %>% 
  mutate(preceded_by = 
           if_else(is.na(preceded_by), "None", preceded_by)) 


table(rep.S2.bayes.data$condition)

rep.S2.sequ.plot1 <- rep.S2.sequ %>%
  group_by(condition, preceded_by) %>% 
  summarize(correct = mean(correct, na.rm = TRUE), .groups = "drop") %>% 
  mutate(
    # X-Achse: Von links nach rechts
    preceded_by = fct_relevel(preceded_by, 
                              "None", 
                              "Absolute Position", 
                              "Relative Position", 
                              "Orientation of Object", 
                              "Orientation of Feature"),
    condition = fct_relevel(condition, 
                            "Absolute Position", 
                            "Relative Position", 
                            "Orientation of Object", 
                            "Orientation of Feature") %>% fct_rev())

heat_plot2 <- ggplot(rep.S2.sequ.plot1, 
                     aes(x = preceded_by, y = condition, fill = correct)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#d7191c", mid = "#ffffbf", high = "#1a9641", 
                       midpoint = 0.5, limit = c(0,1), name = "Genauigkeit") +
  geom_text(aes(label = paste0(round(correct * 100, 1), "%")), color = "black") +
  labs(
    x = "Preceded by",
    y = "Condition") +
  theme_minimal() +
  theme(legend.position = "none")

heat_plot2


# Study 3 -----------------------------------
rep.S3.bayes.data  <- rep.data %>%
  filter(valid != "drop") %>% # valid participants only
  filter(study == "study3") %>%
  filter(trial != "fam") %>%
  select(condition, subid, sex, aged, agey, agem, correct, trial, rt, cue) %>%
  mutate(condition = factor(condition, levels = c(
    "Size of Object", # first level becomes reference
    "Size of Feature",
    "Number of Object",
    "Number of Feature"))) %>%
  mutate(sex = factor(sex, levels = c(
    "0", # first level becomes reference
    "1"))) %>%
  mutate(z.trial = scale(as.numeric(trial)),
         ageinyears = aged/365.25,
         z.age = ageinyears - mean(ageinyears),
         z.sex = scale(as.numeric(sex))) %>%
  mutate(item = gsub("_A.png", "", cue)) %>%
  mutate(item = gsub("_B.png", "", item))




rep.S3.sequ <- rep.S3.bayes.data %>%
  mutate(trial = as.integer(trial)) %>%
  arrange(subid, trial) %>%
  group_by(subid) %>%
  mutate(
    change = condition != lag(condition, default = first(condition)),
    last_block_val = if_else(change, lag(condition), NA_character_)) %>%
  fill(last_block_val, .direction = "down") %>%
  rename(preceded_by = last_block_val) %>%
  select(-change) %>%
  ungroup() %>% 
  mutate(preceded_by = 
           if_else(is.na(preceded_by), "None", preceded_by)) 


table(rep.S3.bayes.data$condition)

rep.S3.sequ.plot1 <- rep.S3.sequ %>%
  group_by(condition, preceded_by) %>% 
  summarize(correct = mean(correct, na.rm = TRUE), .groups = "drop") %>% 
  mutate(
    # X-Achse: Von links nach rechts
    preceded_by = fct_relevel(preceded_by, 
                              "None", 
                              "Number of Object", 
                              "Number of Feature", 
                              "Size of Object", 
                              "Size of Feature"),
    condition = fct_relevel(condition, 
                            "Number of Object", 
                            "Number of Feature", 
                            "Size of Object", 
                            "Size of Feature") %>% fct_rev())

heat_plot3 <- ggplot(rep.S3.sequ.plot1, 
                     aes(x = preceded_by, y = condition, fill = correct)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#d7191c", mid = "#ffffbf", high = "#1a9641", 
                       midpoint = 0.5, limit = c(0,1), name = "Genauigkeit") +
  geom_text(aes(label = paste0(round(correct * 100, 1), "%")), color = "black") +
  labs(
    x = "Preceded by",
    y = "Condition") +
  theme_minimal() +
  theme(legend.position = "none")



# combining plots ----------------------------

heat_plot1 <- heat_plot1 + theme(axis.title.x = element_blank())
heat_plot2 <- heat_plot2 + theme(axis.title.x = element_blank())

# Den Plots direkt Titel geben, falls sie noch keine haben
heat_complete <- (
  (heat_plot1 + labs(title = "Study 1")) / 
    (heat_plot2 + labs(title = "Study 2")) / 
    (heat_plot3 + labs(title = "Study 3"))) + 
  plot_layout(guides = "collect")

heat_complete + canvas(width=27, height= 30, units="cm", dpi = 600)


# save plots

ggsave(
  filename = "../illustrations/heat_complete.pdf",
  plot = heat_complete,
  width = 27,
  height = 16.9,
  units = "cm",
  dpi = 600)

# shell.exec("../illustrations/S1_complete.pdf")

ggsave(
  filename = "./../illustrations/heat_complete.jpg",
  plot = heat_complete,
  width = 27,
  height = 16.9,
  units = "cm",
  dpi = 600)

ggsave(
  filename = "./../illustrations/heat_complete.png",
  plot = heat_complete,
  width = 27,
  height = 16.9,
  units = "cm",
  dpi = 600)

ggsave(
  filename = "./../illustrations/heat_complete.svg",
  plot = heat_complete,
  width = 27,
  height = 16.9,
  units = "cm",
  dpi = 600)










