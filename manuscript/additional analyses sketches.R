
### Prepare session ---------------------------------------

library(tidyverse)

# load data sets
rep.data <- readRDS("../data/symlitrep_final_data.rds")

rep.S1.bayes.data  <- rep.data %>%
  filter(valid != "drop") %>% # valid participants only 
  filter(study == "study1") %>% # in study one
  filter(trial != "fam") %>% 
  select(condition, subid, sex, aged, correct, trial, rt) %>%
  mutate(z.trial = scale(as.numeric(trial)),
         ageinyears = aged/365.25, 
         z.age = ageinyears - mean(ageinyears),
         z.sex = scale(as.numeric(sex)))

rep.S2.bayes.data  <- rep.data %>%
  filter(valid != "drop") %>% # valid participants only 
  filter(study == "study2") %>% # in study one
  filter(trial != "fam") %>% 
  select(condition, subid, sex, aged, correct, trial, rt) %>%
  mutate(z.trial = scale(as.numeric(trial)),
         ageinyears = aged/365.25, 
         z.age = ageinyears - mean(ageinyears),
         z.sex = scale(as.numeric(sex)))

rep.S3.bayes.data  <- rep.data %>%
  filter(valid != "drop") %>% # valid participants only 
  filter(study == "study3") %>% # in study3
  filter(trial != "fam") %>% 
  select(condition, subid, sex, aged, correct, trial, rt) %>%
  mutate(z.trial = scale(as.numeric(trial)),
         ageinyears = aged/365.25, 
         z.age = ageinyears - mean(ageinyears),
         z.sex = scale(as.numeric(sex)))

# load models
S1.full.bm <- readRDS("../models/S1.full.bm.rds")
S2.full.bm <- readRDS("../models/S2.full.bm.rds")
S3.full.bm <- readRDS("../models/S3.full.bm.rds")


# checking models -----------------------
library(brms)
pp_check(S3.full.bm)

# additional analyses ---------------------------


# preregistered item analysis ---------
# 
# An additional exploratory analysis will include a random effect for item level effects (Model: correct ~ task*z.age +z.trial +z.sex +(z.trial|id) +(z.age|item)).
# Results will help to evaluate the equivalence of items within a task and be reported in the supplements. Due to the low number of individual items within a
# task we expect this model to be less diagnostic with regard to our main research question and, therefore, will not include the term in the main analysis.


# possible add-ons
# - a model including all conditions
# - comparing difficulty across items and tasks


#   object vs feature ---------------------------------
# - orfe vs orob orientation
# - sife vs siob size
# - nufe vs nuob number

objectfeature.data  <- rep.data %>%
  filter(valid != "drop") %>% # valid participants only 
  filter(trial != "fam") %>% 
  filter(cond %in% c("orfe", "orob", "sife", "siob", "nufe", "nuob")) %>% 
  select(condition, cond, subid, sex, aged, correct, trial, rt, cue) %>%
  mutate(z.trial = scale(as.numeric(trial)),
         ageinyears = aged/356.25, 
         z.age = ageinyears - mean(ageinyears),
         z.sex = scale(as.numeric(sex)),
         objectfeature = case_when(
           grepl("ob", cond) ~ "object",
           grepl("fe", cond) ~ "feature",
           TRUE ~ NA_character_))

table(objectfeature.data$condition)
table(objectfeature.data$objectfeature)

perc_objectfeature.data <- objectfeature.data %>% 
  group_by(objectfeature) %>% 
  summarize(mean = mean(correct))

perc_objectfeature.data



# round vs angular (study 1) ----------------------------------------
# Study One Study1 - cue A = rund, cue B eckig ...if one of them is easier

rep.S1.shape.data  <- rep.data %>%
  filter(valid != "drop") %>% # valid participants only 
  filter(study == "study1") %>% # in study one
  filter(trial != "fam") %>% 
  select(condition, subid, sex, aged, correct, trial, rt, cue) %>%
  mutate(z.trial = scale(as.numeric(trial)),
         ageinyears = aged/356.25, 
         z.age = ageinyears - mean(ageinyears),
         z.sex = scale(as.numeric(sex)),
         shape = case_when(
           grepl("A", cue) ~ "round",
           grepl("B", cue) ~ "angular",
           TRUE ~ NA_character_))

# shape    mean
# <chr>   <dbl>
#   1 angular 0.709
# 2 round   0.709

# :- D

perc_rep.S1.shape.data <- rep.S1.shape.data %>% 
  group_by(condition, shape) %>% 
  summarize(mean = mean(correct))

perc_rep.S1.shape.data

# Reaction Times ---------------

# just reaction times and perc correct across aged

rt.data  <- rep.data %>%
  filter(valid != "drop") %>% # valid participants only 
  filter(trial != "fam") %>% 
  select(condition, subid, sex, aged, correct, trial, rt, cue) %>%
  mutate(z.trial = scale(as.numeric(trial)),
         ageinyears = aged/356.25, 
         z.age = ageinyears - mean(ageinyears),
         z.sex = scale(as.numeric(sex))) %>% 
  filter(rt >= 40) %>% 
  filter(rt <= 30000)
  




















