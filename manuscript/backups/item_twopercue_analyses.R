




S1.item.model <- readRDS("../models/S1.item.model.rds")



# item
pp_check(S1.item.model)
pp_check(S1.item.model, type = "bars")

S1.item.coef <- summarise_draws(as_draws_df(S1.item.model)) %>%
  filter(grepl("^b_", variable) |
           grepl("^sd_", variable)) %>%
  mutate(variable = gsub("b_", "", variable)) %>%
  mutate(variable = gsub("condition", "", variable)) %>%
  data.frame(row.names = "variable")


# ITEM
# all > 1000
mean(S1.item.coef$ess_bulk)
min(S1.item.coef$ess_bulk)
max(S1.item.coef$ess_bulk)

# all = 1  
mean(S1.item.coef$rhat)
min(S1.item.coef$rhat)
max(S1.item.coef$rhat)




# Cue Intercept beta = `r S1.full.coef["sd_cue__Intercept", "mean"]`,
# 95% CrI [`r S1.full.coef["sd_cue__Intercept", "q5"]`, `r S1.full.coef["sd_cue__Intercept", "q95"]`]

# Cue Type	Log-Odds	Accuracy (%)
# Average cue	        2.10	89.1%
# 1 SD below average	1.65	84.0%
# 1 SD above average	2.55	92.7%






# summary(S1.item.model)
item_effects <- ranef(S1.item.model)$item

# Extract random intercepts
item_intercepts <- as_tibble(item_effects[, , "Intercept"], rownames = "item") %>%
  rename(
    estimate = Estimate,
    lower = Q2.5,
    upper = Q97.5) %>% 
  mutate(condition = substr(item,1,4))


item.plot <- ggplot(item_intercepts, aes(x = reorder(item, estimate), y = estimate, color = condition)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +
  facet_wrap(~ condition, scales = "free_y", ncol = 1) +
  labs(
    title = "Item-Level Random Intercepts by Condition",
    x = "Item",
    y = "Estimated Deviation from Overall Intercept (log-odds)",
    color = "Condition"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"))
