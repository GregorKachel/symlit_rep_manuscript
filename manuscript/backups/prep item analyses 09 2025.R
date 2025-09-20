
S1.item.model <- readRDS("../models/S1.item.model.rds")

# for reporting BULK_ESS and Coefficients
S1.item.coef <- summarise_draws(as_draws_df(S1.item.model)) # ! standard is just 90% CrI

# NOTE: # as_draws_df() from posterior uses 90% CrIs
# we preregistered using 2.5% and 97.5% quantiles
draws_df <- as_draws_df(S1.item.model)
quantiles_df <- as.data.frame(t(apply(draws_df, 2, quantile, probs = c(0.025, 0.975))))
quantiles_df <- tibble::rownames_to_column(quantiles_df, var = "variable")
names(quantiles_df)[2:3] <- c("q2.5", "q97.5")

S1.item.coef <- left_join(S1.item.coef, quantiles_df, by = "variable") 

# for table in appendix:
S1.item.coef.table <- S1.item.coef

# # for reporting in text:
# S1.item.coef.text <- S1.item.coef %>% 
#   filter(grepl("^b_", variable)) %>% 
#   mutate(variable = gsub("b_", "", variable)) %>% 
#   mutate(variable = gsub("condition", "", variable)) %>% 
#   data.frame(row.names = "variable")

S1.item.coef.table.fin <- S1.item.coef.table %>% 
  slice(1:16) %>% 
  # filter(grepl("^b_", variable)) %>% 
  mutate(variable = gsub("b_", "", variable)) %>% 
  mutate(variable = gsub("condition", "", variable)) %>% 
  mutate(variable = gsub("([^ ])([A-Z])", "\\1 \\2", variable)) %>% 
  mutate(variable = gsub(":", " × ", variable)) %>% 
  mutate(variable = gsub("z.age", "Age*", variable)) %>% 
  mutate(variable = gsub("z.trial", "Trial*", variable)) %>% 
  mutate(variable = gsub("sex1", "Sex (Male)", variable)) %>%
  mutate(
    variable = gsub("sd_item__ Intercept", "Item Intercept (SD)", variable),
    variable = gsub("sd_item__Age\\*", "Item × Age Slope (SD)", variable),
    variable = gsub("sd_subid__ Intercept", "Subject Intercept (SD)", variable),
    variable = gsub("sd_subid__Trial\\*", "Subject × Trial slope (SD)", variable),
    variable = gsub("cor_item__ Intercept__Age\\*", "Correlation: Item Intercept & Age Slope", variable),
    variable = gsub("cor_subid__ Intercept__Trial\\*", "Correlation: Subject Intercept & Trial Slope", variable)) %>% 
  rename(
    Predictor = variable,
    Estimate = mean,
    SD = sd,
    MAD = mad,
    `2.5% CrI` = q2.5, 
    `97.5% CrI` = q97.5, 
    `Bulk ESS` = ess_bulk,
    `Tail ESS` = ess_tail) %>%
  mutate(
    `95% CrI` = paste0("[", sprintf("%.2f", `2.5% CrI`), ", ", sprintf("%.2f", `97.5% CrI`), "]"),
    `Bulk ESS` = format(round(`Bulk ESS`), big.mark = ","),
    `Tail ESS` = format(round(`Tail ESS`), big.mark = ",")) %>%
  select(Predictor, Estimate, SD, MAD, `95% CrI`, `Bulk ESS`, `Tail ESS`)

# add line to highlight the random effects section
S1.item.coef.table.fin <- bind_rows(
  S1.item.coef.table.fin[1:10, ],
  tibble(
    Predictor = "Random Effects",
    Estimate = NA_real_,
    SD = NA_real_,
    MAD = NA_real_,
    `95% CrI` = "",
    `Bulk ESS` = "",
    `Tail ESS` = ""
  ),
  S1.item.coef.table.fin[11:16, ]
)


# ft <- flextable(S1.item.coef.table.fin) %>%
#   theme_apa() %>%
#   set_caption("suppl-S1-item-table") %>%
#   fontsize(size = 8, part = "all") %>%
#   set_table_properties(layout = "autofit") %>%
#   add_footer_lines(values = "Note. Estimates represent posterior means with 95% equal-tailed credible intervals (CrIs). MAD indicates Median Absolute Deviation. ESS refers to effective sample size. R̂ values omitted as they are all ~1 indicating convergence.  * = variables were standardized.") %>%
#   fontsize(part = "footer", size = 8) %>%  
#   autofit()
# ft


ft <- flextable(S1.item.coef.table.fin) %>%
  theme_apa() %>%
  set_caption("suppl-S1-item-table") %>%
  fontsize(size = 8, part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  add_footer_lines(values = "Note. Estimates represent posterior means with 95% equal-tailed credible intervals (CrIs). MAD indicates Median Absolute Deviation. ESS refers to effective sample size. R̂ values omitted as they are all ~1 indicating convergence.  * = variables were standardized.") %>%
  fontsize(part = "footer", size = 8) %>%
  bold(i = ~ Predictor == "Random effects", bold = TRUE, part = "body") %>% 
  merge_h(i = ~ Predictor == "Random effects") %>% # merge across columns
  autofit()

ft
