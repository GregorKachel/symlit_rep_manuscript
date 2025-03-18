# prepare data 
rep.S1.bayes.data  <- rep.data %>%
  filter(valid != "drop") %>% # valid participants only 
  filter(study == "study1") %>% # in study one
  filter(trial != "fam") %>% 
  select(condition, subid, sex, aged, correct, trial) %>%
  mutate(z.trial = scale(as.numeric(trial)),
         ageinyears = aged/356.25, 
         z.age = ageinyears - mean(ageinyears),
         z.sex = scale(as.numeric(sex)))




S1.full.bm<-brm(correct~condition*z.age+z.trial+z.sex+(z.trial|subid), 
                data= rep.S1.bayes.data, 
                family=bernoulli(),
                chains = 4,
                iter= 2000,
                cores= 4)

S1.full.bm <- add_criterion(S1.full.bm, c("loo", "waic"))

# Saving the model (to not rerun it every time)
saveRDS(S1.full.bm, "../models/S1.full.bm.rds")




# load model from rds
S1.full.bm <- readRDS("../models/S1.full.bm.rds")
# table(rep.S1.bayes.data$condition)

nd1 <- tibble(z.age = rep(seq(from = min(rep.S1.bayes.data$z.age), 
                              to = max(rep.S1.bayes.data$z.age), 
                              length.out = 50),4),
              condition = c(rep("Representation",50), 
                            rep("Pars Pro Toto",50), 
                            rep("Complex Form Analogy",50), 
                            rep("Simple Form Analogy",50)), 
              # the four conditions in the data
              z.sex = rep(0,200),
              z.trial = rep(0,200))


f1 <- fitted(S1.full.bm, 
             newdata = nd1, 
             re_formula = NA) %>% 
  # this tells the function to ignore the random effects - in theory, we could generate predictions for specific individuals
  as_tibble() %>%
  bind_cols(nd1)%>%
  mutate(age = z.age + mean(rep.S1.bayes.data$ageinyears)) # convert age back to the original scale by adding the mean of the data


# summarize the data to include them in the plot later on
d1 <- rep.S1.bayes.data%>%
  group_by(subid, ageinyears, condition)%>%
  summarise(mean = mean(correct)) 















# Plot without facetting
p1 <- f1 %>%
  mutate(age = age * 365.25)%>%
  group_by(condition)%>%
  summarise(
    Q2.5_closest_to.5 = Q2.5[which.min(abs(Q2.5-.5))],
    estimate_closest_to.5 = Estimate[which.min(abs(Q2.5-.5))],
    days = age[which.min(abs(Q2.5-.5))],
    months = round(days/30.5),
    years = round(days/365.25, 2),
    monthlabels = as.character(paste(months, "months"))
  )

ggplot()+
  geom_hline(yintercept = .5, lty = 2, alpha = .75)+
  geom_point(data = d1, aes(x = ageinyears, y = mean, col = condition), alpha = .5, shape = 1)+
  geom_smooth(data = f1, aes(x = age, y = Estimate, ymin = Q2.5, ymax = Q97.5, fill =condition, col = condition), 
              stat = "identity", alpha = .25)+
  geom_point(data=p1, aes(x = days/365.25, y = .5, fill = condition, col = condition), stat = "identity", size=3)+
  geom_text(data=p1, aes(label = months, x = days/365.25, y = .24, fill = condition, col = condition), angle=90, size = 4, parse=T)+
  geom_text(data=p1, aes(label = "months", x = days/365.25, y = .38, fill = condition, col = condition), angle=90, size = 4, parse=T)+
  theme_minimal()+
  scale_color_ptol(name = "condition")+
  scale_fill_ptol(name = "condition")+
  #facet_grid(~condition)+
  labs(x = "Age", y="Proportion correct")+
  ylim(0,1)+
  xlim(3,7)+
  theme(legend.position = "bottom")








# FACET plot

S1_plot <- ggplot()+
  geom_hline(yintercept = .5, lty = 2, alpha = .75)+
  geom_point(data = d1, aes(x = ageinyears, y = mean, col = condition), alpha = .5, shape = 1)+
  geom_smooth(data = f1, aes(x = age, y = Estimate, ymin = Q2.5, ymax = Q97.5, fill =condition, col = condition), 
              stat = "identity", alpha = .25)+
  geom_point(data=p1, aes(x = days/365.25, y = .5, fill = condition, col = condition), stat = "identity", size=3)+
  geom_text(data=p1, aes(label = months, x = days/365.25, y = .24, fill = condition, col = condition), angle=90, size = 4, parse=T)+
  geom_text(data=p1, aes(label = "months", x = days/365.25, y = .38, fill = condition, col = condition), angle=90, size = 4, parse=T)+
  theme_minimal()+
  scale_color_ptol(name = "condition")+
  scale_fill_ptol(name = "condition")+
  facet_grid(~condition)+
  labs(x = "Age", y="Proportion correct")+
  ylim(0,1)+
  xlim(3,7)+
  theme(legend.position = "bottom")


S1_plot








