#==============================
# Working title: Psychological fingerprints of elite football playing positions
# Author: Leonie JT Balter
# Date: Sep 2025
#==============================

#==============================
# Demographics
#==============================
# Sample size total
length(df2$id)

# Sample size, group: 1 = player, 2 = control
table(df2$group)

# Sample size per cluster
table(df2$cluster)

# Age
df2 %>%
  summarise(mean_age = mean(age, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE),
            min_age = min(age, na.rm = TRUE),
            max_age = max(age, na.rm = TRUE)
  )

# Age by group
df2 %>%
  group_by(group) %>%
  summarise(mean_age = mean(age, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE),
            min_age = min(age, na.rm = TRUE),
            max_age = max(age, na.rm = TRUE)
  )%>%
  ungroup()

# Education by group: 1 = player, 2 = control
df2 %>%
  group_by(group) %>%
  summarise(mean_school = mean(school_in_years_, na.rm = TRUE),
            sd_school = sd(school_in_years_, na.rm = TRUE)
  )%>%
  ungroup()

# Median matches analyzed
df2$matches_analyzed_45_min_<-as.numeric(df2$matches_analyzed_45_min_)
median(df2$matches_analyzed_45_min_,na.rm=T)
min(df2$matches_analyzed_45_min_,na.rm=T)
max(df2$matches_analyzed_45_min_,na.rm=T)

df2 <- df2 %>%
  mutate(across(c(avg_correct_pass:avg_interceptions), as.numeric))

# Make position category variable
df2$position_cat <- ifelse(df2$position %in% c("Centre Back", "Left Wing-Back", "Right Wing-Back"), "Defender",
                           ifelse(df2$position %in% c("Central Midfielder", "Defensive Midfielder"), "Midfielder", 
                                  ifelse(df2$position %in% c("Striker", "Left Winger","Right Winger"), "Striker", 
                                         "Control"))
)

df2 <- df2 %>%
  dplyr::select(id, cluster,team,position,position_cat, group,age, everything())

table(df2$position_cat)

#==============================
# Mean center by position and run model
#==============================
df2<-df2%>%select(id:age,avg_total_pass:avg_interceptions)

df2 <- df2 %>%
  group_by(position_cat) %>%
  mutate(across(avg_correct_pass:avg_interceptions, 
                list(mean = ~mean(.x, na.rm = TRUE)), 
                .names = "mean_{.col}")) %>%
  ungroup() 

df2<-df2%>%
  group_by(id,position_cat) %>%
  mutate(across(avg_correct_pass:avg_interceptions, 
                ~ .x - get(paste0("mean_", deparse(substitute(.x)))), 
                .names = "c_{.col}"))

# Unique positions to predict for
pred_data <- data.frame(position_cat = unique(df2$position_cat))

# Bootstrap settings
set.seed(123) 
n_boot <- 1000
boot_preds <- list()

for (i in 1:n_boot) {
  boot_sample <- df2[sample(nrow(df2), replace = TRUE), ]
  boot_model <- multinom(cluster ~ position_cat, data = boot_sample, trace = FALSE)
  boot_pred <- predict(boot_model, newdata = pred_data, type = "probs")
  boot_preds[[i]] <- boot_pred
}

# Combine predictions
boot_array <- array(unlist(boot_preds), dim = c(nrow(pred_data), 3, n_boot))

# Calculate mean and 95% CI for each cluster probability
mean_probs <- apply(boot_array, c(1, 2), mean)
lower_probs <- apply(boot_array, c(1, 2), function(x) quantile(x, 0.025))
upper_probs <- apply(boot_array, c(1, 2), function(x) quantile(x, 0.975))

# Reshape to long format
pred_data_long <- pred_data %>%
  mutate(cluster1_prob = mean_probs[, 1],
         cluster2_prob = mean_probs[, 2],
         cluster3_prob = mean_probs[, 3],
         cluster1_lower = lower_probs[, 1],
         cluster2_lower = lower_probs[, 2],
         cluster3_lower = lower_probs[, 3],
         cluster1_upper = upper_probs[, 1],
         cluster2_upper = upper_probs[, 2],
         cluster3_upper = upper_probs[, 3]) %>%
  pivot_longer(cols = starts_with("cluster"), names_to = c("Cluster", ".value"),
               names_pattern = "cluster(\\d+)_(prob|lower|upper)") %>%
  mutate(Cluster = paste0("Cluster ", Cluster))

(positionpredplot <- ggplot(pred_data_long, aes(axis1 = Cluster, axis2 = position_cat, y = prob)) +
    geom_alluvium(aes(fill = position_cat), width = 1/12) +
    geom_stratum(width = 1/12, fill = "black", color = "grey") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), angle = 90, color = "white", size = 3) +
    scale_fill_manual(values = c("#1f995f", "#6e6868", "#5b0b6e", "#E3B505")) +
    labs(
      title = "Predicted probability of cluster by position",
      y = NULL,
      x = NULL
    ) +
    guides(fill = guide_legend(title = NULL,nrow = 1, byrow = TRUE))+
    theme_void()+
    theme(
      legend.position = "bottom",
      legend.key.size = unit(0.8, "lines"), 
      legend.text = element_text(size=11.5),
      plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
)

# Per position (without controls)
# Defender as reference
model <- glm(cluster ~ position_cat, family = binomial(),data = filter(df2,position_cat!="Control",cluster != 3))
tab_model(model)

# Midfielder as reference
model <- glm(cluster ~ relevel(position_cat, ref = c("Midfielder")),family = binomial(),data = filter(df2, position_cat != "Control", cluster != 3))
tab_model(model)

#==============================
# Performance metrics
#==============================
df2_nocontrol<-df2%>%filter(position_cat!="Control",cluster!=3)

predictors <- c("c_avg_correct_pass", "c_goals", "c_avg_shots_per_game", 
                "c_accuracy_of_shots", "c_assists", "c_avg_steal_attempt", 
                "c_avg_successful_steals", "c_challenges_won_on_defense", 
                "c_total_challenges_on_defense", 
                "c_total_challenges_on_offense", "c_avg_attempted_dribbles", 
                "c_avg_successful_dribbles", "c_avg_interceptions")

var_labels <- c(
  c_avg_correct_pass = "Correct passes",
  c_goals = "Goals",
  c_avg_shots_per_game = "Shots",
  c_accuracy_of_shots = "Accuracy of shots",
  c_assists = "Assists",
  c_avg_steal_attempt = "Steal attempts",
  c_avg_successful_steals = "Successful steals",
  c_challenges_won_on_defense = "Challenges won on defense",
  c_total_challenges_on_defense = "Total challenges on defense",
  c_total_challenges_on_offense = "Total challenges on offense",
  c_avg_attempted_dribbles = "Attempted dribbles",
  c_avg_successful_dribbles = "Successful dribbles",
  c_avg_interceptions = "Interceptions"
)

plot_data <- list()

for (pred in predictors) {
  formula <- as.formula(paste0("cluster ~ ", pred))
  model <- glm(formula, data = df2_nocontrol, family = binomial())
  coef_summary <- summary(model)$coefficients
  p_val <- coef_summary[2, 4]
  OR <- exp(coef_summary[2, 1])  # Odds ratio from beta
  
  # Sequence of predictor values
  newdata <- data.frame(seq_var = seq(
    min(df2_nocontrol[[pred]], na.rm = TRUE),
    max(df2_nocontrol[[pred]], na.rm = TRUE),
    length.out = 100
  ))
  names(newdata) <- pred
  
  preds <- predict(model, newdata = newdata, type = "link", se.fit = TRUE)
  
  pred_summary <- data.frame(
    x = newdata[[pred]],
    prob_mean = plogis(preds$fit),
    prob_lower = plogis(preds$fit - 1.96 * preds$se.fit),
    prob_upper = plogis(preds$fit + 1.96 * preds$se.fit)
  )
  
  color <- ifelse(p_val < 0.05, "green4", "blue")
  
  p <- ggplot(pred_summary, aes(x = x, y = prob_mean)) +
    geom_line(color = color, size = 1.2) +
    geom_ribbon(aes(ymin = prob_lower, ymax = prob_upper), alpha = 0.2, fill = color) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    labs(
      title = var_labels[[pred]],
      x = paste0(var_labels[[pred]], "\n(standardized within position)"),
      y = "Probability\n(Cluster = 2)"
    ) +
    theme_minimal(base_size = 13) +
    apatheme +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  
  plot_data[[pred]] <- list(plot = p, pval = p_val, OR = OR)
}

ordered_plots <- plot_data %>%
  purrr::imap_dfr(~ tibble(
    pred = .y,
    pval = .x$pval,
    OR = .x$OR,
    plot = list(.x$plot)
  )) %>%
  arrange(pval > 0.05, desc(OR)) 

(performance_plot <- wrap_plots(ordered_plots$plot, ncol = 4) +
    plot_annotation(
      title = "Predicted probability curves for membership of Cluster 2",
      caption = "Green = p < .05, Blue = p > .05",
      theme = theme(
        plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 12.5)
      )
    )
)

#==============================
# Create table for supplement
#==============================
results_list <- list()

for (pred in predictors) {
  formula <- as.formula(paste0("cluster ~ ", pred))
  model <- glm(formula, data = df2_nocontrol, family = binomial())
  model_summary <- summary(model)
  coef_summary <- model_summary$coefficients
  
# Extract estimate
  est <- coef_summary[2, 1]
  se <- coef_summary[2, 2]
  p_val <- coef_summary[2, 4]
  
  # Compute odds ratio and CI
  OR <- exp(est)
  CI_lower <- exp(est - 1.96 * se)
  CI_upper <- exp(est + 1.96 * se)
  
  results_list[[pred]] <- data.frame(
    Predictor = var_labels[[pred]],  # Use clean label
    OR = OR,
    CI_lower = CI_lower,
    CI_upper = CI_upper,
    p = p_val
  )
}

performanceresults <- do.call(rbind, results_list) %>%
  mutate(
    OR = round(OR, 2),
    CI = paste0("[", round(CI_lower, 2), ", ", round(CI_upper, 2), "]"),
    p = round(p, 4)
  ) %>%
  select(Predictor, OR, CI, p) %>%
  arrange(p > 0.05, desc(OR))

print(performanceresults)