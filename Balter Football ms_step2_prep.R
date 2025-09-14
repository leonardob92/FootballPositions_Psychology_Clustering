#==============================
# Working title: Psychological fingerprints of elite football playing positions
# Author: Leonie JT Balter
# Date: Sep 2025
#==============================

# setwd

# Load data
df<-read_excel("Base_Futebol_2021_Reduced.xlsx")%>%
  rename_all(tolower)%>%
  rename("id"="participant",
         "digitspan_combined"="...18")

names(df) <- gsub("[[:punct:] ]+", "_", names(df))  # Replace spaces and punctuation with underscores

# Histograms
vars <- c("neuroticism", "extroversion", "openness", "agreeableness", "conscientiousness",
          "simple_manual_reaction_time", "digitspan_combined", "tower_of_hanoi_moves_",
          "d_kefs_design_fluency_score_overall", "d_kefs_cwi_overall", "d_kefs_tmt_overall")

# Reshape data to long format
df_long <- df %>%
  select(all_of(vars)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

(histo<-ggplot(df_long, aes(x = value, fill = variable)) +
  geom_histogram(bins = 30, color = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 3,
             labeller = labeller(variable = label_rename)) +
  scale_fill_viridis_d(option = "C", guide = "none") +  
  theme_minimal() +  apatheme+
  labs(x = "Value", y = "Score", title = NULL)
)

# Heatmap
mot <- df %>%
  select(id,
         neuroticism, extroversion, openness, agreeableness, conscientiousness,
         simple_manual_reaction_time, digitspan_combined, tower_of_hanoi_moves_,
         d_kefs_design_fluency_score_overall, d_kefs_cwi_overall, d_kefs_tmt_overall) %>%
  na.omit()%>%
  mutate(across(everything(), scale))

mot2 <- mot %>%
  select(-id)

cor_test <- cor.mtest(mot2, conf.level = 0.95)
cor_mat <- cor(mot2, method = "spearman", use = "pairwise.complete.obs")

colnames(cor_mat) <- label_rename[colnames(cor_mat)]
rownames(cor_mat) <- label_rename[rownames(cor_mat)]

diag(cor_mat) <- NA 

corrplot(cor_mat,
         method = "color",
         type = "upper",
         diag = FALSE,
         col = viridis::viridis(200),
         addCoef.col = "black",
         tl.col = "black",
         tl.cex = 0.9,
         tl.srt = 45,
         p.mat = cor_test$p,
         sig.level = 0.05,
         number.cex = 0.75,
         insig = "blank",
         na.label = " ")
dev.off()

