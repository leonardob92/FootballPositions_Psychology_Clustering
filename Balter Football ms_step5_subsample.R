#==============================
# Working title: Psychological fingerprints of elite football playing positions
# Author: Leonie JT Balter
# Date: Sep 2025
#==============================
df<-read_excel("SwedishBrazilianSamples_Controls_ExecutiveFunctions_Leonie.xlsx")%>%
  rename_all(tolower)%>%
  rename("id"="person",
         "gender"='gender (1 = male, 2 = female)',
         "dataset"='dataset (1 = swedish, 2 = brazilian)',
         "group"='players/control (p=1, c = 0)')%>%
  mutate(group = case_when(
      group == 1 ~ "Player",
      group == 2 ~ "Control",
      TRUE ~ NA_character_
    ),
    d_kefs_design_fluency_score_overall = rowMeans(cbind(df1scale, df2scale, df3scale), na.rm = TRUE),
    d_kefs_cwi_overall = rowMeans(cbind(cwi1scale, cwi2scale, cwi3scale), na.rm = TRUE),
    d_kefs_tmt_overall = rowMeans(cbind(tmt2scale, tmt3scale, tmt4scale), na.rm = TRUE)
  )

# Heatmap
mot <- df %>%
  select(id,
         df1scale, df2scale, df3scale,
         cwi1scale, cwi2scale, cwi3scale,
         tmt2scale, tmt3scale, tmt4scale)%>%
  na.omit()%>%
  mutate(across(everything(), scale))

mot2 <- mot %>%
  select(-id)

cor_test <- cor.mtest(mot2, conf.level = 0.95)
cor_mat <- cor(mot2, method = "spearman", use = "pairwise.complete.obs")

colnames(cor_mat) <- label_rename2[colnames(cor_mat)]
rownames(cor_mat) <- label_rename2[rownames(cor_mat)]

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
         na.label = " ",
         title = "Spearman correlation Heatmap of\nexecutive function measures",
         mar = c(0, 0, 3, 0))
dev.off()

#===========================================
# Cluster analysis using mclust
#===========================================
mot2<-df%>%
  dplyr::select(id, df1scale, df2scale, df3scale,
                cwi1scale, cwi2scale, cwi3scale,
                tmt2scale, tmt3scale, tmt4scale)%>%
  na.omit()

length(unique(mot2$id))

mot <- mot2[, -1]

# Scale
mot <- mot %>%
  na.omit() %>%
  mutate_all(list(scale))

set.seed(123)
mc<-Mclust(mot,prior = priorControl()) 
summary(mc)

BIC <- mclustBIC(mot,prior = priorControl())
summary(BIC)

#==========
# Select model
#==========
model <- Mclust(mot, modelNames = "VEV", G = 2, x = BIC)
summary(model, parameters=TRUE)

# Get probabilities, means, variances
summary(model, parameters = FALSE)

# Check how many have uncertainty > 50%? (equally likely being assigned to another cluster)
sum(model$uncertainty > 0.5)

# Probability for an observation to be in a given cluster
probtable <- as.data.frame(round(model$z * 100, 2))
print(probtable)

uncertainty <- data.frame(
  id = 1:nrow(mot),
  cluster = model$classification,
  uncertainty = model$uncertainty
)

uncertainty %>%
  group_by(cluster) %>%
  summarise(mean_uncertainty = mean(uncertainty, na.rm = TRUE)*100,
            count = n())

# Bootstrapping
bootstrap<-MclustBootstrap(model, nboot=1000, method="bs")
summary(bootstrap,what="se")

# Extract bootstrapped mixing proportions
bootstrap_df <- as_tibble(bootstrap$pro, .name_repair = "unique") %>%
  pivot_longer(cols = everything(), names_to = "cluster", values_to = "proportion")

(bootstrap_gmm_means2<-ggplot(bootstrap_df, aes(x = proportion, color=cluster,fill = cluster)) +
    geom_histogram(bins = 30, alpha = 0.6) +
    colorfill2+
    labs(title = "Bootstrap distributions of GMM mixing proportions",
         x = "Mixing proportion",y = "Frequency") +
    theme_minimal(base_size = 13)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold",size=13),
          strip.text = element_text(size = 11, face = "bold"),
          legend.text=element_text(size=11))
)
dev.off()

bootstrap_means_df <- as.data.frame(bootstrap$mean)
bootstrap_means_df_long <- bootstrap_means_df %>%
  pivot_longer(cols = everything(), names_to = "component", values_to = "mean")

bootstrap_means_df_long$cluster <- dplyr::case_when(
  stringr::str_detect(bootstrap_means_df_long$component, "\\.1$") ~ 1,
  stringr::str_detect(bootstrap_means_df_long$component, "\\.2$") ~ 2,
  stringr::str_detect(bootstrap_means_df_long$component, "\\.3$") ~ 3,
  TRUE ~ NA_integer_
)

bootstrap_means_df_long <- bootstrap_means_df_long %>%
  mutate(measure = str_remove(component, "\\.\\d+$"))

# Plot
component_means <- bootstrap_means_df_long %>%
  group_by(cluster,measure) %>%
  summarise(mean_value = mean(mean, na.rm = TRUE))

component_means$cluster<-as.factor(component_means$cluster)
bootstrap_means_df_long$measure <- label_rename2[bootstrap_means_df_long$measure]
component_means$measure <- label_rename2[component_means$measure]

# Estimated means
means <- data.frame(Profile = factor(1:model$G),
                    t(model$parameters$mean)) |>
  pivot_longer(cols = -1,
               names_to = "Variable",
               values_to = "Mean")

means$Variable <- factor(means$Variable, 
                         levels = colnames(model$data))

# add mixing probabilities corresponding to profiles
means <- means %>%
  add_column(MixPro = model$parameters$pro[means$Profile])
means

bootstrap_means_df_long$cluster <- factor(bootstrap_means_df_long$cluster,
                                          levels = c("1", "2"),
                                          labels = c("Cluster 1 (n = 174)", "Cluster 2 (n = 124)"))

component_means$cluster <- factor(component_means$cluster,
                                  levels = c("1", "2"),
                                  labels = c("Cluster 1 (n = 174)", "Cluster 2 (n = 124)"))
cluster_colors <- c("Cluster 1 (n = 174)" = "#f2acd7", "Cluster 2 (n = 124)" = "#8E0152")

(meansplots <- ggplot(bootstrap_means_df_long, aes(x = as.numeric(mean), color = cluster, fill = cluster)) +
    geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
    facet_wrap(~measure, ncol = 1, scales = "free_y") +
    geom_vline(data = component_means, aes(color = cluster, xintercept = mean_value), linetype = "solid", size = 1.5) +
    scale_fill_manual(name=NULL,values = cluster_colors) +
    scale_color_manual(name=NULL,values = cluster_colors) +
    labs(title = "Bootstrap distributions of GMM cluster means",
         x = "Mean value", y = "Frequency") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold",size=13),
          strip.text = element_text(size = 11, face = "bold"),
          legend.text=element_text(size=11)
    ))


# Distribution of probabilities for all observations aligning to each of the three clusters
probabilities <- model$z 
colnames(probabilities) <- paste0('C', 1:2)

probabilities <- probabilities %>%
  as.data.frame() %>%
  mutate(id = row_number()) %>%
  tidyr::gather(cluster, probability, -id)

custom_labels <- c("C1" = "Cluster 1",
                   "C2" = "Cluster 2")

(probplot<-ggplot(probabilities, aes(probability)) +
    geom_histogram(aes(fill=cluster),bins=50) +
    facet_wrap(~ cluster, nrow = 3, labeller = labeller(cluster = custom_labels),scales="free_y") + 
    labs(title = "Distribution of probabilities by cluster", x = "Probability", y = "Count" 
    ) +
    theme_minimal() + 
    colorfill2+
    apatheme+
    theme(strip.text = element_text(face = "bold", size = 12),
          axis.title = element_text(size = 14),
          plot.title = element_text(hjust=0.5,face="bold"),
          legend.position = "none")
)

# Get cluster assignment for each id
clusters <- model$classification
parameters <- model$parameters

# Assign clusters to original df
mot$cluster <- clusters
clustered_mot <- data.frame(id = mot2$id, cluster = clusters, mot2[, -1])

## Visualisation
(number<-fviz_mclust(mc, "BIC", palette = "jco")+
    ggtitle("Model Selection", 
            subtitle = "Best model: VEV\nOptimal clusters: n = 2")+
    theme(plot.title=element_text(face="bold",hjust=0),
          plot.subtitle=element_text(hjust=0))
)

# Classification: plot showing the clustering 
(class <- fviz_mclust(model, "classification", geom = "point", pointsize = 1.5)+
    colorfill2+
    labs(color = NULL, fill = NULL,shape=NULL)+
    theme(plot.title=element_text(face="bold",hjust=0),
          plot.subtitle=element_text(hjust=0))
)

# Classification uncertainty
(uncer<-fviz_mclust(model, "uncertainty")+
    colorfill2+
    labs(shape=NULL,color = NULL, fill = NULL))+
  theme(plot.title=element_text(face="bold",hjust=0),
        plot.subtitle=element_text(hjust=0))

(panel <- number + class + uncer +
  plot_annotation(tag_levels = 'A',title = NULL)+
  plot_layout(ncol = 3) &
  theme(legend.position = "bottom",
        legend.direction="vertical",
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.5, 'cm'),
        plot.tag = element_text(size = 12,face = 'bold'),
        plot.tag.position = "topleft",
        plot.title=element_text(face='bold'))
)

# Density plots of var
df2<-clustered_mot%>%
  dplyr::select(id,cluster)%>%
  left_join(df,by="id")

long_data<-df2%>%
  select(id,cluster,df1scale, df2scale, df3scale,
         cwi1scale, cwi2scale, cwi3scale,
         tmt2scale, tmt3scale, tmt4scale
         )%>% #
  pivot_longer(cols = c(df1scale, df2scale, df3scale,
                        cwi1scale, cwi2scale, cwi3scale,
                        tmt2scale, tmt3scale, tmt4scale
                        ), names_to = "measure", values_to = "value")

long_data <- long_data %>%
  distinct(id, measure, .keep_all = TRUE)%>%
  dplyr::select(-id)

long_data$cluster <- factor(long_data$cluster, levels = c("1", "2"),
                            labels = c("Cluster 1",
                                       "Cluster 2")
)

# Create density plots for each variable by cluster level
median_values <- long_data %>%
  group_by(measure, cluster) %>%
  summarise(median_value = median(value, na.rm = TRUE))%>%
  ungroup()


density_plots <- long_data %>%
  mutate(measure = recode(measure, !!!label_rename2)) %>%
  ggplot(aes(x = value, after_stat(..density..), fill = as.factor(cluster))) +
  geom_density(alpha = 0.7) + 
  facet_wrap(~ measure, scales = "free") +
  labs(x = "Score", y = "Density") +
  geom_vline(data = median_values %>% mutate(measure = recode(measure, !!!label_rename2)),
             aes(xintercept = median_value, color = as.factor(cluster)),
             linetype = "dashed", size = 0.8, show.legend = FALSE) +
  scale_fill_manual(name = NULL, values = c("#f2acd7", "#8E0152", "#5b0b6e", "#E3B505", "red")) +
  scale_color_manual(name = NULL, values = c("#f2acd7", "#8E0152", "#5b0b6e", "#E3B505", "red")) +
  apatheme +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 2))) +
  theme(strip.background = element_rect(colour="white", fill="white"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title=element_text(size=12,face="bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom",
        strip.text.x = element_text(face = "bold", size = 13),
        legend.text = element_text(size = 14,face="bold"),
        legend.title = element_text(size = 13, face = "plain", hjust = 0.5)
  )
density_plots

# extract
means <- data.frame(model$parameters$mean) %>%
  rownames_to_column() %>%
  pivot_longer(cols = c(X1, X2), names_to = "Profile", values_to = "Mean") %>%
  mutate(Mean = round(Mean, 2))

means<-means %>%
  mutate(Profile = dplyr::recode(Profile, 
                                 X1 = "Cluster 1",
                                 X2 = "Cluster 2"))%>%
  rename("measure"="rowname")%>%
  transform(measure = reorder(measure, -Mean))

means <- means %>%
  mutate(Profile = factor(Profile, levels = means %>%
                            group_by(Profile) %>%
                            summarise(mean_value = mean(Mean, na.rm = TRUE)) %>%
                            arrange(mean_value) %>%
                            pull(Profile)))

profile_order <- c("Cluster 1",
                   "Cluster 2"
)
means$Profile <- factor(means$Profile, levels = profile_order)

(p <- ggplot(data = means, aes(x = Profile, y = as.numeric(Mean), group =as.factor(measure), fill = as.factor(measure))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black", alpha = 0.7) +
  scale_fill_paletteer_d(name = NULL,"RColorBrewer::PRGn",labels = label_rename2)+
  labs(x = NULL, y = "Standardized mean")+
  theme_minimal()+
  ggtitle("Standardized mean values of\nexecutive function measures by cluster")+
  apatheme+
  theme_bw(base_size = 14) +
  geom_hline(yintercept=0, size=0.3,color= "black")+
  theme(plot.title=element_text(size=13,face='bold',hjust=0.5),
        axis.text.x = element_text(size=13,angle = 0, hjust = 0.5,color="black",face="bold"), 
        axis.text.y = element_text(size=13,color="black"),
        axis.title=element_text(size=13,face="bold"),
        legend.position = "top",
        legend.text = element_text(face="bold",size=11), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y=element_line(size=(0.4)), 
        axis.ticks.x=element_line(size=(0.4)))+
  guides(fill = guide_legend(nrow = 2))
)

panel2 <- p/density_plots+
  plot_annotation(tag_levels = 'A',title = "")+
  plot_layout(ncol = 1, heights = c(1.2,1.6)) & theme(legend.position = "bottom")+#guides = "collect"
  theme(legend.direction = "vertical",
        plot.tag = element_text(size = 15,face = 'bold'),
        legend.text = element_text(size = 12),
        plot.tag.position = "topleft",
        plot.title=element_text(size=16,face='bold'))
panel2


# FIT INDICES
# Entropy (classification certainty)
probs <- model$z # posterior conditional probs
probs_map <- apply(probs, 1, max) # maximum a posteriori probs
clusters <- model$classification # cluster assignment for each obs
n <- model$n # number of obs
K <- model$G # number of latent profiles
E <- 1 + sum(probs * log(probs))/(n * log(K))
E

# Case-specific entropy contributions
Ei <- 1 + rowSums(probs * log(probs))/log(K)
sum(Ei)/n

df_entropy <- data.frame(clusters = as.factor(clusters), entropy = Ei)
df_entropy %>%
  group_by(clusters)%>%
  summarise(count = n(),
            mean = mean(entropy),
            sd = sd(entropy),
            min = min(entropy),
            max = max(entropy))

df_entropy$clusters <- factor(df_entropy$clusters, levels = c("1", "2"),
                              labels = c("Cluster 1",
                                         "Cluster 2")
)

(entropyplot<-ggplot(df_entropy, aes(y = clusters, x = entropy, fill = clusters)) +
    geom_density_ridges(stat = "binline", bins = 21,scale = 0.9, alpha = 0.8) +
    scale_x_continuous(breaks = seq(0, 1 ,by=0.2),limits = c(0, 1.05)) +
    scale_y_discrete(limits = rev(levels(factor(df_entropy$cluster)))) + 
    geom_vline(xintercept = E, lty = 2) +
    scale_fill_manual(name=NULL,values = c("#f2acd7", "#8E0152"))+
    ggtitle("Entropy")+
    labs(x = "Case-specific entropy contribution",y = "Latent cluster") +
    theme_ridges(center_axis_labels = TRUE) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          panel.spacing = unit(1, "lines"),
          strip.text.x = element_text(size = 8)))
 
# Average posterior probabilities by cluster:
df_AvePP <- data.frame(clusters = as.factor(clusters), pp = probs_map)
df_AvePP %>%
  group_by(clusters) %>%
  summarise(count = n(),
            mean = mean(pp),
            sd = sd(pp),
            min = min(pp),
            max = max(pp))


df_AvePP$clusters <- factor(df_AvePP$clusters, levels = c("1", "2"),
                            labels = c("Cluster 1",
                                       "Cluster 2"))
(appplot<-ggplot(df_AvePP, aes(y = clusters, x = pp, fill = clusters)) +
    geom_density_ridges(stat = "binline", bins = 21,scale = 0.9, alpha = 0.8) +
    scale_x_continuous(breaks = seq(0, 1, by=0.2),limits = c(0, 1.05)) +
    scale_y_discrete(limits = rev(levels(factor(df_AvePP$cluster)))) + 
    scale_fill_manual(name=NULL,values = c("#f2acd7", "#8E0152"))+
    ggtitle("Average posterior probabilities")+
    labs(x = "Probabilities", y = "Latent cluster") +
    theme_ridges(center_axis_labels = TRUE) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          panel.spacing = unit(1, "lines"),
          strip.text.x = element_text(size = 8)))
 
df2$position_cat <- ifelse(df2$position %in% c("Centre Back", "Left Wing-Back", "Right Wing-Back"), "Defender",
                            ifelse(df2$position %in% c("Central Midfielder", "Defensive Midfielder"), "Midfielder", 
                                   ifelse(df2$position %in% c("Striker", "Left Winger","Right Winger"), "Striker", 
                                          "Control"))
 )
                            
df2<-df2%>%distinct()
df2$cluster<-as.factor(df2$cluster)
df2$group<-as.factor(df2$group)
# group 1 = player, group 2 = control

model <- glm(cluster ~ group, family = binomial(),data = df2)
tab_model(model)

(plot_swedish <- df2 %>%
    mutate(group = factor(group, levels = c("Control", "Player")),
           cluster = factor(cluster)) %>%
  group_by(group, cluster) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = group, y = freq, fill = cluster)) +
  geom_col(position = "fill") +  
    colorfill2+
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Proportion", x = NULL, fill = "Cluster") +
  apatheme+
  theme(legend.text=element_text(size=13),
        axis.text.x = element_text(size=13),
        axis.title.y = element_text(size=13))+
  ggtitle("Cluster composition by\nplayer and control")
)

(plot_position <- df2 %>%
  mutate(cluster = factor(cluster),
         position_cat = factor(position_cat)) %>%
  group_by(position_cat, cluster) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = position_cat, y = freq, fill = cluster)) +
  geom_col(position = "fill", alpha = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
    colorfill2+
  labs(y = "Proportion", x = NULL, fill = "Cluster") +
  apatheme +
  theme(
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  ) +
  ggtitle("Cluster composition by\nposition category")
  )

df2 <- df2 %>%
  dplyr::select(id, cluster,position,position_cat, group,age, everything())

#==============================
# Per position (with and without controls)
#==============================
# Control as reference, with controls
model <- glm(cluster ~ position_cat, family = binomial(),data = df2)
tab_model(model)

# Defender as reference, without controls
df2$position_cat<-as.factor(df2$position_cat)
df2$position_cat <- relevel(df2$position_cat, ref = "Defender")

model <- glm(cluster ~ position_cat, family = binomial(),data = filter(df2,position_cat!="Control"))
tab_model(model)

# Midfielder as reference, without controls
df2$position_cat <- relevel(df2$position_cat, ref = "Midfielder")
model <- glm(cluster ~ position_cat,family = binomial(),data = filter(df2, position_cat != "Control"))
tab_model(model)

pred_data <- data.frame(position_cat = unique(df2$position_cat))

# Bootstrap settings
set.seed(123)
n_boot <- 1000
boot_preds <- list()

for (i in 1:n_boot) {
  boot_sample <- df2[sample(nrow(df2), replace = TRUE), ]
  boot_model <- glm(cluster ~ position_cat, data = boot_sample, family = binomial)
  boot_pred <- predict(boot_model, newdata = pred_data, type = "response")
  boot_preds[[i]] <- boot_pred
}

# Combine bootstrap predictions into matrix
boot_matrix <- do.call(rbind, boot_preds)  # n_boot x nrow(pred_data)

# Calculate mean and 95% CI for each position category
mean_probs <- apply(boot_matrix, 2, mean)
lower_probs <- apply(boot_matrix, 2, quantile, probs = 0.025)
upper_probs <- apply(boot_matrix, 2, quantile, probs = 0.975)

pred_data_long <- pred_data %>%
  mutate(prob = mean_probs,
         lower = lower_probs,
         upper = upper_probs)

# Create predicted probabilities for both clusters (Cluster 1 and Cluster 2)
# Cluster 1 prob = 1 - Cluster 2 prob (since binary)
pred_data_long <- pred_data_long %>%
  mutate(
    Cluster2_prob = prob,
    Cluster1_prob = 1 - prob
  ) %>%
  select(position_cat, Cluster1_prob, Cluster2_prob) %>%
  pivot_longer(cols = starts_with("Cluster"), names_to = "Cluster", values_to = "prob") %>%
  mutate(Cluster = ifelse(Cluster == "Cluster1_prob", "Cluster 1", "Cluster 2"))

pred_data_long$position_cat <- factor(pred_data_long$position_cat,
                                      levels = c("Control", "Defender", "Midfielder", "Striker"))

(positionpredplot <- ggplot(pred_data_long,
                           aes(axis1 = Cluster, axis2 = position_cat, y = prob)) +
  geom_alluvium(aes(fill = position_cat), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), angle = 90, color = "white", size = 3) +
  scale_fill_manual(values = c("#E3B505","#f2acd7", "#8E0152", "#542788")) +
  labs(
    title = "Predicted probability of cluster by position",
    y = NULL,
    x = NULL
  ) +
  guides(fill = guide_legend(title = NULL, nrow = 1, byrow = TRUE)) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 11.5),
    plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
)
