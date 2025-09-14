#==============================
# Working title: Psychological fingerprints of elite football playing positions
# Author: Leonie JT Balter
# Date: Sep 2025
#==============================

#==============================
# Cluster analysis using mclust 
#==============================
mot2<-df%>%
  dplyr::select(id,neuroticism,extroversion,openness,agreeableness,conscientiousness,
                simple_manual_reaction_time,digitspan_combined,tower_of_hanoi_moves_,d_kefs_design_fluency_score_overall,
                d_kefs_cwi_overall,d_kefs_tmt_overall)%>%
  na.omit()

mot <- mot2[, -1]

# Scale
mot <- mot %>%
  na.omit() %>%
  mutate_all(list(scale))

set.seed(123)
mc<-Mclust(mot,prior = priorControl()) 
summary(mc)

# Including a prior distribution over the mixture parameters is an effective way to avoid singularities and degeneracies in maximum likelihood estimation. 
# Can help to prevent overfitting and improve model performance.
BIC <- mclustBIC(mot,prior = priorControl())
summary(BIC)

# Also check the AIC 
IC1 <- Mclust(mot, G=1)
IC2 <- Mclust(mot, G=2)
IC3 <- Mclust(mot, G=3)
IC4 <- Mclust(mot, G=4)

(aic1 <- 2*IC1$df - 2*IC1$loglik)
(aic2 <- 2*IC2$df - 2*IC2$loglik)
(aic3 <- 2*IC3$df - 2*IC3$loglik)
(aic4 <- 2*IC4$df - 2*IC4$loglik)

# Like BIC but penalizes model uncertainty (posterior entropy).
ICL <- mclustICL(mot)
summary(ICL)

# Select model
model <- Mclust(mot, modelNames = "VEI", G = 3, x = BIC)
summary(model, parameters=TRUE)

# Do some model checks
# Get probabilities, means, variances
summary(model, parameters = FALSE)

# Check how many of n = 247 have uncertainty > 50%? (equally likely being assigned to another cluster)
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

uncertainty %>%
  group_by(cluster) %>%
  filter(uncertainty > 0.50) %>%
  ggplot(aes(uncertainty, reorder(id, uncertainty))) +
  geom_point() +
  labs(title = "Participants with high classification uncertainty ( > 50%)") +
  facet_wrap(~ cluster, scales = 'free_y', nrow = 1)+
  apatheme

# Bootstrapping
bootstrap<-MclustBootstrap(model, nboot=1000, method="bs") #”bs” for nonparametric bootstrap
summary(bootstrap,what="se")

# Extract bootstrapped mixing proportions
bootstrap_df <- as_tibble(bootstrap$pro, .name_repair = "unique") %>%
  pivot_longer(cols = everything(), names_to = "cluster", values_to = "proportion")

#pdf("FIGURES/bootstrap_gmm_prop.pdf", width = 6, height = 2.8)
(bootstrap_gmm_means2<-ggplot(bootstrap_df, aes(x = proportion, color=cluster,fill = cluster)) +
    geom_histogram(bins = 30, alpha = 0.6) +
    # geom_vline(data = component_means, aes(color=cluster,xintercept = mean_value),linetype = "dashed", size = 1) +
    colorfill+
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

component_means <- bootstrap_means_df_long %>%
  group_by(cluster,measure) %>%
  summarise(mean_value = mean(mean, na.rm = TRUE))

component_means$cluster<-as.factor(component_means$cluster)
bootstrap_means_df_long$measure <- label_rename[bootstrap_means_df_long$measure]
component_means$measure <- label_rename[component_means$measure]

# Estimated means
means <- data.frame(Profile = factor(1:model$G),
                    t(model$parameters$mean)) |>
  pivot_longer(cols = -1,
               names_to = "Variable",
               values_to = "Mean")

means$Variable <- factor(means$Variable, levels = colnames(model$data))

# add mixing probabilities corresponding to profiles
means <- means |> 
  add_column(MixPro = model$parameters$pro[means$Profile])
means

bootstrap_means_df_long$cluster <- factor(bootstrap_means_df_long$cluster,
                                          levels = c("1", "2", "3"),
                                          labels = c("Cluster 1", "Cluster 2", "Cluster 3"))

component_means$cluster <- factor(component_means$cluster,
                                  levels = c("1", "2", "3"),
                                  labels = c("Cluster 1", "Cluster 2", "Cluster 3"))

(meansplots <- ggplot(bootstrap_means_df_long, aes(x = mean, color = cluster, fill = cluster)) +
    geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
    facet_wrap(~measure, ncol = 1, scales = "free_y") +
    geom_vline(data = component_means, aes(color = cluster, xintercept = mean_value), linetype = "solid", size = 1.5) +
    scale_fill_manual(name=NULL,values = cluster_colors) +
    scale_color_manual(name=NULL,values = cluster_colors) +
    labs(title = "Bootstrap distributions of GMM cluster means",
         x = "Mean value", y = "Frequency") +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 12,hjust=0.5),
      axis.text.x = element_text(angle = 0, hjust = 1, face = "bold", color = "black"),
      axis.text.y = element_text(angle = 0, hjust = 1, face = "bold", color = "black"),
      strip.text = element_text(size = 12),
      axis.title.y = element_text(size = 13, face = "bold"),
      axis.title.x = element_text(size = 13, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid.major = element_line(color = "grey95")
    ))

# Distribution of probabilities for all observations aligning to each of the three clusters
probabilities <- model$z 
colnames(probabilities) <- paste0('C', 1:3)

probabilities <- probabilities %>%
  as.data.frame() %>%
  mutate(id = row_number()) %>%
  tidyr::gather(cluster, probability, -id)

custom_labels <- c("C1" = "Cluster 1",
                   "C2" = "Cluster 2",
                   "C3" = "Cluster 3")

(probplot<-ggplot(probabilities, aes(probability)) +
    geom_histogram(aes(fill=cluster),bins=50) +
    facet_wrap(~ cluster, nrow = 3, labeller = labeller(cluster = custom_labels),scales="free_y") + 
    labs(title = "Distribution of probabilities by cluster", x = "Probability", y = "Count" 
    ) +
    theme_minimal() + 
    colorfill+
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

# Visualise
(number<-fviz_mclust(mc, "BIC", palette = "jco")+
    ggtitle("Model Selection", 
            subtitle = "Best model: VEI\nOptimal clusters: n = 3")+
    theme(plot.title=element_text(face="bold",hjust=0),
          plot.subtitle=element_text(hjust=0))
)

# Classification: plot showing the clustering 
(class <- fviz_mclust(model, "classification", geom = "point", pointsize = 1.5)+
    colorfill+
    labs(color = NULL, fill = NULL,shape=NULL)+
    theme(plot.title=element_text(face="bold",hjust=0),
          plot.subtitle=element_text(hjust=0))
)

# Classification uncertainty
# Larger symbols indicate the more uncertain observations
(uncer<-fviz_mclust(model, "uncertainty")+
    colorfill+
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

# Density plots of traits
df2<-clustered_mot%>%
  dplyr::select(id,cluster)%>%
  left_join(df,by="id")

long_data<-df2%>%
  select(id,cluster,neuroticism,extroversion,openness,agreeableness,conscientiousness,
         simple_manual_reaction_time,digitspan_combined,tower_of_hanoi_moves_,d_kefs_design_fluency_score_overall,
         d_kefs_cwi_overall,d_kefs_tmt_overall)%>% #
  pivot_longer(cols = c(neuroticism,extroversion,openness,agreeableness,conscientiousness,
                        simple_manual_reaction_time,digitspan_combined,tower_of_hanoi_moves_,d_kefs_design_fluency_score_overall,
                        d_kefs_cwi_overall,d_kefs_tmt_overall), names_to = "measure", values_to = "value")

long_data <- long_data %>%
  distinct(id, measure, .keep_all = TRUE)%>%
  dplyr::select(-id)

long_data$cluster <- factor(long_data$cluster, levels = c("1", "2", "3"),
                            labels = c("Cluster 1",
                                       "Cluster 2",
                                       "Cluster 3")
)

long_data$measure <- factor(long_data$measure,
                            levels = c("conscientiousness","extroversion","openness","agreeableness","neuroticism",
                                       "d_kefs_design_fluency_score_overall","d_kefs_tmt_overall","d_kefs_cwi_overall",
                                       "simple_manual_reaction_time",  "tower_of_hanoi_moves_", "digitspan_combined"))

median_values <- long_data %>%
  group_by(measure, cluster) %>%
  summarise(median_value = median(value, na.rm = TRUE))%>%
  ungroup()

(density_plots <- long_data %>%
  mutate(measure = recode(measure, !!!label_rename)) %>%
  ggplot(aes(x = value, after_stat(..density..), fill = as.factor(cluster))) +
  geom_density(alpha = 0.7) + 
  facet_wrap(~ measure, scales = "free") +
  labs(x = "Score", y = "Density") +
  geom_vline(data = median_values %>% mutate(measure = recode(measure, !!!label_rename)),
             aes(xintercept = median_value, color = as.factor(cluster)),
             linetype = "dashed", size = 0.8, show.legend = FALSE) +
  scale_fill_manual(name = NULL, values = c("#1f995f", "#6e6868", "#5b0b6e", "#E3B505", "red")) +
  scale_color_manual(name = NULL, values = c("#1f995f", "#6e6868", "#5b0b6e", "#E3B505", "red")) +
  theme_bw(base_size = 14) + 
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
)

means <- data.frame(model$parameters$mean) %>%
  rownames_to_column() %>%
  pivot_longer(cols = c(X1, X2,X3), names_to = "Profile", values_to = "Mean") %>%
  mutate(Mean = round(Mean, 2))

means<-means %>%
  mutate(Profile = dplyr::recode(Profile, 
                                 X1 = "Cluster 1",
                                 X2 = "Cluster 2",
                                 X3 = "Cluster 3"))%>%
  rename("measure"="rowname")%>%
  transform(measure = reorder(measure, -Mean))

means <- means %>%
  mutate(Profile = factor(Profile, levels = means %>%
                            group_by(Profile) %>%
                            summarise(mean_value = mean(Mean, na.rm = TRUE)) %>%
                            arrange(mean_value) %>%
                            pull(Profile)))

profile_order <- c("Cluster 1",
                   "Cluster 2",
                   "Cluster 3"
)
means$Profile <- factor(means$Profile, levels = profile_order)
means$measure <- factor(means$measure,
                        levels = c("conscientiousness","extroversion","openness","agreeableness","neuroticism",
                                   "d_kefs_design_fluency_score_overall","d_kefs_tmt_overall","d_kefs_cwi_overall",
                                   "simple_manual_reaction_time",  "tower_of_hanoi_moves_", "digitspan_combined"))

(p <- ggplot(data = means, aes(x = Profile, y = Mean, group = measure, fill = measure)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black", alpha = 0.7) +
  scale_fill_paletteer_d(name = NULL, palette = "MexBrewer::Atentado",labels = label_rename)+
  labs(x = NULL, y = "Standardized mean")+
  theme_minimal()+
  apatheme+
  theme_bw(base_size = 14) +
  geom_hline(yintercept=0, size=0.3,color= "black")+
  theme(axis.text.x = element_text(size=13,angle = 0, hjust = 0.5,color="black",face="bold"), 
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

# Fit indices
# Entropy (classification certainty)
probs <- model$z # posterior conditional probs
probs_map <- apply(probs, 1, max) # maximum a posteriori probs
clusters <- model$classification
n <- model$n # number of obs
K <- model$G # number of clusters
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
            max = max(entropy))%>%
  ungroup()

#Entropy
df_entropy$clusters <- factor(df_entropy$clusters, levels = c("1", "2", "3"),
                              labels = c("Cluster 1",
                                         "Cluster 2",
                                         "Cluster 3")
)

(entropyplot<-ggplot(df_entropy, aes(y = clusters, x = entropy, fill = clusters)) +
    geom_density_ridges(stat = "binline", bins = 21,scale = 0.9, alpha = 0.7) +
    scale_x_continuous(breaks = seq(0, 1 ,by=0.1),limits = c(0, 1.05)) +
    scale_y_discrete(limits = rev(levels(factor(df_entropy$cluster)))) + 
    geom_vline(xintercept = E, lty = 2) +
    scale_fill_manual(name=NULL,values = c("#1f995f", "#6e6868", "#5b0b6e","#E3B505"))+
    ggtitle("Entropy")+
    labs(x = "Case-specific entropy contribution",y = "Latent cluster") +
    theme_ridges(center_axis_labels = TRUE) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          panel.spacing = unit(1, "lines"),
          strip.text.x = element_text(size = 8)))

# Average posterior probabilities by cluster
df_AvePP <- data.frame(clusters = as.factor(clusters), pp = probs_map)
df_AvePP %>%
  group_by(clusters)%>%
  summarise(count = n(),
            mean = mean(pp),
            sd = sd(pp),
            min = min(pp),
            max = max(pp))

df_AvePP$clusters <- factor(df_AvePP$clusters, levels = c("1", "2", "3"),
                            labels = c("Cluster 1",
                                       "Cluster 2",
                                       "Cluster 3"))
(appplot<-ggplot(df_AvePP, aes(y = clusters, x = pp, fill = clusters)) +
    geom_density_ridges(stat = "binline", bins = 21,scale = 0.9, alpha = 0.7) +
    scale_x_continuous(breaks = seq(0, 1, by=0.2),limits = c(0, 1.05)) +
    scale_y_discrete(limits = rev(levels(factor(df_AvePP$cluster)))) + 
    scale_fill_manual(name=NULL,values = c("#1f995f", "#6e6868", "#5b0b6e","#E3B505"))+
    ggtitle("Average posterior probabilities")+
    labs(x = "Probabilities", y = "Latent cluster") +
    theme_ridges(center_axis_labels = TRUE) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          panel.spacing = unit(1, "lines"),
          strip.text.x = element_text(size = 8)))