#==============================
# Working title: Psychological fingerprints of elite football playing positions
# Author: Leonie JT Balter
# Date: Sep 2025
#==============================

#==============================
# Some handy packages and functions
#==============================
pacman::p_load(dplyr,VIM,readxl,tidyverse,hms,ggpubr,sjPlot,openxlsx,mclust,factoextra,scales,patchwork,
               ggridges,RColorBrewer,dichromat,paletteer,factoextra,corrplot,viridis,reshape2,marginaleffects,nnet)

apatheme=theme_bw()+
  theme(strip.text=element_text(size=rel(1),colour="black"),
        strip.background = element_rect(linewidth = (1),fill="white"),
        axis.line=element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=11,colour="black"),
        legend.text = element_text(size = 10.5,colour="black"),
        legend.position ="bottom",
        axis.text.x = element_text(angle = 0, size=13,colour="black"), 
        axis.text.y = element_text(angle = 0, size=13,colour="black"), 
        axis.title.x = element_text(size=13,colour="black"), 
        axis.title.y = element_text(size=13,colour="black"), 
        axis.line.x = element_line(color="black", linewidth = 0.5), 
        axis.line.y = element_line(color="black", linewidth = 0.5),
        axis.ticks.y=element_line(linewidth=(0.75)), 
        axis.ticks.x=element_line(linewidth=(0.75)), 
        axis.title=element_text(colour="black",size=12),
        legend.key.size = unit(0.3, "cm"),
        legend.key.width =unit(0.4, 'cm'),
        legend.spacing = unit(0.2, 'cm'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = rel(1.2), hjust=0.5,face="bold",angle = 00,colour="black"))

label_rename <- c(
  "tower_of_hanoi_moves_" = "Tower of Hanoi",
  "neuroticism" = "Neuroticism",
  "agreeableness" = "Agreeableness",
  "simple_manual_reaction_time" = "Simple Reaction time",
  "conscientiousness" = "Conscientiousness",
  "extroversion" = "Extraversion",
  "digitspan_combined" = "Digit Span",
  "d_kefs_design_fluency_score_overall" = "Design fluency",
  "d_kefs_cwi_overall" = "Color-Word Interference",
  "openness" = "Openness",
  "d_kefs_tmt_overall" = "Trail Making Test"
)

label_rename2 <- c(
  "df1scale"="DF1","df2scale"="DF2","df3scale"="DF3",
  "cwi1scale"="CWI1","cwi2scale"="CWI2","cwi3scale"="CWI3",
  "tmt2scale"="TMT2","tmt3scale"="TMT3","tmt4scale"="TMT4")
  
colorfill <- list(
  scale_shape_manual(
    name = NULL,
    values = c(15, 16, 17),
    labels = c("Cluster 1", "Cluster 2", "Cluster 3")
  ),
  scale_color_manual(
    name = NULL,
    values = c("#1f995f", "#6e6868", "#5b0b6e"),
    labels = c("Cluster 1", "Cluster 2", "Cluster 3")
  ),
  scale_fill_manual(
    name = NULL,
    values = c("#1f995f", "#6e6868", "#5b0b6e"),
    labels = c("Cluster 1", "Cluster 2", "Cluster 3")
  )
)

cluster_colors <- c("Cluster 1" = "#1f995f", "Cluster 2" = "#6e6868", "Cluster 3" = "#5b0b6e")

colorfill2 <- list(
  scale_shape_manual(
    name = NULL,
    values = c(15, 16, 17),
    labels = c("Cluster 1 (n = 174)", "Cluster 2 (n = 124)", "Cluster 3")
  ),
  scale_color_manual(
    name = NULL,
    values = c("#f2acd7","#8E0152","#5b0b6e"),
    #  values = c("#5b0b6e", "#E3B505", "#6e6868"),
    labels = c("Cluster 1 (n = 174)", "Cluster 2 (n = 124)", "Cluster 3")
  ),
  scale_fill_manual(
    name = NULL,
    values = c("#f2acd7","#8E0152","#5b0b6e"),
    #  values = c("#F7A39A","#805B6D"),
    # values = c("#5b0b6e", "#E3B505", "#6e6868"),
    labels = c("Cluster 1 (n = 174)", "Cluster 2 (n = 124)", "Cluster 3")
  )
)
