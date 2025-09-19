library(tidyverse)
library(ggplot2)
library(dplyr)


# Figure 2
Fig2 <- read_csv("Figure_2.csv")
Fig2_A <- Fig2 %>%
  filter(subgroup %in% c("total","sex"))
Fig2_A$class <- factor(Fig2_A$class, levels = c("total","female","male"))

group.colors <- c(`total` = "#84C3B7", `female` = "#B7B2D0", `male` ="#EAAA60")

## panel a
panel_A <- ggplot(Fig2_A,aes(x=subgroup, y = RR,color = class,group = class)) +
  facet_grid(~factor(outcome, levels = c("All-cause","Substance use","Schizophrenia","Bipolar affective disorder","Depression","Anxiety",
                                         "Other","Unspecified")))+
  geom_errorbar(aes(x=class, ymin=RRlow,ymax=RRhigh,group = class),width = 0.15,position = position_dodge(width = 0.01),size = 0.5)+
  geom_point(aes(x = class, y = RR,group = class),position = position_dodge(width = 0.01),size = 1.5)+
  geom_hline(aes(yintercept=1),linetype="dashed",color = "#A6A9B6") +
  ggtitle('')+
  xlab("")+
  ylab("Relative Risk (95% CI)")+
  theme_bw()+
  scale_color_manual(values =group.colors)+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.text.x = element_blank(),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(), ## from here
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(), ## to here is used to customize the panel border when using facet_grid
        legend.text = element_text(size = 12,colour = "black",family = 'serif'),
        legend.title = element_text(size = 12,colour = "black",family = 'serif'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
panel_A


## panel b
Fig2_B <- Fig2 %>%
  filter(subgroup %in% c("built-up ratio"))
Fig2_B$class <- factor(Fig2_B$class, levels = c("low","medium","high"))
group.colors <- c(`low` = "#84C3B7", `medium` = "#B7B2D0", `high` ="#EAAA60")

panel_B <- ggplot(Fig2_B,aes(x=class, y = RR,color = class,group = class)) +
  facet_grid(~factor(outcome, levels = c("All-cause","Substance use","Schizophrenia","Bipolar affective disorder","Depression","Anxiety",
                                         "Other","Unspecified")))+
  geom_errorbar(aes(x=class, ymin=RRlow,ymax=RRhigh,group = class),width = 0.15,position = position_dodge(width = 0.01),size = 0.5)+
  geom_point(aes(x = class, y = RR,group = class),position = position_dodge(width = 0.01),size = 1.5)+
  geom_hline(aes(yintercept=1),linetype="dashed",color = "#A6A9B6") +
  ggtitle('')+
  xlab("")+
  ylab("Relative Risk (95% CI)")+
  theme_bw()+
  scale_color_manual(values =group.colors)+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.text.x = element_blank(),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(), ## from here
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(), ## to here is used to customize the panel border when using facet_grid
        legend.text = element_text(size = 12,colour = "black",family = 'serif'),
        legend.title = element_text(size = 12,colour = "black",family = 'serif'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
panel_B


## panel c
Fig2_C <- Fig2 %>%
  filter(subgroup %in% c("GDP"))
Fig2_C$class <- factor(Fig2_C$class, levels = c("low","medium","high"))
group.colors <- c(`low` = "#84C3B7", `medium` = "#B7B2D0", `high` ="#EAAA60")

panel_C <- ggplot(Fig2_C,aes(x=class, y = RR,color = class,group = class)) +
  facet_grid(~factor(outcome, levels = c("All-cause","Substance use","Schizophrenia","Bipolar affective disorder","Depression","Anxiety",
                                         "Other","Unspecified")))+
  geom_errorbar(aes(x=class, ymin=RRlow,ymax=RRhigh,group = class),width = 0.15,position = position_dodge(width = 0.01),size = 0.5)+
  geom_point(aes(x = class, y = RR,group = class),position = position_dodge(width = 0.01),size = 1.5)+
  geom_hline(aes(yintercept=1),linetype="dashed",color = "#A6A9B6") +
  ggtitle('')+
  xlab("")+
  ylab("Relative Risk (95% CI)")+
  theme_bw()+
  scale_color_manual(values =group.colors)+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.text.x = element_blank(),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(), ## from here
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(), ## to here is used to customize the panel border when using facet_grid
        legend.text = element_text(size = 12,colour = "black",family = 'serif'),
        legend.title = element_text(size = 12,colour = "black",family = 'serif'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
panel_C


## panel d
Fig2_D <- Fig2 %>%
  filter(subgroup %in% c("nonfire"))
Fig2_D$class <- factor(Fig2_D$class, levels = c("low","high"))
group.colors <- c(`low` = "#B7B2D0", `high` ="#EAAA60")

panel_D <- ggplot(Fig2_D,aes(x=class, y = RR,color = class,group = class)) +
  facet_grid(~factor(outcome, levels = c("All-cause","Substance use","Schizophrenia","Bipolar affective disorder","Depression","Anxiety",
                                         "Other","Unspecified")))+
  geom_errorbar(aes(x=class, ymin=RRlow,ymax=RRhigh,group = class),width = 0.15,position = position_dodge(width = 0.01),size = 0.5)+
  geom_point(aes(x = class, y = RR,group = class),position = position_dodge(width = 0.01),size = 1.5)+
  geom_hline(aes(yintercept=1),linetype="dashed",color = "#A6A9B6") +
  ggtitle('')+
  xlab("")+
  ylab("Relative Risk (95% CI)")+
  theme_bw()+
  scale_color_manual(values =group.colors)+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.text.x = element_blank(),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(), ## from here
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(1.5, "mm"),
        strip.background = element_blank(), ## to here is used to customize the panel border when using facet_grid
        legend.text = element_text(size = 12,colour = "black",family = 'serif'),
        legend.title = element_text(size = 12,colour = "black",family = 'serif'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
panel_D



# Figure 3
compare <- read_csv("Figure_3.csv")
compare$outcome <- factor(compare$outcome, levels = c("All-cause","Substance use","Schizophrenia","Bipolar affective disorder","Depression","Anxiety",
                                                      "Other","Unspecified"))

# panel b
scaleFUN <- function(x) sprintf("%.2f", x)
compare_mid <- compare[compare$class %in% c("RR"),]

panel_B <- ggplot(compare_mid,aes(x = group)) + 
  geom_errorbar(aes(ymin = estimate_low, ymax = estimate_high, color = group),width = 0.1,position = position_dodge(width = 0.2),size = 0.5)+
  geom_point(aes(x = group, y = estimate, color = group))+
  facet_wrap(~outcome,ncol = 8) +
  xlab('')+
  ylab(expression("Relative risk (95% CI)"))+
  scale_y_continuous(labels=scaleFUN) + 
  scale_fill_brewer(palette = "Pastel1")+
  scale_color_brewer(palette = "Pastel1")+
  geom_hline(yintercept = 1.000, color = "#9F8772", linetype = "dashed",size = 0.7)+
  theme_bw()+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        legend.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(), ## from here
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(), ## to here is used to customize the panel border when using facet_grid
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())  
panel_B


# panel c
compare_mid <- compare[compare$class %in% c("AF"),]
panel_C <- ggplot(compare_mid,aes(x = group)) + 
  geom_bar(aes(x = group, y = estimate, fill = group),width = 0.5,alpha = 0.88,stat="identity") + 
  geom_errorbar(aes(ymin = estimate_low, ymax = estimate_high, color = group),width = 0.1,position = position_dodge(width = 0.2),size = 0.5)+
  facet_wrap(~outcome,ncol = 8) +
  xlab('')+
  ylab(expression("Relative risk (95% CI)"))+
  scale_y_continuous(labels=scaleFUN) +
  scale_fill_brewer(palette = "Pastel1")+
  scale_color_brewer(palette = "Pastel1")+
  geom_hline(yintercept = 0.000, color = "#9F8772", linetype = "dashed",size = 0.7)+
  theme_bw()+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        legend.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(), ## from here
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(), ## to here is used to customize the panel border when using facet_grid
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
panel_C

