library(ggplot2)
library(patchwork)
library(ggtext)



#####################################------CUE---######################################
neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

Daten$depth <- factor(Daten$depth, levels = c(0,1,2,3,4,5))


CUE_plot <- ggplot(Daten, aes(x = depth, y = cue, fill = oxygen)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 1,
               position = position_dodge(width = 0.75)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  labs(x = "depth section",
       y = "CUE") +
  scale_fill_manual(name = NULL,
                    values = c("O2" = "lightblue", "N2" = "orange"),
                    labels = c("O2" = "oxic", "N2" = "anoxic")) + 
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.position = "none")

print(CUE_plot)



#########################---------------MBC--------------############################

neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

Daten$depth <- factor(Daten$depth, levels = c(0,1,2,3,4,5))


MBC_plot <- ggplot(Daten, aes(x = depth, y = mbcug, fill = oxygen)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 1,
               position = position_dodge(width = 0.75)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  labs(x = "depth section",
       y = expression(MBC~"("*mu*g~C~g~peat^{-1}*")")) +
  scale_fill_manual(name = NULL,
                    values = c("O2" = "lightblue", "N2" = "orange"),
                    labels = c("O2" = "oxic", "N2" = "anoxic")) + 
  scale_y_log10() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.position = "none")

print(MBC_plot)



##########################-------------Growth-----------#############################

neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

Daten$depth <- factor(Daten$depth, levels = c(0,1,2,3,4,5))


Growth_plot <- ggplot(Daten, aes(x = depth, y = cprodug, fill = oxygen)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 1,
               position = position_dodge(width = 0.75)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  labs(x = "depth section",
       y = expression(Microbial~growth~"("*mu*g~C~g~peat^{-1}~d^{-1}*")")) +
  scale_fill_manual(name = NULL,
                    values = c("O2" = "lightblue", "N2" = "orange"),
                    labels = c("O2" = "oxic", "N2" = "anoxic")) + 
  scale_y_log10() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.position = "none")

print(Growth_plot)



#####################----------Respiration---------------###############################

neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

Daten$depth <- factor(Daten$depth, levels = c(0,1,2,3,4,5))


Respiration_plot <- ggplot(Daten, aes(x = depth, y = respirationa, fill = oxygen)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 1,
               position = position_dodge(width = 0.75)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  labs(x = "depth section",
       y = expression(Respiration~"("*mu*g~CO[2]-C~g~peat^{-1}~d^{-1}*")")) +
  scale_fill_manual(name = NULL,
                    values = c("O2" = "lightblue", "N2" = "orange"),
                    labels = c("O2" = "oxic", "N2" = "anoxic")) + 
  scale_y_log10() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.position = "bottom")

print(Respiration_plot)




#####################---------turnover time----------##############################

neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

Daten$depth <- factor(Daten$depth, levels = c(0,1,2,3,4,5))


turnover_plot <- ggplot(Daten, aes(x = depth, y = turnover, fill = oxygen)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 1,
               position = position_dodge(width = 0.75)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  labs(x = "depth section",
       y = "Turnover time (d)") +
  scale_fill_manual(name = NULL,
                    values = c("O2" = "lightblue", "N2" = "orange"),
                    labels = c("O2" = "oxic", "N2" = "anoxic")) + 
  scale_y_log10() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.position = "bottom")

print(turnover_plot)


######################--------------Combining all plots----------#####################

all_plots <- CUE_plot + MBC_plot + Growth_plot + Respiration_plot + turnover_plot

all_plots+ plot_layout(ncol = 2)




############################-----------C---------------#################################

neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

# Data frame with significance letters
sig_letters <- data.frame(
  site = c(rep("P", 5), rep("T", 5)),
  depth = factor(c(1, 2, 3, 4, 5,
                   0, 1, 2, 4, 5)),
  C = c(530, 525, 523, 520, 515,
        470, 537, 530, 510, 509),
  label = c("Aa", "Aa", "Aa", "Aa", "Aa",
            "Ba", "Aa", "Aa", "Aa", "Aa")
)

# Creating the plot
C_plot <- ggplot(Daten, aes(x = factor(depth), y = Ca, fill = site)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 2,
               position = position_dodge(width = 0.8)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  geom_text(data = sig_letters,
            aes(x = depth, y = C, label = label),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 3.5) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  scale_fill_manual(values = c("P" = "#92D050", "T" = "#FFC000")) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)) +
  labs(x = "depth section",
       y = expression("C (mg g peat "^{-1}*")"))

print(C_plot)




#####################-----------N--------##############################################
neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

# Data frame with significance letters
sig_letters <- data.frame(
  site = c(rep("P", 5), rep("T", 5)),
  depth = factor(c(1, 2, 3, 4, 5,
                   0, 1, 2, 4, 5)),
  N = c(21.5, 25, 27, 28.5, 25.5,
        14.5, 26, 25, 21, 20),
  label = c("Aa", "Aa", "Aa", "Aa", "Aa",
            "Ba", "Aa", "Aa", "Aa", "Aa")
)


# Creating the plot
N_plot <- ggplot(Daten, aes(x = factor(depth), y = Na, fill = site)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 2,
               position = position_dodge(width = 0.8)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  geom_text(data = sig_letters,
            aes(x = depth, y = N, label = label),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 3.5) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  scale_fill_manual(values = c("P" = "#92D050", "T" = "#FFC000")) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)) +
  labs(x = "depth section",
       y = expression("N (mg g peat "^{-1}*")"))

print(N_plot)




########################----------------H-----------------##############################
neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

# Data frame with significance letters
sig_letters <- data.frame(
  site = c(rep("P", 5), rep("T", 5)),
  depth = factor(c(1, 2, 3, 4, 5,
                   0, 1, 2, 4, 5)),
  H = c(67, 66.7, 65.5, 63, 62.5,
        63.5, 67.7, 65.7, 60.7, 61.5),
  label = c("Aa", "Aa", "Aa", "Aa", "Aa",
            "ABa", "Aa", "Aa", "Ba", "Ba")
)

# Creating the plot
H_plot <- ggplot(Daten, aes(x = factor(depth), y = Ha, fill = site)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 2,
               position = position_dodge(width = 0.8)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  geom_text(data = sig_letters,
            aes(x = depth, y = H, label = label),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 3.5) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  scale_fill_manual(values = c("P" = "#92D050", "T" = "#FFC000")) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)) +
  labs(x = "depth section",
       y = expression("H (mg g peat "^{-1}*")"))

print(H_plot)



######################---------------O---------------#################################
neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

# Data frame with significance letters
sig_letters <- data.frame(
  site = c(rep("P", 5), rep("T", 5)),
  depth = factor(c(1, 2, 3, 4, 5,
                   0, 1, 2, 4, 5)),
  O = c(430, 435, 385, 395, 375,
        450, 420, 370, 380, 385),
  label = c("Aa", "Aa", "Aa", "Aa", "Aa",
            "Aa", "Ba", "Ba", "Ba", "Ba")
)

# Creating the plot
O_plot <- ggplot(Daten, aes(x = factor(depth), y = Oa, fill = site)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 2,
               position = position_dodge(width = 0.8)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  geom_text(data = sig_letters,
            aes(x = depth, y = O, label = label),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 3.5) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  scale_fill_manual(values = c("P" = "#92D050", "T" = "#FFC000")) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)) +
  labs(x = "depth section",
       y = expression("O (mg g peat "^{-1}*")"))

print(O_plot)



#######################---------------S--------------------###############################
neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

# Data frame with significance letters
sig_letters <- data.frame(
  site = c(rep("P", 5), rep("T", 5)),
  depth = factor(c(1, 2, 3, 4, 5,
                   0, 1, 2, 4, 5)),
  S = c(2.5, 5, 7.7, 7.4, 10.5,
        3, 5.5, 5.25, 7.6, 7.25),
  label = c("Ab", "Aa", "Aa", "Aa", "Aa",
            "Ba", "ABa", "ABa", "Aa", "ABa")
)

# Creating the plot
S_plot <- ggplot(Daten, aes(x = factor(depth), y = S, fill = site)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 2,
               position = position_dodge(width = 0.8)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  geom_text(data = sig_letters,
            aes(x = depth, y = S, label = label),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 3.5) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  scale_fill_manual(values = c("P" = "#92D050", "T" = "#FFC000")) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)) +
  labs(x = "depth section",
       y = expression("S (mg g peat "^{-1}*")"))

print(S_plot)



##########################-----------------C/N----------------########################
neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

# Data frame with significance letters
sig_letters <- data.frame(
  site = c(rep("P", 5), rep("T", 5)),
  depth = factor(c(1, 2, 3, 4, 5,
                   0, 1, 2, 4, 5)),
  CN = c(75, 51, 31, 38.5, 31.5,
        100, 36.5, 27, 35, 35),
  label = c("Aa", "Aa", "Aa", "Aa", "Aa",
            "Aa", "BCa", "Ca", "BCa", "Ba")
)

# Creating the plot
CN_plot <- ggplot(Daten, aes(x = factor(depth), y = CNa, fill = site)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 2,
               position = position_dodge(width = 0.8)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  geom_text(data = sig_letters,
            aes(x = depth, y = CN, label = label),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 3.5) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  scale_fill_manual(values = c("P" = "#92D050", "T" = "#FFC000")) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)) +
  labs(x = "depth section",
       y = "C/N")

print(CN_plot)




#####################---------------H/C--------------################################
neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

# Data frame with significance letters
sig_letters <- data.frame(
  site = c(rep("P", 5), rep("T", 5)),
  depth = factor(c(1, 2, 3, 4, 5,
                   0, 1, 2, 4, 5)),
  HC = c(1.53, 1.55, 1.52, 1.465, 1.455,
         1.66, 1.56, 1.5, 1.44, 1.45),
  label = c("Aa", "Aa", "ABa", "Ba", "Ba",
            "Aa", "Ba", "Ca", "Da", "Da")
)

# Creating the plot
HC_plot <- ggplot(Daten, aes(x = factor(depth), y = HC, fill = site)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 2,
               position = position_dodge(width = 0.8)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  geom_text(data = sig_letters,
            aes(x = depth, y = HC, label = label),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 3.5) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  scale_fill_manual(values = c("P" = "#92D050", "T" = "#FFC000")) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)) +
  labs(x = "depth section",
       y = "H/C")

print(HC_plot)



####################----------------O/C----------------##############################
neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

# Data frame with significance letters
sig_letters <- data.frame(
  site = c(rep("P", 5), rep("T", 5)),
  depth = factor(c(1, 2, 3, 4, 5,
                   0, 1, 2, 4, 5)),
  OC = c(0.67, 0.69, 0.59, 0.61, 0.595,
         0.755, 0.67, 0.54, 0.57, 0.575),
  label = c("Aa", "Aa", "Aa", "Aa", "Aa",
            "Aa", "Ba", "Ba", "Ba", "Ba")
)

# Creating the plot
OC_plot <- ggplot(Daten, aes(x = factor(depth), y = OC, fill = site)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 2,
               position = position_dodge(width = 0.8)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  geom_text(data = sig_letters,
            aes(x = depth, y = OC, label = label),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 3.5) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  scale_fill_manual(values = c("P" = "#92D050", "T" = "#FFC000")) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)) +
  labs(x = "depth section",
       y = "O/C")

print(OC_plot)


###############------------Combining all element plots-################################
C_plot + N_plot + H_plot + O_plot + S_plot + CN_plot + HC_plot + OC_plot






###############--------------C uptake---------###################################
neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

Daten$depth <- factor(Daten$depth, levels = c(0,1,2,3,4,5))


ggplot(Daten, aes(x = depth, y = cuptakeugd, fill = oxygen)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 1,
               position = position_dodge(width = 0.75)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  labs(x = "depth section",
       y = expression(C~uptake~(mu*g~C~g~peat^{-1}~d^{-1}))) +
  scale_fill_manual(name = NULL,
                    values = c("O2" = "lightblue", "N2" = "orange"),
                    labels = c("O2" = "oxic", "N2" = "anoxic")) + 
  scale_y_log10() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.position = "bottom")




################-------------qCO2--------------######################################
neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

Daten$depth <- factor(Daten$depth, levels = c(0,1,2,3,4,5))


ggplot(Daten, aes(x = depth, y = qco2, fill = oxygen)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 1,
               position = position_dodge(width = 0.75)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  labs(x = "depth section",
       y = expression(qCO[2]~(mu*g~CO[2] - C~mg~MBC^{-1}~h^{-1}))) +
  scale_fill_manual(name = NULL,
                    values = c("O2" = "lightblue", "N2" = "orange"),
                    labels = c("O2" = "oxic", "N2" = "anoxic")) + 
  scale_y_log10() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.position = "bottom")



################---------------qGrowth----------#####################################
neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

Daten$depth <- factor(Daten$depth, levels = c(0,1,2,3,4,5))


ggplot(Daten, aes(x = depth, y = qgrowth, fill = oxygen)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 1,
               position = position_dodge(width = 0.75)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  labs(x = "depth section",
       y = expression(qGrowth~(mu*g~C~mg~MBC^{-1}~h^{-1}))) +
  scale_fill_manual(name = NULL,
                    values = c("O2" = "lightblue", "N2" = "orange"),
                    labels = c("O2" = "oxic", "N2" = "anoxic")) + 
  scale_y_log10() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.position = "bottom")




##################------DNA concentration---------####################################
neue_labels <- c("P" = "Palsa", "T" = "Thermokarst")

Daten$depth <- factor(Daten$depth, levels = c(0,1,2,3,4,5))


ggplot(Daten, aes(x = depth, y = dnam, fill = oxygen)) +
  geom_boxplot(outlier.shape = 16,
               outlier.size = 1,
               position = position_dodge(width = 0.75)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               linetype = "solid",
               color = "black",
               position = position_dodge(width = 0.75)) +
  facet_wrap(~ site, labeller = as_labeller(neue_labels)) +
  labs(x = "depth section",
       y = expression(DNA[PP]~(mu*g~g~peat^{-1}))) +
  scale_fill_manual(name = NULL,
                    values = c("O2" = "lightblue", "N2" = "orange"),
                    labels = c("O2" = "oxic", "N2" = "anoxic")) + 
  scale_y_log10() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.position = "bottom")



