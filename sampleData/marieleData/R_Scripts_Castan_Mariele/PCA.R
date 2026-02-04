library(ggplot2)
library(patchwork)
library(ggrepel)
library(factoextra)



##########Creating subsets for palsa and thermokarst
Palsa <- subset(Daten_PCA, Daten_PCA$site == "P")
Thermokarst <- subset(Daten_PCA, Daten_PCA$site == "T")




#######################-----------------PCA----------##############################
# Colours
var_colors <- c(
  "0" = "#440154", "1" = "#3B528B", "2" = "#21908C",
  "3" = "#5DC863", "4" = "#FDE725", "5" = "#FF6F00"
)

arrow_scale <- 8
label_scale <- 8

#----------------------------Palsa---------------------------#
colnames(Palsa)[6:28] <- c(
  "N", "C", "H", "S", "O", "C/N", "x", "H/C", "O/C",
  "X", "X", "CUE", "X", "respiration", "X", "growth",
  "X", "MBC", "X", "X", "X", "X", "turnover time")

Palsa$depth <- factor(Palsa$depth, levels = names(var_colors))

pca_p <- prcomp(Palsa[, c(6,7,8,9,10,11,13,14,17,19,21,23,28)],
                center = TRUE, scale. = TRUE)

scores_p <- as.data.frame(pca_p$x[, 1:2])
colnames(scores_p) <- c("PC1", "PC2")
scores_p$depth <- Palsa$depth

loadings_p <- as.data.frame(pca_p$rotation[, 1:2])
colnames(loadings_p) <- c("PC1", "PC2")
loadings_p$var <- rownames(loadings_p)


p <- ggplot() +
  geom_point(data = scores_p, aes(x = PC1, y = PC2, fill = depth),
             shape = 21, colour = "black", size = 4, stroke = 0.5) +
  scale_fill_manual(values = var_colors, drop = TRUE) +
  geom_segment(data = loadings_p,
               aes(x = 0, y = 0, xend = PC1 * arrow_scale, yend = PC2 * arrow_scale),
               arrow = arrow(length = unit(0.2, "cm")), colour = "black", linewidth = 1) +
  geom_text_repel(data = loadings_p,
                  aes(x = PC1 * label_scale, y = PC2 * label_scale, label = var),
                  size = 5, max.overlaps = Inf, force = 2, segment.color = NA) +
  theme_bw() +
  labs(title = "Palsa",
       x = paste0("PC1 (", round(100 * summary(pca_p)$importance[2,1], 1), "%)"),
       y = paste0("PC2 (", round(100 * summary(pca_p)$importance[2,2], 1), "%)"),
       fill = "depth section") +
  theme(
    legend.position = "bottomleft",
    plot.title = element_text(size = 15, hjust = 0.5),
    axis.title = element_text(size = 15),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )



#----------------------------Thermokarst---------------------------#
colnames(Thermokarst)[6:28] <- c(
  "N", "C", "H", "S", "O", "C/N", "x", "H/C", "O/C",
  "X", "X", "CUE", "X", "respiration", "X", "growth",
  "X", "MBC", "X", "X", "X", "X", "turnover time")

Thermokarst$depth <- factor(Thermokarst$depth, levels = names(var_colors))

pca_t <- prcomp(Thermokarst[, c(6,7,8,9,10,11,13,14,17,19,21,23,28)],
                center = TRUE, scale. = TRUE)

scores_t <- as.data.frame(pca_t$x[, 1:2])
colnames(scores_t) <- c("PC1", "PC2")
scores_t$depth <- Thermokarst$depth

loadings_t <- as.data.frame(pca_t$rotation[, 1:2])
colnames(loadings_t) <- c("PC1", "PC2")
loadings_t$var <- rownames(loadings_t)

t <- ggplot() +
  geom_point(data = scores_t, aes(x = PC1, y = PC2, fill = depth),
             shape = 21, colour = "black", size = 4, stroke = 0.5) +
  scale_fill_manual(values = var_colors, drop = TRUE) +
  geom_segment(data = loadings_t,
               aes(x = 0, y = 0, xend = PC1 * arrow_scale, yend = PC2 * arrow_scale),
               arrow = arrow(length = unit(0.2, "cm")), colour = "black", linewidth = 1) +
  geom_text_repel(data = loadings_t,
                  aes(x = PC1 * label_scale, y = PC2 * label_scale, label = var),
                  size = 5, max.overlaps = Inf, force = 2, segment.color = NA) +
  theme_bw() +
  labs(title = "Thermokarst",
       x = paste0("PC1 (", round(100 * summary(pca_t)$importance[2,1], 1), "%)"),
       y = paste0("PC2 (", round(100 * summary(pca_t)$importance[2,2], 1), "%)"),
       fill = "depth section") +
  theme(
    legend.position = "bottomleft",
    plot.title = element_text(size = 15, hjust = 0.5),
    axis.title = element_text(size = 15),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )


#------------------------Combine both plots-----------------------------------#

combined <- p + t +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.key.size = unit(0.7, "cm"),
  )

print(combined)


#####################---------------Testing significances------------####################

#------------Palsa
set.seed(42)

orig_data <- Palsa[, c(6,7,8,9,10,11,13,14,17,19,21,23,28)]

obs_inertia <- sum(pca_p$sdev[1:2]^2)

nrepet <- 999

perm_inertia <- replicate(nrepet, {
  d_perm <- apply(orig_data, 2, sample)
  p_perm <- prcomp(d_perm, center = TRUE, scale. = TRUE)
  sum(p_perm$sdev[1:2]^2)
})

# p-value
p_value <- (sum(perm_inertia >= obs_inertia) + 1) / (nrepet + 1)
cat("Beobachtete Inertie:", round(obs_inertia, 3), "\n")
cat("Permutationstest p-Wert:", round(p_value, 4), "\n")

# Influence of axes
get_eigenvalue(pca_p)

# Contribution of variables
fviz_contrib(pca_p, choice = "var", axes = 1:2)




#------------Thermokarst
set.seed(42)

orig_data <- Thermokarst[, c(6,7,8,9,10,11,13,14,17,19,21,23,28)]

obs_inertia <- sum(pca_t$sdev[1:2]^2)

nrepet <- 999

perm_inertia <- replicate(nrepet, {
  d_perm <- apply(orig_data, 2, sample)
  p_perm <- prcomp(d_perm, center = TRUE, scale. = TRUE)
  sum(p_perm$sdev[1:2]^2)
})

# p-value
p_value <- (sum(perm_inertia >= obs_inertia) + 1) / (nrepet + 1)
cat("Beobachtete Inertie:", round(obs_inertia, 3), "\n")
cat("Permutationstest p-Wert:", round(p_value, 4), "\n")

# Influence of axes
get_eigenvalue(pca_t)

# Contribution of variables
fviz_contrib(pca_t, choice = "var", axes = 1:2)




