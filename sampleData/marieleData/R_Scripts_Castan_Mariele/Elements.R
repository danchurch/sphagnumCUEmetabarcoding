library(car)
library(emmeans)



###########################--------------C---------------#############################

############### Depth within the Palsa

df_palsa <- subset(Daten, site == "P")
model_palsa <- aov(Ca ~ depth, data = df_palsa)

residuen <- residuals(model_palsa)


## Homogeneity of variances
leveneTest(Ca ~ depth, data = df_palsa)
plot(fitted(model_palsa), residuals(model_palsa),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


##Normality
shapiro.test(residuen)

# QQ-Plot
qqnorm(residuen, main = "QQ-Plot der Residuen")
qqline(residuen, col = "red")

# Histogramm
hist(residuen, breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


##ANOVA
summary(model_palsa)




############### Depth within the Thermokarst

df_thermo <- subset(Daten, site == "T")
model_thermo <- aov(Ca ~ depth, data = df_thermo)

residuen <- residuals(model_thermo)


## Homogeneity of variances
leveneTest(Ca ~ depth, data = df_thermo)
plot(fitted(model_thermo), residuals(model_thermo),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


##Normality
shapiro.test(residuen)

# QQ-Plot
qqnorm(residuen, main = "QQ-Plot der Residuen")
qqline(residuen, col = "red")

# Histogramm
hist(residuen, breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


##ANOVA
summary(model_thermo)

emm <- emmeans(model_thermo, ~ depth)
pairs(emm, adjust = "tukey")



############## Comparision depths palsa vs. thermokarst

############## Depth section 1
df_depth1 <- subset(Daten, depth == "1")

## Homogeneity of variances
leveneTest(Ca ~ site, data = df_depth1)

## Normality
shapiro_results <- lapply(
  split(df_depth1$Ca, df_depth1$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(Ca ~ site, data = df_depth1, var.equal = TRUE)



############## Depth section 2
df_depth2 <- subset(Daten, depth == "2")

## Homogeneity of variances
leveneTest(Ca ~ site, data = df_depth2)

## Normality
shapiro_results <- lapply(
  split(df_depth2$Ca, df_depth2$site),
  shapiro.test
)
print(shapiro_results)

## Welch t-test
t.test(Ca ~ site, data = df_depth2, var.equal = FALSE)




############## Depth section 4
df_depth4 <- subset(Daten, depth == "4")

## Homogeneity of variances
leveneTest(Ca ~ site, data = df_depth4)

## Normality
shapiro_results <- lapply(
  split(df_depth4$Ca, df_depth4$site),
  shapiro.test
)
print(shapiro_results)

## Welch t-test
t.test(Ca ~ site, data = df_depth4, var.equal = FALSE)




############## Depth section 5
df_depth5 <- subset(Daten, depth == "5")

## Homogeneity of variances
leveneTest(Ca ~ site, data = df_depth5)

## Normality
shapiro_results <- lapply(
  split(df_depth5$Ca, df_depth5$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(Ca ~ site, data = df_depth5, var.equal = TRUE)





###########################--------------N---------------#############################

############### Depth within the Palsa

df_palsa <- subset(Daten, site == "P")
model_palsa <- aov(Na ~ depth, data = df_palsa)

residuen <- residuals(model_palsa)


## Homogeneity of variances
leveneTest(Na ~ depth, data = df_palsa)
plot(fitted(model_palsa), residuals(model_palsa),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


##Normality
shapiro.test(residuen)

# QQ-Plot
qqnorm(residuen, main = "QQ-Plot der Residuen")
qqline(residuen, col = "red")

# Histogramm
hist(residuen, breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


##ANOVA
summary(model_palsa)




############### Depth within the Thermokarst

df_thermo <- subset(Daten, site == "T")
model_thermo <- aov(Na ~ depth, data = df_thermo)

residuen <- residuals(model_thermo)


## Homogeneity of variances
leveneTest(Na ~ depth, data = df_thermo)
plot(fitted(model_thermo), residuals(model_thermo),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


##Normality
shapiro.test(residuen)

# QQ-Plot
qqnorm(residuen, main = "QQ-Plot der Residuen")
qqline(residuen, col = "red")

# Histogramm
hist(residuen, breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


##ANOVA
summary(model_thermo)

emm <- emmeans(model_thermo, ~ depth)
pairs(emm, adjust = "tukey")



############## Comparision depths palsa vs. thermokarst

############## Depth section 1
df_depth1 <- subset(Daten, depth == "1")

## Homogeneity of variances
leveneTest(Na ~ site, data = df_depth1)

## Normality
shapiro_results <- lapply(
  split(df_depth1$Na, df_depth1$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(Na ~ site, data = df_depth1, var.equal = TRUE)



############## Depth section 2
df_depth2 <- subset(Daten, depth == "2")

## Homogeneity of variances
leveneTest(Na ~ site, data = df_depth2)

## Normality
shapiro_results <- lapply(
  split(df_depth2$Na, df_depth2$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(Na ~ site, data = df_depth2, var.equal = TRUE)




############## Depth section 4
df_depth4 <- subset(Daten, depth == "4")

## Homogeneity of variances
leveneTest(Na ~ site, data = df_depth4)

## Normality
shapiro_results <- lapply(
  split(df_depth4$Na, df_depth4$site),
  shapiro.test
)
print(shapiro_results)

## Welch t-test
t.test(Na ~ site, data = df_depth4, var.equal = FALSE)




############## Depth section 5
df_depth5 <- subset(Daten, depth == "5")

## Homogeneity of variances
leveneTest(Na ~ site, data = df_depth5)

## Normality
shapiro_results <- lapply(
  split(df_depth5$Na, df_depth5$site),
  shapiro.test
)
print(shapiro_results)

## Welch t-test
t.test(Na ~ site, data = df_depth5, var.equal = FALSE)



###########################--------------H---------------#############################

############### Depth within the Palsa

df_palsa <- subset(Daten, site == "P")
model_palsa <- aov(Ha ~ depth, data = df_palsa)

residuen <- residuals(model_palsa)


## Homogeneity of variances
leveneTest(Ha ~ depth, data = df_palsa)
plot(fitted(model_palsa), residuals(model_palsa),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


##Normality
shapiro.test(residuen)

# QQ-Plot
qqnorm(residuen, main = "QQ-Plot der Residuen")
qqline(residuen, col = "red")

# Histogramm
hist(residuen, breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


##ANOVA
summary(model_palsa)




############### Depth within the Thermokarst

df_thermo <- subset(Daten, site == "T")
model_thermo <- aov(Ha ~ depth, data = df_thermo)

residuen <- residuals(model_thermo)


## Homogeneity of variances
leveneTest(Ha ~ depth, data = df_thermo)
plot(fitted(model_thermo), residuals(model_thermo),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


##Normality
shapiro.test(residuen)

# QQ-Plot
qqnorm(residuen, main = "QQ-Plot der Residuen")
qqline(residuen, col = "red")

# Histogramm
hist(residuen, breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


##ANOVA
summary(model_thermo)

emm <- emmeans(model_thermo, ~ depth)
pairs(emm, adjust = "tukey")



############## Comparision depths palsa vs. thermokarst

############## Depth section 1
df_depth1 <- subset(Daten, depth == "1")

## Homogeneity of variances
leveneTest(Ha ~ site, data = df_depth1)

## Normality
shapiro_results <- lapply(
  split(df_depth1$Ha, df_depth1$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(Ha ~ site, data = df_depth1, var.equal = TRUE)



############## Depth section 2
df_depth2 <- subset(Daten, depth == "2")

## Homogeneity of variances
leveneTest(Ha ~ site, data = df_depth2)

## Normality
shapiro_results <- lapply(
  split(df_depth2$Ha, df_depth2$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(Ha ~ site, data = df_depth2, var.equal = TRUE)




############## Depth section 4
df_depth4 <- subset(Daten, depth == "4")

## Homogeneity of variances
leveneTest(Ha ~ site, data = df_depth4)

## Normality
shapiro_results <- lapply(
  split(df_depth4$Ha, df_depth4$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(Ha ~ site, data = df_depth4, var.equal = TRUE)




############## Depth section 5
df_depth5 <- subset(Daten, depth == "5")

## Homogeneity of variances
leveneTest(Ha ~ site, data = df_depth5)

## Normality
shapiro_results <- lapply(
  split(df_depth5$Ha, df_depth5$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(Ha ~ site, data = df_depth5, var.equal = TRUE)





###########################--------------O---------------#############################

############### Depth within the Palsa

df_palsa <- subset(Daten, site == "P")
model_palsa <- aov(Oa ~ depth, data = df_palsa)

residuen <- residuals(model_palsa)


## Homogeneity of variances
leveneTest(Oa ~ depth, data = df_palsa)
plot(fitted(model_palsa), residuals(model_palsa),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


##Normality
shapiro.test(residuen)

# QQ-Plot
qqnorm(residuen, main = "QQ-Plot der Residuen")
qqline(residuen, col = "red")

# Histogramm
hist(residuen, breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


##ANOVA
summary(model_palsa)




############### Depth within the Thermokarst

df_thermo <- subset(Daten, site == "T")
model_thermo <- aov(Oa ~ depth, data = df_thermo)

residuen <- residuals(model_thermo)


## Homogeneity of variances
leveneTest(Oa ~ depth, data = df_thermo)
plot(fitted(model_thermo), residuals(model_thermo),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


##Normality
shapiro.test(residuen)

# QQ-Plot
qqnorm(residuen, main = "QQ-Plot der Residuen")
qqline(residuen, col = "red")

# Histogramm
hist(residuen, breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


##ANOVA
summary(model_thermo)

emm <- emmeans(model_thermo, ~ depth)
pairs(emm, adjust = "tukey")



############## Comparision depths palsa vs. thermokarst

############## Depth section 1
df_depth1 <- subset(Daten, depth == "1")

## Homogeneity of variances
leveneTest(Oa ~ site, data = df_depth1)

## Normality
shapiro_results <- lapply(
  split(df_depth1$Oa, df_depth1$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(Oa ~ site, data = df_depth1, var.equal = TRUE)



############## Depth section 2
df_depth2 <- subset(Daten, depth == "2")

## Homogeneity of variances
leveneTest(Oa ~ site, data = df_depth2)

## Normality
shapiro_results <- lapply(
  split(df_depth2$Oa, df_depth2$site),
  shapiro.test
)
print(shapiro_results)

## Welch t-test
t.test(Oa ~ site, data = df_depth2, var.equal = FALSE)




############## Depth section 4
df_depth4 <- subset(Daten, depth == "4")

## Homogeneity of variances
leveneTest(Oa ~ site, data = df_depth4)

## Normality
shapiro_results <- lapply(
  split(df_depth4$Oa, df_depth4$site),
  shapiro.test
)
print(shapiro_results)

## Welch t-test
t.test(Oa ~ site, data = df_depth4, var.equal = FALSE)




############## Depth section 5
df_depth5 <- subset(Daten, depth == "5")

## Homogeneity of variances
leveneTest(Oa ~ site, data = df_depth5)

## Normality
shapiro_results <- lapply(
  split(df_depth5$Oa, df_depth5$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(Oa ~ site, data = df_depth5, var.equal = TRUE)





###########################--------------S---------------#############################

############### Depth within the Palsa

df_palsa <- subset(Daten, site == "P")
model_palsa <- aov(S ~ depth, data = df_palsa)

residuen <- residuals(model_palsa)


## Homogeneity of variances
leveneTest(S ~ depth, data = df_palsa)
plot(fitted(model_palsa), residuals(model_palsa),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


##Normality
shapiro.test(residuen)

# QQ-Plot
qqnorm(residuen, main = "QQ-Plot der Residuen")
qqline(residuen, col = "red")

# Histogramm
hist(residuen, breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


##ANOVA
summary(model_palsa)

emm <- emmeans(model_palsa, ~ depth)
pairs(emm, adjust = "tukey")



############### Depth within the Thermokarst

df_thermo <- subset(Daten, site == "T")
model_thermo <- aov(S ~ depth, data = df_thermo)

residuen <- residuals(model_thermo)


## Homogeneity of variances
leveneTest(S ~ depth, data = df_thermo)
plot(fitted(model_thermo), residuals(model_thermo),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


##Normality
shapiro.test(residuen)

# QQ-Plot
qqnorm(residuen, main = "QQ-Plot der Residuen")
qqline(residuen, col = "red")

# Histogramm
hist(residuen, breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


##ANOVA
summary(model_thermo)

emm <- emmeans(model_thermo, ~ depth)
pairs(emm, adjust = "tukey")



############## Comparision depths palsa vs. thermokarst

############## Depth section 1
df_depth1 <- subset(Daten, depth == "1")

## Homogeneity of variances
leveneTest(S ~ site, data = df_depth1)

## Normality
shapiro_results <- lapply(
  split(df_depth1$S, df_depth1$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(S ~ site, data = df_depth1, var.equal = TRUE)



############## Depth section 2
df_depth2 <- subset(Daten, depth == "2")

## Homogeneity of variances
leveneTest(S ~ site, data = df_depth2)

## Normality
shapiro_results <- lapply(
  split(df_depth2$S, df_depth2$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(S ~ site, data = df_depth2, var.equal = TRUE)




############## Depth section 4
df_depth4 <- subset(Daten, depth == "4")

## Homogeneity of variances
leveneTest(S ~ site, data = df_depth4)

## Normality
shapiro_results <- lapply(
  split(df_depth4$S, df_depth4$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(S ~ site, data = df_depth4, var.equal = TRUE)




############## Depth section 5
df_depth5 <- subset(Daten, depth == "5")

## Homogeneity of variances
leveneTest(S ~ site, data = df_depth5)

## Normality
shapiro_results <- lapply(
  split(df_depth5$S, df_depth5$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(S ~ site, data = df_depth5, var.equal = TRUE)






###########################--------------C/N---------------#############################
#### Yeo-Johnsson transformation

# estimating lambda
pt <- powerTransform(Daten$CNa, family="yjPower")

summary(pt)

# tranformation
Daten$CN_t <- yjPower(Daten$CNa, lambda=pt$lambda)




############### Depth within the Palsa

df_palsa <- subset(Daten, site == "P")
model_palsa <- aov(CN_t ~ depth, data = df_palsa)

residuen <- residuals(model_palsa)


## Homogeneity of variances
leveneTest(CN_t ~ depth, data = df_palsa)
plot(fitted(model_palsa), residuals(model_palsa),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


##Normality
shapiro.test(residuen)

# QQ-Plot
qqnorm(residuen, main = "QQ-Plot der Residuen")
qqline(residuen, col = "red")

# Histogramm
hist(residuen, breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


##ANOVA
summary(model_palsa)




############### Depth within the Thermokarst

df_thermo <- subset(Daten, site == "T")
model_thermo <- aov(CN_t ~ depth, data = df_thermo)

residuen <- residuals(model_thermo)


## Homogeneity of variances
leveneTest(CN_t ~ depth, data = df_thermo)
plot(fitted(model_thermo), residuals(model_thermo),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


##Normality
shapiro.test(residuen)

# QQ-Plot
qqnorm(residuen, main = "QQ-Plot der Residuen")
qqline(residuen, col = "red")

# Histogramm
hist(residuen, breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


##ANOVA
summary(model_thermo)

emm <- emmeans(model_thermo, ~ depth)
pairs(emm, adjust = "tukey")



############## Comparision depths palsa vs. thermokarst

############## Depth section 1
df_depth1 <- subset(Daten, depth == "1")

## Homogeneity of variances
leveneTest(CN_t ~ site, data = df_depth1)

## Normality
shapiro_results <- lapply(
  split(df_depth1$CN_t, df_depth1$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(CN_t ~ site, data = df_depth1, var.equal = TRUE)



############## Depth section 2
df_depth2 <- subset(Daten, depth == "2")

## Homogeneity of variances
leveneTest(CN_t ~ site, data = df_depth2)

## Normality
shapiro_results <- lapply(
  split(df_depth2$CN_t, df_depth2$site),
  shapiro.test
)
print(shapiro_results)

## Welch t-test
t.test(CN_t ~ site, data = df_depth2, var.equal = FALSE)




############## Depth section 4
df_depth4 <- subset(Daten, depth == "4")

## Homogeneity of variances
leveneTest(CN_t ~ site, data = df_depth4)

## Normality
shapiro_results <- lapply(
  split(df_depth4$CN_t, df_depth4$site),
  shapiro.test
)
print(shapiro_results)

## Welch t-test
t.test(CN_t ~ site, data = df_depth4, var.equal = FALSE)




############## Depth section 5
df_depth5 <- subset(Daten, depth == "5")

## Homogeneity of variances
leveneTest(CN_t ~ site, data = df_depth5)

## Normality
shapiro_results <- lapply(
  split(df_depth5$CN_t, df_depth5$site),
  shapiro.test
)
print(shapiro_results)

## Welch t-test
t.test(CN_t ~ site, data = df_depth5, var.equal = FALSE)







###########################--------------H/C---------------#############################

############### Depth within the Palsa

df_palsa <- subset(Daten, site == "P")
model_palsa <- aov(HC ~ depth, data = df_palsa)

residuen <- residuals(model_palsa)


## Homogeneity of variances
leveneTest(HC ~ depth, data = df_palsa)
plot(fitted(model_palsa), residuals(model_palsa),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


##Normality
shapiro.test(residuen)

# QQ-Plot
qqnorm(residuen, main = "QQ-Plot der Residuen")
qqline(residuen, col = "red")

# Histogramm
hist(residuen, breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


##ANOVA
summary(model_palsa)

emm <- emmeans(model_palsa, ~ depth)
pairs(emm, adjust = "tukey")


############### Depth within the Thermokarst

df_thermo <- subset(Daten, site == "T")
model_thermo <- aov(HC ~ depth, data = df_thermo)

residuen <- residuals(model_thermo)


## Homogeneity of variances
leveneTest(HC ~ depth, data = df_thermo)
plot(fitted(model_thermo), residuals(model_thermo),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


##Normality
shapiro.test(residuen)

# QQ-Plot
qqnorm(residuen, main = "QQ-Plot der Residuen")
qqline(residuen, col = "red")

# Histogramm
hist(residuen, breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


##ANOVA
summary(model_thermo)

emm <- emmeans(model_thermo, ~ depth)
pairs(emm, adjust = "tukey")



############## Comparision depths palsa vs. thermokarst

############## Depth section 1
df_depth1 <- subset(Daten, depth == "1")

## Homogeneity of variances
leveneTest(HC ~ site, data = df_depth1)

## Normality
shapiro_results <- lapply(
  split(df_depth1$HC, df_depth1$site),
  shapiro.test
)
print(shapiro_results)

## Welch t-test
t.test(HC ~ site, data = df_depth1, var.equal = FALSE)



############## Depth section 2
df_depth2 <- subset(Daten, depth == "2")

## Homogeneity of variances
leveneTest(HC ~ site, data = df_depth2)

## Normality
shapiro_results <- lapply(
  split(df_depth2$HC, df_depth2$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(HC ~ site, data = df_depth2, var.equal = TRUE)




############## Depth section 4
df_depth4 <- subset(Daten, depth == "4")

## Homogeneity of variances
leveneTest(HC ~ site, data = df_depth4)

## Normality
shapiro_results <- lapply(
  split(df_depth4$HC, df_depth4$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(HC ~ site, data = df_depth4, var.equal = TRUE)




############## Depth section 5
df_depth5 <- subset(Daten, depth == "5")

## Homogeneity of variances
leveneTest(HC ~ site, data = df_depth5)

## Normality
shapiro_results <- lapply(
  split(df_depth5$HC, df_depth5$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(HC ~ site, data = df_depth5, var.equal = TRUE)







###########################--------------O/C---------------#############################

############### Depth within the Palsa

df_palsa <- subset(Daten, site == "P")
model_palsa <- aov(OC ~ depth, data = df_palsa)

residuen <- residuals(model_palsa)


## Homogeneity of variances
leveneTest(OC ~ depth, data = df_palsa)
plot(fitted(model_palsa), residuals(model_palsa),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


##Normality
shapiro.test(residuen)

# QQ-Plot
qqnorm(residuen, main = "QQ-Plot der Residuen")
qqline(residuen, col = "red")

# Histogramm
hist(residuen, breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


##ANOVA
summary(model_palsa)




############### Depth within the Thermokarst

df_thermo <- subset(Daten, site == "T")
model_thermo <- aov(OC ~ depth, data = df_thermo)

residuen <- residuals(model_thermo)


## Homogeneity of variances
leveneTest(OC ~ depth, data = df_thermo)
plot(fitted(model_thermo), residuals(model_thermo),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


##Normality
shapiro.test(residuen)

# QQ-Plot
qqnorm(residuen, main = "QQ-Plot der Residuen")
qqline(residuen, col = "red")

# Histogramm
hist(residuen, breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


##ANOVA
summary(model_thermo)

emm <- emmeans(model_thermo, ~ depth)
pairs(emm, adjust = "tukey")



############## Comparision depths palsa vs. thermokarst

############## Depth section 1
df_depth1 <- subset(Daten, depth == "1")

## Homogeneity of variances
leveneTest(OC ~ site, data = df_depth1)

## Normality
shapiro_results <- lapply(
  split(df_depth1$OC, df_depth1$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(OC ~ site, data = df_depth1, var.equal = TRUE)



############## Depth section 2
df_depth2 <- subset(Daten, depth == "2")

## Homogeneity of variances
leveneTest(OC ~ site, data = df_depth2)

## Normality
shapiro_results <- lapply(
  split(df_depth2$OC, df_depth2$site),
  shapiro.test
)
print(shapiro_results)

## Welch t-test
t.test(OC ~ site, data = df_depth2, var.equal = FALSE)




############## Depth section 4
df_depth4 <- subset(Daten, depth == "4")

## Homogeneity of variances
leveneTest(OC ~ site, data = df_depth4)

## Normality
shapiro_results <- lapply(
  split(df_depth4$OC, df_depth4$site),
  shapiro.test
)
print(shapiro_results)

## Welch t-test
t.test(OC ~ site, data = df_depth4, var.equal = FALSE)




############## Depth section 5
df_depth5 <- subset(Daten, depth == "5")

## Homogeneity of variances
leveneTest(HC ~ site, data = df_depth5)

## Normality
shapiro_results <- lapply(
  split(df_depth5$HC, df_depth5$site),
  shapiro.test
)
print(shapiro_results)

## two-sample t-test
t.test(OC ~ site, data = df_depth5, var.equal = TRUE)




