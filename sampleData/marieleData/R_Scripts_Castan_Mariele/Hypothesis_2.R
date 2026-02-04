library(car)
library(FSA)


####Creating subsets for palsa and thermokarst
Palsa <- subset(Daten, Daten$site == "P")
Palsa_O2 <- subset(Palsa, Palsa$oxygen == "O2")
Thermokarst <- subset(Daten, Daten$site == "T")
Thermokarst_N2 <- subset(Thermokarst, Thermokarst$oxygen == "N2")



################---------------CUE------------------###############################
## Use of cuet --> cue transformed via square root

############ Palsa

model <- aov(cuet ~ depth, data = Palsa_O2)


## Homogeneity of variances
leveneTest(cuet ~ depth, data = Palsa_O2)

plot(fitted(model), residuals(model),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


## Normality
shapiro.test(residuals(model))

## QQ-Plot
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

# Histogramm
hist(residuals(model), breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


## ANOVA
summary(model)




############ Thermokarst

model <- aov(cuet ~ depth, data = Thermokarst_N2)


## Homogeneity of variances
leveneTest(cuet ~ depth, data = Thermokarst_N2)

plot(fitted(model), residuals(model),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


## Normality
shapiro.test(residuals(model))

## QQ-Plot
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

# Histogramm
hist(residuals(model), breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


## ANOVA
summary(model)



#######################---------------Respiration---------------########################
# Use of respirationat2 --> respiration transformed via natural logarithm

############ Palsa

model <- aov(respirationat2 ~ depth, data = Palsa_O2)


## Homogeneity of variances
leveneTest(respirationat2 ~ depth, data = Palsa_O2)

plot(fitted(model), residuals(model),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


## Normality
shapiro.test(residuals(model))

## QQ-Plot
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

# Histogramm
hist(residuals(model), breaks = 30, col = "lightblue", main = "Histogramm der Residuen")

## Kruskal-Wallis test
kruskal.test(respirationa ~ depth, data = Palsa_O2)

dunnTest(
  respirationa ~ depth, 
  data = Palsa_O2,
  method = "bonferroni"
)




############ Thermokarst

model <- aov(respirationat2 ~ depth, data = Thermokarst_N2)


## Homogeneity of variances
leveneTest(respirationat2 ~ depth, data = Thermokarst_N2)

plot(fitted(model), residuals(model),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


## Normality
shapiro.test(residuals(model))

## QQ-Plot
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

# Histogramm
hist(residuals(model), breaks = 30, col = "lightblue", main = "Histogramm der Residuen")

## ANOVA
summary(model)





########################----------------MBC------------------##############################
# Use of mbcugat --> mbc transformed via square root

############ Palsa

model <- aov(mbcugat ~ depth, data = Palsa_O2)


## Homogeneity of variances
leveneTest(mbcugat ~ depth, data = Palsa_O2)

plot(fitted(model), residuals(model),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


## Normality
shapiro.test(residuals(model))

## QQ-Plot
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

# Histogramm
hist(residuals(model), breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


## ANOVA
summary(model)





############ Thermokarst

model <- aov(mbcugat ~ depth, data = Thermokarst_N2)


## Homogeneity of variances
leveneTest(mbcugat ~ depth, data = Thermokarst_N2)

plot(fitted(model), residuals(model),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


## Normality
shapiro.test(residuals(model))

## QQ-Plot
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

# Histogramm
hist(residuals(model), breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


## Kruskal-Wallis test
kruskal.test(mbcug ~ depth, data = Thermokarst_N2)





##############################----------Growth-----------#############################
# Using cprodugt2 --> Growth transformed via natural logarithm

############ Palsa

model <- aov(cprodugt2 ~ depth, data = Palsa_O2)


## Homogeneity of variances
leveneTest(cprodugt2 ~ depth, data = Palsa_O2)

plot(fitted(model), residuals(model),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


## Normality
shapiro.test(residuals(model))

## QQ-Plot
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

# Histogramm
hist(residuals(model), breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


## ANOVA
summary(model)





############ Thermokarst

model <- aov(cprodugt2 ~ depth, data = Thermokarst_N2)


## Homogeneity of variances
leveneTest(cprodugt2 ~ depth, data = Thermokarst_N2)

plot(fitted(model), residuals(model),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


## Normality
shapiro.test(residuals(model))

## QQ-Plot
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

# Histogramm
hist(residuals(model), breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


## ANOVA
summary(model)





##############################----------Turnover-----------#############################
# Using turnovert --> Turnover transformed via square root

############ Palsa

model <- aov(turnovert ~ depth, data = Palsa_O2)


## Homogeneity of variances
leveneTest(turnovert ~ depth, data = Palsa_O2)

plot(fitted(model), residuals(model),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


## Normality
shapiro.test(residuals(model))

## QQ-Plot
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

# Histogramm
hist(residuals(model), breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


## ANOVA
summary(model)





############ Thermokarst

model <- aov(turnovert ~ depth, data = Thermokarst_N2)


## Homogeneity of variances
leveneTest(turnovert ~ depth, data = Thermokarst_N2)

plot(fitted(model), residuals(model),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Residuen vs. Vorhersagewerte")
abline(h = 0, col = "red")


## Normality
shapiro.test(residuals(model))

## QQ-Plot
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

# Histogramm
hist(residuals(model), breaks = 30, col = "lightblue", main = "Histogramm der Residuen")


## ANOVA
summary(model)



