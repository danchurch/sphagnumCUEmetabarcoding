library(car)





######################---------------CUE-MBC---------------#############################


## Testing for normality and homogeneity of variances

sites <- unique(Daten$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cue", "mbcug")])
  
  sw_1 <- shapiro.test(df_nomiss$cue)
  sw_2 <- shapiro.test(df_nomiss$mbcug)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(mbcug ~ cue, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$cue)
  grp <- ifelse(df_nomiss$cue <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten, site == s)
  
  idx <- complete.cases(df_sub$cue, df_sub$mbcug)
  x <- df_sub$cue[idx]
  y <- df_sub$mbcug[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}







######################---------------CUE-Growth---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cue", "cprodug")])
  
  sw_1 <- shapiro.test(df_nomiss$cue)
  sw_2 <- shapiro.test(df_nomiss$cprodug)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cprodug ~ cue, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$cue)
  grp <- ifelse(df_nomiss$cue <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten, site == s)
  
  idx <- complete.cases(df_sub$cue, df_sub$cprodug)
  x <- df_sub$cue[idx]
  y <- df_sub$cprodug[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




######################---------------CUE-Respiration---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cue", "respirationa")])
  
  sw_1 <- shapiro.test(df_nomiss$cue)
  sw_2 <- shapiro.test(df_nomiss$respirationa)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(respirationa ~ cue, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$cue)
  grp <- ifelse(df_nomiss$cue <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten, site == s)
  
  idx <- complete.cases(df_sub$cue, df_sub$respirationa)
  x <- df_sub$cue[idx]
  y <- df_sub$respirationa[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}






######################---------------CUE-Turnover---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cue", "turnover")])
  
  sw_1 <- shapiro.test(df_nomiss$cue)
  sw_2 <- shapiro.test(df_nomiss$turnover)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(turnover ~ cue, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$cue)
  grp <- ifelse(df_nomiss$cue <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten, site == s)
  
  idx <- complete.cases(df_sub$cue, df_sub$turnover)
  x <- df_sub$cue[idx]
  y <- df_sub$turnover[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




#######################---------------CUE-C---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cue", "C")])
  
  sw_1 <- shapiro.test(df_nomiss$cue)
  sw_2 <- shapiro.test(df_nomiss$C)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cue ~ C, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$C)
  grp <- ifelse(df_nomiss$C <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$cue, df_sub$C)
  x <- df_sub$cue[idx]
  y <- df_sub$C[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}





########################---------------CUE-N---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cue", "N")])
  
  sw_1 <- shapiro.test(df_nomiss$cue)
  sw_2 <- shapiro.test(df_nomiss$N)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cue ~ N, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$N)
  grp <- ifelse(df_nomiss$N <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$cue, df_sub$N)
  x <- df_sub$cue[idx]
  y <- df_sub$N[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}






########################---------------CUE-H---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cue", "H")])
  
  sw_1 <- shapiro.test(df_nomiss$cue)
  sw_2 <- shapiro.test(df_nomiss$H)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cue ~ H, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$H)
  grp <- ifelse(df_nomiss$H <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$cue, df_sub$H)
  x <- df_sub$cue[idx]
  y <- df_sub$H[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}







########################---------------CUE-O---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cue", "O")])
  
  sw_1 <- shapiro.test(df_nomiss$cue)
  sw_2 <- shapiro.test(df_nomiss$O)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cue ~ O, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$O)
  grp <- ifelse(df_nomiss$O <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$cue, df_sub$O)
  x <- df_sub$cue[idx]
  y <- df_sub$O[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}





########################---------------CUE-S---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cue", "S")])
  
  sw_1 <- shapiro.test(df_nomiss$cue)
  sw_2 <- shapiro.test(df_nomiss$S)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cue ~ S, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$S)
  grp <- ifelse(df_nomiss$S <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$cue, df_sub$S)
  x <- df_sub$cue[idx]
  y <- df_sub$S[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}





########################---------------CUE-C/N---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cue", "CN")])
  
  sw_1 <- shapiro.test(df_nomiss$cue)
  sw_2 <- shapiro.test(df_nomiss$CN)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cue ~ CN, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$CN)
  grp <- ifelse(df_nomiss$CN <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$cue, df_sub$CN)
  x <- df_sub$cue[idx]
  y <- df_sub$CN[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}





########################---------------CUE-H/C---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cue", "HC")])
  
  sw_1 <- shapiro.test(df_nomiss$cue)
  sw_2 <- shapiro.test(df_nomiss$HC)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cue ~ HC, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$HC)
  grp <- ifelse(df_nomiss$HC <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$cue, df_sub$HC)
  x <- df_sub$cue[idx]
  y <- df_sub$HC[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}



########################---------------CUE-O/C---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cue", "OC")])
  
  sw_1 <- shapiro.test(df_nomiss$cue)
  sw_2 <- shapiro.test(df_nomiss$OC)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cue ~ OC, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$OC)
  grp <- ifelse(df_nomiss$OC <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$cue, df_sub$OC)
  x <- df_sub$cue[idx]
  y <- df_sub$OC[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}





######################---------------MBC-Growth---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("mbcug", "cprodug")])
  
  sw_1 <- shapiro.test(df_nomiss$mbcug)
  sw_2 <- shapiro.test(df_nomiss$cprodug)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cprodug ~ mbcug, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$mbcug)
  grp <- ifelse(df_nomiss$mbcug <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten, site == s)
  
  idx <- complete.cases(df_sub$mbcug, df_sub$cprodug)
  x <- df_sub$mbcug[idx]
  y <- df_sub$cprodug[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




######################---------------MBC-Respiration---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("mbcug", "respirationa")])
  
  sw_1 <- shapiro.test(df_nomiss$mbcug)
  sw_2 <- shapiro.test(df_nomiss$respirationa)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(respirationa ~ mbcug, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$mbcug)
  grp <- ifelse(df_nomiss$mbcug <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten, site == s)
  
  idx <- complete.cases(df_sub$mbcug, df_sub$respirationa)
  x <- df_sub$mbcug[idx]
  y <- df_sub$respirationa[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}





######################---------------MBC-Turnover---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("mbcug", "turnover")])
  
  sw_1 <- shapiro.test(df_nomiss$mbcug)
  sw_2 <- shapiro.test(df_nomiss$turnover)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(turnover ~ mbcug, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$mbcug)
  grp <- ifelse(df_nomiss$mbcug <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten, site == s)
  
  idx <- complete.cases(df_sub$mbcug, df_sub$turnover)
  x <- df_sub$mbcug[idx]
  y <- df_sub$turnover[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------MBC-C---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("mbcug", "C")])
  
  sw_1 <- shapiro.test(df_nomiss$mbcug)
  sw_2 <- shapiro.test(df_nomiss$C)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(mbcug ~ C, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$C)
  grp <- ifelse(df_nomiss$C <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$mbcug, df_sub$C)
  x <- df_sub$mbcug[idx]
  y <- df_sub$C[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------MBC-N---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("mbcug", "N")])
  
  sw_1 <- shapiro.test(df_nomiss$mbcug)
  sw_2 <- shapiro.test(df_nomiss$N)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(mbcug ~ N, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$N)
  grp <- ifelse(df_nomiss$N <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$mbcug, df_sub$N)
  x <- df_sub$mbcug[idx]
  y <- df_sub$N[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------MBC-H---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("mbcug", "H")])
  
  sw_1 <- shapiro.test(df_nomiss$mbcug)
  sw_2 <- shapiro.test(df_nomiss$H)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(mbcug ~ H, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$H)
  grp <- ifelse(df_nomiss$H <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$mbcug, df_sub$H)
  x <- df_sub$mbcug[idx]
  y <- df_sub$H[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------MBC-O---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("mbcug", "O")])
  
  sw_1 <- shapiro.test(df_nomiss$mbcug)
  sw_2 <- shapiro.test(df_nomiss$O)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(mbcug ~ O, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$O)
  grp <- ifelse(df_nomiss$O <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$mbcug, df_sub$O)
  x <- df_sub$mbcug[idx]
  y <- df_sub$O[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}





########################---------------MBC-S---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("mbcug", "S")])
  
  sw_1 <- shapiro.test(df_nomiss$mbcug)
  sw_2 <- shapiro.test(df_nomiss$S)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(mbcug ~ S, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$S)
  grp <- ifelse(df_nomiss$S <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$mbcug, df_sub$S)
  x <- df_sub$mbcug[idx]
  y <- df_sub$S[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}





########################---------------MBC-CN---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("mbcug", "CN")])
  
  sw_1 <- shapiro.test(df_nomiss$mbcug)
  sw_2 <- shapiro.test(df_nomiss$CN)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(mbcug ~ CN, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$CN)
  grp <- ifelse(df_nomiss$CN <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$mbcug, df_sub$CN)
  x <- df_sub$mbcug[idx]
  y <- df_sub$CN[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------MBC-HC---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("mbcug", "HC")])
  
  sw_1 <- shapiro.test(df_nomiss$mbcug)
  sw_2 <- shapiro.test(df_nomiss$HC)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(mbcug ~ HC, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$HC)
  grp <- ifelse(df_nomiss$HC <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$mbcug, df_sub$HC)
  x <- df_sub$mbcug[idx]
  y <- df_sub$HC[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}





########################---------------MBC-OC---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("mbcug", "OC")])
  
  sw_1 <- shapiro.test(df_nomiss$mbcug)
  sw_2 <- shapiro.test(df_nomiss$OC)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(mbcug ~ OC, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$OC)
  grp <- ifelse(df_nomiss$OC <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$mbcug, df_sub$OC)
  x <- df_sub$mbcug[idx]
  y <- df_sub$OC[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




######################---------------Growth-Respiration---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cprodug", "respirationa")])
  
  sw_1 <- shapiro.test(df_nomiss$cprodug)
  sw_2 <- shapiro.test(df_nomiss$respirationa)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(respirationa ~ cprodug, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$cprodug)
  grp <- ifelse(df_nomiss$cprodug <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten, site == s)
  
  idx <- complete.cases(df_sub$cprodug, df_sub$respirationa)
  x <- df_sub$cprodug[idx]
  y <- df_sub$respirationa[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}



######################---------------Growth-Turnover---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cprodug", "turnover")])
  
  sw_1 <- shapiro.test(df_nomiss$cprodug)
  sw_2 <- shapiro.test(df_nomiss$turnover)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(turnover ~ cprodug, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$cprodug)
  grp <- ifelse(df_nomiss$cprodug <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten, site == s)
  
  idx <- complete.cases(df_sub$cprodug, df_sub$turnover)
  x <- df_sub$cprodug[idx]
  y <- df_sub$turnover[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}



########################---------------Growth-C---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cprodug", "C")])
  
  sw_1 <- shapiro.test(df_nomiss$cprodug)
  sw_2 <- shapiro.test(df_nomiss$C)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cprodug ~ C, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$C)
  grp <- ifelse(df_nomiss$C <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$cprodug, df_sub$C)
  x <- df_sub$cprodug[idx]
  y <- df_sub$C[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------Growth-N---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cprodug", "N")])
  
  sw_1 <- shapiro.test(df_nomiss$cprodug)
  sw_2 <- shapiro.test(df_nomiss$N)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cprodug ~ N, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$N)
  grp <- ifelse(df_nomiss$N <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$cprodug, df_sub$N)
  x <- df_sub$cprodug[idx]
  y <- df_sub$N[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}



########################---------------Growth-H---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cprodug", "H")])
  
  sw_1 <- shapiro.test(df_nomiss$cprodug)
  sw_2 <- shapiro.test(df_nomiss$H)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cprodug ~ H, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$H)
  grp <- ifelse(df_nomiss$H <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$cprodug, df_sub$H)
  x <- df_sub$cprodug[idx]
  y <- df_sub$H[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------Growth-O---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cprodug", "O")])
  
  sw_1 <- shapiro.test(df_nomiss$cprodug)
  sw_2 <- shapiro.test(df_nomiss$O)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cprodug ~ O, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$O)
  grp <- ifelse(df_nomiss$O <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$cprodug, df_sub$O)
  x <- df_sub$cprodug[idx]
  y <- df_sub$O[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}





########################---------------Growth-S---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cprodug", "S")])
  
  sw_1 <- shapiro.test(df_nomiss$cprodug)
  sw_2 <- shapiro.test(df_nomiss$S)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cprodug ~ S, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$S)
  grp <- ifelse(df_nomiss$S <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$cprodug, df_sub$S)
  x <- df_sub$cprodug[idx]
  y <- df_sub$S[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------Growth-C/N---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cprodug", "CN")])
  
  sw_1 <- shapiro.test(df_nomiss$cprodug)
  sw_2 <- shapiro.test(df_nomiss$CN)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cprodug ~ CN, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$CN)
  grp <- ifelse(df_nomiss$CN <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$cprodug, df_sub$CN)
  x <- df_sub$cprodug[idx]
  y <- df_sub$CN[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------Growth-H/C---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cprodug", "HC")])
  
  sw_1 <- shapiro.test(df_nomiss$cprodug)
  sw_2 <- shapiro.test(df_nomiss$HC)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cprodug ~ HC, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$HC)
  grp <- ifelse(df_nomiss$HC <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$cprodug, df_sub$HC)
  x <- df_sub$cprodug[idx]
  y <- df_sub$HC[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}



########################---------------Growth-O/C---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("cprodug", "OC")])
  
  sw_1 <- shapiro.test(df_nomiss$cprodug)
  sw_2 <- shapiro.test(df_nomiss$OC)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(cprodug ~ OC, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$OC)
  grp <- ifelse(df_nomiss$OC <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$cprodug, df_sub$OC)
  x <- df_sub$cprodug[idx]
  y <- df_sub$OC[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}





######################---------------Respiration-Turnover---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("respirationa", "turnover")])
  
  sw_1 <- shapiro.test(df_nomiss$respirationa)
  sw_2 <- shapiro.test(df_nomiss$turnover)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(turnover ~ respirationa, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$respirationa)
  grp <- ifelse(df_nomiss$respirationa <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten, site == s)
  
  idx <- complete.cases(df_sub$respirationa, df_sub$turnover)
  x <- df_sub$respirationa[idx]
  y <- df_sub$turnover[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}





########################---------------Respiration-C---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("respirationa", "C")])
  
  sw_1 <- shapiro.test(df_nomiss$respirationa)
  sw_2 <- shapiro.test(df_nomiss$C)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(respirationa ~ C, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$C)
  grp <- ifelse(df_nomiss$C <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$respirationa, df_sub$C)
  x <- df_sub$respirationa[idx]
  y <- df_sub$C[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------Respiration-N---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("respirationa", "N")])
  
  sw_1 <- shapiro.test(df_nomiss$respirationa)
  sw_2 <- shapiro.test(df_nomiss$N)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(respirationa ~ N, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$N)
  grp <- ifelse(df_nomiss$N <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$respirationa, df_sub$N)
  x <- df_sub$respirationa[idx]
  y <- df_sub$N[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------Respiration-H---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("respirationa", "H")])
  
  sw_1 <- shapiro.test(df_nomiss$respirationa)
  sw_2 <- shapiro.test(df_nomiss$H)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(respirationa ~ H, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$H)
  grp <- ifelse(df_nomiss$H <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$respirationa, df_sub$H)
  x <- df_sub$respirationa[idx]
  y <- df_sub$H[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------Respiration-O---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("respirationa", "O")])
  
  sw_1 <- shapiro.test(df_nomiss$respirationa)
  sw_2 <- shapiro.test(df_nomiss$O)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(respirationa ~ O, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$O)
  grp <- ifelse(df_nomiss$O <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$respirationa, df_sub$O)
  x <- df_sub$respirationa[idx]
  y <- df_sub$O[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}



########################---------------Respiration-S---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("respirationa", "S")])
  
  sw_1 <- shapiro.test(df_nomiss$respirationa)
  sw_2 <- shapiro.test(df_nomiss$S)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(respirationa ~ S, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$S)
  grp <- ifelse(df_nomiss$S <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$respirationa, df_sub$S)
  x <- df_sub$respirationa[idx]
  y <- df_sub$S[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------Respiration-C/N---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("respirationa", "CN")])
  
  sw_1 <- shapiro.test(df_nomiss$respirationa)
  sw_2 <- shapiro.test(df_nomiss$CN)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(respirationa ~ CN, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$CN)
  grp <- ifelse(df_nomiss$CN <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$respirationa, df_sub$CN)
  x <- df_sub$respirationa[idx]
  y <- df_sub$CN[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------Respiration-H/C---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("respirationa", "HC")])
  
  sw_1 <- shapiro.test(df_nomiss$respirationa)
  sw_2 <- shapiro.test(df_nomiss$HC)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(respirationa ~ HC, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$HC)
  grp <- ifelse(df_nomiss$HC <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$respirationa, df_sub$HC)
  x <- df_sub$respirationa[idx]
  y <- df_sub$HC[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




##################---------------Respiration-O/C---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("respirationa", "OC")])
  
  sw_1 <- shapiro.test(df_nomiss$respirationa)
  sw_2 <- shapiro.test(df_nomiss$OC)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(respirationa ~ OC, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$OC)
  grp <- ifelse(df_nomiss$OC <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$respirationa, df_sub$OC)
  x <- df_sub$respirationa[idx]
  y <- df_sub$OC[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}



#######################---------------Turnover-C---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("turnover", "C")])
  
  sw_1 <- shapiro.test(df_nomiss$turnover)
  sw_2 <- shapiro.test(df_nomiss$C)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(turnover ~ C, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$C)
  grp <- ifelse(df_nomiss$C <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$turnover, df_sub$C)
  x <- df_sub$turnover[idx]
  y <- df_sub$C[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------turnover-N---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("turnover", "N")])
  
  sw_1 <- shapiro.test(df_nomiss$turnover)
  sw_2 <- shapiro.test(df_nomiss$N)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(turnover ~ N, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$N)
  grp <- ifelse(df_nomiss$N <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$turnover, df_sub$N)
  x <- df_sub$turnover[idx]
  y <- df_sub$N[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------turnover-H---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("turnover", "H")])
  
  sw_1 <- shapiro.test(df_nomiss$turnover)
  sw_2 <- shapiro.test(df_nomiss$H)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(turnover ~ H, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$H)
  grp <- ifelse(df_nomiss$H <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$turnover, df_sub$H)
  x <- df_sub$turnover[idx]
  y <- df_sub$H[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------turnover-O---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("turnover", "O")])
  
  sw_1 <- shapiro.test(df_nomiss$turnover)
  sw_2 <- shapiro.test(df_nomiss$O)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(turnover ~ O, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$O)
  grp <- ifelse(df_nomiss$O <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$turnover, df_sub$O)
  x <- df_sub$turnover[idx]
  y <- df_sub$O[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}



########################---------------turnover-S---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("turnover", "S")])
  
  sw_1 <- shapiro.test(df_nomiss$turnover)
  sw_2 <- shapiro.test(df_nomiss$S)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(turnover ~ S, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$S)
  grp <- ifelse(df_nomiss$S <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$turnover, df_sub$S)
  x <- df_sub$turnover[idx]
  y <- df_sub$S[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------turnover-C/N---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("turnover", "CN")])
  
  sw_1 <- shapiro.test(df_nomiss$turnover)
  sw_2 <- shapiro.test(df_nomiss$CN)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(turnover ~ CN, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$CN)
  grp <- ifelse(df_nomiss$CN <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$turnover, df_sub$CN)
  x <- df_sub$turnover[idx]
  y <- df_sub$CN[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




########################---------------turnover-H/C---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("turnover", "HC")])
  
  sw_1 <- shapiro.test(df_nomiss$turnover)
  sw_2 <- shapiro.test(df_nomiss$HC)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(turnover ~ HC, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$HC)
  grp <- ifelse(df_nomiss$HC <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$turnover, df_sub$HC)
  x <- df_sub$turnover[idx]
  y <- df_sub$HC[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}




##################---------------turnover-O/C---------------#############################

## Testing for normality and homogeneity of variances

sites <- unique(Daten_PCA$site)

for (s in sites) {
  cat("=== Site:", s, "===\n")
  df_sub <- subset(Daten_PCA, site == s)
  
  df_nomiss <- na.omit(df_sub[, c("turnover", "OC")])
  
  sw_1 <- shapiro.test(df_nomiss$turnover)
  sw_2 <- shapiro.test(df_nomiss$OC)
  cat("Shapiro-Wilk 1: W =", round(sw_1$statistic, 3),
      ", p =", round(sw_1$p.value, 3), "\n")
  cat("Shapiro-Wilk 2: W =", round(sw_2$statistic, 3),
      ", p =", round(sw_2$p.value, 3), "\n\n")
  
  mod <- lm(turnover ~ OC, data = df_nomiss)
  sw_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk Residuen: W =", round(sw_res$statistic, 3),
      ", p =", round(sw_res$p.value, 3), "\n")
  
  med_1 <- median(df_nomiss$OC)
  grp <- ifelse(df_nomiss$OC <= med_1, "low", "high")
  lev <- leveneTest(residuals(mod) ~ grp)
  cat("Levene-Test: \n")
  print(lev)
  cat("\n")
}




## Spearman correlation

method_choice <- "spearman"

for (s in c("P", "T")) {
  cat("=== Site:", s, "===\n")
  
  df_sub <- subset(Daten_PCA, site == s)
  
  idx <- complete.cases(df_sub$turnover, df_sub$OC)
  x <- df_sub$turnover[idx]
  y <- df_sub$OC[idx]
  
  test <- cor.test(x, y,
                   method = method_choice,
                   exact = FALSE)
  
  print(test)
  cat("\n")
}



