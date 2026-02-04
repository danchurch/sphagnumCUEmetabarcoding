library(readr)


######### Data frame Daten
Daten <- read_delim("~/Uni/Master/Masterarbeit/Statistik/Daten.csv", 
                    delim = ";", escape_double = FALSE, col_types = cols(site = col_factor(levels = c("P", 
                                                                                                      "T")), oxygen = col_factor(levels = c("O2", 
                                                                                                                                            "N2")), depth = col_factor(levels = c("0", 
                                                                                                                                                                                  "1", "2", "3", "4", "5"))), trim_ws = TRUE)

######## Data frame Daten_PCA for PCA and correlations
Daten_PCA <- read_delim("~/Uni/Master/Masterarbeit/Statistik/Daten_PCA.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
