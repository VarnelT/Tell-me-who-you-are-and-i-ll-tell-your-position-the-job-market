#---------------- librairies nécessaires----------------------
install.packages("haven")
install.packages("estimatr")
install.packages("tidyverse")
install.packages("fastDummies")
install.packages("corrplot")
install.packages("rstatix")
install.packages("modelsummary")
#-------------------------------------------------------------
library(haven) 
library(rstatix)
library(estimatr)
library(tidyverse)
library(fastDummies)
library(modelsummary)
library(corrplot)

# Importation de la base de données
df <- read_dta("DM_Subject_2_Data.dta")
dim(df)

#--------------------QUESTION 1------------------------------------------------
df1 <- df %>%
  filter(t == 6, acteu == 1)%>%
  mutate(
    age2  = age^2
  ) %>%
  droplevels()


regression <- function(y, data = df1){
  
  # On convertit l'argument en caractère si c'est un nom non cité
  y <- deparse(substitute(y))
  
  # Définition des listes de variables explicatives
  rhs1 <- "origine"
  rhs2 <- c(rhs1, "age", "age2", "homme")
  rhs3 <- c(rhs2, "region", "tuu_r")
  rhs4 <- c(rhs3, "education")
  rhs5 <- c(rhs4, "csp_actif", "contrat")
  
  # Estimation des modèles avec reformulate()
  m1 <- lm_robust(reformulate(rhs1, y), data = data)
  m2 <- lm_robust(reformulate(rhs2, y), data = data)
  m3 <- lm_robust(reformulate(rhs3, y), data = data)
  m4 <- lm_robust(reformulate(rhs4, y), data = data)
  m5 <- lm_robust(reformulate(rhs5, y), data = data)
  
  # On regroupe pour le tableau
  models <- list(
    "modèle(1)" = m1, 
    "modèle(2)" = m2, 
    "modèle(3)" = m3, 
    "modèle(4)" = m4, 
    "modèle(5)" = m5
  )
  
  # Rendu du tableau
  modelsummary(
    models,
    stars = TRUE,
    coef_omit = "Intercept",
    gof_omit = "AIC|BIC|Log.Lik|F|Std.Errors",
    title = paste("Analyse de la variable :", y),
    notes = "Écarts-types robustes (HC2)."
  )
  
}

  # a) Modèle LOG 
regression(logsalhoraire)
# a) Modèle en niveau 
regression(salhoraire)

#------------------------------------------------------------

boxplot(
  logsalhoraire ~ origine,
  data = df,
  col = "lightblue",
  main = "Distribution des salaires selon le niveau d'éducation",
  xlab = "Niveau d'éducation",
  ylab = "Salaire (€)")


 # Exemple
table_data <- table(df1$education, df1$origine)

barplot(
  table_data,
  beside = TRUE, # empilé
  col = c("skyblue", "orange","red", "black", "purple"),
  legend = TRUE,
  main = "Interaction entre deux variables qualitatives"
)
