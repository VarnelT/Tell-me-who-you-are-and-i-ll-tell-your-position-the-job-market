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

  # a) Modèle LOG 
# Modèle 1 : Effet brut de l'origine
m1 <- lm_robust(logsalhoraire~ origine, data = df1)
# Modèle 2 : Ajout des variables démographiques et cycle de vie
m2 <- lm_robust(logsalhoraire ~ origine + age + age2 + homme, data = df1)
# Modèle 3 : Ajout des contrôles géographiques (Effets de structure)
m3 <- lm_robust(logsalhoraire ~ origine + age + age2 + homme + region + tuu_r, 
                data = df1)
# Modèle 4 : Modèle complet avec Capital Humain (Blocage du canal éducation)
m4 <- lm_robust(logsalhoraire ~ origine + age + age2 + homme + region + tuu_r + education, 
                data = df1)
# Modèle 5 : Modèle complet avec bad controls
m5 <- lm_robust(logsalhoraire ~ origine + age + age2 + homme + region + tuu_r + csp_actif + contrat+ education, 
                data = df1)

# 3. Tableau récapitulatif
list(
  "Modèle (1)" = m1, 
  "Modèle (2)" = m2, 
  "Modèle (3)" = m3, 
  "Modèle (4)" = m4,
  "Modèle (5)" = m5
) %>%
  modelsummary(
    stars = TRUE,
    coef_omit = "Intercept",
    gof_omit = "AIC|BIC|Log.Lik|F|Std.Errors",
    title = "Évolution des coefficients du salaire horaire",
    notes = "Écarts-types robustes (HC2) entre parenthèses."
  )


#------------------------------------------------------------

boxplot(
  logsalhoraire ~ csp_actif,
  data = df,
  col = "lightblue",
  main = "Distribution des salaires selon le niveau d'éducation",
  xlab = "Niveau d'éducation",
  ylab = "Salaire (€)")


 # Exemple
table_data <- table(df$csp_actif, df$education)

barplot(
  table_data,
  beside = TRUE, # empilé
  col = c("skyblue", "orange","red", "black", "purple"),
  legend = TRUE,
  main = "Interaction entre deux variables qualitatives"
)

hist(df$age)
