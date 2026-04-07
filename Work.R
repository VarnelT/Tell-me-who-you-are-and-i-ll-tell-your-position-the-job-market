#---------------- librairies nécessaires----------------------
install.packages("haven")
install.packages("estimatr")
install.packages("tidyverse")
install.packages("fastDummies")
install.packages("corrplot")
install.packages("rstatix")
install.packages("modelsummary")
install.packages("plm")
#-------------------------------------------------------------
library(haven) 
library(plm)
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


#--------------------QUESTION 2------------------------------------------------

df1 <- df1 %>%
  mutate(
    # Instrument : le parent ayant la plus haute CSP
    csp_max = pmax(csp_mere, csp_pere, na.rm = TRUE) %>% as.factor()
  )
          #--------Pertinence----------
m_iv <- iv_robust(
  logsalhoraire ~ origine + age + age2 + homme + region + tuu_r + education | 
    origine + age + age2 + homme + region + tuu_r + csp_mere,
  data = df1,
  diagnostics = TRUE # TRÈS IMPORTANT pour avoir le F-test
)

summary(m_iv)


        #-------Estimation------------
models_iv <- list(
  "Modèle 4" = m4, # Ton modèle précédent
  "IV (2SLS)" = m_iv
)

modelsummary(
  models_iv,
  stars = TRUE,
  coef_omit = "Intercept",
  gof_omit = "AIC|BIC|Log.Lik|F|Std.Errors",
  title = "Comparaison MCO vs Variables Instrumentales",
  notes = "Instruments : CSP la plus haute des deux parents."
)
#----------------------Question 3--------------------------------------

      #---a) Modèle avec exogénéité des résidus et des effets fixes individuels---
df_panel <- df %>%
  filter(t %in% c(1, 6), acteu == 1) %>%
  mutate(age2 = age^2) %>%
  group_by(id_individu) %>%
  filter(n() == 2) %>%
  ungroup()

      #Estimation MCO Empilé (Pooled OLS)

m3a <- plm(
  logsalhoraire ~ origine + age + age2 + homme + region + tuu_r + education,
  data = df_panel,
  model = "pooling"
)

# Affichage avec écarts-types robustes (Clustered par individu)
# TRÈS IMPORTANT en panel 
summary(m3a, vcov = vcovHC(m3a, type = "HC1", cluster = "group"))


#-----------------------------------------------------------------------------
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
#------------------------------------------------------------------------------
# ------------------------------------------------------------
# Question 4 – Modèle de probabilité linéaire en panel
# ------------------------------------------------------------

# Dans cette partie, on s’intéresse à la marge extensive du marché du travail,
# c’est-à-dire à la probabilité pour un individu actif d’être au chômage
# plutôt qu’en emploi. La variable dépendante est donc une indicatrice
# de chômage, prenant la valeur 1 si l’individu est chômeur et 0 s’il
# est actif occupé.

# Bien que la variable expliquée soit binaire, on estime dans un premier
# temps un modèle de probabilité linéaire (LPM). Dans ce cadre,
# l’espérance conditionnelle de la variable dépendante correspond à
# la probabilité d’être au chômage conditionnellement aux variables
# explicatives. Les coefficients peuvent donc être interprétés
# directement comme des variations de probabilité.

# Les données ayant une structure de panel (les individus sont observés
# sur 6 trimestres), le modèle estimé s’écrit :

# Y_it = X_it * beta + alpha_i + u_it

# où :
# Y_it  : indicatrice de chômage pour l’individu i au trimestre t
# X_it  : ensemble des variables explicatives (origine, âge, sexe,
#         éducation, localisation, etc.)
# alpha_i : hétérogénéité individuelle inobservée
# u_it : terme d’erreur idiosyncratique

# Dans cette question, on suppose que les effets individuels non observés
# sont exogènes, c’est-à-dire non corrélés avec les variables explicatives.
# Cette hypothèse correspond au cadre des modèles à effets aléatoires
# (random effects).

# L’estimation permet d’évaluer l’effet de la variable origine sur la
# probabilité d’être au chômage toutes choses égales par ailleurs,
# en contrôlant pour les caractéristiques individuelles et géographiques.

# Les coefficients associés aux différentes modalités de la variable
# origine mesurent l’écart de probabilité de chômage par rapport au
# groupe de référence (les individus d’origine française).

# Par exemple, un coefficient estimé de 0.05 pour une catégorie
# d’origine signifie que les individus appartenant à ce groupe ont
# environ 5 points de pourcentage de probabilité supplémentaire
# d’être au chômage, toutes choses égales par ailleurs.

# À l’inverse, un coefficient négatif indiquerait une probabilité
# plus faible d’être au chômage relativement au groupe de référence.

# L’analyse des résultats permet ainsi d’identifier l’existence
# éventuelle d’inégalités d’accès à l’emploi selon l’origine,
# après prise en compte des autres déterminants observables du
# statut sur le marché du travail.
# Installer le package si besoin
install.packages("plm")

library(plm)

# On garde uniquement les individus actifs (occupés ou chômeurs)
df_panel <- df %>%
  filter(actif == 1)

# Création de la variable binaire chômage
df_panel <- df_panel %>%
  mutate(
    chomage = ifelse(acteu == 0, 1, 0),
    age2 = age^2
  )

# Transformation en données de panel
pdata <- pdata.frame(df_panel, index = c("id", "t"))

# Modèle de probabilité linéaire avec effets aléatoires
m_panel_RE <- plm(
  chomage ~ origine + age + age2 + homme + region + tuu_r + education,
  data = pdata,
  model = "random"
)

# Résumé des résultats
summary(m_panel_RE)
library(lmtest)
library(sandwich)

coeftest(
  m_panel_RE,
  vcov = vcovHC(m_panel_RE, type = "HC1")
)
