# Labor Market Integration in France: The Role of Immigrant Background

[![Institution](https://img.shields.io/badge/Institution-ENSAE%20Paris-003366)](https://www.ensae.fr)
[![R](https://img.shields.io/badge/Language-R-276DC3?logo=r&logoColor=white)](https://www.r-project.org)
[![Data](https://img.shields.io/badge/Data-INSEE%20Enquête%20Emploi-blue)](https://www.insee.fr)
[![Status](https://img.shields.io/badge/Status-Complete-brightgreen)]()

## Overview

This project quantifies the effect of **immigrant background** on labor market outcomes in France, using a panel extracted from the INSEE Employment Survey (Enquête Emploi) covering 10,000 individuals observed over six consecutive quarters (2014–2016). By combining panel econometric methods with instrumental variable strategies, the analysis isolates the causal contribution of origin to wage disparities and employment access, net of individual characteristics.

The study examines two margins of the labor market simultaneously: the **intensive margin** (hourly wages among the employed) and the **extensive margin** (probability of being in employment vs. unemployed or inactive). The research was conducted by **Varnel TIENTCHEU** and **Shella LANKOANDE** at ENSAE – Institut Polytechnique de Paris.

## Research Question

> Does immigrant origin — measured both by direct immigration status and by parental background — have a statistically significant and economically meaningful effect on wages and employment probability in France, after controlling for human capital, socio-demographic characteristics, and local labor market conditions?

## Data

| Source | Description |
|---|---|
| **INSEE Enquête Emploi** | French Labor Force Survey (panel component), 2014–2016 |
| **Observations** | 10,000 individuals × 6 quarterly waves |
| **Outcome variables** | Net hourly wage (`salhoraire`), log hourly wage (`logsalhoraire`), labor market status (employed / unemployed / inactive) |
| **Individual covariates** | Age, sex, education level, parental occupation (CSP), immigrant/descendant indicators, self-reported health, marital status |
| **Geographic covariates** | Region, urban unit size, local share of population by origin |

> The dataset (`DM_Subject_2_Data.dta`) is provided in Stata format. Raw survey microdata are subject to INSEE access conditions and are not redistributed in this repository.

## Methodology

### Descriptive Analysis
- Distributional comparisons of wages and employment rates across origin groups
- Correlation matrix and balance checks on key covariates
- Graphical representation of labor market gaps by gender, education, and geographic area

### Intensive Margin — Wage Equation
- **OLS/WLS** panel regressions of log hourly wage on origin indicators and controls
- **Fixed-Effects (FE)** model to absorb unobserved time-invariant individual heterogeneity
- **Random-Effects (RE)** model with Hausman test to justify FE choice
- **Instrumental Variables (IV)** estimation to address remaining endogeneity in the origin variable

### Extensive Margin — Employment Probability
- **Probit / Logit** models for the probability of employment, controlling for the full individual and geographic covariate set
- Marginal effects computed at the mean to quantify the employment gap attributable to immigrant background

**Stack:** R (`plm`, `estimatr`, `modelsummary`, `haven`, `tidyverse`, `fastDummies`, `margins`, `corrplot`)

## Key Results

Panel fixed-effects estimates reveal a statistically significant **wage penalty** associated with immigrant and second-generation status, even after controlling for education, age, sector, and geography. The penalty is amplified for non-European origin groups and is persistent across the observation window.

On the extensive margin, probit models document a significantly **lower probability of employment** for individuals of immigrant origin, with the gap being partially but not fully explained by human capital differences. IV estimates confirm that the OLS coefficients do not merely reflect selection on observables.

Full regression tables, marginal effect plots, and robustness checks are available in the Quarto report (`Rapport.qmd` / `Rapport.html`).

## Replication

```r
# 1. Clone the repository
# git clone https://github.com/VarnelT/labor-market-integration-france.git

# 2. Install required R packages
install.packages(c(
  "haven", "plm", "estimatr", "tidyverse",
  "fastDummies", "modelsummary", "corrplot",
  "margins", "rstatix"
))

# 3. Open and render the Quarto report (requires Quarto CLI)
quarto render Rapport.qmd

# Or run the analysis interactively inside RStudio by opening Rapport.qmd
```

The dataset `DM_Subject_2_Data.dta` is included in the repository. All results in the report are fully reproducible from this single Stata-format data file.

## References

- Aeberhardt, R., & Pouget, J. (2010). National Origin Differences in Wages and Hierarchical Positions: Evidence on French Full-Time Male Workers from a Large Matched Employer–Employee Dataset. *Annals of Economics and Statistics*, (99/100), 117–139.
- Mundlak, Y. (1978). On the Pooling of Time Series and Cross Section Data. *Econometrica*, 46(1), 69–85.
- Wooldridge, J. M. (2010). *Econometric Analysis of Cross Section and Panel Data* (2nd ed.). MIT Press.
- INSEE. *Enquête Emploi en Continu* — Documentation méthodologique, 2014–2016.

---

*Panel Econometrics & Labor Economics — ENSAE Paris*
