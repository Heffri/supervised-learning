# Supervised Statistical Learning — Projects

Two statistical learning projects applying regression and mixed-effects modelling to real datasets. Implemented in R using RMarkdown for reproducible analysis.

**Course:** Introduction to Supervised Statistical Learning — Stockholm University (MT5021)

---

## Project A — Computer Price Regression

Predicting computer prices from hardware specifications using multiple linear regression with model selection and cross-validation.

**Dataset:** 80 computers with 7 features — RAM (MB), clock speed (GHz), hard drive (GB), CD-ROM speed, OS, and age — predicting retail price.

**Methods used:**

- Log-transformed linear regression (correcting right-skewed price distribution)
- Three model selection approaches: full model, manual feature reduction, and backward elimination via AIC (`step()`)
- Outlier detection and removal using Cook's distance
- Multicollinearity diagnostics via VIF
- Predictive evaluation via 80/20 train-test split (RMSE with back-transformation)
- Leave-One-Out Cross-Validation (LOOCV) using `caret`
- Final model comparison table (Adjusted R², RMSE)

→ See `project_a/src/` for the full analysis.

---

## Project B — Mussel Stress Response to Copper Exposure (HSP70)

Analysing whether copper exposure triggers a heat shock protein (HSP70) stress response in blue mussels, using a hierarchical experimental design with mixed-effects models.

**Dataset:** 72 mussel samples from 6 locations across 2 regions (North/South Sweden), exposed to either copper treatment or control conditions. Measured HSP70 and actin protein levels.

**Methods used:**

- Hierarchical mixed-effects models (`lme4` / `lmerTest`) with nested random effects (region → location → container)
- Log-transformation for normality (verified via Shapiro-Wilk test and QQ-plots)
- Variance component analysis — decomposing total variation into region, location, container, and residual
- Treatment × Region interaction modelling
- Likelihood ratio test comparing models with and without interaction
- Back-transformation of coefficients to interpret treatment effect as a multiplicative factor on the original scale
- Full model diagnostics (residual plots, QQ-plots, grouped residual analysis)

**Key finding:** Copper exposure significantly increased normalised HSP70 expression. The treatment effect was estimated as a multiplicative factor on the original protein scale, with confidence intervals derived from the log-scale model.

→ See `project_b/src/` for the full analysis.

---

## Repository Structure

```
supervised-learning/
├── project_a/
│   ├── src/
│   │   ├── main.Rmd        # Full RMarkdown analysis
│   │   └── slut.R          # Final model comparison script
│   └── data/
│       └── A7_datorer.csv   # Computer hardware dataset
├── project_b/
│   ├── src/
│   │   ├── main.Rmd        # Full RMarkdown analysis
│   │   ├── functions.R     # All modelling and diagnostics code
│   │   └── final_model.r   # Selected final model specification
│   └── data/
│       └── B3_musslor_HSP70.csv  # Mussel HSP70 measurements
├── .gitignore
└── README.md
```

## Requirements

```r
install.packages(c(
  "tidyverse", "caret", "car",        # Project A
  "lme4", "lmerTest", "effects"        # Project B
))
```

## Reproducing the Analysis

Open `project_a/src/main.Rmd` or `project_b/src/main.Rmd` in RStudio and knit to HTML or PDF.
