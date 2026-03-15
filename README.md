# Supervised Statistical Learning — Projects

Two statistical learning projects applying regression and classification methods to real datasets. Implemented in R using RMarkdown for reproducible analysis.

**Course:** Introduction to Supervised Statistical Learning — Stockholm University (MT5021)

---

## Project A — [Short description of what Project A was about]

Exploratory and predictive analysis using supervised learning techniques on the `A7_datorer` dataset.

**Methods used:** <!-- e.g. Linear regression, LASSO, Ridge, cross-validation -->

**Key findings:** <!-- 1-2 sentences about what you found -->

→ See `project_a/report/` for the full report.

---

## Project B — [Short description of what Project B was about]

<!-- Brief description of Project B's dataset and goal -->

**Methods used:** <!-- e.g. Classification trees, random forests, logistic regression -->

**Key findings:** <!-- 1-2 sentences -->

→ See `project_b/report/` for the full report.

---

## Repository Structure

```
supervised-learning/
├── project_a/
│   ├── src/
│   │   └── main.Rmd        # Analysis and modelling
│   ├── data/               # Dataset(s) used
│   └── report/             # Rendered PDF/HTML report
├── project_b/
│   ├── src/
│   │   └── main.Rmd
│   ├── data/
│   └── report/
└── README.md
```

## Requirements

R packages used:

```r
install.packages(c("tidyverse", "caret", "glmnet", "rmarkdown", "knitr"))
```

## Reproducing the Analysis

Open `project_a/src/main.Rmd` or `project_b/src/main.Rmd` in RStudio and knit to HTML or PDF.

## What's Next

- [ ] Fill in project descriptions above once reports are reviewed
- [ ] Extend Project B with an additional model comparison
- [ ] Add a combined summary notebook comparing methods across both projects
