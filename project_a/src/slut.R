---
    title: "Analys av Datordata - Regressionsmodellering"
author: ""
date: "`r Sys.Date()`"
output: html_document
---
    
```
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

## Inledning

Denna rapport analyserar datordata genom att använda linjär regression med log-transformerad responsvariabel. Vi jämför olika modellselektionsmetoder och utvärderar modellprestanda genom korsvalidering.

## Datainläsning och förberedelser

```{r}
# Ladda data och förberedelser
data <- read.csv("A7_datorer.csv")

# Kontrollera saknade värden
sum(is.na(data))

# Rensa och kategorisera om faktorer
cleaned_data <- data
cleaned_data$x6 <- factor(cleaned_data$x6)
levels(cleaned_data$x6)[levels(cleaned_data$x6) %in% c('""""', "WinNT4.0", "Win95", "MSOffice", "WinNT")] <- "j"
cleaned_data$x5 <- factor(cleaned_data$x5)
levels(cleaned_data$x5)[levels(cleaned_data$x5) %in% c("8x", "12x", "16x", "20x", "24x")] <- "Other"
cleaned_data$x1 <- factor(cleaned_data$x1)
```

## Inledande modellanalys

```{r}
# Inledande modell: full modell på original data
full_model <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7, data = cleaned_data)

# VIF för multikolinearitetskontroll
library(car)
vif(full_model)

plot(full_model, main = "Diagnostik full modell")
```

## Outlier-hantering med Cook's Distance

```{r}
# Hantering av outliers med Cook's distance (log-transformerad y)
full_model_log <- lm(log(y) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7, data = cleaned_data)
cook_threshold <- 4 / nrow(cleaned_data)
cooks_d <- cooks.distance(full_model_log)
outliers <- which(cooks_d > cook_threshold)
data_no_outliers_full <- cleaned_data[-outliers, ]

# Samma för minimierad och backward modeller på log(y)
minimized_model_log <- lm(log(y) ~ x1 + x2 + x3 + x7, data = cleaned_data)
cooks_d_min <- cooks.distance(minimized_model_log)
outliers_min <- which(cooks_d_min > cook_threshold)
data_no_outliers_min <- cleaned_data[-outliers_min, ]

backward_start <- lm(log(y) ~ x1 + x2 + x3 + x4 + x6 + x7, data = cleaned_data)
backward_model_log <- step(backward_start, direction = "backward", trace = FALSE)
cooks_d_back <- cooks.distance(backward_model_log)
outliers_back <- which(cooks_d_back > cook_threshold)
data_no_outliers_back <- cleaned_data[-outliers_back, ]
```

## Slutgiltiga modeller på rensade data

```{r}
# Skapa slutgiltiga modeller på rensade data
full_model_final <- lm(log(y) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7, data = data_no_outliers_full)
minimized_model_final <- lm(log(y) ~ x1 + x2 + x3 + x7, data = data_no_outliers_min)
backward_model_final <- step(lm(log(y) ~ x1 + x2 + x3 + x4 + x6 + x7, data = data_no_outliers_back),
                             direction = "backward", trace = FALSE)
```

## Modelldiagnostik

```
# Diagnostikplotar och VIF för slutmodeller
plot_diagnostics <- function(model, data, title_prefix) {
    par(mfrow = c(2,2))
    plot(model, main = paste(title_prefix, "- Diagnostik"))
    par(mfrow = c(1,1))
    
    cat(title_prefix, "VIF:\n")
    print(vif(model))
}
```

```{r}
plot_diagnostics(full_model_final, data_no_outliers_full, "Full modell")
plot_diagnostics(minimized_model_final, data_no_outliers_min, "Minimerad modell")
plot_diagnostics(backward_model_final, data_no_outliers_back, "Backward elimination")
```

## Prediktiv utvärdering med Train/Test Split

```{r}
# Prediktiv förmåga via RMSE (train/test-split med back-transform)
set.seed(123)
train_index <- sample(seq_len(nrow(cleaned_data)), size = 0.8 * nrow(cleaned_data))
train_data <- cleaned_data[train_index, ]
test_data <- cleaned_data[-train_index, ]

# Funktion för log-modell med back-transform av prediktioner för RMSE
rmse_log_model <- function(formula, train_dat, test_dat) {
    mod <- lm(formula, data = train_dat)
    pred_log <- predict(mod, newdata = test_dat)
    pred_original <- exp(pred_log)
    sqrt(mean((test_dat$y - pred_original)^2))
}

rmse_full <- rmse_log_model(log(y) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7, train_data, test_data)
rmse_min <- rmse_log_model(log(y) ~ x1 + x2 + x3 + x7, train_data, test_data)
rmse_back <- rmse_log_model(backward_model_final$call$formula, train_data, test_data)

cat("RMSE Full modell:", rmse_full, "\n")
cat("RMSE Minimerad modell:", rmse_min, "\n")
cat("RMSE Backward elimination:", rmse_back, "\n")
```

## Leave-One-Out Cross-Validation

```{r}
# LOOCV med caret på back-transformed prediktioner
library(caret)
set.seed(123)

train_control <- trainControl(method = "LOOCV", savePredictions = "final", summaryFunction = defaultSummary)

train_loocv <- function(formula, data) {
    model <- train(formula, data = data, method = "lm", trControl = train_control, metric = "RMSE")
    model$pred$pred_original <- exp(model$pred$pred)
    model$pred$obs_original <- exp(model$pred$obs)
    rmse_orig <- sqrt(mean((model$pred$obs_original - model$pred$pred_original)^2))
    list(model = model, rmse_original = rmse_orig)
}

loocv_full <- train_loocv(log(y) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7, data_no_outliers_full)
loocv_min  <- train_loocv(log(y) ~ x1 + x2 + x3 + x7, data_no_outliers_min)
loocv_back <- train_loocv(backward_model_final$call$formula, data_no_outliers_back)

cat("LOOCV RMSE Full:", loocv_full$rmse_original, "\n")
cat("LOOCV RMSE Min:", loocv_min$rmse_original, "\n")
cat("LOOCV RMSE Back:", loocv_back$rmse_original, "\n")
```

## Sammanställning av resultat

```{r}
# Sammanställning av modeller och resultat
summary_table <- data.frame(
    Modell = c("Full modell", "Minimerad modell", "Backward elimination"),
    Dataset = c("data_no_outliers_full", "data_no_outliers_min", "data_no_outliers_back"),
    Adjusted_R2 = c(summary(full_model_final)$adj.r.squared,
                    summary(minimized_model_final)$adj.r.squared,
                    summary(backward_model_final)$adj.r.squared),
    RMSE_LOOCV = c(loocv_full$rmse_original, loocv_min$rmse_original, loocv_back$rmse_original),
    stringsAsFactors = FALSE
)

knitr::kable(summary_table, caption = "Sammanställning av modellprestanda")
```
