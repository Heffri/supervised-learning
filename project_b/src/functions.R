# =============================================================================
# ÖVNINGSPROJEKT B3: HSP70-studie på musslor
# =============================================================================

# Ladda nödvändiga paket
library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(car)

# Läs in data
raw_data <- read.csv("data/raw/B3_musslor_HSP70.csv")

# Konvertera till faktorer
raw_data$omr <- as.factor(raw_data$omr)
raw_data$lokal <- as.factor(raw_data$lokal)
raw_data$behandl <- as.factor(raw_data$behandl)
raw_data$behallare <- as.factor(raw_data$behallare)

# Normalisera HSP70
raw_data$hsp70_norm <- raw_data$hsp70 / raw_data$aktin

# =============================================================================
# FRÅGESTÄLLNING 1: Analysera variationen för kontrollerna
# =============================================================================

# Filtrera ut endast kontroller
kontroll_data <- subset(raw_data, behandl == "Kontroll")

cat("\n=== FRÅGESTÄLLNING 1: ANALYS AV KONTROLLER ===\n\n")

# --- Deskriptiv statistik ---
cat("Deskriptiv statistik för normaliserad HSP70 (kontroller):\n")
print(summary(kontroll_data$hsp70_norm))

cat("\nDeskriptiv statistik per område:\n")
print(by(kontroll_data$hsp70_norm, kontroll_data$omr, summary))

# Visualisera fördelningen
par(mfrow = c(1, 2))
boxplot(hsp70_norm ~ omr,
    data = kontroll_data,
    main = "HSP70_norm per område (kontroller)",
    xlab = "Område", ylab = "Normaliserad HSP70"
)
hist(kontroll_data$hsp70_norm,
    main = "Fördelning HSP70_norm",
    xlab = "Normaliserad HSP70", col = "lightblue"
)

# --- Undersök transformationer ---
cat("\n--- Undersökning av transformationer ---\n")

# Testa log-transformation
kontroll_data$log_hsp70_norm <- log(kontroll_data$hsp70_norm)

par(mfrow = c(2, 2))
hist(kontroll_data$hsp70_norm,
    main = "Original data",
    xlab = "HSP70_norm", col = "lightblue"
)
qqnorm(kontroll_data$hsp70_norm, main = "QQ-plot: Original")
qqline(kontroll_data$hsp70_norm)

hist(kontroll_data$log_hsp70_norm,
    main = "Log-transformerad data",
    xlab = "log(HSP70_norm)", col = "lightgreen"
)
qqnorm(kontroll_data$log_hsp70_norm, main = "QQ-plot: Log-transform")
qqline(kontroll_data$log_hsp70_norm)

# Shapiro-Wilk test för normalitet
cat("\nShapiro-Wilk test för normalitet:\n")
cat("Original data: p =", shapiro.test(kontroll_data$hsp70_norm)$p.value, "\n")
cat("Log-transformerad: p =", shapiro.test(kontroll_data$log_hsp70_norm)$p.value, "\n")

# --- Hierarkisk modell för variationsanalys ---
cat("\n--- Hierarkisk mixed-effects modell ---\n")

# Modell 1: Original skala
modell_kontroll_orig <- lmer(
    hsp70_norm ~ omr + (1 | omr:lokal) + (1 | omr:lokal:behallare),
    data = kontroll_data
)

cat("\nModell på original skala:\n")
print(summary(modell_kontroll_orig))
print(anova(modell_kontroll_orig))

# Modell 2: Log-transformerad
modell_kontroll_log <- lmer(
    log_hsp70_norm ~ omr + (1 | omr:lokal) + (1 | omr:lokal:behallare),
    data = kontroll_data
)

cat("\nModell på log-skala:\n")
print(summary(modell_kontroll_log))
print(anova(modell_kontroll_log))

# --- Modelldiagnostik ---
cat("\n--- Modelldiagnostik ---\n")

par(mfrow = c(2, 2))

# Original modell
plot(modell_kontroll_orig, main = "Residuals vs Fitted (Original)")
res_orig <- residuals(modell_kontroll_orig)
qqnorm(res_orig, main = "QQ-plot residualer (Original)")
qqline(res_orig)

# Log-transformerad modell
plot(modell_kontroll_log, main = "Residuals vs Fitted (Log)")
res_log <- residuals(modell_kontroll_log)
qqnorm(res_log, main = "QQ-plot residualer (Log)")
qqline(res_log)

# --- Varianskomponenter ---
cat("\n--- Varianskomponenter ---\n")

vc_orig <- as.data.frame(VarCorr(modell_kontroll_orig))
cat("\nOriginal skala:\n")
print(vc_orig)

vc_log <- as.data.frame(VarCorr(modell_kontroll_log))
cat("\nLog-skala:\n")
print(vc_log)

# Beräkna proportion av total varians
total_var_orig <- sum(vc_orig$vcov)
cat("\nProportion av total varians (original):\n")
print(vc_orig$vcov / total_var_orig)

total_var_log <- sum(vc_log$vcov)
cat("\nProportion av total varians (log):\n")
print(vc_log$vcov / total_var_log)

# --- Test för områdesskillnad ---
cat("\n--- Statistiskt test för skillnad mellan områden ---\n")

# Från ANOVA-tabellen (redan utförd ovan)
anova_result <- anova(modell_kontroll_log)
cat("\nF-test för område-effekt:\n")
print(anova_result)

# Beräkna medelvärden per område
medel_per_omr <- aggregate(hsp70_norm ~ omr,
    data = kontroll_data,
    FUN = function(x) c(mean = mean(x), sd = sd(x))
)
cat("\nMedelvärden och standardavvikelser per område:\n")
print(medel_per_omr)

# Konfidensintervall för fixed effects
cat("\nKonfidensintervall för områdeseffekt:\n")
print(confint(modell_kontroll_log, parm = "beta_"))

# =============================================================================
# FRÅGESTÄLLNING 2: Analysera behandlingseffekten av koppar
# =============================================================================

cat("\n\n=== FRÅGESTÄLLNING 2: BEHANDLINGSEFFEKT AV KOPPAR ===\n\n")

# Använd hela datasetet
hela_data <- raw_data

# --- Deskriptiv statistik ---
cat("Deskriptiv statistik per behandling:\n")
print(by(hela_data$hsp70_norm, hela_data$behandl, summary))

cat("\nDeskriptiv statistik per behandling och område:\n")
print(aggregate(hsp70_norm ~ behandl + omr,
    data = hela_data,
    FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x))
))

# Visualisera
par(mfrow = c(1, 2))
boxplot(hsp70_norm ~ behandl,
    data = hela_data,
    main = "HSP70_norm per behandling",
    xlab = "Behandling", ylab = "Normaliserad HSP70",
    col = c("lightblue", "salmon")
)

boxplot(hsp70_norm ~ behandl + omr,
    data = hela_data,
    main = "HSP70_norm per behandling och område",
    xlab = "Behandling:Område", ylab = "Normaliserad HSP70",
    col = c("lightblue", "salmon"),
    names = c("Kontroll:N", "Cu:N", "Kontroll:S", "Cu:S")
)

# --- Test för transformation ---
hela_data$log_hsp70_norm <- log(hela_data$hsp70_norm)

par(mfrow = c(1, 2))
hist(hela_data$hsp70_norm,
    main = "HSP70_norm (alla data)",
    col = "lightblue", xlab = "Normaliserad HSP70"
)
hist(hela_data$log_hsp70_norm,
    main = "log(HSP70_norm)",
    col = "lightgreen", xlab = "log(Normaliserad HSP70)"
)

# --- Fullständig blandad modell ---
cat("\n--- Blandad modell med behandling och område ---\n")

# Modell utan samspel
modell_behandling <- lmer(
    log_hsp70_norm ~ behandl + omr +
        (1 | omr:lokal) + (1 | omr:lokal:behallare),
    data = hela_data
)

cat("\nModell utan samspel:\n")
print(summary(modell_behandling))
print(anova(modell_behandling))

# Modell med samspel behandling:område
modell_behandling_samspel <- lmer(
    log_hsp70_norm ~ behandl * omr +
        (1 | omr:lokal) + (1 | omr:lokal:behallare),
    data = hela_data
)

cat("\nModell med samspel behandling:område:\n")
print(summary(modell_behandling_samspel))
print(anova(modell_behandling_samspel))

# Jämför modeller
cat("\nLikelihood ratio test (med vs utan samspel):\n")
print(anova(modell_behandling, modell_behandling_samspel))

# --- Modelldiagnostik ---
cat("\n--- Modelldiagnostik (slutlig modell) ---\n")

par(mfrow = c(2, 2))
plot(modell_behandling_samspel, main = "Residuals vs Fitted")
res_behandling <- residuals(modell_behandling_samspel)
qqnorm(res_behandling, main = "QQ-plot residualer")
qqline(res_behandling)

# Residualer per grupp
plot(hela_data$behandl, res_behandling,
    xlab = "Behandling", ylab = "Residualer",
    main = "Residualer per behandling"
)
abline(h = 0, lty = 2, col = "red")

plot(hela_data$omr, res_behandling,
    xlab = "Område", ylab = "Residualer",
    main = "Residualer per område"
)
abline(h = 0, lty = 2, col = "red")

# --- Samspelsplot ---
cat("\n--- Samspelsplot ---\n")

par(mfrow = c(1, 1))
interaction.plot(hela_data$behandl, hela_data$omr, hela_data$log_hsp70_norm,
    xlab = "Behandling", ylab = "log(HSP70_norm)",
    trace.label = "Område",
    col = c("blue", "red"), lwd = 2,
    main = "Samspel: Behandling × Område"
)

# Alternativ med effects-paketet
if (requireNamespace("effects", quietly = TRUE)) {
    plot(effect("behandl:omr", modell_behandling_samspel),
        multiline = TRUE,
        main = "Samspelseffekt: Behandling × Område"
    )
}

# --- Skattning av behandlingseffekt ---
cat("\n--- Skattning av behandlingseffekt ---\n")

# Fixed effects
fixef_summary <- summary(modell_behandling_samspel)
cat("\nFixed effects:\n")
print(fixef_summary$coefficients)

# Beräkna behandlingseffekt i original skala
# På log-skala: log(Cu) - log(Kontroll) = log(Cu/Kontroll)
# I original skala: exp(koefficient) = multiplikativ effekt

coef_behandl <- fixef(modell_behandling_samspel)["behandlCu"]
cat("\nBehandlingseffekt på log-skala:", coef_behandl, "\n")
cat(
    "Behandlingseffekt i original skala (multiplikativ):",
    exp(coef_behandl), "\n"
)
cat(
    "Detta innebär att Cu-behandling ökar HSP70_norm med en faktor på:",
    exp(coef_behandl), "\n"
)
cat("Eller ökning med", (exp(coef_behandl) - 1) * 100, "%\n")

# Konfidensintervall
ci <- confint(modell_behandling_samspel, parm = "beta_")
cat("\nKonfidensintervall för behandlingseffekt:\n")
print(ci[grep("behandl", rownames(ci)), ])

# Konvertera till original skala
cat("\nKonfidensintervall i original skala (multiplikativ):\n")
ci_behandl <- ci[grep("behandlCu", rownames(ci)), ]
cat("Lower:", exp(ci_behandl[1]), "\n")
cat("Upper:", exp(ci_behandl[2]), "\n")
