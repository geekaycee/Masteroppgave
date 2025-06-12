## Sluttkode Søgne ##

library(mgcv)       # GAM-modellering  
library(forecast)   # ARIMA og prognoser  
library(ggplot2)    # Visualisering  
library(dplyr)      # Data-manipulasjon  
library(tidyr)      # Omforming til langt format  
library(gratia)     # Residualplott

# Les inn data og forbered  
data <- read.csv("H:/OneDrive/MASTER/Masteroppgave/Sluttmodellering/Komplett/sogne_komplett_med_vind_sol_cleaned.csv")  
data$Dato <- as.Date(data$Dato)  
data$time_index <- 1:nrow(data)  
data$day_of_year <- as.numeric(format(data$Dato, "%j"))  
min_non_zero <- min(data$Tilsig[data$Tilsig > 0])  
data$log_Tilsig <- log(data$Tilsig + min_non_zero/10)  

# Tilpass GAM  
gam_model <- gam(log_Tilsig ~ s(Temperatur, bs="tp") +   
                   s(Nedbor, bs="tp") +   
                   s(Evaporasjon, bs="tp") +   
                   s(Snodybde, bs="tp") +   
                   s(Avrenning, bs="tp") +   
                   s(Vindstyrke, bs="tp") +   
                   s(Solstraaling, bs="tp") +  
                   s(time_index, k=20) +  
                   s(day_of_year, bs="cc", k=20),  
                 data=data, method="REML", select=TRUE)  

# Få prediksjoner og residualer  
gam_pred <- predict(gam_model)  
gam_resid <- resid(gam_model)  

# Tilpass ARIMA  
arima_model <- auto.arima(ts(gam_resid))  
arima_fitted <- fitted(arima_model)  

# Lag hybrid prediksjoner  
hybrid_pred_log <- gam_pred + arima_fitted  
mse_log <- mean(gam_resid^2)  
hybrid_pred <- exp(hybrid_pred_log + 0.5 * mse_log) - min_non_zero/10

# Legg prediksjonen inn i datasettet 
data$hybrid_pred <- hybrid_pred

# Beregn ytelsesmål  
rmse_val <- sqrt(mean((data$Tilsig - hybrid_pred)^2))  
mae_val <- mean(abs(data$Tilsig - hybrid_pred))  
r2_val <- cor(data$Tilsig, hybrid_pred)^2  
dev_explained <- summary(gam_model)$dev.expl * 100  

# Skriv ut ytelsesmål  
cat("Ytelsesmål for hybridmodellen:\n")  
cat("RMSE:", round(rmse_val, 3), "\n")  
cat("MAE:", round(mae_val, 3), "\n")  
cat("R-squared:", round(r2_val, 3), "\n")  
cat("Forklart varians (%):", round(dev_explained, 1), "\n")  
cat("GAM AIC:", round(AIC(gam_model), 1), "\n")  
cat("ARIMA AIC:", round(AIC(arima_model), 1), "\n\n")  

# Lag residualplott med gratia  
appraise(gam_model)  

# 1. Lagre smooth plot med 4K oppløsning
tiff("smooth_plot_4K.tiff", width = 3840, height = 2160, res = 300, compression = "lzw")
draw(gam_model)       # Dette tegner plottet til filen
dev.off()             # Lukker filen

# 2. Vis samme plott i RStudio
draw(gam_model)

# Vis edf-verdiene direkte
s_table <- summary(gam_model)$s.table
print("Effective degrees of freedom (edf) for each smooth term:")
print(round(s_table[,"edf"], 2))

# Beregn concurvity for modellen  
conc <- concurvity(gam_model, full = TRUE)  
print("Concurvity-resultater:")  
print(conc)  


#Plott av predikerte vs observerte verdier for 3 år.

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Sørg for at hybrid_pred er numerisk
data <- data %>% mutate(hybrid_pred = as.numeric(hybrid_pred))

# Velg tre år
years_to_plot <- c(1980, 1994, 2008)

# Filtrer og klargjør data
plot_data <- data %>%
  filter(year(Dato) %in% years_to_plot) %>%
  mutate(
    År = factor(year(Dato)),
    Dato_plot = as.Date(format(Dato, "2000-%m-%d"))  # alle datoer får år 2000
  ) %>%
  pivot_longer(
    cols = c("Tilsig", "hybrid_pred"),
    names_to = "Serie",
    values_to = "Verdi"
  ) %>%
  mutate(Serie = recode(Serie,
                        "Tilsig" = "Observert",
                        "hybrid_pred" = "Modellert"))

# Lag figur
fig <- ggplot(plot_data, aes(x = Dato_plot, y = Verdi)) +
  geom_line(aes(color = Serie, linetype = Serie), size = 0.8) +
  scale_color_manual(values = c("Observert" = "blue", "Modellert" = "darkorange")) +
  scale_linetype_manual(values = c("Observert" = "solid", "Modellert" = "solid")) +
  facet_wrap(~ År, ncol = 1, scales = "free_y") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(
    title    = "Observert vs. Modellert tilsig for Søgne",
    subtitle = "Utvalgte år: 1980, 1994, 2008 (x-akse viser måneder)",
    x        = "Måned",
    y        = "Tilsig (m³/s)",
    color    = "Serie",
    linetype = "Serie"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x   = element_text(angle = 45, hjust = 1)
  )

# Lagre til A4 med høy vertikal plass
ggsave("tilsig_søgne_A4_høyde.png", plot = fig, width = 21, height = 29.7, units = "cm", dpi = 300)

# Lagre som TIFF (stående A4, 300 dpi)
ggsave("tilsig_søgne_A4_høyde.tiff", plot = fig, width = 21, height = 29.7, units = "cm", dpi = 300, device = "tiff")





## Rolling origin validering ##


library(dplyr)
library(mgcv)
library(forecast)

# Forbered datasettet 
data$Year <- as.integer(format(data$Dato, "%Y"))

# Initialiser tomt resultatobjekt
results <- data.frame(
  TestYear = integer(),
  RMSE = numeric(),
  MAE = numeric(),
  R2 = numeric()
)

# Start rolling origin fra 2011 til 2020
for (test_year in 2011:2020) {
  
  cat("\nBehandler år:", test_year, "\n")
  
  # Treningsdata: alt frem til året før
  train_data <- data %>% filter(Year < test_year)
  test_data  <- data %>% filter(Year == test_year)
  
  # Sjekk at vi har nok data
  if (nrow(test_data) == 0) next
  
  # Tilpass GAM
  model <- gam(log_Tilsig ~ s(Temperatur, bs="tp") +
                 s(Nedbor, bs="tp") +
                 s(Evaporasjon, bs="tp") +
                 s(Snodybde, bs="tp") +
                 s(Avrenning, bs="tp") +
                 s(Vindstyrke, bs="tp") +
                 s(Solstraaling, bs="tp") +
                 s(time_index, k=20) +
                 s(day_of_year, bs="cc", k=20),
               data = train_data, method = "REML", select = TRUE)
  
  # Prediksjon for testår
  gam_pred_log <- predict(model, newdata = test_data)
  
  # Bias-korreksjon
  mse_train <- mean(resid(model)^2)
  gam_pred <- exp(gam_pred_log + 0.5 * mse_train) - min_non_zero/10
  
  # Beregn ytelsesmål
  rmse <- sqrt(mean((test_data$Tilsig - gam_pred)^2))
  mae  <- mean(abs(test_data$Tilsig - gam_pred))
  r2   <- cor(test_data$Tilsig, gam_pred)^2
  
  # Lagre resultat
  results <- rbind(results, data.frame(
    TestYear = test_year,
    RMSE = rmse,
    MAE = mae,
    R2 = r2
  ))
}

# Vis resultater per år
print(results)

# Gjennomsnittlig ytelse
cat("\nGjennomsnittlig ytelse over 2011–2020:\n")
cat("RMSE:", round(mean(results$RMSE), 2), "\n")
cat("MAE:", round(mean(results$MAE), 2), "\n")
cat("R²:", round(mean(results$R2), 3), "\n")





# --- SAMMENLIGNING MOT HBV FOR PERIODEN 10.02.1992–31.12.1992 (Sogne, external validation) --- #

library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyr)

# 1. Definer observasjoner og HBV-prediksjoner
data <- data %>%
  mutate(
    HBV = ifelse(Dato >= as.Date("1992-02-10") & Dato <= as.Date("1992-12-31"), Tilsig, NA),
    Observasjon = ifelse(Dato >= as.Date("1980-01-01") & Dato <= as.Date("1992-02-09"), Tilsig, NA)
  )

# 2. Tren/test-splitt: bruk observasjoner frem til 09.02.1992 til å trene, prediker for HBV-perioden
train_data <- data %>% filter(!is.na(Observasjon))
test_data  <- data %>% filter(!is.na(HBV))

# 3. Tilpass GAM på treningsdata
model <- gam(log_Tilsig ~ s(Temperatur, bs = "tp") +
               s(Nedbor, bs = "tp") +
               s(Evaporasjon, bs = "tp") +
               s(Snodybde, bs = "tp") +
               s(Avrenning, bs = "tp") +
               s(Vindstyrke, bs = "tp") +
               s(Solstraaling, bs = "tp") +
               s(time_index, k = 20) +
               s(day_of_year, bs = "cc", k = 20),
             data = train_data, method = "REML", select = TRUE)

# 4. Predikér HBV-perioden
gam_pred_log <- predict(model, newdata = test_data)
mse_train <- mean(resid(model)^2)
gam_pred <- exp(gam_pred_log + 0.5 * mse_train) - min_non_zero / 10

# 5. Sammenligningsdatasett
sammenlign_data <- test_data %>%
  mutate(GAM = gam_pred) %>%
  select(Dato, GAM, HBV)

# 6. Statistikk: gjennomsnitt og standardavvik
statistikk <- sammenlign_data %>%
  summarise(
    Gjennomsnitt_GAM = mean(GAM, na.rm = TRUE),
    Gjennomsnitt_HBV = mean(HBV, na.rm = TRUE),
    SD_GAM = sd(GAM, na.rm = TRUE),
    SD_HBV = sd(HBV, na.rm = TRUE)
  )

cat("Sammenligning av gjennomsnitt og standardavvik:\n")
print(round(statistikk, 2))

# 7. Tillegg: korrelasjon og feil
pearson_corr <- cor(sammenlign_data$GAM, sammenlign_data$HBV, method = "pearson", use = "complete.obs")
spearman_corr <- cor(sammenlign_data$GAM, sammenlign_data$HBV, method = "spearman", use = "complete.obs")
rmse <- sqrt(mean((sammenlign_data$GAM - sammenlign_data$HBV)^2, na.rm = TRUE))
mae  <- mean(abs(sammenlign_data$GAM - sammenlign_data$HBV), na.rm = TRUE)

cat("\nYtelsesmål mellom GAM og HBV:\n")
cat("Pearson korrelasjon:", round(pearson_corr, 3), "\n")
cat("Spearman korrelasjon:", round(spearman_corr, 3), "\n")
cat("RMSE:", round(rmse, 2), "\n")
cat("MAE:", round(mae, 2), "\n")

# 8. Plot
library(ggplot2)
library(dplyr)
library(tidyr)

# Omform data til langt format
plot_data <- sammenlign_data %>%
  pivot_longer(cols = c("GAM", "HBV"), names_to = "Modell", values_to = "Tilsig")

# Lagre som TIFF
tiff("GAM_vs_HBV_Sogne.tiff", width = 2000, height = 1200, res = 300)

ggplot(plot_data, aes(x = Dato, y = Tilsig, color = Modell)) +
  geom_line(size = 0.7) +
  labs(
    title = "",
    subtitle = "Periode: 10.02.1992–31.12.1992",
    x = "Dato", y = "Tilsig (m³/s)",
    color = "Modell"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

dev.off()


# 9. Antall observasjoner
cat("Antall dager i sammenligningsperioden:", nrow(test_data), "\n")






