# --- Klimascenarioanalyse for Søgne  ---

library(mgcv)
library(ggplot2)
library(dplyr)
library(lubridate)

# 1. Les inn data
data <- read.csv("H:/OneDrive/MASTER/Masteroppgave/Sluttmodellering/Komplett/sogne_komplett_med_vind_sol_cleaned.csv")
data$Dato <- as.Date(data$Dato)
data$year <- year(data$Dato)
data$day_of_year <- yday(data$Dato)
data$time_index <- 1:nrow(data)

# 2. Begrens til referanseperiode
data <- data %>% filter(year >= 1980, year <= 2020)

# 3. Log-transformer tilsig
min_non_zero <- min(data$Tilsig[data$Tilsig > 0])
data$log_Tilsig <- log(data$Tilsig + min_non_zero / 10)

# 4. Tren GAM-modell
gam_model <- gam(log_Tilsig ~ s(Temperatur) + s(Nedbor) + s(Evaporasjon) +
                   s(Snodybde) + s(Avrenning) + s(Vindstyrke) + s(Solstraaling) +
                   s(time_index, k=20) + s(day_of_year, bs="cc", k=20),
                 data = data, method = "REML", select = TRUE)

# 5. Beregn daglig gjennomsnitt (x_historisk)
daily_hist <- data %>%
  group_by(day_of_year) %>%
  summarise(
    Temperatur = mean(Temperatur),
    Nedbor = mean(Nedbor),
    Evaporasjon = mean(Evaporasjon),
    Snodybde = mean(Snodybde),
    Avrenning = mean(Avrenning),
    Vindstyrke = mean(Vindstyrke),
    Solstraaling = mean(Solstraaling),
    Tilsig = mean(Tilsig),
    .groups = "drop"
  )

# Fjern dag 366 hvis den finnes
daily_hist <- daily_hist %>% filter(day_of_year <= 365)

# 6. Lag klimajustert versjon for 2100 (Søgne)
daily_2100 <- daily_hist
daily_2100$Temperatur   <- daily_hist$Temperatur + 4.2
daily_2100$Nedbor       <- daily_hist$Nedbor * 1.10
daily_2100$Evaporasjon  <- daily_hist$Evaporasjon * 1.105
daily_2100$Snodybde     <- daily_hist$Snodybde * 0.05
# Øvrige variabler beholdes uendret

# 7. Legg til dato og forklaringsvariabler
daily_hist$Dato <- as.Date(daily_hist$day_of_year, origin = "2099-12-31")
daily_2100$Dato <- daily_hist$Dato

daily_hist$time_index <- 1:nrow(daily_hist)
daily_2100$time_index <- 1:nrow(daily_2100)

daily_hist$day_of_year <- 1:365
daily_2100$day_of_year <- 1:365

# 8. Predikér tilsig med GAM
mse_log <- mean(resid(gam_model)^2)

daily_hist$Tilsig_pred <- exp(
  predict(gam_model, newdata = daily_hist) + 0.5 * mse_log
) - min_non_zero / 10

daily_2100$Tilsig_pred <- exp(
  predict(gam_model, newdata = daily_2100) + 0.5 * mse_log
) - min_non_zero / 10


# 9. Plott resultater
plot_data <- bind_rows(
  daily_hist %>% select(Dato, Tilsig_pred) %>% mutate(Scenario = "Historisk (1980–2020)"),
  daily_2100 %>% select(Dato, Tilsig_pred) %>% mutate(Scenario = "2100 (RCP8.5)")
)

ggplot(plot_data, aes(x = Dato, y = Tilsig_pred, color = Scenario)) +
  geom_line(size = 1) +
  labs(
    title = "Søgne: Modellert daglig tilsig historisk vs. 2100",
    x = "Dato", y = "Tilsig (m³/s)", color = "Scenario"
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_minimal() +
  theme(legend.position = "top")  

# Lagre som TIFF i standardkatalog
ggsave(
  filename = "Søgne_klimascenario_2100.tiff",
  plot = p,
  device = "tiff",
  path = "./",     # ← lagres i nåværende arbeidsmappe
  width = 8, height = 5, dpi = 300, units = "in"
)

# 10. Legg til sesong for historisk og 2100
daily_hist$season <- factor(case_when(
  month(daily_hist$Dato) %in% c(12, 1, 2) ~ "Vinter",
  month(daily_hist$Dato) %in% c(3, 4, 5) ~ "Vår",
  month(daily_hist$Dato) %in% c(6, 7, 8) ~ "Sommer",
  TRUE ~ "Høst"
), levels = c("Vinter", "Vår", "Sommer", "Høst"))

daily_2100$season <- daily_hist$season  # samme datoer

# 11. Beregn årlig og sesongvis endring
season_summary <- daily_hist %>%
  group_by(season) %>%
  summarise(historisk = mean(Tilsig_pred), .groups = "drop") %>%
  left_join(
    daily_2100 %>%
      group_by(season) %>%
      summarise(framtid = mean(Tilsig_pred), .groups = "drop"),
    by = "season"
  ) %>%
  mutate(endring_prosent = (framtid / historisk - 1) * 100)

# Årlig endring
aarlig_endring <- (mean(daily_2100$Tilsig_pred) / mean(daily_hist$Tilsig_pred) - 1) * 100

# 12. Skriv ut
print("Årlig endring i tilsig (%):")
print(round(aarlig_endring, 2))

print("Sesongvis endring i tilsig (%):")
print(season_summary %>% select(season, endring_prosent) %>% mutate(endring_prosent = round(endring_prosent, 2)))

