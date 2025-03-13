# Installer les packages nécessaires
if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

# Charger les bibliothèques
library(quantmod)
library(ggplot2)
library(TTR)  # Pour les moyennes mobiles exponentielles (EMA)
library(dplyr)
library(scales)  # Assurer une bonne gestion des dates

# Télécharger les données Bitcoin
btc_data <- getSymbols("BTC-USD", src = "yahoo", auto.assign = FALSE)

# Convertir les données en data.frame
btc_df <- data.frame(Date = index(btc_data), coredata(btc_data))
colnames(btc_df)[2] <- "Close"

# Filtrer les données à partir du 1er janvier 2015
start_date <- as.Date("2015-01-01")
end_display_date <- as.Date("2035-01-01")
btc_df <- btc_df[btc_df$Date >= start_date & btc_df$Date <= end_display_date, ]

# Calculer les moyennes mobiles exponentielles (EMA 4 jours et 1 an)
btc_df <- btc_df %>%
  mutate(
    MA4Y = zoo::rollmean(Close, k = 365.25/2, fill = NA, align = "right"),
    EMA1Y = EMA(Close, n = 365.25/4)    # Moyenne mobile exponentielle 1 an (365 jours)
  )

# Ajouter les dates des halvings passés et futurs
halving_dates <- as.Date(c(
  "2016-07-09", # 1er halving
  "2020-05-11", # 2e halving
  "2024-04-20", # 3e halving (prévision)
  "2028-04-15"  # 4e halving (prévision)
))
halving_labels <- rep("Halving", length(halving_dates))

# Générer les lignes verticales tous les 2 ans à partir de 2016, limité à 2032
vertical_lines <- seq(from = as.Date("2016-01-01"), to = end_display_date, by = "2 years")

# Vérifier que la dernière zone ne dépasse pas la limite définie
valid_zones <- sum(vertical_lines <= end_display_date) - 1

# Ajouter les étoiles pour les points les plus hauts et les plus bas dans chaque zone
extrema <- lapply(seq(1, valid_zones - 1, by = 2), function(i) {
  subset_data <- btc_df %>% filter(Date >= vertical_lines[i] & Date < vertical_lines[i + 1])
  if (nrow(subset_data) > 0) {
    data.frame(
      Date = c(subset_data$Date[which.max(subset_data$Close)], subset_data$Date[which.min(subset_data$Close)]),
      Close = c(max(subset_data$Close, na.rm = TRUE), min(subset_data$Close, na.rm = TRUE)),
      Color = c("green", "red")
    )
  }
}) %>% bind_rows()

# Créer un graphique avec ggplot2
ggplot(data = btc_df, aes(x = Date)) +
  geom_line(aes(y = Close), color = "black", size = 0.5) +              # Prix BTC
  geom_line(aes(y = MA4Y), color = "orange", linetype = "solid", size = 0.25) +  # Moyenne mobile exponentielle 4 jours
  geom_line(aes(y = EMA1Y), color = "yellow", linetype = "solid", size =0.25) + # Moyenne mobile exponentielle 1 an
  geom_vline(xintercept = as.numeric(halving_dates), color = "blue") +
  geom_vline(xintercept = as.numeric(vertical_lines[seq(1, valid_zones, by = 2)]), color = "green", linetype = "dashed") + # Lignes vertes
  geom_vline(xintercept = as.numeric(vertical_lines[seq(2, valid_zones, by = 2)]), color = "red", linetype = "dashed") + # Lignes rouges
  annotate("rect", xmin = vertical_lines[seq(1, valid_zones - 1, by = 2)], 
           xmax = vertical_lines[seq(2, valid_zones, by = 2)], 
           ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) + # Zones hachurées
  geom_text(data = extrema, aes(x = Date, y = Close, color = Color), label = "★", size = 5) + # Ajout des étoiles
  scale_color_identity() +
  geom_text(data = data.frame(Date = halving_dates, Label = halving_labels),
            aes(x = Date, y = max(btc_df$Close, na.rm = TRUE), label = Label),
            vjust = -1, color = "blue", size = 4) +
  scale_x_date(labels = date_format("%Y")) + # Correction du format des dates
  labs(title = "Prix du Bitcoin avec moyennes mobiles exponentielles et dates des halvings",
       x = "Date",
       y = "Prix de clôture") +
  theme_minimal()

################################################################################
###############
# Prévisions #
##############

# Télécharger les données historiques du Bitcoin depuis Yahoo Finance
btc_data <- data.frame(Date = index(btc_data), coredata(btc_data))
colnames(btc_data) <- c("ds", "Open", "High", "Low", "y", "Volume", "Adjusted")  # Renommer 'Close' en 'y'

# Préparer les données pour Prophet
btc_data <- btc_data[, c("ds", "y")]  # Garder uniquement les colonnes nécessaires

# Initialiser le modèle Prophet
model <- prophet()

# Ajouter une saisonnalité tous les 4 ans
model <- add_seasonality(model, name = 'quadrennial', period = 365.25 * 4, fourier.order = 3)

# Ajuster le modèle
model <- fit.prophet(model, btc_data)

# Créer un dataframe pour les prévisions sur 365 jours
future <- make_future_dataframe(model, periods = 365.25 * 8)
forecast <- predict(model, future)

# Afficher les prévisions avec Prophet
plot(model, forecast) +
  ggtitle("Prévisions des prix du Bitcoin avec Prophet") +
  xlab("Date") +
  ylab("Prix en USD")

# Afficher les composantes des prévisions (y compris la saisonnalité tous les 4 ans)
prophet_plot_components(model, forecast)

################################################################################
###################
# Les variations #
##################

# Obtenir le prix de clôture tous les 2 ans, en incluant le dernier point si nécessaire
closing_prices <- btc_df %>%
  filter(Date %in% vertical_lines | Date == max(btc_df$Date)) %>%
  arrange(Date) %>%
  select(Date, Close)

# Calculer les plus-values entre les périodes spécifiques, arrondies en pourcentage précis,
# ajouter une colonne de division des gains et une colonne multiplicateur de plus-value
closing_prices <- closing_prices %>%
  mutate(
    PlusValue = paste0(round(((Close - lag(Close)) / lag(Close)) * 100, 2), " %"),  # Plus-value en pourcentage avec précision
    Multiplicateur = round((Close - lag(Close)) / lag(Close) + 1, 2)  # Multiplicateur de plus-value
  )

print(closing_prices)


# Télécharger les données Bitcoin
btc_data <- getSymbols("BTC-USD", src = "yahoo", auto.assign = FALSE)

# Transformation des données
btc_df <- data.frame(date = index(btc_data),
                     price = as.numeric(Cl(btc_data)))

# Filtrer depuis 2015
btc_df <- btc_df %>% filter(date >= as.Date("2015-01-01"))

# Vérifier que la colonne price n'a pas de valeurs NA
btc_df <- btc_df %>% filter(!is.na(price))

# Calcul de la variation journalière en pourcentage
btc_df <- btc_df %>% mutate(percentage_change = (price / lag(price) - 1) * 100)

# Supprimer les lignes avec NA après le lag
btc_df <- btc_df %>% filter(!is.na(percentage_change))

# Calcul de la variance et de l'écart-type glissants (fenêtre de 30 jours)
btc_df <- btc_df %>% mutate(
  variance_change = rollapply(percentage_change, width = 30, FUN = var, fill = NA, align = "right"),
  std_dev_change = rollapply(percentage_change, width = 30, FUN = sd, fill = NA, align = "right")
)

# Ajouter une moyenne mobile exponentielle (EMA) sur 30 jours
btc_df <- btc_df %>% mutate(ema_change = EMA(percentage_change, n = 30))

# Ajouter le cours logarithmique du Bitcoin
btc_df <- btc_df %>% mutate(log_price = log(price))

# Premier graphique : Variation journalière, EMA, variance et écart-type
g1 <- ggplot(btc_df, aes(x = date)) +
  geom_line(aes(y = percentage_change), color = "blue", alpha = 0.5) +  # Variations journalières
  geom_line(aes(y = ema_change), color = "red", size = 1) +  # Tendance moyenne mobile
  geom_line(aes(y = variance_change), color = "purple", size = 1, linetype = "dotted") +  # Variance glissante
  geom_line(aes(y = std_dev_change), color = "orange", size = 1, linetype = "twodash") +  # Écart-type glissant
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 1) +  # Ligne zéro
  labs(title = "Variation journalière, EMA 30 jours, variance et écart-type glissants du Bitcoin",
       x = "Année",
       y = "Variation / Variance / Écart-type en %") +
  theme_minimal()

# Deuxième graphique : Cours logarithmique du Bitcoin
g2 <- ggplot(btc_df, aes(x = date, y = log_price)) +
  geom_line(color = "green", size = 1) +
  labs(title = "Cours logarithmique du prix du Bitcoin depuis 2015",
       x = "Année",
       y = "Prix log10") +
  theme_minimal()

# Superposition des deux graphiques dans le même cadre
grid.arrange(g1, g2, ncol = 1)
