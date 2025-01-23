# Installer les packages nécessaires
if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

# Charger les bibliothèques
library(quantmod)
library(ggplot2)
library(prophet)

################################################################################
#######################
# Tracer les halvings #
#######################

# Charger les bibliothèques
library(quantmod)
library(ggplot2)
library(TTR)  # Pour les moyennes mobiles exponentielles (EMA)
library(dplyr)

# Télécharger les données Bitcoin
btc_data <- getSymbols("BTC-USD", src = "yahoo", auto.assign = FALSE)

# Convertir les données en data.frame
btc_df <- data.frame(Date = index(btc_data), coredata(btc_data))
colnames(btc_df)[2] <- "Close"

# Filtrer les données à partir du 1er janvier 2015
start_date <- as.Date("2015-01-01")
btc_df <- btc_df[btc_df$Date >= start_date, ]

# Calculer les moyennes mobiles exponentielles (EMA 4 jours et 1 an)
btc_df <- btc_df %>%
  mutate(
    MA4Y = zoo::rollmean(Close, k = 365.25/2, fill = NA, align = "right"),
    EMA1Y = EMA(Close, n = 365.25/4)    # Moyenne mobile exponentielle 1 an (365 jours)
  )

# Ajouter les dates des halvings passés et futurs
halving_dates <- as.Date(c(
  #"2012-11-28", # 1er halving
  "2016-07-09", # 2e halving
  "2020-05-11", # 3e halving
  "2024-04-20", # 4e halving (prévision)
  "2028-04-15", # 5e halving (prévision)
  "2032-04-10"  # 6e halving (prévision)
))
halving_labels <- paste("Halving", 1:length(halving_dates))

# Ajouter les débuts et fins des années de halving
halving_years <- format(halving_dates, "%Y")
start_of_years <- as.Date(paste0(halving_years, "-01-01"))
end_of_years <- as.Date(paste0(halving_years, "-12-31"))

# Créer un graphique avec ggplot2
ggplot(data = btc_df, aes(x = Date)) +
  geom_line(aes(y = Close), color = "black", size = 0.25) +              # Prix BTC
  geom_line(aes(y = MA4Y), color = "orange", linetype = "solid", size = 1) +  # Moyenne mobile exponentielle 4 jours
  geom_line(aes(y = EMA1Y), color = "blue", linetype = "solid", size = 1) + # Moyenne mobile exponentielle 1 an
  geom_vline(xintercept = as.numeric(halving_dates), color = "red") +
  geom_text(data = data.frame(Date = halving_dates, Label = halving_labels),
            aes(x = Date, y = max(btc_df$Close, na.rm = TRUE), label = Label),
            vjust = -1, color = "red", size = 4) +
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