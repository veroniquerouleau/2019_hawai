## 1. Créer une série temporelle du CO2 atmosphérique
library(tidyverse)

hawai <- read_csv(file = "hawai.csv") 
#,car le séparateur de colonne est une virgule et le séparateur décimal est un point.

library("forecast")
library("fpp2")

library("lubridate")
# pour passer d'une date en format décimale (par exemple : 1958.167) 
# à une date en format année/mois/jour heure/minute/seconde UTC.
date <- date_decimal(hawai$time)
date

# pour ajouter une colonne comprenant la date transformée et enlever la colonne "time"
hawai_date <- hawai %>% mutate(Date = date) %>% select(-time)

# pour créer une série temporelle de type ts
hawai_ts <- ts(hawai_date %>% select(-Date), 
               start = c(hawai_date$Date[1] %>% year(), 1),
               frequency = 12)
hawai_ts

# Visualisation de la série temporelle avec autoplot()
autoplot(hawai_ts)

# 2. Séparer la série en parties d'entraînement (environ 70% des données) 
# et en partie test

hawai_ts_train <- window(hawai_ts, start = 1958, end = 1988.999)
hawai_ts_train

hawai_ts_test <- window(hawai_ts, start = 1989)
hawai_ts_test

# 3. Créer un modèle ETS sur les données d'entraînement, puis projeter la 
# prévision de CO2 atmosphérique pour comparer aux données test

# Générer un modèle ETS (error, tend and seasonnal)
hawai_model <- ets(hawai_ts_train)
hawai_model

# Visualisation de l'évolution des différentes composantes
autoplot(hawai_model)

# Générer une prédiction
hawai_ets <- hawai_ts_train %>% ets()
hawai_fc <- hawai_ets %>% forecast()
hawai_fc %>% autoplot()

# Analyse d'exactitude effectuée sur la prévision
accuracy(hawai_fc, hawai_ts)

# 4. Effectuer une analyse des résidus
# Analyse des résidus effectuée sur le modèle
checkresiduals(hawai_ets)
# L'analyse des seuils (95%) de significtion de l'autocorrélation indique qu'il existe des corrélations significatives, mais que les données situées près les unes des autres pourraient être plus difficiles à modéliser.
# Les résidus contiennet de l'autocorrélation (ce qui devrait être évité) : il existe une structure dans les données qui n'a pas été capturée par le modèle.
# Le test de Ljung-Box indique que la probabilité que ces données soient générées selon un bruit blanc est inférieure à 0.0001% (p< 2.2e-16).
# En d'autres mots, la probabilité que la série soit un bruit blanc est presque nulle.
# Enfin, les résidus semblent distribués normalement.