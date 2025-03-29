# Robustheitstest AIC für die Modellgüte (Vergleich Poisson, Hurdle 
# und NB-Modelle)

# Poisson-Modelle
pois_anger <- glm(
  Wut_Count ~
    anger.norm.capt +
    anger.title +
    fear.norm.capt +
    fear.title +
    hope.norm.capt +
    hope.title +
    enthusiasm.norm.capt +
    enthusiasm.title +
    pop_confl +
    pop_adv +
    pop * campaign,
  offset(offset),
  data = df,
  family = "poisson"
)

pois_fear <- glm(
  Angst_Count ~
    anger.norm.capt +
    anger.title +
    fear.norm.capt +
    fear.title +
    hope.norm.capt +
    hope.title +
    enthusiasm.norm.capt +
    enthusiasm.title +
    pop_confl +
    pop_adv +
    pop * campaign,
  offset(offset),
  data = df,
  family = "poisson"
)

pois_hope <- glm(
  Hoffnung_Count ~
    anger.norm.capt +
    anger.title +
    fear.norm.capt +
    fear.title +
    hope.norm.capt +
    hope.title +
    enthusiasm.norm.capt +
    enthusiasm.title +
    pop_confl +
    pop_adv +
    pop * campaign,
  offset(offset),
  data = df,
  family = "poisson"
)

pois_enth <- glm(
  Enthusiasmus_Count ~
    anger.norm.capt +
    anger.title +
    fear.norm.capt +
    fear.title +
    hope.norm.capt +
    hope.title +
    enthusiasm.norm.capt +
    enthusiasm.title +
    pop_confl +
    pop_adv +
    pop * campaign,
  offset(offset),
  data = df,
  family = "poisson"
)
# AIC Vergleich
AIC(pois_anger, model_anger)
AIC(pois_fear, model_fear)
AIC(pois_hope, model_hope)
AIC(pois_enth, model_enthusiasm)


# AIC-Werte der Modelle
aic_pois_anger <- 1239372.75
aic_model_anger <- 74663.02
aic_pois_fear <- 1021961.94
aic_model_fear <- 71477.24
aic_pois_hope <- 1023606.65
aic_model_hope <- 71498.61
aic_pois_enth <- 617457.05
aic_model_enthusiasm <- 64520.74

# Berechnung der Delta AICs
delta_aic_anger <- aic_pois_anger - aic_model_anger
delta_aic_fear <- aic_pois_fear - aic_model_fear
delta_aic_hope <- aic_pois_hope - aic_model_hope
delta_aic_enth <- aic_pois_enth - aic_model_enthusiasm

# Berechnung der AIC-Wahrscheinlichkeiten
w_anger <- exp(-delta_aic_anger / 2) / (exp(-delta_aic_anger / 2) + exp(-delta_aic_fear / 2) + exp(-delta_aic_hope / 2) + exp(-delta_aic_enth / 2))
w_fear <- exp(-delta_aic_fear / 2) / (exp(-delta_aic_anger / 2) + exp(-delta_aic_fear / 2) + exp(-delta_aic_hope / 2) + exp(-delta_aic_enth / 2))
w_hope <- exp(-delta_aic_hope / 2) / (exp(-delta_aic_anger / 2) + exp(-delta_aic_fear / 2) + exp(-delta_aic_hope / 2) + exp(-delta_aic_enth / 2))
w_enth <- exp(-delta_aic_enth / 2) / (exp(-delta_aic_anger / 2) + exp(-delta_aic_fear / 2) + exp(-delta_aic_hope / 2) + exp(-delta_aic_enth / 2))

# Ausgabe der Ergebnisse
cat("Delta AIC Werte:\n")
cat("Anger: ", delta_aic_anger, "\n")
cat("Fear: ", delta_aic_fear, "\n")
cat("Hope: ", delta_aic_hope, "\n")
cat("Enthusiasm: ", delta_aic_enth, "\n")

cat("\nAIC Model Wahrscheinlichkeiten:\n")
cat("Anger: ", w_anger, "\n")
cat("Fear: ", w_fear, "\n")
cat("Hope: ", w_hope, "\n")
cat("Enthusiasm: ", w_enth, "\n")



# Hurdle Modell für Wut
hurdle_anger <- hurdle(
  Wut_Count ~
    anger.norm.capt +
    anger.title +
    fear.norm.capt +
    fear.title +
    hope.norm.capt +
    hope.title +
    enthusiasm.norm.capt +
    enthusiasm.title +
    pop_confl +
    pop_adv +
    pop * campaign,
  data = df,
  dist = "poisson" # für den Zählteil ein Poisson-Modell
)

# Hurdle Modell für Angst
hurdle_fear <- hurdle(
  Angst_Count ~
    anger.norm.capt +
    anger.title +
    fear.norm.capt +
    fear.title +
    hope.norm.capt +
    hope.title +
    enthusiasm.norm.capt +
    enthusiasm.title +
    pop_confl +
    pop_adv +
    pop * campaign,
  data = df,
  dist = "poisson" # für den Zählteil ein Poisson-Modell
)

# Hurdle Modell für Hoffnung
hurdle_hope <- hurdle(
  Hoffnung_Count ~
    anger.norm.capt +
    anger.title +
    fear.norm.capt +
    fear.title +
    hope.norm.capt +
    hope.title +
    enthusiasm.norm.capt +
    enthusiasm.title +
    pop_confl +
    pop_adv +
    pop * campaign,
  data = df,
  dist = "poisson" # für den Zählteil ein Poisson-Modell
)

# Hurdle Modell für Enthusiasmus
hurdle_enth <- hurdle(
  Enthusiasmus_Count ~
    anger.norm.capt +
    anger.title +
    fear.norm.capt +
    fear.title +
    hope.norm.capt +
    hope.title +
    enthusiasm.norm.capt +
    enthusiasm.title +
    pop_confl +
    pop_adv +
    pop * campaign,
  data = df,
  dist = "poisson" # für den Zählteil ein Poisson-Modell
)

AIC(hurdle_anger, model_anger)
AIC(hurdle_fear, model_fear)
AIC(hurdle_hope, model_hope)
AIC(hurdle_enth, model_enthusiasm)
