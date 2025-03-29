library(tidyverse)
library(MASS) #für negative Binomial Modell
library(pscl) #für zero-inflated Modell
library(stargazer)

df1 <- readRDS("Data Objects/Comments/Sentiment_Kommentare_Final.rds")

###### Anzahl der emotionalen Kommentare im Datensatz erstellen #####
df1 <- df1 %>% 
  group_by(video_id) %>%
  reframe(Wut_Count = ifelse(ed8.ANGER >= 1, 1, 0),
          Angst_Count = ifelse(ed8.FEAR >= 1, 1, 0),
          Traurigkeit_Count = ifelse(ed8.SADNESS >= 1, 1, 0),
          Ekel_Count = ifelse(ed8.DISGUST >= 1, 1, 0),
          Stolz_Count = ifelse(ed8.PRIDE >= 1, 1, 0),
          Hoffnung_Count = ifelse(ed8.HOPE >= 1, 1, 0),
          Freude_Count = ifelse(ed8.JOY >= 1, 1, 0),
          Enthusiasmus_Count = ifelse(ed8.ENTHUSIASM >= 1, 1, 0))%>%
  group_by(video_id)%>%
  reframe(Wut_Count = sum(Wut_Count),
          Angst_Count = sum(Angst_Count),
          Traurigkeit_Count = sum(Traurigkeit_Count),
          Ekel_Count = sum(Ekel_Count),
          Stolz_Count = sum(Stolz_Count),
          Hoffnung_Count = sum(Hoffnung_Count),
          Freude_Count = sum(Freude_Count),
          Enthusiasmus_Count = sum(Enthusiasmus_Count))

# Sichern des Datensatzes
saveRDS(df1, file = "Data Objects/Count_Kommentare.rds")
df1 <- readRDS("Data Objects/Count_Kommentare.rds")
df2 <- readRDS("Data Objects/Kombi_Emo_Populis_Final.rds")

df <- inner_join(df1, df2, by = "video_id")
df <- df %>% filter(
  publishedAt <= '2025-02-24'
)



####### Relevante Variablen Codieren #########
# Konfliktive und Befürwortende Kommunikation zusammenführen
df$pop_confl <-
  (df$popdict_elite+df$popdict_sov_n)

df$pop_adv <-
  (df$popdict_people+df$popdict_sov_p)


# Offset erstellen
df$offset <- log(1.1 + df$campaign_con * -1)


################################################################################
####### Negative Binomial Modelle für unterschiedliche Emotionen
################################################################################
# Wut
model_anger <- glm.nb(
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
    pop_adv+
    pop* campaign,
  offset(offset),
  data = df
)


# Angst
model_fear <- glm.nb(
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
    pop_adv+
    pop* campaign,
  offset(offset),
  data = df
)


# Hoffnung
model_hope <- glm.nb(
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
    pop_adv+
    pop* campaign,
  offset(offset),
  data = df
)

# Enthusiasmus
model_enthusiasm <- glm.nb(
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
    pop_adv+
    pop* campaign,
  offset(offset),
  data = df
)

covariate.labels <- c("Wut Video", "Wut Titel", "Angst Video", "Angst Titel", "Hoffnung Video",
                      "Hoffnung Titel", "Enthusiasmus Video", "Enthusiasmus Titel", "Konfl. Pop", 
                      "Befürw. Pop", "Populismus", "Wahlkampf","Wahlkampf*Pop")

stargazer(
  model_anger,
  model_fear,
  model_hope,
  model_enthusiasm,
  type = "latex",
  covariate.labels = covariate.labels,
  out = "Modelle_Emotionen_Interaktion.tex"
)

################################################################################
####### Crossvalidierung: Negative Binomial Modelle für unterschiedliche Emotionen
################################################################################

### Liste der Superspreader

df1 <- readRDS("Data Objects/Comments/Sentiment_Kommentare_Final.rds")

### Umgang mit den "Super-Spreadern"
account_comment_counts <- df1 %>%
  group_by(authorDisplayName) %>%
  summarise(comment_count = n()) %>%
  arrange(desc(comment_count)) 

Q <- quantile(account_comment_counts$comment_count, probs = 0.99)

account_comment_counts <-
  account_comment_counts %>% filter(comment_count > Q)

author_list <- account_comment_counts$authorDisplayName


###### Datensatz erstellen #####
df1 <- df1 %>% filter(!authorDisplayName %in% author_list)


df1 <- df1 %>% 
  group_by(video_id) %>%
  reframe(Wut_Count = ifelse(ed8.ANGER >= 1, 1, 0),
          Angst_Count = ifelse(ed8.FEAR >= 1, 1, 0),
          Traurigkeit_Count = ifelse(ed8.SADNESS >= 1, 1, 0),
          Ekel_Count = ifelse(ed8.DISGUST >= 1, 1, 0),
          Stolz_Count = ifelse(ed8.PRIDE >= 1, 1, 0),
          Hoffnung_Count = ifelse(ed8.HOPE >= 1, 1, 0),
          Freude_Count = ifelse(ed8.JOY >= 1, 1, 0),
          Enthusiasmus_Count = ifelse(ed8.ENTHUSIASM >= 1, 1, 0))%>%
  group_by(video_id)%>%
  reframe(Wut_Count = sum(Wut_Count),
          Angst_Count = sum(Angst_Count),
          Traurigkeit_Count = sum(Traurigkeit_Count),
          Ekel_Count = sum(Ekel_Count),
          Stolz_Count = sum(Stolz_Count),
          Hoffnung_Count = sum(Hoffnung_Count),
          Freude_Count = sum(Freude_Count),
          Enthusiasmus_Count = sum(Enthusiasmus_Count))

df2 <- readRDS("Data Objects/Kombi_Emo_Populis_Final.rds")

df <- inner_join(df1, df2, by = "video_id")
df <- df %>% filter(
  publishedAt <= '2025-02-24'
)


# Offset erstellen
df$offset <- log(1.1 + df$campaign_con * -1)


# Wut
model_anger_cv <- glm.nb(
  Wut_Count ~ 
    anger.norm.capt + 
    anger.title +
    fear.norm.capt + 
    fear.title + 
    hope.norm.capt + 
    hope.title + 
    enthusiasm.norm.capt + 
    enthusiasm.title + 
    pop* campaign+
    popdict_elite +
    popdict_sov_n +
    popdict_people +
    popdict_sov_p,
  offset(offset),
  data = df
)


# Angst
model_fear_cv <- glm.nb(
  Angst_Count ~ 
    anger.norm.capt + 
    anger.title +
    fear.norm.capt + 
    fear.title + 
    hope.norm.capt + 
    hope.title + 
    enthusiasm.norm.capt + 
    enthusiasm.title + 
    pop* campaign+
    popdict_elite +
    popdict_sov_n +
    popdict_people +
    popdict_sov_p,
  offset(offset),
  data = df
)


# Hoffnung
model_hope_cv <- glm.nb(
  Hoffnung_Count ~ 
    anger.norm.capt + 
    anger.title +
    fear.norm.capt + 
    fear.title + 
    hope.norm.capt + 
    hope.title + 
    enthusiasm.norm.capt + 
    enthusiasm.title + 
    pop* campaign+
    popdict_elite +
    popdict_sov_n +
    popdict_people +
    popdict_sov_p,
  offset(offset),
  data = df
)

# Enthusiasmus
model_enthusiasm_cv <- glm.nb(
  Enthusiasmus_Count ~ 
    anger.norm.capt + 
    anger.title +
    fear.norm.capt + 
    fear.title + 
    hope.norm.capt + 
    hope.title + 
    enthusiasm.norm.capt + 
    enthusiasm.title + 
    pop* campaign+
    popdict_elite +
    popdict_sov_n +
    popdict_people +
    popdict_sov_p,
  offset(offset),
  data = df
)



stargazer(
  model_anger_cv,
  model_fear_cv,
  model_hope_cv,
  model_enthusiasm_cv,
  type = "latex",
  covariate.labels = covariate.labels,
  out = "Modelle_Emotionen_cv.tex")



###### Modelle verstehen: Predicted Values und Visualisierung ######
# Wut und vorhergesagte wütende Kommentare

# Erstelle eine Sequenz von anger.norm.capt-Werten (von Minimum bis Maximum)
anger_seq <- seq(min(df$anger.norm.capt, na.rm = TRUE), 
                 max(df$anger.norm.capt, na.rm = TRUE), 
                 length.out = 50)  # 50 Werte für eine glatte Kurve

# Erstelle alle Kombinationen von campaign, pop und anger.norm.capt
newdata_anger <- expand.grid(
  campaign = c(0, 1),  # Routine & Wahlkampf
  pop = c("populistisch", "nicht-populistisch"),
  anger.norm.capt = anger_seq
)

# Füge die durchschnittlichen Werte für die anderen Variablen hinzu
newdata_anger <- newdata_anger %>%
  mutate(
    anger.title = mean(df$anger.title, na.rm = TRUE),
    fear.norm.capt = mean(df$fear.norm.capt, na.rm = TRUE), 
    fear.title = mean(df$fear.title, na.rm = TRUE), 
    hope.norm.capt = mean(df$hope.norm.capt, na.rm = TRUE),
    hope.title = mean(df$hope.title, na.rm = TRUE),
    enthusiasm.norm.capt = mean(df$enthusiasm.norm.capt, na.rm = TRUE),
    enthusiasm.title = mean(df$enthusiasm.title, na.rm = TRUE), 
    popdict_elite = mean(df$popdict_elite_norm, na.rm = TRUE),
    popdict_sov_n = mean(df$popdict_sov_n_norm, na.rm = TRUE),
    popdict_people = mean(df$popdict_people_norm, na.rm = TRUE),
    popdict_sov_p = mean(df$popdict_sov_p_norm, na.rm = TRUE)
  )

# Vorhersage mit Standardfehlern berechnen
predictions <- predict(model_anger, newdata_anger, type = "response", se.fit = TRUE)

# Vorhersagewerte und Standardfehler zum Dataframe hinzufügen
newdata_anger <- newdata_anger %>%
  mutate(
    pred = predictions$fit,
    se = predictions$se.fit,
    lower = pred - 1.96 * se,  # 95%-Konfidenzintervall (untere Grenze)
    upper = pred + 1.96 * se   # 95%-Konfidenzintervall (obere Grenze)
  )

# Visualisierung mit ggplot2
fig_anger <-
ggplot(newdata_anger, aes(x = anger.norm.capt, y = pred, color = pop)) +
  geom_line(size = 1) +  # Hauptlinie
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = pop), alpha = 0.2) +  # Konfidenzintervall als Schattierung
  labs(
    title = "",
    x = "Gesendete Wut",
    y = "Vorhergesagte Anzahl wütender Kommentare",
    color = "Populismus",
    fill = "Populismus"
  ) +
  theme_minimal()+
  theme(
    legend.position = "top",
    legend.background = element_rect(
      fill = "lightgrey",
      size = 0.5,
      linetype = "solid"
    ),
    axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),
    axis.title.y = element_text(size = 12)
  )+
  facet_wrap(~campaign, labeller = as_labeller(c(
    "0" = "Routine-Phase",
    "1" = "Wahlkampf"
  ))
  )





# Angst und vorhergesagte ängstliche Kommentare
fear_seq <- seq(min(df$fear.norm.capt, na.rm = TRUE), 
                max(df$fear.norm.capt, na.rm = TRUE), 
                length.out = 50)  

newdata_fear <- expand.grid(
  campaign = c(0, 1),  # Routine & Wahlkampf
  pop = c("populistisch", "nicht-populistisch"),
  fear.norm.capt = fear_seq
)

newdata_fear <- newdata_fear %>%
  mutate(
    anger.norm.capt = mean(df$fear.norm.capt, na.rm = TRUE), 
    anger.title = mean(df$anger.title, na.rm = TRUE),
    fear.title = mean(df$fear.title, na.rm = TRUE), 
    hope.norm.capt = mean(df$hope.norm.capt, na.rm = TRUE),
    hope.title = mean(df$hope.title, na.rm = TRUE),
    enthusiasm.norm.capt = mean(df$enthusiasm.norm.capt, na.rm = TRUE),
    enthusiasm.title = mean(df$enthusiasm.title, na.rm = TRUE), 
    popdict_elite = mean(df$popdict_elite_norm, na.rm = TRUE),
    popdict_sov_n = mean(df$popdict_sov_n_norm, na.rm = TRUE),
    popdict_people = mean(df$popdict_people_norm, na.rm = TRUE),
    popdict_sov_p = mean(df$popdict_sov_p_norm, na.rm = TRUE)
  )

predictions <- predict(model_fear, newdata_fear, type = "response", se.fit = TRUE)

newdata_fear <- newdata_fear %>%
  mutate(
    pred = predictions$fit,
    se = predictions$se.fit,
    lower = pred - 1.96 * se,  
    upper = pred + 1.96 * se   
  )

fig_fear <-
ggplot(newdata_fear, aes(x = fear.norm.capt, y = pred, color = pop)) +
  geom_line(size = 1) +  # Hauptlinie
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = pop), alpha = 0.2) +  
  labs(
    title = "",
    x = "Gesendete Angst",
    y = "Vorhergesagte Anzahl ängstlicher Kommentare",
    color = "Populismus",
    fill = "Populismus"
  ) +
  theme_minimal()+
  theme(
    legend.position = "top",
    legend.background = element_rect(
      fill = "lightgrey",
      size = 0.5,
      linetype = "solid"
    ),
    axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),
    axis.title.y = element_text(size = 12)
  )+
  facet_wrap(~campaign, labeller = as_labeller(c(
    "0" = "Routine-Phase",
    "1" = "Wahlkampf"
  ))
  )



# Hoffnung und vorhergesagte hoffnungsvolle Kommentare
hope_seq <- seq(min(df$hope.norm.capt, na.rm = TRUE), 
                max(df$hope.norm.capt, na.rm = TRUE), 
                length.out = 50)  

newdata_hope <- expand.grid(
  campaign = c(0, 1),  # Routine & Wahlkampf
  pop = c("populistisch", "nicht-populistisch"),
  hope.norm.capt = hope_seq
)

newdata_hope <- newdata_hope %>%
  mutate(
    anger.norm.capt = mean(df$hope.norm.capt, na.rm = TRUE),
    anger.title = mean(df$anger.title, na.rm = TRUE),
    fear.norm.capt = mean(df$fear.norm.capt, na.rm = TRUE), 
    fear.title = mean(df$fear.title, na.rm = TRUE), 
    hope.title = mean(df$hope.title, na.rm = TRUE),
    enthusiasm.norm.capt = mean(df$enthusiasm.norm.capt, na.rm = TRUE),
    enthusiasm.title = mean(df$enthusiasm.title, na.rm = TRUE), 
    popdict_elite = mean(df$popdict_elite_norm, na.rm = TRUE),
    popdict_sov_n = mean(df$popdict_sov_n_norm, na.rm = TRUE),
    popdict_people = mean(df$popdict_people_norm, na.rm = TRUE),
    popdict_sov_p = mean(df$popdict_sov_p_norm, na.rm = TRUE)
  )

predictions <- predict(model_hope, newdata_hope, type = "response", se.fit = TRUE)

newdata_hope <- newdata_hope %>%
  mutate(
    pred = predictions$fit,
    se = predictions$se.fit,
    lower = pred - 1.96 * se,  
    upper = pred + 1.96 * se   
  )

fig_hope <-
ggplot(newdata_hope, aes(x = hope.norm.capt, y = pred, color = pop)) +
  geom_line(size = 1) +  # Hauptlinie
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = pop), alpha = 0.2) +  
  labs(
    title = "",
    x = "Gesendete Hoffnung",
    y = "Vorhergesagte Anzahl hoffnungsvoller Kommentare",
    color = "Populismus",
    fill = "Populismus"
  ) +
  theme_minimal()+
  theme(
    legend.position = "top",
    legend.background = element_rect(
      fill = "lightgrey",
      size = 0.5,
      linetype = "solid"
    ),
    axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),
    axis.title.y = element_text(size = 12)
  )+
  facet_wrap(~campaign, labeller = as_labeller(c(
    "0" = "Routine-Phase",
    "1" = "Wahlkampf"
  ))
  )



# Enthusiasmus und vorhergesagte enthusiastische Kommentare
enthusiasm_seq <- seq(min(df$enthusiasm.norm.capt, na.rm = TRUE), 
                      max(df$enthusiasm.norm.capt, na.rm = TRUE), 
                      length.out = 50)  

newdata_enthusiasm <- expand.grid(
  campaign = c(0, 1),  # Routine & Wahlkampf
  pop = c("populistisch", "nicht-populistisch"),
  enthusiasm.norm.capt = enthusiasm_seq
)

newdata_enthusiasm <- newdata_enthusiasm %>%
  mutate(
    anger.norm.capt = mean(df$enthusiasm.norm.capt, na.rm = TRUE),
    anger.title = mean(df$anger.title, na.rm = TRUE),
    fear.norm.capt = mean(df$fear.norm.capt, na.rm = TRUE), 
    fear.title = mean(df$fear.title, na.rm = TRUE), 
    hope.norm.capt = mean(df$hope.norm.capt, na.rm = TRUE),
    hope.title = mean(df$hope.title, na.rm = TRUE),
    enthusiasm.title = mean(df$enthusiasm.title, na.rm = TRUE), 
    popdict_elite = mean(df$popdict_elite_norm, na.rm = TRUE),
    popdict_sov_n = mean(df$popdict_sov_n_norm, na.rm = TRUE),
    popdict_people = mean(df$popdict_people_norm, na.rm = TRUE),
    popdict_sov_p = mean(df$popdict_sov_p_norm, na.rm = TRUE)
  )

predictions <- predict(model_enthusiasm, newdata_enthusiasm, type = "response", se.fit = TRUE)

newdata_enthusiasm <- newdata_enthusiasm %>%
  mutate(
    pred = predictions$fit,
    se = predictions$se.fit,
    lower = pred - 1.96 * se,  
    upper = pred + 1.96 * se   
  )

fig_enthusiasm <-
ggplot(newdata_enthusiasm, aes(x = enthusiasm.norm.capt, y = pred, color = pop)) +
  geom_line(size = 1) +  
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = pop), alpha = 0.2) + 
  labs(
    title = "",
    x = "Gesendeter Enthusiasmus",
    y = "Vorhergesagte Anzahl enthusiastischer Kommentare",
    color = "Populismus",
    fill = "Populismus"
  ) +
  theme_minimal()+
  theme(
    legend.position = "top",
    legend.background = element_rect(
      fill = "lightgrey",
      size = 0.5,
      linetype = "solid"
    ),
    axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),
    axis.title.y = element_text(size = 12)
  )+
  facet_wrap(~campaign, labeller = as_labeller(c(
    "0" = "Routine-Phase",
    "1" = "Wahlkampf"
  ))
  )


ggpubr::ggarrange(fig_anger, fig_fear, fig_hope, fig_enthusiasm)