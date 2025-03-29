########
# Skript für die deskriptive Unterscheidung zwischen Routine und Wahlkampfphase
########

library(tidyverse)
D <- readRDS("Data Objects/Sentiment_Untertitel_Kommentare_Final.rds")

D%>%
  group_by(pop, campaign) %>%
  summarize(Wut = mean(anger.norm.capt, na.rm = T),
            Angst = mean(fear.norm.capt, na.rm = T),
            Traurigkeit = mean(sadness.norm.capt, na.rm = T),
            Ekel = mean(disgust.norm.capt, na.rm = T),
            Freude = mean(joy.norm.capt, na.rm = T),
            Hoffnung = mean(hope.norm.capt, na.rm = T),
            Stolz = mean(pride.norm.capt, na.rm = T),
            Enthusiasmus = mean(enthusiasm.norm.capt, na.rm = T))%>% 
  ungroup()%>%
  pivot_longer(cols = c(-"pop", -"campaign")) %>%
  ggplot(., aes(x = factor(name, c("Wut", "Angst", "Ekel",
                                   "Traurigkeit", "Freude", "Enthusiasmus",
                                   "Stolz", "Hoffnung")), # Reihenfolge nach Widmann 2021
                y = value, 
                fill = factor(campaign, levels = c(0,1), labels = c("Nein", "Ja"))))+
  geom_col(position = "dodge")+
  labs(title = "Durchschnittliche Standardisierte Emotionswerte der Videoinhalte nach Parteityp",
       x = "Emotion",
       y = "Durchschnittliche Standardisierte Emotionswerte")+
  scale_fill_discrete(name = "Wahlkampf")+
  facet_wrap(~pop)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="lightgrey", 
                                         size=0.5, linetype="solid"),
        axis.text =element_text(size=12),
        axis.title.y = element_text(size = 12))



D %>%
  mutate(month = as.Date(floor_date(publishedAt, "month"))) %>% 
  group_by(month, party) %>%
  summarize(Wut = mean(anger.norm.capt, na.rm = T),
            Angst = mean(fear.norm.capt, na.rm = T),
            Traurigkeit = mean(sadness.norm.capt, na.rm = T),
            Ekel = mean(disgust.norm.capt, na.rm = T),
            Freude = mean(joy.norm.capt, na.rm = T),
            Hoffnung = mean(hope.norm.capt, na.rm = T),
            Stolz = mean(pride.norm.capt, na.rm = T),
            Enthusiasmus = mean(enthusiasm.norm.capt, na.rm = T))%>%
  ungroup()%>%
  pivot_longer(cols = c(-"month", -"party"))%>%
  ggplot(., aes(x = month, 
                y = value, 
                color = factor(name, c("Wut", "Angst", "Ekel",
                                       "Traurigkeit", "Freude", "Enthusiasmus",
                                       "Stolz", "Hoffnung"))))+
  geom_line()+
  geom_vline(xintercept = as.Date("2024-11-16"), linetype = "dashed", color = "black")+
  facet_wrap(~party) +  
  labs(title = "Emotionen pro Partei im Zeitverlauf",
       x = "Monat",
       y = "Emotionen",
       color = "Emotion") +
  theme_minimal()





D %>%
  mutate(month = as.Date(floor_date(publishedAt, "month"))) %>% 
  group_by(month, party) %>%
  summarize(Neg = mean(anger.norm.capt, na.rm = T)+
              mean(fear.norm.capt, na.rm = T)+
              mean(sadness.norm.capt, na.rm = T)+
              mean(disgust.norm.capt, na.rm = T),
            Pos = mean(joy.norm.capt, na.rm = T)+
              mean(hope.norm.capt, na.rm = T)+
              mean(pride.norm.capt, na.rm = T)+
              mean(enthusiasm.norm.capt, na.rm = T))%>%
  ungroup()%>%
  pivot_longer(cols = c(-"month", -"party"))%>%
  ggplot(., aes(x = month, 
                y = value, 
                color = factor(name, c("Neg", "Pos"))))+
  geom_line(size = 2, alpha = 0.75)+
  geom_vline(xintercept = as.Date("2024-11-06"), linetype = "dashed", color = "black")+
  facet_wrap(~party) +  
  labs(title = "",
       x = "",
       y = "Standardisierte Werte",
       color = "Emotion") +
  theme_minimal()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="lightgrey", 
                                         size=0.5, linetype="solid"),
        axis.text =element_text(size=12),
        axis.title.y = element_text(size = 12))


Q1 <- D%>%
  filter(campaign == 1)%>%
  group_by(pop) %>%
  summarize(Wut = mean(anger.norm.capt, na.rm = T),
            Angst = mean(fear.norm.capt, na.rm = T),
            Traurigkeit = mean(sadness.norm.capt, na.rm = T),
            Ekel = mean(disgust.norm.capt, na.rm = T),
            Freude = mean(joy.norm.capt, na.rm = T),
            Hoffnung = mean(hope.norm.capt, na.rm = T),
            Stolz = mean(pride.norm.capt, na.rm = T),
            Enthusiasmus = mean(enthusiasm.norm.capt, na.rm = T))
  
Q2 <- D%>%
  filter(campaign == 0)%>%
  group_by(pop) %>%
  summarize(Wut_routine = mean(anger.norm.capt, na.rm = T),
            Angst_routine = mean(fear.norm.capt, na.rm = T),
            Traurigkeit_routine = mean(sadness.norm.capt, na.rm = T),
            Ekel_routine = mean(disgust.norm.capt, na.rm = T),
            Freude_routine = mean(joy.norm.capt, na.rm = T),
            Hoffnung_routine = mean(hope.norm.capt, na.rm = T),
            Stolz_routine = mean(pride.norm.capt, na.rm = T),
            Enthusiasmus_routine = mean(enthusiasm.norm.capt, na.rm = T))

Q <- inner_join(Q1, Q2, by = "pop")

Q %>%
  group_by(pop)%>%
  summarize(Wut = Wut - Wut_routine,
            Angst = Angst - Wut_routine,
            Traurigkeit = Traurigkeit - Traurigkeit_routine,
            Ekel = Ekel - Ekel_routine,
            Freude = Freude - Freude_routine,
            Hoffnung = Hoffnung - Hoffnung_routine,
            Stolz = Stolz - Stolz_routine,
            Enthusiasmus = Enthusiasmus - Enthusiasmus_routine)%>%
  pivot_longer(cols = -"pop") %>%
  ggplot(., aes(x = factor(name, c("Wut", "Angst", "Ekel",
                                   "Traurigkeit", "Freude", "Enthusiasmus",
                                   "Stolz", "Hoffnung")), # Reihenfolge nach Widmann 2021
                y = value, 
                fill = pop))+
  geom_col(position = "dodge")+
  labs(title = "Veränderung der genutzten Emotionen zwischen Routine und Wahlkampfphase",
       x = "Emotion",
       y = "Durchschnittliche Standardisierte Emotionswerte")+
  scale_fill_discrete(name = "Populismus")