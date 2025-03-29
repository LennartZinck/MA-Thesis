### Vergleich der durchschnittlichen gesendeten Emotionen und die darauf folgenden
### Emotionen. 

library(tidyverse)

###### ANALYSE GRUPPIERT NACH POPULISMUS ######
df1 <- readRDS("Data Objects/Captions/Sentiment_Untertitel_Final.rds")

#1a) Durchschnittliche Emotionen für Populisten und Nicht-Populisten
df1 %>%
  filter(pop != "Keine Daten")%>%
  group_by(pop) %>%
  summarize(Wut = mean(anger.norm.capt, na.rm = T)*100, # Darstellung der %, siehe Widmann (2021)
            Angst = mean(fear.norm.capt, na.rm = T)*100,
            Traurigkeit = mean(sadness.norm.capt, na.rm = T)*100,
            Ekel = mean(disgust.norm.capt, na.rm = T)*100,
            Freude = mean(joy.norm.capt, na.rm = T)*100,
            Hoffnung = mean(hope.norm.capt, na.rm = T)*100,
            Stolz = mean(pride.norm.capt, na.rm = T)*100,
            Enthusiasmus = mean(enthusiasm.norm.capt, na.rm = T)*100)%>% 
  pivot_longer(cols = -"pop") %>%
  ggplot(., aes(x = factor(name, c("Wut", "Angst", "Ekel",
                                   "Traurigkeit", "Freude", "Enthusiasmus",
                                   "Stolz", "Hoffnung")), # Reihenfolge nach Widmann 2021
                y = value, 
                fill = factor(pop, c("populistisch", "nicht-populistisch"))))+ # Reihenfolge nach Widmann (2021)
  geom_col(position = "dodge")+
  labs(title = "",
       x = "",
       y = "Durchschnittliche Standardisierte Werte")+
  scale_fill_discrete(name = "Populismus")+ # Umbenennung der Legende
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3))+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="lightgrey", 
                                         size=0.5, linetype="solid"),
        axis.text =element_text(size=12),
        axis.title.y = element_text(size = 12))


#1a) Durchschnittliche Emotionen zwischen Wahlkampf und Routine gruppiert nach Partei
df1 %>%
  filter(zeit_sekunden < 3601)%>%
  group_by(party, campaign) %>%
  summarize(Wut = mean(anger.norm.capt, na.rm = T)*100, # Darstellung der %, siehe Widmann (2021)
            Angst = mean(fear.norm.capt, na.rm = T)*100,
            Traurigkeit = mean(sadness.norm.capt, na.rm = T)*100,
            Ekel = mean(disgust.norm.capt, na.rm = T)*100,
            Freude = mean(joy.norm.capt, na.rm = T)*100,
            Hoffnung = mean(hope.norm.capt, na.rm = T)*100,
            Stolz = mean(pride.norm.capt, na.rm = T)*100,
            Enthusiasmus = mean(enthusiasm.norm.capt, na.rm = T)*100)%>% 
  pivot_longer(cols = c(-"party", -"campaign")) %>%
  ggplot(., aes(x = factor(name, c("Wut", "Angst", "Ekel",
                                   "Traurigkeit", "Freude", "Enthusiasmus",
                                   "Stolz", "Hoffnung")), # Reihenfolge nach Widmann 2021
                y = value,
                fill = as.factor(campaign)))+
  geom_col(position = "dodge")+
  labs(title = "Durchschnittliche Standardisierte Emotionswerte der Videoinhalte nach Parteityp",
       x = "Emotion",
       y = "Durchschnittliche Standardisierte Werte")+
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3))+
  facet_wrap(~party)




#### 1b) Wie sieht der Vergleich der Kommentarspalten aus? Sind Kommentarspalten von
#### populistischen Parteien emotionaler als die von nicht-populistischen Sendern?

# Durchschnittliche Emotionen pro Video
df2 <- readRDS("Data Objects/Comments/Sentiment_Kommentare_Final.rds")


# Wahlkampfphase (Ja / Nein)
df2$campaign <- ifelse(df2$publishedAt >= "2024-11-07", 1,0)


df2 <- df2 %>%
  filter(pop != "Keine Daten")%>%
  group_by(pop, video_id, party, campaign) %>%
  summarize(Wut = mean(anger.norm, na.rm = T),
            Angst = mean(fear.norm, na.rm = T),
            Traurigkeit = mean(sadness.norm, na.rm = T),
            Ekel = mean(disgust.norm, na.rm = T),
            Freude = mean(joy.norm, na.rm = T),
            Hoffnung = mean(hope.norm, na.rm = T),
            Stolz = mean(pride.norm, na.rm = T),
            Enthusiasmus = mean(enthusiasm.norm, na.rm = T)) %>%
  ungroup()


df2 %>% group_by(pop) %>%
  summarize(Wut = mean(Wut, na.rm = T)*100,
            Angst = mean(Angst, na.rm = T)*100,
            Traurigkeit = mean(Traurigkeit, na.rm = T)*100,
            Ekel = mean(Ekel, na.rm = T)*100,
            Freude = mean(Freude, na.rm = T)*100,
            Hoffnung = mean(Hoffnung, na.rm = T)*100,
            Stolz = mean(Stolz, na.rm = T)*100,
            Enthusiasmus = mean(Enthusiasmus, na.rm = T)*100)%>%
  pivot_longer(cols = -"pop") %>%
  ggplot(., aes(x = factor(name, c("Wut", "Angst", "Ekel",
                                   "Traurigkeit", "Freude", "Enthusiasmus",
                                   "Stolz", "Hoffnung")), # Reihenfolge nach Widmann 2021
                y = value, 
                fill = pop))+
  geom_col(position = "dodge")+
  labs(title = "",
       x = "",
       y = "Durchschnittliche Standardisierte Emotionswerte")+
  scale_fill_discrete(name = "Populismus")+
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4))





### 1c) Vergleich gesendete Nachricht und Reaktion darauf. 
df3 <- df1 %>%
  filter(pop != "Keine Daten")%>%
  group_by(pop, video_id, party) %>%
  summarize(Wut_Video = mean(anger.norm.capt, na.rm = T),
            Angst_Video = mean(fear.norm.capt, na.rm = T),
            Traurigkeit_Video = mean(sadness.norm.capt, na.rm = T),
            Ekel_Video = mean(disgust.norm.capt, na.rm = T),
            Freude_Video = mean(joy.norm.capt, na.rm = T),
            Hoffnung_Video = mean(hope.norm.capt, na.rm = T),
            Stolz_Video = mean(pride.norm.capt, na.rm = T),
            Enthusiasmus_Video = mean(enthusiasm.norm.capt, na.rm = T))

# Nur daten von Videos für die Untertitel und Kommentarspalten vorhanden sind
df4 <- inner_join(df2, df3, by = c("video_id", "party", "pop"))


# Darstellung der Zusammenhänge:
df4 %>% ggplot(., aes(x = Wut_Video, y = Wut, color = party))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "lm")

df4 %>% ggplot(., aes(x = Angst_Video, y = Angst, color = pop))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "lm")


df4 %>% 
  group_by(pop)%>%
  summarize(Wut = cor(Wut, Wut_Video, use = "complete.obs"),
            Angst = cor(Angst, Angst_Video, use = "complete.obs"),
            Ekel = cor(Ekel, Ekel_Video, use = "complete.obs"),
            Traurigkeit = cor(Traurigkeit, Traurigkeit_Video, use = "complete.obs"),
            Hoffnung = cor(Hoffnung, Hoffnung_Video, use = "complete.obs"),
            Freude = cor(Freude, Freude_Video, use = "complete.obs"),
            Enthusiasmus = cor(Enthusiasmus, Enthusiasmus_Video, use = "complete.obs"),
            Stolz = cor(Stolz, Stolz_Video, use = "complete.obs"))%>%
  
  
  pivot_longer(cols = -"pop") %>%
  ggplot(., aes(x = factor(name, c("Wut", "Angst", "Ekel",
                                   "Traurigkeit", "Freude", "Enthusiasmus",
                                   "Stolz", "Hoffnung")), # Reihenfolge nach Widmann 2021
                y = value, 
                fill = pop))+
  geom_col(position = "dodge")+
  labs(title = "Korrelationen zwischen den gesendeten Emotionen und den Reaktionen nach Parteityp",
       x = "Emotion",
       y = "Korrelationskoeffizient")

# Nach Partei
df4 %>% 
  group_by(party)%>%
  summarize(Wut = cor(Wut, Wut_Video, use = "complete.obs"),
            Angst = cor(Angst, Angst_Video, use = "complete.obs"),
            Ekel = cor(Ekel, Ekel_Video, use = "complete.obs"),
            Traurigkeit = cor(Traurigkeit, Traurigkeit_Video, use = "complete.obs"),
            Hoffnung = cor(Hoffnung, Hoffnung_Video, use = "complete.obs"),
            Freude = cor(Freude, Freude_Video, use = "complete.obs"),
            Enthusiasmus = cor(Enthusiasmus, Enthusiasmus_Video, use = "complete.obs"),
            Stolz = cor(Stolz, Stolz_Video, use = "complete.obs"))%>%
  
  
  pivot_longer(cols = -"party") %>%
  ggplot(., aes(x = factor(name, c("Wut", "Angst", "Ekel",
                                   "Traurigkeit", "Freude", "Enthusiasmus",
                                   "Stolz", "Hoffnung")), # Reihenfolge nach Widmann 2021
                y = value))+
  geom_point()+
  labs(title = "Korrelationen zwischen den gesendeten Emotionen und den Reaktionen nach Parteityp",
       x = "Emotion",
       y = "Korrelationskoeffizient")+
  geom_abline(intercept = 0, slope = 0)+
  facet_wrap(~party)



###### ANALYSE GRUPPIERT NACH PARTEIEN ######

#### 2a) Durchschnittliche Emotionen gesendet pro Partei

df1 %>%
  filter(!is.na(party))%>%
  group_by(party) %>%
  summarize(Wut = mean(anger.norm.capt, na.rm = T),
            Angst = mean(fear.norm.capt, na.rm = T),
            Traurigkeit = mean(sadness.norm.capt, na.rm = T),
            Ekel = mean(disgust.norm.capt, na.rm = T),
            Freude = mean(joy.norm.capt, na.rm = T),
            Hoffnung = mean(hope.norm.capt, na.rm = T),
            Stolz = mean(pride.norm.capt, na.rm = T),
            Enthusiasmus = mean(enthusiasm.norm.capt, na.rm = T))%>% 
  
  
  pivot_longer(cols = -"party") %>%
  ggplot(., aes(x = factor(name, c("Wut", "Angst", "Ekel",
                                   "Traurigkeit", "Freude", "Enthusiasmus",
                                   "Stolz", "Hoffnung")), # Reihenfolge nach Widmann 2021
                y = value, 
                shape = party))+
  geom_point(size = 3)+
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6)) +
  labs(title = "Durchschnittliche Standardisierte Emotionswerte der Videoinhalte nach Partei",
       x = "Emotion",
       y = "Durchschnittliche Standardisierte Emotionswerte")

df1 %>%
  filter(!is.na(party))%>%
  group_by(party) %>%
  summarize(Wut = mean(anger.norm.capt, na.rm = T),
            Angst = mean(fear.norm.capt, na.rm = T),
            Traurigkeit = mean(sadness.norm.capt, na.rm = T),
            Ekel = mean(disgust.norm.capt, na.rm = T),
            Freude = mean(joy.norm.capt, na.rm = T),
            Hoffnung = mean(hope.norm.capt, na.rm = T),
            Stolz = mean(pride.norm.capt, na.rm = T),
            Enthusiasmus = mean(enthusiasm.norm.capt, na.rm = T))%>% 
  ungroup()%>%
  
  pivot_longer(cols = -"party") %>%
  ggplot(., aes(x = factor(name, c("Wut", "Angst", "Ekel",
                                   "Traurigkeit", "Freude", "Enthusiasmus",
                                   "Stolz", "Hoffnung")), # Reihenfolge nach Widmann 2021
                y = value, 
                fill = party))+
  geom_col(position = "dodge")+
  scale_fill_manual(values = c("#0489DB", "#8f47d4", "#000000", "#1AA037", "#E3000F", "#FFEF00", "#E3000F")) +
  labs(title = "Durchschnittliche Standardisierte Emotionswerte der Videoinhalte nach Partei",
       x = "Emotion",
       y = "Durchschnittliche Standardisierte Emotionswerte")+
  theme_classic()




##### 2b) Darstellung der Kommentarspalten pro Partei

df2 %>% 
  group_by(party) %>%
  summarize(Wut = mean(Wut, na.rm = T),
            Angst = mean(Angst, na.rm = T),
            Traurigkeit = mean(Traurigkeit, na.rm = T),
            Ekel = mean(Ekel, na.rm = T),
            Freude = mean(Freude, na.rm = T),
            Hoffnung = mean(Hoffnung, na.rm = T),
            Stolz = mean(Stolz, na.rm = T),
            Enthusiasmus = mean(Enthusiasmus, na.rm = T))%>%
  
  pivot_longer(cols = -"party") %>%
  ggplot(., aes(x = factor(name, c("Wut", "Angst", "Ekel",
                                   "Traurigkeit", "Freude", "Enthusiasmus",
                                   "Stolz", "Hoffnung")), # Reihenfolge nach Widmann 2021
                y = value, 
                shape = party))+
  geom_point(size = 3)+
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6)) +
  labs(title = "Durchschnittliche Standardisierte Emotionswerte der Kommentarspalten nach Partei",
       x = "Emotion",
       y = "Durchschnittliche Standardisierte Emotionswerte")


# 2c) Darstellung der Korrelation von Emotionen Video/Kommentarspale pro Partei

df4 %>% 
  group_by(party)%>%
  summarize(Wut = cor(Wut, Wut_Video, use = "complete.obs"),
            Angst = cor(Angst, Angst_Video, use = "complete.obs"),
            Ekel = cor(Ekel, Ekel_Video, use = "complete.obs"),
            Traurigkeit = cor(Traurigkeit, Traurigkeit_Video, use = "complete.obs"),
            Hoffnung = cor(Hoffnung, Hoffnung_Video, use = "complete.obs"),
            Freude = cor(Freude, Freude_Video, use = "complete.obs"),
            Enthusiasmus = cor(Enthusiasmus, Enthusiasmus_Video, use = "complete.obs"),
            Stolz = cor(Stolz, Stolz_Video, use = "complete.obs"))%>%
  
  
  pivot_longer(cols = -"party") %>%
  ggplot(., aes(x = factor(name, c("Wut", "Angst", "Ekel",
                                   "Traurigkeit", "Freude", "Enthusiasmus",
                                   "Stolz", "Hoffnung")), # Reihenfolge nach Widmann 2021
                y = value, 
                shape = party))+
  geom_point(size = 3)+
  geom_hline(yintercept = 0)+
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6)) +
  labs(title = "Durchschnittliche Korrelation Emotionen nach Partei",
       x = "Emotion",
       y = "Durchschnittliche Korrelation Emotionen")


#### Wahlkampf
df4 %>% 
  group_by(party, campaign)%>%
  summarize(Wut = cor(Wut, Wut_Video, use = "complete.obs"),
            Angst = cor(Angst, Angst_Video, use = "complete.obs"),
            Ekel = cor(Ekel, Ekel_Video, use = "complete.obs"),
            Traurigkeit = cor(Traurigkeit, Traurigkeit_Video, use = "complete.obs"),
            Hoffnung = cor(Hoffnung, Hoffnung_Video, use = "complete.obs"),
            Freude = cor(Freude, Freude_Video, use = "complete.obs"),
            Enthusiasmus = cor(Enthusiasmus, Enthusiasmus_Video, use = "complete.obs"),
            Stolz = cor(Stolz, Stolz_Video, use = "complete.obs"))%>%
  
  
  pivot_longer(cols = c(-"party", -"campaign")) %>%
  ggplot(., aes(x = factor(name, c("Wut", "Angst", "Ekel",
                                   "Traurigkeit", "Freude", "Enthusiasmus",
                                   "Stolz", "Hoffnung")), # Reihenfolge nach Widmann 2021
                y = value, 
                shape = factor(campaign)))+
  geom_point(size = 3)+
  geom_hline(yintercept = 0)+
  scale_shape_manual(values = c(0, 1)) +
  labs(title = "Korrelation Emotionen nach Partei",
       x = "Emotion",
       y = "Korrelation Video Emotion und Kommentarspalte Emotion")+
  facet_wrap(~party)
