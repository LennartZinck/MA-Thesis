##### Deskriptive Auswertung der Ergebnisse aus dem Populismus Wörterbuch von (Gründl 2022)

library(tidyverse)

Pop_Sent_Captions_All <- readRDS(
  "Data Objects/Pop_Sent_Captions_All_Final.rds"
)

# Darstellung der aggregierten populistischen Kommunikation im Zeitverlauf. 

Pop_Sent_Captions_All <-
  Pop_Sent_Captions_All %>% mutate(pop.norm = results_complete/n_tokens)

D <- Pop_Sent_Captions_All %>%
  mutate(month = floor_date(publishedAt, "month")) %>% 
  group_by(month, party) %>%
  summarise(sum = sum(pop.norm, na.rm = T), .groups = "drop")  

D <- D %>% filter(month < "2025-02-23")

D$month <- as.Date(D$month)

ggplot(D, aes(x = month, y = sum, color = party)) +
  geom_line(linewidth = 1, alpha = 0.75) +   
  geom_point(size = 2) +
  geom_vline(xintercept = as.Date("2024-11-06"), linetype = "dashed", color = "black")+
  labs(title = "",
       x = "",
       y = "Standardisierte Populistische Kommunikation",
       color = "Partei") +
  theme_minimal()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  scale_color_manual(values = c(
    "SPD" = "#E3000F",  
    "CDU/CSU" = "#000000",  
    "Die Grünen" = "#1AA000",  
    "Die Linke" = "#8819e6",
    "BSW" = "#360089",
    "AfD" = "#0489DB",
    "FDP" = "#FFF000"
  )) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="lightgrey", 
                                         size=0.5, linetype="solid"),
        axis.text =element_text(size=12),
        axis.title.y = element_text(size = 12))


# Elemente populistischer Kommunikation pro Partei Wahlkampf vs Routine
anti_elit <-
  Pop_Sent_Captions_All %>%
  group_by(party, campaign) %>%
  summarize(meanelite = mean(popdict_elite / n_tokens, na.rm = TRUE) * 100) %>%
  ungroup()

people_cent <- 
  Pop_Sent_Captions_All %>%
  group_by(party, campaign) %>%
  summarize(meanpeople = mean(popdict_people / n_tokens, na.rm = TRUE)*100) %>% 
  ungroup() 


adv_sov <-
  Pop_Sent_Captions_All %>%
  group_by(party, campaign) %>%
  summarize(mean_sov_p = mean(popdict_sov_p / n_tokens, na.rm = TRUE)*100) %>%  # Use summarize()
  ungroup()

confl_sov <-
  Pop_Sent_Captions_All %>%
  group_by(party, campaign) %>%
  summarize(mean_sov_n = mean(popdict_sov_n / n_tokens, na.rm = TRUE)*100) %>%  # Use summarize()
  ungroup()

df <- 
  inner_join(anti_elit, people_cent, by = c("party", "campaign"))


df <- 
  inner_join(df, adv_sov, by = c("party", "campaign"))


df <- 
  inner_join(df, confl_sov, by = c("party", "campaign"))


df <- 
  df%>%
  pivot_longer(cols = c(-"party", -"campaign"))




ggplot(df, aes(
  x = fct_reorder(party, value, .desc = TRUE),
  y = value,
  fill = factor(
    name,
    labels = c(
      "Souveränität der Eliten",
      "Souveränität des Volkes",
      "Anti-Elitismus",
      "Volkszentriertheit"
    )
  )
)) +
  geom_col() +
  labs(x = "", y = "Standardisierte populistische Kommunikation", title = "") +
  scale_fill_discrete(name = "") +
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



# Elemente populistischer Kommunikation pro Partei Wahlkampf vs Routine
anti_elit <-
  Pop_Sent_Captions_All %>%
  group_by(party, campaign) %>%
  summarize(meanelite = mean(popdict_elite / results_complete, na.rm = TRUE) * 100) %>%
  ungroup()

people_cent <- 
  Pop_Sent_Captions_All %>%
  group_by(party, campaign) %>%
  summarize(meanpeople = mean(popdict_people / results_complete, na.rm = TRUE)*100) %>% 
  ungroup() 


adv_sov <-
  Pop_Sent_Captions_All %>%
  group_by(party, campaign) %>%
  summarize(mean_sov_p = mean(popdict_sov_p / results_complete, na.rm = TRUE)*100) %>%  # Use summarize()
  ungroup()

confl_sov <-
  Pop_Sent_Captions_All %>%
  group_by(party, campaign) %>%
  summarize(mean_sov_n = mean(popdict_sov_n / results_complete, na.rm = TRUE)*100) %>%  # Use summarize()
  ungroup()

df <- 
  inner_join(anti_elit, people_cent, by = c("party", "campaign"))


df <- 
  inner_join(df, adv_sov, by = c("party", "campaign"))


df <- 
  inner_join(df, confl_sov, by = c("party", "campaign"))


df <- 
  df%>%
  pivot_longer(cols = c(-"party", -"campaign"))


ggplot(df, aes(
  x = factor(campaign, labels = c(
    "0" = "Routine-Phase",
    "1" = "Wahlkampf")),
  y = value,
  fill = factor(
    name,
    labels = c(
      "Souveränität der Eliten",
      "Souveränität des Volkes",
      "Anti-Elitismus",
      "Volkszentriertheit"
    )
  )
)
) +
  geom_col() +
  labs(x = "", y = "Anteil der Elemente an der Gesamtheit populistischer Kommunikation", title = "") +
  scale_fill_discrete(name = "") +
  theme_minimal()+
  theme(
    legend.position = "top",
    legend.background = element_rect( 
      fill = "lightgrey",
      size = 0.5,
      linetype = "solid"
    ),
    axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
    axis.title.y = element_text(size = 12)
  )+
  facet_wrap(~party)

