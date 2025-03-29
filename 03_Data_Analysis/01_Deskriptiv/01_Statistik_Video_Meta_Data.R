library(tidyverse)
library(flextable) # Für den Export der Tabellen

D <- readRDS("Video_Meta_Captions_Clean.rds")


# Man sieht an dieser Zusammenfassung eine krasse Diskrepanz für die Aufrufe
# und Kommentare pro Video zwischen den Parteien.
df_summary <- D %>%
  group_by(party) %>%
  summarise(
    nr_video = n(),
    avg_views = round(mean(viewCount, na.rm = TRUE)),
    avg_comments = round(mean(commentCount, na.rm = TRUE)),
    all_views = sum(viewCount, na.rm = TRUE),
    all_comments = sum(commentCount, na.rm = TRUE)
  )

table_flex <- df_summary %>%
  flextable() %>%
  set_header_labels(
    nr_video = "Anzahl der Videos",
    party = "Partei",
    avg_views = "Durchschnittliche Aufrufe",
    avg_comments = "Durchschnittliche Kommentare",
    all_views = "Gesamte Aufrufe",
    all_comments = "Gesamte Kommentare"
  ) %>%
  theme_apa()%>%
  colformat_double(j = c("avg_views", "avg_comments", "all_views", "all_comments"), digits = 0)

save_as_image(table_flex, path = "Video_Statistiken_Parteien.png")



# Videostatistiken für die einzelnen Kanäle
df_summary_2 <- D %>%
  group_by(channel) %>%
  summarise(
    nr_video = n(),
    avg_views = round(mean(viewCount, na.rm = TRUE)),
    avg_comments = round(mean(commentCount, na.rm = TRUE)),
    all_views = sum(viewCount, na.rm = TRUE),
    all_comments = sum(commentCount, na.rm = TRUE)
  )

table_flex_2 <- df_summary_2 %>%
  flextable() %>%
  set_header_labels(
    nr_video = "Anzahl der Videos",
    channel = "Kanal",
    avg_views = "Durchschnittliche Aufrufe",
    avg_comments = "Durchschnittliche Kommentare",
    all_views = "Gesamte Aufrufe",
    all_comments = "Gesamte Kommentare"
  ) %>%
  theme_apa()%>%
  colformat_double(j = c("avg_views", "avg_comments", "all_views", "all_comments"), digits = 0)

save_as_image(table_flex_2, path = "Video_Statistiken_Kanäle.png")


# Wie sieht die Verteilung des öffentlichen Interesses zwischen Wahlkampf und Routineperiode aus?
df_campaign <- D %>%
  group_by(campaign) %>%
  summarise(
    nr_video = n(),
    avg_views = round(mean(viewCount, na.rm = TRUE)),
    avg_comments = round(mean(commentCount, na.rm = TRUE)),
    all_views = sum(viewCount, na.rm = TRUE),
    all_comments = sum(commentCount, na.rm = TRUE)
  )

df_campaign <- df_campaign %>%
  mutate(campaign = case_when(
    campaign == "0" ~ "Nein",
    campaign == "1" ~ "Ja")
    )

table_flex_campaign <- df_campaign %>%
  flextable() %>%
  set_header_labels(
    campaign = "Wahlkampf",
    nr_video = "Anzahl der Videos",
    party = "Partei",
    avg_views = "Durchschnittliche Aufrufe",
    avg_comments = "Durchschnittliche Kommentare",
    all_views = "Gesamte Aufrufe",
    all_comments = "Gesamte Kommentare"
  ) %>%
  theme_apa() %>%
  colformat_double(j = c("avg_views", "avg_comments", "all_views", "all_comments"), digits = 0)
  
save_as_image(table_flex_campaign, path = "Video_Statistiken_Wahlkampf.png")




# Unterscheiden sich die Parteien in ihrer Reaktion auf den Wahlkampf?

df_campaign_party <- D %>%
  group_by(campaign, party) %>%
  summarise(
    nr_video = n(),
    avg_views = round(mean(viewCount, na.rm = TRUE)),
    avg_comments = round(mean(commentCount, na.rm = TRUE)),
    all_views = sum(viewCount, na.rm = TRUE),
    all_comments = sum(commentCount, na.rm = TRUE)
  )

df_campaign_party <- df_campaign_party %>%
  mutate(campaign = case_when(
    campaign == "0" ~ "Nein",
    campaign == "1" ~ "Ja")
  )


df_change <- df_campaign_party %>%
  group_by(party) %>%
  summarise(
    increase_videos = round((nr_video[campaign == "Ja"] - nr_video[campaign == "Nein"]), 1),
    increase_avg_views = round((avg_views[campaign == "Ja"] - avg_views[campaign == "Nein"]), 1),
    increase_avg_comments = round((avg_comments[campaign == "Ja"] - avg_comments[campaign == "Nein"]), 1)
  )


table_flex_campaign <- df_change %>%
  flextable() %>%
  set_header_labels(
    party = "Partei",
    increase_videos = "Mehr Videos",
    increase_avg_views = "Veränderung durchschnittliche Aufrufe",
    increase_avg_comments = "Veränderung durchschnittliche Kommentare"
  ) %>%
  theme_apa() %>%
  colformat_double(j = c("increase_videos", "increase_avg_views", "increase_avg_comments"), digits = 0)

save_as_image(table_flex_campaign, path = "Video_Statistiken_Veränderung.png")