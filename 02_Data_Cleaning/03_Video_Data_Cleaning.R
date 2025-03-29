### Dieses Skript übernimmt das Datensäubern, Pre-Processing der Daten auf der
### Video Ebene (Untertitel und Meta Daten).
library(tidyverse)

D <- readRDS("Video_Meta_Captions_All.rds")

# Überprüfen, ob es NA's auf der text Variable gibt.
table(!is.na(D$text))

# Audio Deskriptionen entfernen
audio <- c("\\[Applaus\\]", "\\[Musik\\]", "\\[Music\\]")

for (i in audio){
  D$text <- str_replace_all(D$text, i, "")
}

# Lässt nur ASCII Symbole in den Untertiteln zu:
D$text <- str_replace_all(D$text, "[^\\x20-\\x7E\\xC0-\\xFF]", "")


# Emojis aus den Titeln und Beschreibungen entfernen
D$title <- str_replace_all(D$title, "[^\\x20-\\x7E\\xC0-\\xFF]", "")
D$description <- str_replace_all(D$description, "[^\\x20-\\x7E\\xC0-\\xFF]", "")


# Links und andere Textbausteine entfernen
D$description <- str_replace_all(D$description, "Offizieller Kanal der AfD-Fraktion.*", " ")
D$description <- str_replace_all(D$description, "Folge uns.*", " ")
D$description <- str_replace_all(D$description, "https.\\S+", " ")
D$description <- str_replace_all(D$description, "www\\S+", " ")
D$description <- str_replace_all(D$description, "@\\S+", " ")


# Entferne Leerzeichen am Anfang und Ende des Kommentarfelds
D$text <- str_replace_all(D$text, "^\\s+|\\s+$", "")


# Hashtags als Variable aus der Video Beschreibung herausziehen
D <- D %>%
  mutate(hashtags = sapply(str_extract_all(description, "#\\S+"), 
                           function(x) paste(x, collapse = " ")))

# Video Länge in ein lesbares Format übertragen
D$Video_Length <- lubridate::as.period(D$Video_Length)
D$zeit_sekunden <- as.numeric(D$Video_Length)


# Wahlkampfphase (Ja / Nein)
D$campaign <- ifelse(D$publishedAt >= "2024-11-07", 1,0)

# Als kontinuierliche Variable, siehe auch Klinger et al. (2023):
D$campaign_con <- as.numeric(date(D$publishedAt) - ymd("2025-02-23"))



saveRDS(D, file = "Video_Meta_Captions_Clean.rds")

