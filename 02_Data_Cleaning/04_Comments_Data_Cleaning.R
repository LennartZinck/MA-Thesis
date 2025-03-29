### Dieses Skript übernimmt das Datensäubern, Pre-Processing der Daten auf der
### Kommentar Ebene, also Kommentarinhalt, Kommentatornamen, etc.

library(tidyverse)
library(tidytext)
library(stringr)
library(lubridate)


# Kommentare laden
D <- readRDS("Comments_All.rds")

# Kommentare auswählen, die spätestens am 23. Februar 2025 geschrieben wurden
D$publishedAt <- lubridate::as_date(D$publishedAt)
D <- D %>% filter(publishedAt >= '2024-8-06' & publishedAt <= '2025-2-23')



### Umgang mit ASCII unkompatiblen Usernamen (bspw. @野狗-f2f, @มาลินีเวนเซล)
D$authorDisplayName <- ifelse(str_detect(D$authorDisplayName, "[^\\x20-\\x7E\\xC0-\\xFF]"), NA, D$authorDisplayName)

# Alle nicht ASCII kompatiblen Nutzernamen mit ihren Kommentaren aus dem Datensatz entfernen
D <- D %>% filter(!is.na(authorDisplayName))



# Lässt nur ASCII Symbole im Kommentarfeld zu:

D$textDisplay <- str_replace_all(D$textDisplay, "[^\\x20-\\x7E\\xC0-\\xFF]", "")


# Die Zeitstempel entfernen (bspw.	< href=";=110">1:50</> or <a href)

D$textDisplay <- str_replace_all(D$textDisplay, regex("<a\\s+href=[^>]+>", ignore_case = TRUE), "")

syntax <- c("<br>", "&quot;", "&#39;","<b>", "</b>", "\\.?</ >", "</a>")

for (i in syntax) {
  D$textDisplay <- str_replace_all(D$textDisplay, i, " ")
}

# Ersetze & mit "und"
D$textDisplay <- str_replace_all(D$textDisplay, regex("&amp;", ignore_case = T), "und")

# Entfernen von @ Symbolen und @Accountnamen
D$textDisplay <- str_replace_all(D$textDisplay, regex("@\\S+[a-zA-Z0-9.-]", ignore_case = T), " ")

# URLs entfernen
# alle https:// und http:// URLs
D$textDisplay <- str_replace_all(D$textDisplay, "(https|http)?:\\/\\/(\\w|\\.|\\/|\\?|\\=|\\&|\\%|\\-|\\+)*\\b", " ")
D$textDisplay <- str_replace_all(D$textDisplay, "(www\\.)?[a-zA-Z0-9.-]+\\.[a-z]+(/[a-zA-Z0-9._~:/?#[\\\\]@!$&'()*+,;=%-]*)?", " ")


# Entferne doppelte Leerzeichen (etwa "AfD  <3" to "AfD <3")
D$textDisplay <- str_replace_all(D$textDisplay, "\\s+", " ")

# Entferne Leerzeichen am Anfang und Ende des Kommentarfelds
D$textDisplay <- str_replace_all(D$textDisplay, "^\\s+|\\s+$", "")

# Sichern für Datenanalyse via Topic Modelling und Sentiment Analysis
saveRDS(D, file = "Comments_All_Clean.rds")

