##### In diesem Skript werden die beiden Wörterbücher "NRC" und "ed8" auf die 
##### Kommentare und Untertitel angewendet.

library(quanteda)
library(tidyverse)
library(tidytext)
library(stringr)
library(lubridate)
library(ngram)

# Parallele Verarbeitung via Quanteda
quanteda_options(threads = 8)

#----
# Zuschnitt oder Alle Kommentare
df1 <- readRDS("Data Objects/Comments/Comments_All_Clean.rds")

# Corpus Object auf der Grundlage der Kommentare erstellen
C1 <- corpus(df1, text_field = "textDisplay")

# Tokenizing der Daten
Tokens <- tokens(C1, remove_numbers = TRUE, remove_punct = TRUE,
                 remove_separators = TRUE, split_hyphens = FALSE,
                 remove_url = TRUE)


# Token Objekt sichern und laden
saveRDS(Tokens, "Data Objects/Comments/Tokens_Sentiment_All.rds")
Tokens <- readRDS("Data Objects/Comments/Tokens_Sentiment_All.rds")


################################################################################
# ed8 Wörterbuch für die Kommentare
# Downloaded und Abgewandeltes Skript von https://github.com/tweedmann/3x8emotions
#
# Literatur:
# Widmann, Tobias, und Maximilian Wich. 2022. „Creating and Comparing Dictionary, 
# Word Embedding, and Transformer-Based Models to Measure Discrete Emotions in German Political Text“.
# Political Analysis 31(4): 626–41. doi:10.1017/pan.2022.15.
################################################################################

ed8 <- dictionary(file = "Model Objects/Dictionaries/ed8.yml",
                  format = "YAML")

# Bigrams, um für die Negierung von Aussagen zu kontrollieren
toks_neg_bigram <- tokens_compound(Tokens, pattern = phrase("nicht *"))
toks_neg_bigram <- tokens_compound(toks_neg_bigram, pattern = phrase("nichts *"))
toks_neg_bigram <- tokens_compound(toks_neg_bigram, pattern = phrase("kein *"))
toks_neg_bigram <- tokens_compound(toks_neg_bigram, pattern = phrase("keine *"))
toks_neg_bigram <- tokens_compound(toks_neg_bigram, pattern = phrase("keinen *"))

# Tokens in DFM umwandeln, Füllwörter ausschließen
emo_dfm <- dfm(toks_neg_bigram, remove = stopwords("de"))

# Anwendung des ed8 Wörterbuchs
dict_dfm_results <- dfm_lookup(emo_dfm,ed8)

# Ergebnisse als Datentabelle speichern
df1 <- cbind(df1, convert(dict_dfm_results, to = 'data.frame'))

# Länge der Dokumente speichern
terms_dfm <- dfm(Tokens)
df1$terms_raw <- ntoken(terms_dfm)
df1$terms <- ntoken(emo_dfm)


# Auf Dokumentenlänge normalisierte Emotionswerte
df1$anger.norm <- df1$ed8.ANGER / df1$terms
df1$fear.norm <- df1$ed8.FEAR / df1$terms
df1$disgust.norm <- df1$ed8.DISGUST / df1$terms
df1$sadness.norm <- df1$ed8.SADNESS / df1$terms
df1$joy.norm <- df1$ed8.JOY / df1$terms
df1$enthusiasm.norm <- df1$ed8.ENTHUSIASM / df1$terms
df1$pride.norm <- df1$ed8.PRIDE / df1$terms
df1$hope.norm <- df1$ed8.HOPE / df1$terms

################################################################################
# NRC Wörterbuch für die Kommentare
# 
# Literatur:
# Mohammad, Saif M., und Peter D. Turney. 2013. „Crowdsourcing a Word-Emotion 
# Association Lexicon“. doi:10.48550/arXiv.1308.6297.
################################################################################
Tokens_nrc_emotions <- tokens_lookup(Tokens, dictionary = as.dictionary(subset(
  get_sentiments("nrc"),
  sentiment %in% c(
    "anger",
    "fear",
    "anticipation",
    "trust",
    "surprise",
    "sadness",
    "joy",
    "disgust"
  )
)))


# Wut, Furcht und andere Emotionen mit NRC DFM
tokens_nrc_emotions_dfm <- dfm(Tokens_nrc_emotions)

df1$nrc.FEAR <- as.numeric(tokens_nrc_emotions_dfm[, "fear"])
df1$nrc.ANGER <- as.numeric(tokens_nrc_emotions_dfm[, "anger"])
df1$nrc.SADNESS <- as.numeric(tokens_nrc_emotions_dfm[, "sadness"])
df1$nrc.DISGUST <- as.numeric(tokens_nrc_emotions_dfm[, "disgust"])
df1$nrc.ANTICIPATION <- as.numeric(tokens_nrc_emotions_dfm[, "anticipation"])
df1$nrc.SURPRISE <- as.numeric(tokens_nrc_emotions_dfm[, "surprise"])
df1$nrc.JOY <- as.numeric(tokens_nrc_emotions_dfm[, "joy"])
df1$nrc.TRUST <- as.numeric(tokens_nrc_emotions_dfm[, "trust"])

# Auf Dokumentenlänge normalisierte Emotionswerte
df1$anger.norm.nrc <- df1$nrc.ANGER / df1$terms
df1$fear.norm.nrc <- df1$nrc.FEAR / df1$terms
df1$disgust.norm.nrc <- df1$nrc.DISGUST / df1$terms
df1$sadness.norm.nrc <- df1$nrc.SADNESS / df1$terms
df1$joy.norm.nrc <- df1$nrc.JOY / df1$terms
df1$trust.norm.nrc <- df1$nrc.TRUST / df1$terms
df1$anticipation.norm.nrc <- df1$nrc.ANTICIPATION / df1$terms
df1$surprise.norm.nrc <- df1$nrc.SURPRISE / df1$terms

df1 <- df1 %>% rename(video_id = videoId)
saveRDS(df1, file = "Data Objects/Comments/Sentiment_Kommentare_Final.rds")

################################################################################
# Dieser Abschnitt vergleicht die Ergebnisse der Sentiment Analyse mithilfe
# des 'NRC' und des 'ed8' Wörterbuchs.
# Mithilfe der beiden Wörterbücher werden äußerst 
# unterschiedliche Emotionswerte gefunden.
################################################################################

### Beispielhafte Cross-Validation für Wut ('Anger')
ggplot(df1, aes(x = ed8.ANGER, y = nrc.ANGER))+
  geom_jitter()

### Wann finden die Wörterbücher ed8 und NRC die Emotion nicht, oder zumindest 
### ein Wort, dass für die Emotion steht?
### Die Wörterbücher stimmen zu etwa 81% überein.
### Das liegt aber zu einem sehr großen Teil an den gefundenen Nullen, also
### Kommentaren, die als überhaupt nicht wütend klassifiziert werden.
percentage_equal <-
  ifelse(df1$ed8.ANGER == 0 & df1$nrc.ANGER == 0 |
           df1$ed8.ANGER > 0 & df1$nrc.ANGER > 0,
         1,
         0) %>%
  mean() * 100

### Cross-Validation für Angst ('Fear')
ggplot(df1, aes(x = ed8.FEAR, y = nrc.FEAR))+
  geom_jitter()

### Für Furcht stimmen die Wörterbücher zu 66% überein.
percentage_equal <- 
  ifelse(df1$ed8.FEAR == 0 & df1$nrc.FEAR == 0 | 
         df1$ed8.FEAR != 0 & df1$nrc.FEAR != 0, 1, 0) %>%
  mean() * 100




################################################################################
# 2) Abschnitt für die Analyse der Untertitel
#
#
################################################################################


# Definieren, welche Daten der Analyse zugrunde liegen sollen
df2 <- readRDS("Data Objects/Video_Meta_Captions_Clean.rds")

# Die automatisierten Untertitel pro Video werden in ein Corpus Object transformiert
C2 <- corpus(df2, text_field = "text")

# Tokens auf Basis des Corpus Objects erstellen
Tokens <- tokens(C2, remove_numbers = TRUE, remove_punct = TRUE,
                 remove_separators = TRUE, split_hyphens = FALSE,
                 remove_url = TRUE)

Tokens <- tokens_tolower(Tokens)
Tokens <- tokens_remove(Tokens, pattern = stopwords("de"))

saveRDS(Tokens, "Data Objects/Captions/Tokens_Sentiment_Meta_Captions_All.rds")
Tokens <- readRDS("Data Objects/Captions/Tokens_Sentiment_Meta_Captions_All.rds")


################################################################################
# ed8 Wörterbuch für die Untertitel
#
#
################################################################################
ed8 <- dictionary(file = "Model Objects/Dictionaries/ed8.yml",
                  format = "YAML")

# Bigrams, um für Negierung von Aussagen zu kontrollieren
toks_neg_bigram <- tokens_compound(Tokens, pattern = phrase("nicht *"))
toks_neg_bigram <- tokens_compound(toks_neg_bigram, pattern = phrase("nichts *"))
toks_neg_bigram <- tokens_compound(toks_neg_bigram, pattern = phrase("kein *"))
toks_neg_bigram <- tokens_compound(toks_neg_bigram, pattern = phrase("keine *"))
toks_neg_bigram <- tokens_compound(toks_neg_bigram, pattern = phrase("keinen *"))

# Tokens in DFM umwandeln, Füllwörter aussortieren
emo_dfm <- dfm(toks_neg_bigram, remove = stopwords("de"))

# Anwendung des ed8 Wörterbuchs
dict_dfm_results <- dfm_lookup(emo_dfm,ed8)

# Ergebnisse als Datentabelle speichern
df2 <- cbind(df2, convert(dict_dfm_results, to = 'data.frame'))

# Länge der Dokumente speichern
terms_dfm <- dfm(Tokens)
df2$terms_raw <- ntoken(terms_dfm)
df2$terms <- ntoken(emo_dfm)



# Auf Dokumentenlänge normalisierte Emotionswerte
df2$anger.norm <- df2$ed8.ANGER / df2$terms
df2$fear.norm <- df2$ed8.FEAR / df2$terms
df2$disgust.norm <- df2$ed8.DISGUST / df2$terms
df2$sadness.norm <- df2$ed8.SADNESS / df2$terms
df2$joy.norm <- df2$ed8.JOY / df2$terms
df2$enthusiasm.norm <- df2$ed8.ENTHUSIASM / df2$terms
df2$pride.norm <- df2$ed8.PRIDE / df2$terms
df2$hope.norm <- df2$ed8.HOPE / df2$terms


################################################################################
# Cross-Validierung
# NRC Wörterbuch für die Klassifizierung der Untertitel nach Emotionen
#
################################################################################

Tokens_nrc_emotions <- tokens_lookup(Tokens, dictionary = as.dictionary(subset(
  get_sentiments("nrc"),
  sentiment %in% c(
    "anger",
    "fear",
    "anticipation",
    "trust",
    "surprise",
    "sadness",
    "joy",
    "disgust"
  )
)))


# Wut, Furcht und andere Emotionen mit NRC DFM
tokens_nrc_emotions_dfm <- dfm(Tokens_nrc_emotions)

df2$nrc.FEAR <- as.numeric(tokens_nrc_emotions_dfm[, "fear"])
df2$nrc.ANGER <- as.numeric(tokens_nrc_emotions_dfm[, "anger"])
df2$nrc.SADNESS <- as.numeric(tokens_nrc_emotions_dfm[, "sadness"])
df2$nrc.DISGUST <- as.numeric(tokens_nrc_emotions_dfm[, "disgust"])
df2$nrc.ANTICIPATION <- as.numeric(tokens_nrc_emotions_dfm[, "anticipation"])
df2$nrc.SURPRISE <- as.numeric(tokens_nrc_emotions_dfm[, "surprise"])
df2$nrc.JOY <- as.numeric(tokens_nrc_emotions_dfm[, "joy"])
df2$nrc.TRUST <- as.numeric(tokens_nrc_emotions_dfm[, "trust"])

# Auf Dokumentenlänge normalisierte Emotionswerte
df2$anger.norm.nrc <- df2$nrc.ANGER / df2$terms
df2$fear.norm.nrc <- df2$nrc.FEAR / df2$terms
df2$disgust.norm.nrc <- df2$nrc.DISGUST / df2$terms
df2$sadness.norm.nrc <- df2$nrc.SADNESS / df2$terms
df2$joy.norm.nrc <- df2$nrc.JOY / df2$terms
df2$trust.norm.nrc <- df2$nrc.TRUST / df2$terms
df2$anticipation.norm.nrc <- df2$nrc.ANTICIPATION / df2$terms
df2$surprise.norm.nrc <- df2$nrc.SURPRISE / df2$terms


# Umbenennung der Spalten für die Daten über die Untertitel
df2 <- df2 %>%
  rename(anger.norm.capt = anger.norm,
         fear.norm.capt = fear.norm,
         sadness.norm.capt =  sadness.norm,
         disgust.norm.capt = disgust.norm,
         joy.norm.capt = joy.norm,
         hope.norm.capt =  hope.norm,
         pride.norm.capt = pride.norm,
         enthusiasm.norm.capt  = enthusiasm.norm
  )


################################################################################
#### 3) Abschnitt für die Analyse der Titel
################################################################################

title <- corpus(df2, text_field = "title")

Tokens_title <- tokens(title, remove_numbers = TRUE, remove_punct = TRUE,
                       remove_separators = TRUE, split_hyphens = FALSE,
                       remove_url = TRUE)

Tokens_title <- tokens_tolower(Tokens_title)
Tokens_title <- tokens_wordstem(Tokens_title, language = "german") # Does this work for German words?
Tokens_title <- tokens_remove(Tokens_title, pattern = stopwords("de"))


saveRDS(Tokens_title, "Data Objects/Tokens_Sentiment_Title_All.rds")
Tokens_title <- readRDS("Data Objects/Tokens_Sentiment_Title_All.rds")


# Bigrams, um für Negierung von Aussagen zu kontrollieren
toks_neg_bigram <- tokens_compound(Tokens_title, pattern = phrase("nicht *"))
toks_neg_bigram <- tokens_compound(toks_neg_bigram, pattern = phrase("nichts *"))
toks_neg_bigram <- tokens_compound(toks_neg_bigram, pattern = phrase("kein *"))
toks_neg_bigram <- tokens_compound(toks_neg_bigram, pattern = phrase("keine *"))
toks_neg_bigram <- tokens_compound(toks_neg_bigram, pattern = phrase("keinen *"))

# Tokens in DFM umwandeln, Füllwörter aussortieren
emo_dfm <- dfm(toks_neg_bigram, remove = stopwords("de"))

ed8 <- dictionary(file = "Model Objects/Dictionaries/ed8.yml",
                  format = "YAML")

# Anwendung des ed8 Wörterbuchs
dict_dfm_results <- dfm_lookup(emo_dfm,ed8)

# Ergebnisse als Datentabelle speichern
title <- df2[,c("video_id", "title", "party")]


title <- cbind(title, convert(dict_dfm_results, to = 'data.frame'))


title <- 
  title %>% rename(anger.title = ed8.ANGER,
                   fear.title = ed8.FEAR, 
                   disgust.title = ed8.DISGUST,
                   sadness.title = ed8.SADNESS,
                   joy.title = ed8.JOY,
                   enthusiasm.title = ed8.ENTHUSIASM,
                   pride.title = ed8.PRIDE,
                   hope.title = ed8.HOPE)

title <-
  title[, c("video_id", "anger.title", "fear.title", 
            "disgust.title", "sadness.title", "joy.title",
            "enthusiasm.title", "pride.title", "hope.title")]


df2 <- inner_join(df2, title, by = "video_id")

saveRDS(df2, file = "Data Objects/Captions/Sentiment_Untertitel_Final.rds")



################################################################################
# 3) Zusammenführung der Sentiment Analyse der Kommentare und der Untertitel
#
#
################################################################################
df1 <- readRDS("Data Objects/Comments/Sentiment_Kommentare_Final.rds")
df2 <- readRDS("Data Objects/Captions/Sentiment_Untertitel_Final.rds")

# Berechnung der Durchschnittlichen Emotionen pro Kommentar
df1_mean <- df1 %>%
  group_by(video_id) %>%
  summarize(com_mean_fear = mean(fear.norm, na.rm = TRUE),
            com_mean_anger = mean(anger.norm, na.rm = TRUE),
            com_mean_disgust = mean(disgust.norm, na.rm = TRUE),
            com_mean_sadness = mean(sadness.norm, na.rm = TRUE),
            com_mean_joy = mean(joy.norm, na.rm = TRUE),
            com_mean_pride = mean(pride.norm, na.rm = TRUE),
            com_mean_hope = mean(hope.norm, na.rm = TRUE),
            com_mean_enthusiasm = mean(enthusiasm.norm, na.rm = TRUE))


df2_mean <- inner_join(df1_mean, df2, by = "video_id")
saveRDS(df2_mean, file = "Data Objects/Sentiment_Untertitel_Kommentare_Final.rds")