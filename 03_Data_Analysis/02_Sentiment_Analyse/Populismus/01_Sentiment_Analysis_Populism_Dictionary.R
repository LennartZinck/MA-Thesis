# "Sentiment Analyse" mithilfe des Populismus Wörterbuchs von Gründl 2020

#library(devtools)
# Install the dependency regexhelpeR from GitHub
#devtools::install_github("jogrue/regexhelpeR")

# Install the multidictR package from GitHub
#devtools::install_github("jogrue/multidictR")

# Install the popdictR package from GitHub
#devtools::install_github("jogrue/popdictR")

library(quanteda)
library(tidyverse)
library(popdictR)
library(multidictR)
library(regexhelpeR)

#### Anpassung des Wörterbuchs von Gründl (2020) aus dem Artikel
#### https://journals.sagepub.com/doi/10.1177/1461444820976970

#### Definieren der gesamten Wortliste im kompletten Gründl Wörterbuch
#### als Grundlage für die run_popdict funktion.
gruendl_dictionary_complete <-
  popdictR::gruendl_terms


# Konfliktiver Anti-Elitismus
popdict_elite <- gruendl_dictionary_complete %>% 
  filter(Type3 == "anti-elitism")
popdict_elite <- as.character(popdict_elite$Word)

# Konfliktive Souveränität
popdict_sov_n <- gruendl_dictionary_complete %>% 
  filter(Type3 == "sovereignty" & Type2 == "conflictive")
popdict_sov_n <- as.character(popdict_sov_n$Word)



# Advokative Volkszentriertheit
popdict_people <- gruendl_dictionary_complete %>% 
  filter(Type3 == "people-centrism")
popdict_people <- as.character(popdict_people$Word)

# Advokative Souveränität
popdict_sov_p <- gruendl_dictionary_complete %>% 
  filter(Type3 == "sovereignty" & Type2 == "advocative")
popdict_sov_p <- as.character(popdict_sov_p$Word)





###########################################################
# Angepasste run_popdict Funktionen:
###########################################################

# Populismus Wörterbuch für Anti-Elitäre Statements/Regex
run_popdict_elite <- function(
    corpus,
    dict_version = "current",
    at_level = "document",
    return_value = "count_at_level",
    include_totals = TRUE,
    return_result_only = FALSE,
    custom_replacement,
    remove = NULL
) {
  dict <- popdict_elite
  return(
    multidictR::run_multidict(
      corpus = corpus,
      dict = dict,
      at_level = at_level,
      return_value = return_value,
      include_totals = include_totals,
      return_result_only = return_result_only,
      pattern_type = "regex",
      case_insensitive = TRUE,
      regex_optimize = TRUE,
      regex_make_greedy = FALSE,
      regex_make_lazy = TRUE,
      #dict_name = "dict_gruendl_2020",
      custom_replacement = custom_replacement,
      tolower = TRUE,
      stem = FALSE,
      remove = remove,
      what = "word",
      remove_punct = TRUE,
      remove_symbols = TRUE,
      remove_numbers = TRUE,
      remove_url = TRUE,
      remove_separators = TRUE,
      split_hyphens = FALSE,
      include_docvars = TRUE
    )
  )
}


# Konfliktive Souveränität
run_popdict_sov_n <- function(
    corpus,
    dict_version = "current",
    at_level = "document",
    return_value = "count_at_level",
    include_totals = TRUE,
    return_result_only = FALSE,
    custom_replacement,
    remove = NULL
) {
  dict <- popdict_sov_n
  return(
    multidictR::run_multidict(
      corpus = corpus,
      dict = dict,
      at_level = at_level,
      return_value = return_value,
      include_totals = include_totals,
      return_result_only = return_result_only,
      pattern_type = "regex",
      case_insensitive = TRUE,
      regex_optimize = TRUE,
      regex_make_greedy = FALSE,
      regex_make_lazy = TRUE,
      #dict_name = "dict_gruendl_2020",
      custom_replacement = custom_replacement,
      tolower = TRUE,
      stem = FALSE,
      remove = remove,
      what = "word",
      remove_punct = TRUE,
      remove_symbols = TRUE,
      remove_numbers = TRUE,
      remove_url = TRUE,
      remove_separators = TRUE,
      split_hyphens = FALSE,
      include_docvars = TRUE
    )
  )
}

# Populismus Wörterbuch für Volkszentrierte Aussagen/Regex
run_popdict_people <- function(
    corpus,
    dict_version = "current",
    at_level = "document",
    return_value = "count_at_level",
    include_totals = TRUE,
    return_result_only = FALSE,
    custom_replacement,
    remove = NULL
) {
  dict <- popdict_people
  return(
    multidictR::run_multidict(
      corpus = corpus,
      dict = dict,
      at_level = at_level,
      return_value = return_value,
      include_totals = include_totals,
      return_result_only = return_result_only,
      pattern_type = "regex",
      case_insensitive = TRUE,
      regex_optimize = TRUE,
      regex_make_greedy = FALSE,
      regex_make_lazy = TRUE,
      #dict_name = "dict_gruendl_2020",
      custom_replacement = custom_replacement,
      tolower = TRUE,
      stem = FALSE,
      remove = remove,
      what = "word",
      remove_punct = TRUE,
      remove_symbols = TRUE,
      remove_numbers = TRUE,
      remove_url = TRUE,
      remove_separators = TRUE,
      split_hyphens = FALSE,
      include_docvars = TRUE
    )
  )
}


# Advokative Souveränität
run_popdict_sov_p <- function(
    corpus,
    dict_version = "current",
    at_level = "document",
    return_value = "count_at_level",
    include_totals = TRUE,
    return_result_only = FALSE,
    custom_replacement,
    remove = NULL
) {
  dict <- popdict_sov_p
  return(
    multidictR::run_multidict(
      corpus = corpus,
      dict = dict,
      at_level = at_level,
      return_value = return_value,
      include_totals = include_totals,
      return_result_only = return_result_only,
      pattern_type = "regex",
      case_insensitive = TRUE,
      regex_optimize = TRUE,
      regex_make_greedy = FALSE,
      regex_make_lazy = TRUE,
      #dict_name = "dict_gruendl_2020",
      custom_replacement = custom_replacement,
      tolower = TRUE,
      stem = FALSE,
      remove = remove,
      what = "word",
      remove_punct = TRUE,
      remove_symbols = TRUE,
      remove_numbers = TRUE,
      remove_url = TRUE,
      remove_separators = TRUE,
      split_hyphens = FALSE,
      include_docvars = TRUE
    )
  )
}



###########################################################
# Anwendung der Wörterbücher auf die Untertitel der Videos
###########################################################


D <- readRDS("Data Objects/Video_Meta_Captions_Clean.rds")
D <- D %>% mutate(id = row_number())
corpus <- corpus(D)

# Das 'at_level' wird in diesem Fall auf Dokumentebene gewählt,
# weil die automatischen Untertitel keine Satzzeichen beinhalten.


# Zuerst 0_Adaption_Run_Popdict.R laden
# Anti-Elitäre Aussagen
results_elite <- 
  run_popdict_elite(corpus, at_level = "documents", return_value = "count")
results_elite <- convert(results_elite, to = "data.frame")
results_elite <- results_elite %>% rename(popdict_elite = dict)
#results_elite <- results_elite[,c("video_id", "popdict_elite")]


# Volkszentrierte Aussagen
results_people <- 
  run_popdict_people(corpus, at_level = "documents", return_value = "count")
results_people <- convert(results_people, to = "data.frame")
results_people <- results_people %>% rename(popdict_people = dict)
results_people <- results_people[,c("video_id", "popdict_people")]


# Konfliktive Soveränität
results_sov_n <- 
  run_popdict_sov_n(corpus, at_level = "documents", return_value = "count")
results_sov_n <- convert(results_sov_n, to = "data.frame")
results_sov_n <- results_sov_n %>% rename(popdict_sov_n = dict)
results_sov_n <- results_sov_n[,c("video_id", "popdict_sov_n")]


# Advokative Soveränität
results_sov_p <- 
  run_popdict_sov_p(corpus, at_level = "documents", return_value = "count")
results_sov_p <- convert(results_sov_p, to = "data.frame")
results_sov_p <- results_sov_p %>% rename(popdict_sov_p = dict)
results_sov_p <- results_sov_p[,c("video_id", "popdict_sov_p")]



# Resultate zusammenfügen in einer Datentabelle
results <- inner_join(results_people, results_elite, by = "video_id")
results <- inner_join(results, results_sov_n, by = "video_id")
results <- inner_join(results, results_sov_p, by = "video_id")

results$results_complete <-
              results$popdict_elite + results$popdict_people + results$popdict_sov_n + results$popdict_sov_p



saveRDS(results, file = "Data Objects/Pop_Sent_Captions_All_Final.rds")