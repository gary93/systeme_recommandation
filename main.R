# Importation des librairies utiles au projet
library(jsonlite)
library(dplyr)
library(recommenderlab)
library(data.table)
library(tidytext)
library(tidyr)
library(stringr)
library(gridExtra)
library(ggplot2)

fichier_dataset = "dataset.json"

print("Importation du dataset")
donnees <- stream_in(file(fichier_dataset))

print("Récupération de 10000 échantillons aléatoire")
echantillon_donnees <- donnees[sample(nrow(donnees), 10000), ]

print("Analyse des critiques sentimentales")
echantillon_donnees$reviewText <- as.character(echantillon_donnees $reviewText)
mots_revue <- echantillon_donnees %>%
    select(reviewerID, reviewText, overall) %>%
    unnest_tokens(word, reviewText) %>%
    filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))

print("-Nrc")
nrc <- sentiments %>%
    filter(sentiment %in% c('positive','negative') & lexicon == 'nrc') %>%
    mutate(nrc = ifelse(sentiment == 'positive',1,-1)) %>%
    select(word, nrc)

print("-Bing")
bing <- sentiments %>%
    filter(lexicon == 'bing') %>%
    mutate(bing = ifelse(sentiment == 'positive',1,-1)) %>%
    select(word, bing)

scores_revues <- mots_revue %>%
    left_join(nrc, by = 'word') %>%
    left_join(bing, by = 'word')

sommaire_scores_revues <- scores_revues %>%
    group_by(reviewerID, overall) %>%
    summarise(nrc_score = round(mean(nrc, na.rm = T),3),
    bing_score = round(mean(bing, na.rm = T),3))

print("Affichage des résultats obtenus")
nrc_graphique <- ggplot(sommaire_scores_revues, aes(x = as.character(overall), y = nrc_score))+
    geom_boxplot()+
    labs(x = 'Amazon',
    y = 'NRC')

bing_graphique <- ggplot(sommaire_scores_revues, aes(x = as.character(overall), y = bing_score))+
    geom_boxplot()+
    labs(x = 'Amazon',
    y = 'Bing')

grid.arrange(nrc_graphique, bing_graphique, nrow = 2)
