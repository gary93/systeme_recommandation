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
library(reshape2)

#--------------------##--------------------##--------------------##--------------------#

fichier_dataset = "dataset.json"

print("Importation du dataset")
donnees <- stream_in(file(fichier_dataset))

print("Récupération de 10000 échantillons aléatoire")
echantillon_donnees <- donnees[sample(nrow(donnees), 10000), ]

#--------------------##--------------------##--------------------##--------------------#

print("Analyse des critiques sentimentales")
echantillon_donnees$reviewText <- as.character(echantillon_donnees $reviewText)
mots_revue <- echantillon_donnees %>%
    select(reviewerID, asin, reviewText, overall) %>%
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
    group_by(reviewerID, asin, overall) %>%
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

#--------------------##--------------------##--------------------##--------------------#

# Fusion du score de nrc et de bing dans le meme dataset
donnees_recommendation <- merge(donnees, sommaire_scores_revues, by="asin", all.x = TRUE)
# Suppression des données vide
donnees_recommendation <- na.omit(donnees_recommendation)

#--------------------##--------------------##--------------------##--------------------#

# Recommendation avec un score global
# Creation d'un nouveau dataset pour les recommandations
donnees_recommendation_overall <- data.frame(donnees_recommendation$reviewerID.x, donnees_recommendation$asin, donnees_recommendation$overall.y)
colnames(donnees_recommendation_overall) <- c("reviewerid","asin", "overall")

setorder(donnees_recommendation_overall, asin)

# Limitation du nombre de ligne dans le but de limiter le temps de calcul
donnees_recommendation_overall <- donnees_recommendation_overall[1:7000,]

matrice_notation_overall <- as(as.matrix(acast(donnees_recommendation_overall, reviewerid~ asin)), "realRatingMatrix")
matrice_notation_overall <- matrice_notation_overall[1:50,]

set.seed(1)

reco_model_overall <- Recommender(matrice_notation_overall[1:40], method = "UBCF")
predicted_overall <- predict(reco_model_overall, matrice_notation_overall[40:45], n = 1)

# Prédiction des recommandations
reco_with_overall_score <- as(predicted_overall, "list")

reco_with_overall_score

#--------------------##--------------------##--------------------##--------------------#

# Recommendation avec un score de nrc
# Creation d'un nouveau dataset pour les recommandations
donnees_recommendation_nrc <- data.frame(donnees_recommendation$reviewerID.x, donnees_recommendation$asin, donnees_recommendation$nrc_score)
colnames(donnees_recommendation_nrc) <- c("reviewerid","asin", "nrc")

setorder(donnees_recommendation_nrc, asin)

# Limitation du nombre de ligne dans le but de limiter le temps de calcul
donnees_recommendation_nrc <- donnees_recommendation_nrc[1:7000,]

matrice_notation_nrc <- as(as.matrix(acast(donnees_recommendation_nrc, reviewerid~ asin)), "realRatingMatrix")
matrice_notation_nrc <- matrice_notation_nrc[1:50,]

set.seed(1)

reco_model_nrc <- Recommender(matrice_notation_nrc[1:40], method = "UBCF")
predicted_nrc <- predict(reco_model_nrc, matrice_notation_nrc[40:45], n = 1)

# Prédiction des recommandations
reco_with_nrc_score <- as(predicted_nrc, "list")

reco_with_nrc_score

#--------------------##--------------------##--------------------##--------------------#

# Recommendation avec un score de bing
# Creation d'un nouveau dataset pour les recommandations
donnees_recommendation_bing <- data.frame(donnees_recommendation$reviewerID.x, donnees_recommendation$asin, donnees_recommendation$bing_score)
colnames(donnees_recommendation_bing) <- c("reviewerid","asin", "bing")

setorder(donnees_recommendation_bing, asin)

# Limitation du nombre de ligne dans le but de limiter le temps de calcul
donnees_recommendation_bing <- donnees_recommendation_bing[1:7000,]

matrice_notation_bing <- as(as.matrix(acast(donnees_recommendation_bing, reviewerid~ asin)), "realRatingMatrix")
matrice_notation_bing <- matrice_notation_bing[1:50,]

set.seed(1)

reco_model_bing <- Recommender(matrice_notation_bing[1:40], method = "UBCF")
predicted_bing <- predict(reco_model_bing, matrice_notation_bing[40:45], n = 1)

# Prédiction des recommandations
reco_with_bing_score <- as(predicted_bing, "list")

reco_with_bing_score


# Evaluation des modèles
eval_sets <- evaluationScheme(data = matrice_notation_overall, method = "cross-validation", k=20, given = 2, goodRating = 2)
models_evalue <- list(UBCF = list(name = "UBCF", param = list(method = "cosine")))
liste_resultats <- evaluate(x = eval_sets, method = models_evalue, n = c(1, 5, 10, 20, 30, 40, 50, 100, 500))
avg_matrices <- lapply(liste_resultats, avg)
avg_matrices$UBCF[, 5:8]
plot(liste_resultats, annotate = 1)

eval_sets <- evaluationScheme(data = matrice_notation_overall, method = "cross-validation", k=20, given = 2, goodRating = 2)
models_evalue <- list(POPULAR = list(name = "POPULAR", param = list(method = NULL)))
liste_resultats <- evaluate(x = eval_sets, method = models_evalue, n = c(1, 5, 10, 20, 30, 40, 50, 100, 500))
avg_matrices <- lapply(liste_resultats, avg)
avg_matrices$POPULAR[, 5:8]
plot(liste_resultats , annotate = 1)

eval_sets <- evaluationScheme(data = matrice_notation_overall, method = "cross-validation", k=20, given = 2, goodRating = 2)
models_evalue <- list(SVD = list(name = "SVD", param = list(method = NULL)))
liste_resultats <- evaluate(x = eval_sets, method = models_evalue, n = c(1, 5, 10, 20, 30, 40, 50, 100, 500))
avg_matrices <- lapply(liste_resultats, avg)
avg_matrices$SVD[, 5:8]
plot(liste_resultats , annotate = 1)
