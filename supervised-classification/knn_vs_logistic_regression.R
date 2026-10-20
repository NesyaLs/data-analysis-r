###########################################################################
# TP3_classification_antisèche.R
#
# Objectif du TP3 :
#   - Comprendre et coder une CLASSIFICATION SUPERVISÉE binaire
#   - Sur le jeu de données synthétiques "synth.rda"
#   - Comparer K plus proches voisins (KNN) et régression logistique
#   - Estimer le taux d'erreur (apprentissage/test)
#   - Tracer une frontière de décision et une courbe ROC
#
# Ce script est conçu comme une ANTI-SÈCHE :
#   -> chaque bloc fait une chose précise
#   -> tu peux le réutiliser pendant le partiel
###########################################################################

########################
# 0. Préparation
########################

# Charger les packages nécessaires
library(caret)  # pour knn3 (K plus proches voisins)
library(ROCR)   # pour courbes ROC / AUC

########################
# 1. Chargement des données
########################

# Le fichier synth.rda contient un data.frame nommé "synth".
# Chaque ligne = 1 entreprise
# Colonnes :
#   - Y  : état financier (variable qualitative binaire)
#   - X1 : ratio financier 1 (quantitative)
#   - X2 : ratio financier 2 (quantitative)
load("data/synth.rda")

# Vérifions la structure :
str(synth)          # type des variables
head(synth)         # premières lignes
table(synth$Y)      # combien d'entreprises "saines" / "défaillantes" ?

# Important :
#   Y doit être un "factor" (= variable qualitative).
#   Souvent les levels sont quelque chose comme : "sain", "def".
levels(synth$Y)

########################
# 2. Visualisation du nuage de points
########################

# On représente les entreprises seulement sur les deux ratios (X1, X2)
# et on colore / change le symbole suivant l'état financier.
plot(synth[, c("X1", "X2")],
     pch = as.numeric(synth$Y),            # symbole selon Y
     col = as.numeric(synth$Y),            # couleur selon Y
     xlab = "X1",
     ylab = "X2",
     main = "Entreprises selon les ratios financiers (X1,X2)")

legend("topleft",
       legend = levels(synth$Y),           # noms des modalités de Y
       col    = 1:length(levels(synth$Y)),
       pch    = 1:length(levels(synth$Y)))

# Interprétation :
# - on voit approximativement où se trouvent les entreprises "saines"
#   et "défaillantes" dans le plan des deux ratios.
# - ça donne une intuition : séparation plutôt linéaire ? courbe ? imbriquée ?

###########################################################################
# PARTIE 1 : K PLUS PROCHES VOISINS (KNN)
###########################################################################

########################
# 3. Principe rapide de KNN (rappel théorique)
########################

# - On choisit un nombre K (impair en général : 1, 3, 5, 11, ...)
# - Pour une nouvelle entreprise x = (X1,X2), on cherche les K entreprises
#   les plus proches (distance euclidienne classique).
# - On regarde dans ces K voisins la proportion de "saines" / "défaillantes".
# - On prédit la classe majoritaire (ou la probabilité de chaque classe).

########################
# 4. Construire un classifieur KNN sur TOUTES les données
########################

k <- 11  # nombre de voisins (choisi dans le TP, à ajuster si besoin)

# On entraîne un modèle KNN :
#   - Y ~ .   : Y en fonction de toutes les autres colonnes (ici X1 et X2)
#   - data    : jeu complet synth
#   - k       : nombre de voisins.
g_knn <- knn3(Y ~ ., data = synth, k = k)

# g_knn contient maintenant "la règle" KNN apprise sur synth.

########################
# 5. Prédire pour une nouvelle entreprise
########################

# Supposons une nouvelle entreprise avec X1 = -1, X2 = 1
x_new <- data.frame(X1 = -1, X2 = 1)

# 5.1. Probabilités a posteriori P(Y = classe | X = x_new)
prob_new <- predict(g_knn,
                    newdata = x_new,
                    type = "prob")
prob_new

# 5.2. Classe prédite (la plus probable)
class_new <- predict(g_knn,
                     newdata = x_new,
                     type = "class")
class_new

# Interprétation :
# - prob_new donne les probabilités estimées d'être "sain" / "défaillant".
# - class_new donne la classe avec probabilité maximale (règle de Bayes
#   à coûts 0-1, c'est-à-dire qu'on pénalise autant les erreurs dans les
#   deux sens).

########################
# 6. Frontière de décision KNN (carte de classification)
########################

# Idée :
#   - créer une grille de points couvrant le domaine des données
#   - prédire la classe de chaque point avec g_knn
#   - colorer chaque point : on voit la "frontière" KNN

# 6.1. Grille de points
grid_x1 <- seq(min(synth$X1), max(synth$X1), length.out = 200)
grid_x2 <- seq(min(synth$X2), max(synth$X2), length.out = 200)

grid <- expand.grid(X1 = grid_x1, X2 = grid_x2)  # toutes les combinaisons

# 6.2. Prédiction KNN sur la grille
grid_class <- predict(g_knn,
                      newdata = grid,
                      type = "class")

# 6.3. Visualisation : fond coloré + vraies entreprises
plot(grid,
     col  = as.numeric(grid_class),
     pch  = 20,
     cex  = 0.6,
     main = sprintf("Frontière de décision KNN (k = %d)", k),
     xlab = "X1",
     ylab = "X2")

# On superpose les vraies entreprises
points(synth[, c("X1", "X2")],
       pch = as.numeric(synth$Y),
       col = as.numeric(synth$Y))

legend("topleft",
       legend = levels(synth$Y),
       col    = 1:length(levels(synth$Y)),
       pch    = 1:length(levels(synth$Y)))

# Interprétation :
# - La séparation KNN n'est pas forcément une droite ; elle peut être
#   très "dentelée" si K est petit.
# - Pour K grand, la frontière est plus lisse (sous-apprentissage).
# - Pour K très petit (1), la frontière suit beaucoup le bruit
#   (sur-apprentissage).

###########################################################################
# PARTIE 2 : DÉCOUPAGE APPRENTISSAGE / TEST ET TAUX D'ERREUR
###########################################################################

########################
# 7. Un découpage apprentissage / test
########################

# On sépare les données en deux parties :
#   - apprentissage : servir à ENTRAÎNER le modèle
#   - test         : servir à ÉVALUER le modèle (comme de nouvelles données)

set.seed(1)  # pour que le tirage soit reproductible
n <- nrow(synth)

# On prend 70% des données pour l'apprentissage
ind_train <- sample(1:n, size = round(0.7 * n))

train <- synth[ind_train, ]
test  <- synth[-ind_train, ]

# 7.1. Entraîner un KNN sur l'échantillon d'apprentissage
k <- 11
g_knn <- knn3(Y ~ ., data = train, k = k)

# 7.2. Prédire la classe des individus de l'échantillon test
y_pred_test <- predict(g_knn,
                       newdata = test,
                       type = "class")

# Vraies classes pour l'échantillon test
y_true_test <- test$Y

# 7.3. Taux d'erreur test
err_test <- mean(y_pred_test != y_true_test)
err_test

# Interprétation :
# - err_test = proportion d'entreprises mal classées sur les données test.
# - C'est une estimation de l'erreur "sur de nouvelles données".
# - Mais cette estimation dépend du tirage (du découpage apprentissage/test).

########################
# 8. Répéter pour plusieurs découpages (stabiliser l'estimation)
########################

B <- 50                  # nombre de découpages différents
err_knn <- numeric(B)    # on va stocker les erreurs test ici

set.seed(123)            # pour pouvoir refaire la même expérience

for (b in 1:B) {
  # Tirage aléatoire d'un nouvel échantillon apprentissage (70% des données)
  ind_train <- sample(1:n, size = round(0.7 * n))
  
  train <- synth[ind_train, ]
  test  <- synth[-ind_train, ]
  
  # Entraînement
  g_knn <- knn3(Y ~ ., data = train, k = k)
  
  # Prédiction sur test
  y_pred_test <- predict(g_knn,
                         newdata = test,
                         type = "class")
  y_true_test <- test$Y
  
  # Taux d'erreur de ce découpage
  err_knn[b] <- mean(y_pred_test != y_true_test)
}

# Visualiser la distribution des erreurs test
boxplot(err_knn,
        main = sprintf("Distribution du taux d'erreur test (KNN, k=%d)", k),
        ylab = "Erreur test")

mean(err_knn)  # moyenne des erreurs test sur les B découpages

# Interprétation :
# - le boxplot montre la variabilité induite par le choix du découpage.
# - la moyenne des err_knn donne une estimation plus stable de l'erreur.
# - on peut comparer cette moyenne pour différents K (par exemple K=3,11,25).

###########################################################################
# PARTIE 3 : RÉGRESSION LOGISTIQUE
###########################################################################

########################
# 9. Principe rapide
########################

# Régression logistique binaire :
#   - Y est binaire (ici 2 modalités : ex "sain" et "def")
#   - On modélise la probabilité P(Y = "modalité de référence" | X1, X2)
#     via une fonction logistique de X1 et X2 :
#
#   log( p / (1 - p) ) = β0 + β1 * X1 + β2 * X2
#
#   où p = P(Y = "def" | X1, X2) si "def" est le second niveau de Y.
#
# Le modèle est LINÉAIRE sur le logit → la frontière de décision est une DROITE
# dans le plan (X1,X2) (si on utilise un seuil 0.5).

########################
# 10. Ajuster le modèle logistique sur synth
########################

# On vérifie l'ordre des levels de Y :
levels(synth$Y)
# Par convention R GLM binomial : il modélise la probabilité de la DERNIÈRE
# modalité de Y (levels(Y)[2]) contre la première (levels(Y)[1]).

# On ajuste le modèle logistique complet : Y ~ X1 + X2
mod_logit <- glm(Y ~ X1 + X2,
                 data   = synth,
                 family = binomial)

summary(mod_logit)  # affiche les coefficients β0, β1, β2 et leurs p-values

########################
# 11. Probabilité prédite pour une nouvelle entreprise
########################

# On reprend notre entreprise x_new = (X1 = -1, X2 = 1)
x_new <- data.frame(X1 = -1, X2 = 1)

# type = "response" → renvoie p = P(Y = "dernier niveau" | x_new)
p_new <- predict(mod_logit,
                 newdata = x_new,
                 type   = "response")
p_new

# Si levels(synth$Y) = c("sain","def"), alors :
#   p_new = P(Y = "def" | X1=-1, X2=1)
# donc :
p_def <- p_new
p_sain <- 1 - p_new

p_def
p_sain

# Classe prédite avec seuil 0.5 :
class_new_logit <- ifelse(p_new >= 0.5,
                          levels(synth$Y)[2],  # "def"
                          levels(synth$Y)[1])  # "sain"
class_new_logit

########################
# 12. Frontière de décision de la régression logistique
########################

# On reprend la grille (X1,X2) utilisée pour KNN :
grid <- expand.grid(X1 = grid_x1, X2 = grid_x2)

# Pour chaque point de la grille, on calcule p = P(Y="def"|X)
p_grid <- predict(mod_logit,
                  newdata = grid,
                  type   = "response")

# On convertit la probabilité en classe avec seuil 0.5
grid_class_logit <- ifelse(p_grid >= 0.5,
                           levels(synth$Y)[2],
                           levels(synth$Y)[1])
grid_class_logit <- factor(grid_class_logit, levels = levels(synth$Y))

# Affichage de la frontière :
plot(grid,
     col  = as.numeric(grid_class_logit),
     pch  = 20,
     cex  = 0.6,
     main = "Frontière de décision - régression logistique",
     xlab = "X1", ylab = "X2")

points(synth[, c("X1", "X2")],
       pch = as.numeric(synth$Y),
       col = as.numeric(synth$Y))

legend("topleft",
       legend = levels(synth$Y),
       col    = 1:length(levels(synth$Y)),
       pch    = 1:length(levels(synth$Y)))

# Interprétation :
# - La frontière (séparant les couleurs de fond) est globalement une DROITE,
#   ce qui reflète le caractère linéaire de la régression logistique.
# - On peut la comparer à la frontière KNN, souvent plus irrégulière.

########################
# 13. Taux d'erreur test pour la régression logistique
########################

# On refait un découpage apprentissage / test (ou on réutilise le même).
set.seed(2)
ind_train <- sample(1:n, size = round(0.7 * n))
train <- synth[ind_train, ]
test  <- synth[-ind_train, ]

# Ajustement sur l'apprentissage
mod_logit <- glm(Y ~ X1 + X2,
                 data   = train,
                 family = binomial)

# Probabilités sur l'échantillon test
p_test <- predict(mod_logit,
                  newdata = test,
                  type   = "response")

# Conversion en classes (seuil 0.5)
y_pred_test_logit <- ifelse(p_test >= 0.5,
                            levels(synth$Y)[2],
                            levels(synth$Y)[1])
y_pred_test_logit <- factor(y_pred_test_logit,
                            levels = levels(synth$Y))

y_true_test <- test$Y

# Taux d'erreur test
err_logit <- mean(y_pred_test_logit != y_true_test)
err_logit

# ⚠ Important :
# - On ne compare correctement err_logit et err_knn QUE si on les a
#   calculés sur le MÊME découpage apprentissage/test (mêmes indices train/test).
# - Sinon, la comparaison est biaisée par le hasard du découpage.

###########################################################################
# PARTIE 4 : MATRICE DE CONFUSION, TVP / TVN, COURBE ROC, AUC
###########################################################################

########################
# 14. Matrice de confusion et indicateurs
########################

# Matrice de confusion
tab_conf <- table(ClasseVraie = y_true_test,
                  ClassePred  = y_pred_test_logit)
tab_conf

# On choisit une convention :
#   - Classe POSITIVE = "défaillante" (risque)
#   - Classe NÉGATIVE = "sain"
pos <- levels(synth$Y)[2]  # "def"
neg <- levels(synth$Y)[1]  # "sain"

# VP = vrais positifs = défaillantes bien détectées
VP <- sum(y_true_test == pos & y_pred_test_logit == pos)

# VN = vrais négatifs = saines bien classées
VN <- sum(y_true_test == neg & y_pred_test_logit == neg)

# FP = faux positifs = saines classées "def"
FP <- sum(y_true_test == neg & y_pred_test_logit == pos)

# FN = faux négatifs = défaillantes classées "sain"
FN <- sum(y_true_test == pos & y_pred_test_logit == neg)

VP; VN; FP; FN

# Taux de vrais positifs (TPR, sensibilité, recall de la classe positive)
TVP <- VP / (VP + FN)

# Taux de vrais négatifs (TNR, spécificité)
TVN <- VN / (VN + FP)

TVP
TVN

# Interprétation :
# - TVP élevé : on détecte bien les défaillantes (peu de FN).
# - TVN élevé : on évite de fausses alertes sur les entreprises saines (peu de FP).
# - Selon le contexte (banque, médecine...), on peut privilégier TVP ou TVN.

########################
# 15. Courbe ROC et AUC avec ROCR
########################

# On repart des probabilités p_test = P(Y="def"|X) sur l'échantillon test.
# ROCR travaille avec :
#   prediction(scores, classesVraies)
# On précise label.ordering = c(negatif, positif).
pred_obj <- prediction(p_test, y_true_test,
                       label.ordering = c(neg, pos))

# Courbe ROC = trace TPR (TVP) en fonction de FPR (1 - TVN)
perf_roc <- performance(pred_obj, "tpr", "fpr")

# Tracé
plot(perf_roc,
     main = "Courbe ROC - régression logistique",
     xlab = "FPR = 1 - spécificité",
     ylab = "TPR = sensibilité")
abline(a = 0, b = 1, lty = 2)  # diagonale = classifieur au hasard

# Aire sous la courbe (AUC)
perf_auc <- performance(pred_obj, "auc")
auc <- perf_auc@y.values[[1]]
auc

# Interprétation :
# - AUC varie entre 0.5 (pire : hasard) et 1 (parfait).
# - Entre 0.7 et 0.8 : classifieur acceptable.
# - Au-dessus de 0.8 : très bon classifieur.

###########################################################################
# Fin du script TP3_classification_antisèche.R
###########################################################################
