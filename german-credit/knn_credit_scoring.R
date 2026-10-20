library(caret)
library(ROCR)
library(MASS)

# ------------------ LOAD DATA ------------------ #

load("data/GermanCredit.rda")

data <- GermanCredit

dim(data)
str(data)
head(data)

# ------------------ SCALE DATA ------------------ #

data$Class <- as.factor(data$Class)

# on déclare les varaibles qualitatives en factor (1 niveau par modalités)
data$CheckingAccountStatus <- as.factor(data$CheckingAccountStatus)
data$SavingsAccount        <- as.factor(data$SavingsAccount)
data$Purpose               <- as.factor(data$Purpose)

str(data)

# on prend les colonnes numérique
num_vars <- sapply(data, is.numeric)
num_vars

# on créé une copie standaridisé
data_scaled <- data

#summary(data_scaled[, num_vars])


# ------------------ SPLIT DATA ------------------ #

set.seed(123)

n <- nrow(data_scaled)
# 70 % de donnée pour l'entrainement, 30 pour la vérif
ind_train <- sample(1:n, size = as.integer(0.7 * n))

train <- data_scaled[ind_train, ]
test  <- data_scaled[-ind_train, ]

means <- apply(train[, num_vars], 2, mean)
sds <- apply(train[, num_vars], 2, sd)
train[, num_vars] <- scale(train[, num_vars], center = means, scale = sds)
test[, num_vars] <- scale(test[, num_vars], center = means, scale = sds)

dim(train)
dim(test)

# ------------------ APPLY METHOD ------------------ #

k <- 11  # nombre de voisins

g_knn <- knn3(Class ~ ., data = train, k = k)

# probabilité a posteriori good or bad
prob_test <- predict(g_knn, newdata = test, type = "prob")
head(prob_test)

# classe prédite
y_pred <- predict(g_knn, newdata = test, type = "class")
head(y_pred)

# Vraies classes :
y_true <- test$Class

# on sauvegarde pour la comparaison finale
y_true_base <- y_true
y_pred_base <- y_pred


# ------------------ ANALYSE RESULT ------------------ #

table(y_true, y_pred)  # matrice de confusion brute
# dans la premiere ligne on lit bad / bad : 38;  bad / good : 58.
# le modele prédit donc 38 bad oberservé en bad, et 58 bad oberservé en good 
# c'est très mauvais, cela signifie que le modele prédit 38 mauvais payeur
# et ne detecte pas les 58 mauvais payeur restant (les mets en good)

# dans la deuxieme ligne on voit que le model est meilleur pour prédire des bons payeur
# il trouve 181 bon payeur observé et n'en rate que 23 qu'il place en bad.

# Taux d'erreur test :
test_error <- mean(y_pred != y_true)
test_error
# 27% d'erreur sur les données de test

# ------------------ OPTIMIZE METHOD ------------------ #

# Taux d'erreur test pour B découpages, on répète le découpage B fois pour voir la variabilité
# du taux d'erreur

B <- 50
test_error_B <- rep(NA, B)

set.seed(123)

for (b in 1:B) {
  # Découpage 70% / 30%
  ind_train <- sample(1:n, size = as.integer(0.7 * n))
  train_b <- data_scaled[ind_train, ]
  test_b  <- data_scaled[-ind_train, ]
  
  means_b <- apply(train_b[, num_vars], 2, mean)
  sds_b <- apply(train_b[, num_vars], 2, sd)
  train_b[, num_vars] <- scale(train_b[, num_vars], center = means_b, scale = sds_b)
  test_b[, num_vars] <- scale(test_b[, num_vars], center = means_b, scale = sds_b)
  
  # Apprentissage KNN (toujours avec k voisins)
  g_b <- knn3(Class ~ ., data = train_b, k = k)
  
  # Prédiction sur test_b
  y_pred_b <- predict(g_b, newdata = test_b, type = "class")
  y_true_b <- test_b$Class
  
  # Taux d'erreur test pour ce découpage
  test_error_B[b] <- mean(y_pred_b != y_true_b)
}

summary(test_error_B)
boxplot(test_error_B, ylab = "Taux d'erreur test",
        main = paste("KNN -", k, "voisins"))

# on optimise K

grille_k <- seq(from = 1, to = 25, by = 2)  # K impairs
B <- 30  # nombre de découpages par valeur de K

# Matrice pour stocker les erreurs : lignes = B, colonnes = K
test_error_grid <- matrix(NA, nrow = B, ncol = length(grille_k))
colnames(test_error_grid) <- paste0("K=", grille_k)

set.seed(321)

for (j in seq_along(grille_k)) {
  k_j <- grille_k[j]
  
  for (b in 1:B) {
    ind_train <- sample(1:n, size = as.integer(0.7 * n))
    train_b <- data_scaled[ind_train, ]
    test_b  <- data_scaled[-ind_train, ]
    
    means_b <- apply(train_b[, num_vars], 2, mean)
    sds_b <- apply(train_b[, num_vars], 2, sd)
    train_b[, num_vars] <- scale(train_b[, num_vars], center = means_b, scale = sds_b)
    test_b[, num_vars] <- scale(test_b[, num_vars], center = means_b, scale = sds_b)
    
    g_b <- knn3(Class ~ ., data = train_b, k = k_j)
    
    y_pred_b <- predict(g_b, newdata = test_b, type = "class")
    y_true_b <- test_b$Class
    
    test_error_grid[b, j] <- mean(y_pred_b != y_true_b)
  }
}

# Résumé des erreurs moyennes par K
err_moy <- apply(test_error_grid, 2, mean)
err_moy

# Visualisation : boxplot des erreurs pour chaque K
boxplot(test_error_grid, ylab = "Taux d'erreur test",
        main = "Comparaison des K (KNN sur GermanCredit)",
        las = 2)

# K "optimal" (erreur moyenne minimale)
k_opt <- grille_k[which.min(err_moy)]
k_opt


## ------------------ SPLIT DATA ------------------ ##

set.seed(123)

n <- nrow(data_scaled)
ind_train <- sample(1:n, size = as.integer(0.7 * n))

train <- data_scaled[ind_train, ]
test  <- data_scaled[-ind_train, ]

means <- apply(train[, num_vars], 2, mean)
sds <- apply(train[, num_vars], 2, sd)
train[, num_vars] <- scale(train[, num_vars], center = means, scale = sds)
test[, num_vars] <- scale(test[, num_vars], center = means, scale = sds)


## ------------------ KNN avec K = 13 ------------------ ##

k <- 13

g_knn9 <- knn3(Class ~ ., data = train, k = k)

prob_test9 <- predict(g_knn9, newdata = test, type = "prob")
head(prob_test9)

# classe prédite
threshold <- 0.28
y_pred9 <- ifelse(prob_test9[,"Bad"] >= threshold, "Bad", "Good")
y_pred9 <- factor(y_pred9, levels = levels(test$Class))

y_true  <- test$Class

## ------------------ MATRICE DE CONFUSION ------------------ ##

# Matrice brute
tab9 <- table(Observation = y_true, Prediction = y_pred9)
tab9

# Version détaillée avec caret
cm9 <- confusionMatrix(y_pred9, y_true, positive = "Bad")  # on fixe bien Bad comme classe positive
cm9

# Taux d'erreur global
test_error9 <- mean(y_pred9 != y_true)
test_error9

## ------------------ TPR / FPR (vrais / faux positifs) ------------------ ##

TP <- tab9["Bad","Bad"]   # vrai bad bien prédits
FN <- tab9["Bad","Good"]  # vrai bad mal prédits 
FP <- tab9["Good","Bad"]  # vrai good mal prédits
TN <- tab9["Good","Good"] # vrais Good bien prédits

TPR <- TP / (TP + FN)     # nombre de bad bien prédits sur tous les vrai bads
FPR <- FP / (FP + TN)     # nombre de good mal prédits sur tous les vrai goods

TPR # 72% des bad sont bien prédits
FPR # 36% des good sont mal prédits

# avec ce modèle, la banque accorderait (100-70=30)% de crédits a des mauvais payeur
# avec ce modèle, la banque n'accorderait pas de crédits à 34% des bons payeurs

## ------------------ 5 COURBE ROC & AUC ------------------ ##

# On prend la proba d'être Bad (classe positive)
score_bad <- prob_test9[,"Bad"]

# Objet ROCR : prediction(probabilités, labels)
pred_roc <- prediction(score_bad, y_true, label.ordering = c("Good", "Bad"))  # y_true est un facteur avec "Bad"/"Good"

# Courbe ROC : axe x = FPR, axe y = TPR
perf_roc <- performance(pred_roc, "tpr", "fpr")

plot(perf_roc,
     main = "Courbe ROC - KNN (k = 13) - Classe positive : Bad",
     lwd = 2)
abline(a = 0, b = 1, lty = 2)  # diagonale du hasard

# Aire sous la courbe (AUC)
auc_obj <- performance(pred_roc, "auc")
auc_knn9 <- auc_obj@y.values[[1]]
auc_knn9

## =========================================================
##  COMPARAISON KNN vs "MODELE IA" vs MÉTHODE ALÉATOIRE
## =========================================================

## 1) Résultats des modèles KNN

# KNN 1 : modèle de base (k = 11, seuil = 0.5)
acc_knn_base <- 1 - test_error   # accuracy du KNN "simple"

tab_base <- table(Observation = y_true_base, Prediction = y_pred_base)
TP_base <- tab_base["Bad","Bad"]
FN_base <- tab_base["Bad","Good"]
FP_base <- tab_base["Good","Bad"]
TN_base <- tab_base["Good","Good"]

TPR_base <- TP_base / (TP_base + FN_base)   # détection des Bad
FPR_base <- FP_base / (FP_base + TN_base)   # erreur sur les Good

# KNN 2 : modèle optimisé (k = 13, seuil = 0.28)
acc_knn_opt <- 1 - test_error9   # accuracy avec seuil
TPR_opt <- TPR                   # déjà calculé plus haut
FPR_opt <- FPR

# on garde aussi les y_true du modèle optimisé
y_true_opt <- y_true

## 2) Modèle IA (à remplir avec vos vrais résultats)

acc_ia <- 0.67   # à remplacer
TPR_ia <- 0.70   # à remplacer
FPR_ia <- 0.36   # à remplacer

## 3) Méthode aléatoire (baseline)

set.seed(999)
y_pred_rand <- sample(levels(y_true_opt), length(y_true_opt), replace = TRUE)
y_pred_rand <- factor(y_pred_rand, levels = levels(y_true_opt))

tab_rand <- table(Observation = y_true_opt, Prediction = y_pred_rand)

acc_rand <- 1 - mean(y_pred_rand != y_true_opt)

TP_rand <- tab_rand["Bad","Bad"]
FN_rand <- tab_rand["Bad","Good"]
FP_rand <- tab_rand["Good","Bad"]
TN_rand <- tab_rand["Good","Good"]

TPR_rand <- TP_rand / (TP_rand + FN_rand)   # détection des Bad au hasard
FPR_rand <- FP_rand / (FP_rand + TN_rand)   # erreur sur Good au hasard

## 4) Tableau récapitulatif

model_names <- c("KNN base", "KNN seuil", "Modèle IA", "Aléatoire")

accuracy_vec <- c(acc_knn_base, acc_knn_opt, acc_ia,   acc_rand)
TPR_vec      <- c(TPR_base,     TPR_opt,     TPR_ia,   TPR_rand)
FPR_vec      <- c(FPR_base,     FPR_opt,     FPR_ia,   FPR_rand)

results_df <- data.frame(
  Modele   = model_names,
  Accuracy = round(accuracy_vec, 3),
  TPR_Bad  = round(TPR_vec, 3),
  FPR_Good = round(FPR_vec, 3)
)

print(results_df)

## 5) Plots comparatifs

par(mfrow = c(1, 3))

# Plot 1 : Accuracy
barplot(accuracy_vec, names.arg = model_names,
        ylim = c(0,1),
        main = "Accuracy des modèles",
        ylab = "Accuracy")
abline(h = 0.5, lty = 2)  # niveau hasard approx

# Plot 2 : TPR (détection des Bad)
barplot(TPR_vec, names.arg = model_names,
        ylim = c(0,1),
        main = "TPR (détection des Bad)",
        ylab = "TPR")

# Plot 3 : FPR (erreur sur les Good)
barplot(FPR_vec, names.arg = model_names,
        ylim = c(0,1),
        main = "FPR (erreur sur Good)",
        ylab = "FPR")

par(mfrow = c(1,1))
