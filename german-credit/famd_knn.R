## Packages ----
library(caret)
library(ROCR)
library(FactoMineR)

## Données ----
load("data/GermanCredit.rda")

str(GermanCredit)
head(GermanCredit)

# Variable cible
Y <- GermanCredit$Class

# Variables explicatives (mêmes que dans ton code, mais passées à une FAMD)
X_mix <- GermanCredit[, c("CheckingAccountStatus",
                          "SavingsAccount",
                          "Purpose",
                          "Duration",
                          "Amount",
                          "Age")]

## 1) ACP mixte (FAMD)

# FAMD sur les variables mixtes (quanti + quali)
res_famd <- FAMD(X_mix, ncp = 5, graph = FALSE)

# Coordonnées factorielles des individus sur les 5 premiers axes
F_famd <- res_famd$ind$coord  # n x 5

head(F_famd)

# Standardisation pour le KNN (même logique que ton scale() précédent)
F_scaled <- scale(F_famd)

# Base finale pour le KNN
data_knn <- data.frame(Class = Y, F_scaled)

## 2) KNN ----

set.seed(123)

# séparation apprentissage / test (70/30)
n  <- nrow(data_knn)
tr <- sample(1:n, 0.5 * n)

train <- data_knn[tr, ]
test  <- data_knn[-tr, ]

# nombre de voisins
k <- 11

# Modèle KNN (caret)
g <- knn3(Class ~ ., data = train, k = k)

# Prédiction en classe sur le test
ypred <- predict(g, newdata = test, type = "class")
ytrue <- test$Class

# Erreur de classification
err <- mean(ytrue != ypred)
err

## 3) Indicateurs de performance (confusion) ----

# "Bad" = client à risque (positif)
# "Good" = client sain (négatif)

VP <- sum((ytrue == "Bad")  & (ypred == "Bad"))   # Vrais positifs
FN <- sum((ytrue == "Bad")  & (ypred == "Good"))  # Faux négatifs
VN <- sum((ytrue == "Good") & (ypred == "Good"))  # Vrais négatifs
FP <- sum((ytrue == "Good") & (ypred == "Bad"))   # Faux positifs

VP; FN; VN; FP

# Taux de vrais positifs (rappel pour la classe Bad)
TVP <- VP / (VP + FN)

# Taux de vrais négatifs (rappel pour la classe Good)
TVN <- VN / (VN + FP)

# Taux de faux positifs
TFP <- FP / (FP + VN)

# Taux de faux négatifs 
TFN <- FN / (FN + VP)

TVP; TVN; TFP; TFN

## 4) ROC + AUC ----

# Probabilité d'être "Bad" pour chaque individu du test
proba_test <- predict(g, newdata = test, type = "prob")
score <- proba_test[, "Bad"]

# On s'assure que l'ordre des niveaux est "Good", "Bad"
ytrue_roc <- factor(test$Class, levels = c("Good", "Bad"))

# Objet ROCR
res <- prediction(score, ytrue_roc, label.ordering = c("Good", "Bad"))

# Courbe ROC
roc_perf <- performance(res, "tpr", "fpr")

plot(roc_perf,
     main = "Courbe ROC - KNN (GermanCredit, k = 11)",
     col  = "blue",
     lwd  = 2)
abline(a = 0, b = 1, col = "red", lty = 2)

# AUC
auc <- performance(res, "auc")@y.values[[1]]
auc

# données désequilibré au départ faire attention au seuil, on a plus de good, on va mieux prédire les good que les bads.

