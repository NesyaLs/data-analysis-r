###########################################################################
# ACP_antisèche.R
# Script modèle très commenté pour faire une ACP en R (FactoMineR)
# Exemple basé sur les eaux minérales (eaux.rda), mais réutilisable partout.
#
# Idée : tu peux remplacer le chargement des données par un autre jeu
# et garder TOUT le reste du script comme modèle.
###########################################################################

########################
# 0. Préparation
########################

# Chargement du package utilisé pour l'ACP
library(FactoMineR)  # fournit la fonction PCA() + plot.PCA()

########################
# 1. Chargement des données
########################

# On charge un jeu de données exemple.
# Ici : eaux.minérales (objet 'data' dans le fichier eaux.rda)
load("data/mineral_waters.rda")

# Dans le TP, l'objet s'appelle "data". On le renomme en X pour être clair.
X <- data                        # matrice de données brutes (data.frame)
n <- nrow(X)                     # nombre d'individus (lignes)
p <- ncol(X)                     # nombre de variables (colonnes)

# Coup d'œil rapide aux données
head(X)                          # 6 premières lignes
summary(X)                       # résumés (min, max, moyenne, etc.)

########################
# 2. Moyennes / écarts-types
########################

# Moyenne et écart-type d'UNE variable (ex : saveur.amère)
mean(X$saveur.amère)             # moyenne de la variable
sd(X$saveur.amère)               # écart-type (corrigé : divise par n-1)

# Moyennes de TOUTES les variables
moy <- apply(X, 2, mean)         # moyenne par colonne (2 = dimension des colonnes)
moy

# Écarts-types corrigés de TOUTES les variables
# sd() et donc scale() utilisent l'écart-type "corrigé" :
#   var = somme((x - moy)^2) / (n-1)
#   sd  = sqrt(var)
sigma_corr <- apply(X, 2, sd)
sigma_corr

# Écarts-types NON corrigés (théoriques, division par n)
# En théorie de l'ACP, on utilise var = somme / n.
# On corrige donc en multipliant sd_corrigé par sqrt((n-1)/n).
sigma_acp <- sigma_corr * sqrt((n - 1) / n)
sigma_acp

########################
# 3. Centrage / réduction
########################

# Matrice Y : données centrées (moyenne = 0 pour chaque variable)
# sweep(X, 2, moy, "-") = pour chaque colonne, on soustrait sa moyenne.
Y <- sweep(X, 2, moy, "-")
apply(Y, 2, mean)                # ≈ 0 pour toutes les colonnes

# Matrice Z_corr : centrée-réduite avec sd corrigé (sd = 1, divise par n-1)
Z_corr <- scale(X)               # centrage + réduction (sd corrigé par défaut)
apply(Z_corr, 2, sd)             # ≈ 1

# Matrice Z_acp : centrée-réduite "théorique" (var = 1 avec division par n)
# En ACP théorique, on veut var(Z) = 1 avec var = somme / n.
# Comme scale() fait var_corrigée = somme / (n-1),
# on multiplie Z_corr par sqrt(n/(n-1)) pour avoir la variante non corrigée.
Z_acp <- Z_corr * sqrt(n / (n - 1))
apply(Z_acp, 2, function(z) sum((z - mean(z))^2) / n)  # ≈ 1

########################
# 4. Corrélations / covariances
########################

# Matrice de corrélation (entre les variables)
# cor() utilise la variance corrigée (division par n-1)
cor(X)

# Matrice de corrélation "théorique" via Z_acp :
# R = t(Z) %*% Z / n
R <- t(Z_acp) %*% Z_acp / n
R

# Vérification : les deux sont (presque) identiques :
cor(X)
R[1:5, 1:5]

########################
# 5. ACP NON NORMÉE (sur matrice de COVARIANCE)
########################

#  !! ACP non normée = on travaille sur les données centrées Y,
# mais pas standardisées → variables en échelle d'origine.
# FactoMineR : scale.unit = FALSE → ACP sur covariances.

res_cov <- PCA(X,
               scale.unit = FALSE,  # pas de réduction
               graph = FALSE)

# Graphique des individus (plan 1-2)
plot(res_cov,
     choix = "ind",       # ind = individus
     cex   = 1.3,
     title = "ACP non normée - individus")

# Graphique des variables (cercle des corrélations version covariance)
plot(res_cov,
     choix = "var",       # var = variables
     cex   = 1.3,
     title = "ACP non normée - variables")

# Lien avec la matrice de covariance C
Y_mat <- as.matrix(Y)                     # Y en matrice numérique
C <- t(Y_mat) %*% Y_mat / n               # Covariance (division par n)
eig_C <- eigen(C)$values                  # valeurs propres de C (théorie)
eig_C
res_cov$eig[, 1]                          # valeurs propres vues par FactoMineR
# ⇒ elles coïncident : FactoMineR fait bien l'ACP sur C.

# Interprétation (rappel) :
# - ACP non normée : les variables à plus grande variance dominent les axes.
# - À utiliser uniquement si les variables sont dans la même unité
#   OU si on VEUT garder l'échelle (sinon on préfère l'ACP normée).

########################
# 6. ACP NORMÉE (sur matrice de CORRÉLATION)
########################

# !! 'est le cas le plus classique en analyse de données
# quand les variables ne sont pas sur la même échelle.
# FactoMineR : scale.unit = TRUE par défaut → ACP sur corrélations.

res <- PCA(X,
           scale.unit = TRUE,    # standardisation automatique
           graph      = FALSE)

# 6.1. Graphique des individus (plan 1-2 puis 2-3)
plot(res,
     choix = "ind",
     cex   = 1.3,
     title = "ACP normée - individus (axes 1-2)")

plot(res,
     choix = "ind",
     axes  = c(2, 3),
     cex   = 1.3,
     title = "ACP normée - individus (axes 2-3)")

# Interprétation rappel :
# - Proximité de deux points = profils similaires sur les variables.
# - Attention à la qualité de représentation (cos2) :
#   un individu mal représenté sur 1-2 ne doit pas être sur-interprété.

# 6.2. Graphique des variables (cercle des corrélations)
plot(res,
     choix = "var",
     cex   = 1.3,
     title = "ACP normée - variables (axes 1-2)")

plot(res,
     choix = "var",
     axes  = c(2, 3),
     cex   = 1.3,
     title = "ACP normée - variables (axes 2-3)")

# Interprétation des flèches :
# - Longueur proche du cercle → variable bien représentée sur le plan.
# - Angle faible entre deux flèches → corrélation positive.
# - Angle ~180° → corrélation négative.
# - Angle ~90° → variables peu corrélées.
# - Flèche alignée avec Dim 1 → contribue fortement à Dim 1, etc.
# - la longueur de la fleche c'est le cos carré plus sin carré et ça représente la part de la variance de variable 
# expliqué par les deux dimensions : grande fleche, tout la variance expliqué, petite fleche peu de variance expliqué

########################
# 6.3. Valeurs propres, variance expliquée, Kaiser
########################

# Tableau des valeurs propres (res$eig) :
res$eig
# colonnes :
# 1 = eigenvalue          (valeur propre)
# 2 = % de variance       (variance expliquée par l'axe)
# 3 = % cumulé de variance

# En ACP normée :
# - les variables sont standardisées → Var totale = p = nombre de variables.
# - somme des valeurs propres = p.
# - Valeur propre λ_k = "quantité d'information" captée par l'axe k.

# Rappel pour le critère de Kaiser :
# - on garde les axes dont la valeur propre > 1
#   (ils expliquent plus de variance qu'une variable d'origine).

# Scree plot = graphique du "coude"
plot(res$eig[, 1],
     type = "b",
     xlab = "Numéro de composante",
     ylab = "Valeur propre",
     main = "Scree plot (critère du coude)")

abline(h = 1, lty = 2)  # repère pour Kaiser

########################
# 6.4. Lien avec la matrice de corrélations R
########################

# Matrice R = t(Z_acp) %*% Z_acp / n (corrélations théoriques)
R <- t(Z_acp) %*% Z_acp / n
eig_R <- eigen(R)$values          # valeurs propres théoriques
eig_R
res$eig[, 1]                      # valeurs propres de FactoMineR
# ⇒ On retrouve les mêmes valeurs : ACP = décomposition de R.

########################
# 7. Coordonnées factorielles & cercle des corrélations à la main
########################

# Coordonnées des individus sur les deux premières composantes
F12 <- res$ind$coord[, 1:2]       # matrice n x 2 (scores des individus)
head(F12)

# Coordonnées des variables (corrélations avec les CP)
A12 <- res$var$coord[, 1:2]       # matrice p x 2
A12

# Cercle des corrélations "à la main" (utile pour comprendre)
plot(A12,
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     asp  = 1,
     pch  = 16,
     main = "Cercle des corrélations (axes 1-2)")
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE)
abline(h = 0, v = 0, lty = 2)
text(A12,
     labels = rownames(A12),
     pos    = 3)

########################
# 8. Qualité de représentation (cos2)
########################

# cos2 des individus : proportion de la variance de l'individu
# expliquée par chaque axe.
res$ind$cos2[1:5, 1:2]            # 5 premiers individus sur axes 1-2

# Qualité globale sur le plan 1-2 (somme des cos2 axe1 + axe2)
qual_ind_12 <- apply(res$ind$cos2[, 1:2], 1, sum)
head(qual_ind_12)

# cos2 des variables
res$var$cos2[, 1:2]

# Qualité des variables sur le plan 1-2
qual_var_12 <- apply(res$var$cos2[, 1:2], 1, sum)
qual_var_12

# Rappel :
# - cos2 proche de 1 → individu/variable très bien représenté dans le plan.
# - cos2 faible → information portée par d'autres axes.

###########################################################################
# Fin du script ACP_antisèche.R
###########################################################################

# -------------------------------------------------------------------------
# Pourquoi parfois on divise par n et parfois par (n-1) ?
#
# Il existe deux définitions possibles de la VARIANCE :
#
# (1) Variance "corrigée" :   var_corr = somme((x - moy)^2) / (n - 1)
#     ➜ C’est la formule utilisée par R (sd(), var())
#     ➜ C’est l’estimateur SANS BIAIS de la vraie variance σ² de la population.
#     ➜ Utilisée en statistique INFÉRENTIELLE (estimation).
#
# (2) Variance "non corrigée" :   var = somme((x - moy)^2) / n
#     ➜ Utilisée dans la THÉORIE DE L’ACP (dans les cours d’algèbre linéaire)
#     ➜ Car les matrices qui apparaissent dans l’ACP sont :
#           * Cov(X) = (1/n) * t(Y) %*% Y        (ACP non normée)
#           * Cor(X) = (1/n) * t(Z) %*% Z        (ACP normée)
#     ➜ Permet que la somme des valeurs propres = somme des variances = p
#
# Donc :
#   • R utilise sd(X) = sqrt( somme / (n-1) )  → variance corrigée
#   • La théorie ACP utilise var = somme / n  → variance non corrigée
#
# Pour passer de l’écart-type corrigé (sd_corr) à l’écart-type théorique ACP :
#   sd_acp = sd_corr * sqrt((n-1)/n)
#
# -------------------------------------------------------------------------
