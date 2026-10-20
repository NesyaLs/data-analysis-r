library(FactoMineR)

X <- read.table("data/cheeses.txt", sep = "", header = TRUE, row.names = 1)

n <- nrow(X)
p <- ncol(X)
#print(X)

head(X)
dim(X)

# moyenne et écart type de chaque variable
moy <- apply(X, 2, mean)
sigma <- apply(X, 2, sd) * sqrt((n - 1) / n)

# standardise
Y <- sweep(X, 2, moy, "-")   # centrage
Z <- sweep(Y, 2, sigma, "/") # réduction

# Vérification
apply(Z, 2, mean)
apply(Z, 2, sd)


# Nombre de clusters souhaité
K <- 5

# kmeans en 5 classes sur données standardisées
km <- kmeans(Z, centers = K, nstart = 50)
km$cluster # la partition
table(km$cluster) # effectifs des classes
km$withinss # inertie de chaque classe (within sum of squares)
km$betweenss # inertie inter-classe (between sum of squares)
km$totss # inertie totale (total sum of squares)
sum(km$withinss) + km$betweenss # inertie total = intra + inter
km$betweenss / km$totss # pourcentage d’inertie expliquée par la partition

# Vérifier que ce pourcentage augmente quand K augmente

# La partition est une nouvelle variable qualitative
part <- as.factor(km$cluster) # objet de classe factor
levels(part) <- paste("cluster", 1:K, sep="") # modalités de la variable

# Un data.frame mélange les colonnes numériques et qualitatives !
Xplus <- data.frame(X, part)
# View(Xplus)

res <- PCA(Xplus, quali.sup="part", graph = FALSE)
res$eig
plot(res, choix = "ind", habillage = "part", invisible = "quali")
plot.PCA(res, choix = "var")

# Interprétation des classes avec la fonction catdes
des <- catdes(Xplus, num.var = 10)
des$quanti$cluster1
des$quanti$cluster2

################

# Classification hiérarchique de Ward sur données centrées-réduites
D <- dist(Z)
tree <- hclust(D^2/(2*n), method = "ward.D")
plot(tree, hang=-1, main="Dendrogramme de Ward", xlab="", sub = "")
tree$height
sum(tree$height) # Inertie totale

# Partition en 5 classes avec la CAH de Ward
K <- 5
plot(tree, hang=-1, main="", sub="", xlab="")
rect.hclust(tree, k=K)
part_ward <- cutree(tree, k=K) # partition en 5 classes

# Comparaison avec la partition en 5 classes des kmeans
table(part_ward) # effectif des classes de la partition de Ward
table(km$cluster) # effectifs des classes de la partition des kmeans
table(part_ward, km$cluster) # tableau de contingence entre les deux partitions

W <- sum(tree$height[1:(n-K)]) # inertie intra-classe de la partition de Ward
(1-W/9)*100 # Pourcentage d’inertie expliquée à comparer avec les kmeans

# 1. Calculer les centres des 5 classes de Ward
sp <- split(Z, part_ward)
centres_ward <- t(sapply(sp, colMeans))

# 2. Appliquer les kmeans en partant des centres des classes de Ward
km <- kmeans(Z, centers = centres_ward) # pas besoin de nstart !
table(part_ward, km$cluster) # deux fromages ont changés de classe
km$betweenss/km$totss # Le pourcentage d’inertie expliquée est un peu meilleur


# ================================================================
# === PARTIE 5 : CLUSTERING DE DONNÉES QUALITATIVES ==============
# ================================================================

# Charger les données sur les races de chiens
load("data/dogs.rda")

# Recodage via l’ACM
res <- MCA(chiens, graph = FALSE) # Multiple Correspondance Analysis
res$eig # 10 valeurs propres non nulles
res <- MCA(chiens, ncp=10, graph = FALSE) # 10 composantes principales
F <- res$ind$coord # données recodées

# CAH de Ward sur les données recodées
tree <- hclust(dist(F)^2/(2*nrow(F)), method="ward.D")
plot(tree, hang=-1)

# Couper le dendrogramme en conséquence et interpréter la partition
part <- cutree(tree, k=4) # partition en 4 classes
part <- as.factor(part)
levels(part) <- paste("cluster", 1:4, sep="")
des <- catdes(data.frame(part, chiens), num.var=1)
des$category

