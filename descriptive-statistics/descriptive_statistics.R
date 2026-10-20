# Extraire :
# * la taille des hommes
# * l’indice de la plus petite femme et sa taille
# * le poids des femmes de moins de 160 cm
taille <- c(182, 150, 160, 140.5, 191)
sexe <- c(0, 1, 1, 1, 0) # (0=H et 1=F)
poids <- c(80, 57, 63, 52, 78)

taille[sexe == 0]

taille_femme = taille[sexe == 1]
min_femme <- min(taille_femme) 
indice_min_femme <- which.min(taille[sexe == 1])  # indice parmi les femmes
indice_min_femme_global <- which(taille == min_femme)
indice_min_femme
indice_min_femme_global

poids[sexe == 1 & taille < 160]

# Extraire :
# * les valeurs de x comprises entre 2 et 3
# * les indices de ces valeurs
x <- c(0.1, 0.5, 2.1, 3.5, 2.8, 2.7, 1.9, 2.2, 5.6)

x[x >= 2 & x <= 3]
which(x >= 2 & x <= 3)

# Matrices :
# Simuler 20 répétitions d’une expérience de Bernouilli (10 lancers de pièce)
set.seed(10)
x <- rbinom(12, size = 10, prob = 0.5)
X <- matrix(x, nrow = 4, ncol = 3, byrow = TRUE)
X
X[1, 3] # valeur ligne 1, colonne 3
X[ , 2] # valeurs de la 2ème colonne (toutes les lignes)
X[3, ] # valeurs de la 3ème ligne (toutes les colonnes)
X[3, , drop = FALSE] # idem mais renvoie une matrice
X[c(1, 4), ] # lignes 1 et 4
X[3, -c(1,3)] # ligne 3 et toutes les colonnes sauf 1 et 3
# Concaténer (en colonne) les vecteurs sexe, taille et poids
X <- cbind(sexe, taille, poids)
X[sexe==1, -1] # Matrice des tailles et poids des hommes

# Liste avec 3 éléments (un scalaire, un vecteur et une matrice)
a <- 10
b <- 1:5
c <- matrix(1:6, nrow = 2, ncol = 3)
L <- list(a=a, b=b, c=c) # liste nommée (chaque élément a un nom)
# Extraction avec les crochets ou doubles crochets
L[1] # renvoie une liste

L[[1]] # renvoie le premier élément de la liste (le scalaire)
L[c(1,3)] # liste avec le premier et 3ème élément (le scalaire et la matrice)
# Extraction avec le dollard (liste nommée)
L$a
L$b
L$c

# Un data frame permet d’avoir des colonnes quantitatives et qualitatives :
X <- data.frame(poids = c(80,90,75), taille = c("grand", "grand", "petit"))
X[ , 1] # colonne poids
X$poids # idem
# ou même encore
X[ , "taille"] # idem


# EXERCICE : 

# Simuler la sexe de 20 personnes (avec 30% d’hommes et 70% de femmes)
set.seed(5)
sexe <- rbinom(20, size = 1, prob = 0.3) # 1=H, 0=F
table(sexe)/length(sexe)
# Simuler la taille de 20 personnes
# (Gaussienne d’espérance 170 et d’écart-type 10)
set.seed(5)
taille <- rnorm(20, mean = 170, sd = 10)
hist(taille)
# Créer un data frame (matrice de données décrivant ces 20 personnes)
X <- data.frame(sexe = sexe, taille = taille)
X
# Extraire de X :
# * la proportion d’hommes et de femmes (avec les fonctions table() et length())
# * les lignes des hommes
# * le sexe des personnes de moins de 160 cm
table(X$sexe) / nrow(X)
X[X$sexe == 1, ]
X$sexe[X$taille < 160]

# Vecteurs :
x <- rep(NA, 6)
x
x[c(1,5)] <- 1
x
is.na(x)
x[is.na(x)] <- 2
x

# Matrices :
x <- rep(1:3, times = 4)
X <- matrix(x, 3, 4)
X
X != 2
X[X != 2] <- 0
X
X <- X[-3, ]
X
# Remplacer toutes les valeurs nulles de X1 par les valeurs de X2 correspondantes.
X1 <- matrix(c(0,1,0,1,1,0,0,0,1), ncol = 3)
X2 <- matrix(1:9, ncol = 3)

X1[X1 == 0] <- X2[X1 == 0]
X1
X

data <- read.table("Intima_Media.txt", header = TRUE, sep = " ", dec = ",")
head(data)
str(data)

data$IMC <- data$poids / ( (data$taille/100)^2 )
data
data$mesure[data$IMC > 30]

femmes_actives <- data[data$SEXE == 2 & data$SPORT == 1, ]
femmes_actives

femmes_surpoids_50 <- data[data$SEXE == 2 & data$IMC > 30 & data$AGE > 50, ]
femmes_surpoids_50

don <- read.table("nutriage.txt", header = FALSE, row.names = 1)
don
don <- t(don) # pour transposer
don
class(don) # la fonction t() renvoie une matrice
don <- as.data.frame(don)
rownames(don) <- NULL # Pour supprimer les noms incorrects des 226 lignes
colnames(don) # noms des 13 variables

str(don)
class(don$sexe)
don$sexe <- as.factor(don$sexe)
don
class(don$sexe)
levels(don$sexe) <- c("Homme","Femme")
don$situation <- as.factor(don$situation) #situation familiale (4 modalités)
levels(don$situation) <- c("seul","couple","famille","autre")
don$matgras <- as.factor(don$matgras) # matière grasse consommée (8 modalités)
levels(don$matgras) <- c("beurre","margarine","arachide", "tournesol",
                         "olive", "Isio4","colza","canard")

# Variables qualitatives ordinales : objets de classe ordered
don$viande <- as.ordered(don$viande) # consommation de viande
don$poisson <- as.ordered(don$poisson) # de poisson
don$fruit_crus <- as.ordered(don$fruit_crus) # de fruits
don$fruit_legume_cuits <- as.ordered(don$fruit_legume_cuits) # de légumes
don$chocol <- as.ordered(don$chocol) # de chocolat
# Codage des 5 modalités :
# 0 = jamais, 1 = Moins d’une fois par semaine, 2 = 1 fois par semaine
# 3 = 3 à trois fois par semaine, 4 = 4 à 6 fois par semaine, 5 = 1 fois par jour.
niveaux <- c("jamais","< 1/sem.","1/sem.","2-3/sem.","4-6/sem.","1/jour")
levels(don$viande) <- niveaux

levels(don$poisson) <- niveaux
levels(don$fruit_crus) <- niveaux
levels(don$fruit_legume_cuits) <- niveaux
levels(don$chocol) <- niveaux
save(don, file="nutriage.rda")

load("nutriage.rda")

table(don$matgras) # effectifs
table(don$matgras)/length(don$matgras) # fréquences

# Diagramme en barre (ou en bâtons)
freq <- table(don$matgras)/length(don$matgras)
barplot(freq, ylab = "Fréquences", las=2, col = "green4",
        main = "Type de matière grasse consommée")
# Diagramme circulaire (ou camenbert ou tarte)
library("RColorBrewer")
col <- brewer.pal(8,"Pastel2")
pie(table(don$matgras), col = col, main = "Type de matière grasse consommée")

par(mfrow=c(1,2))
barplot(table(don$sexe)/length(don$situation))
barplot(table(don$situation)/length(don$situation))
par(mfrow=c(1,1))

load("nutriage.rda")
x <- don$taille
n <- length(x)
# Taille moyenne des personnes agées :
sum(x)/n
mean(x)
# Ecart-type :
sqrt(sum((x - mean(x))^2)/(n-1))
sd(x)

# Médiane :
quantile(x, probs = 0.5) # quantile d’ordre 0.5
median(x)
# La moitié des personnes agées mesurent moins de 163 cm.
# Interval inter-déciles :
quantile(x, probs=c(0.1, 0.9))
# 90% des personnes agées mesurent entre 153 et 176 centimères

# Résumés numériques
summary(x)
# Boite à moustache
boxplot(x, ylab = "taille")

# classes égales
hist(x, freq = FALSE, main="Histogramme", ylab = "Densite", xlab = "Taille")
