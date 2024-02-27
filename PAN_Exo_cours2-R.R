#Création d'une base de donnée

Data_frame1 <- data.frame(
  Nom = c("Nom1","Nom2","Nom3","Nom4","Nom5","Nom6","Nom7","Nom8"),
  Age = c(18,19,20,21,23,25,26,35),
  Sexe = c("M","F","F","M","M","F","F","M"),
  Matim = c("Celib","Marié","Celib","Celib","Marié","Marié","Celib","Marié"),
  Nb_enf = c(0,3,1,2,2,0,3,4)
)
View(Data_frame1)
Matice1 <- as.matrix(Data_frame1)

colnames(Matice1)<- c("Name","Old","Sex","Sit_matri","Ct_chld")
rownames(Matice1)<- c("Indiv1","Indiv2","Indiv3","Indiv4","Indiv5","Indiv6","Indiv7","Indiv8")

View(Matice1)

# Statistiques descriptives 
summary(Data_frame1)

# Supposons que 'df' est votre DataFrame et 'sexe' est la colonne qui contient le sexe
part <- table(Data_frame1$Sexe)

# Création du diagramme à secteurs
pie(part, labels = names(part), main = "Répartition par sexe")

# Supposons que 'df' est votre DataFrame et 'age' est la colonne qui contient l'âge

breaks <- seq(15, max(Data_frame1$Age), by = 5)
labels <- paste("[", breaks[-length(breaks)], "-", breaks[-1], "]", sep="")
Data_frame1$classe_age <- cut(Data_frame1$Age, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = labels)

# Installation de la bibliothèque ggplot2 si elle n'est pas déjà installée
if (!require(ggplot2)) install.packages('ggplot2')

# Chargement de la bibliothèque ggplot2
library(ggplot2)

# Création du diagramme en barres
ggplot(Data_frame1, aes(x=classe_age)) +
  geom_bar() +
  xlab("Classe d'âge") +
  ylab("Nombre") +
  ggtitle("Diagramme en barres de la variable 'classe_age'")


#Convexité et Optimisation
#Minimiser f(x, y) = x^4 + y^4 
#sous les contraintes g1(x, y) = x + y - 1 =<0,  x >=0 y >=0 

# Définition de la fonction objectif et des contraintes
f <- function(x, y) {
  return(x^4 + y^4)
}

g1 <- function(x, y) {
  return(x + y - 1)
}

g2 <- function(x, y) {
  return(x)
}

g3 <- function(x, y) {
  return(y)
}

# Définition de la fonction de Lagrange
L <- function(x, y, lambda, mu1, mu2) {
  return(f(x, y) - lambda*g1(x, y) - mu1*g2(x, y) - mu2*g3(x, y))
}

# Dérivées partielles de L
df_dx <- function(x, y, lambda, mu1, mu2) {
  return(4*x^3 - lambda - mu1)
}

df_dy <- function(x, y, lambda, mu1, mu2) {
  return(4*y^3 - lambda - mu2)
}

# Conditions de dualité faible
dual_feasibility <- function(x, y, lambda, mu1, mu2) {
  return(lambda * g1(x, y) == 0 && mu1 * g2(x, y) == 0 && mu2 * g3(x, y) == 0)
}

# Conditions de primal feasibility
primal_feasibility <- function(x, y) {
  return(g1(x, y) <= 0 && g2(x, y) >= 0 && g3(x, y) >= 0)
}

# Initialisation
x <- 0
y <- 0
lambda <- 0
mu1 <- 0
mu2 <- 0

# Taux d'apprentissage
alpha <- 0.01

# Boucle d'optimisation
for (i in 1:1000) {
  # Mise à jour de x et y
  x <- x - alpha * df_dx(x, y, lambda, mu1, mu2)
  y <- y - alpha * df_dy(x, y, lambda, mu1, mu2)
  
  # Mise à jour de lambda, mu1 et mu2
  lambda <- lambda + alpha * g1(x, y)
  mu1 <- mu1 + alpha * g2(x, y)
  mu2 <- mu2 + alpha * g3(x, y)
  
  # Vérification des conditions d'arrêt
  if (primal_feasibility(x, y) && dual_feasibility(x, y, lambda, mu1, mu2) && abs(df_dx(x, y, lambda, mu1, mu2)) < 1e-6 && abs(df_dy(x, y, lambda, mu1, mu2)) < 1e-6) {
    break
  }
}

# Valeur optimale
optimal_value <- f(x, y)
print(paste("La valeur optimale est", optimal_value))

