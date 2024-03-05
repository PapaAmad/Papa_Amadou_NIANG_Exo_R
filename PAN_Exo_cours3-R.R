# Installer les packages nécessaires
install.packages(c("dplyr", "random"))

# Charger les packages
library(dplyr)
library(random)

# Définir le nombre d'observations
n <- 1000

# Générer des données aléatoires
data <- data.frame(
  Sexe = sample(c("Homme", "Femme"), n, replace = TRUE),
  Taille = rnorm(n, mean=170, sd=10), # Taille en cm
  Poids = rnorm(n, mean=70, sd=15), # Poids en kg
  Age = sample(18:100, n, replace = TRUE), # Age en années
  Salaire = runif(n, min=20000, max=100000), # Salaire annuel en euros
  Profession = sample(c("Enseignant", "Ingénieur", "Médecin", "Avocat", "Artiste"), n, replace = TRUE),
  Ville = sample(c("Dakar", "Thiès", "Kaolack", "Ziguinchor", "Touba"), n, replace = TRUE),
  Education = sample(c("Secondaire", "Bac", "Licence", "Master", "Doctorat"), n, replace = TRUE),
  Enfant = sample(0:5, n, replace = TRUE), # Nombre d'enfants
  Fumeur = sample(c("Oui", "Non"), n, replace = TRUE),
  Sportif = sample(c("Oui", "Non"), n, replace = TRUE),
  ScoreSatisfaction = runif(n, 0, 100), # Score de satisfaction
  GroupeSanguin = sample(c("A", "B", "AB", "O"), n, replace = TRUE), # Groupe sanguin
  VaccinéCovid = sample(c(TRUE, FALSE), n, replace = TRUE), # Vacciné contre le Covid-19
  DistanceTravail = rnorm(n, mean=10, sd=5), # Distance du travail en km
  TypeLogement = sample(c("Maison", "Appartement", "Studio"), n, replace = TRUE) # Type de logement
)
# Installer le package nécessaire
install.packages("writexl")

# Charger le package
library(writexl)

# Exporter la base de données en format Excel
write_xlsx(data, "MaBaseDeDonnées.xlsx")

# Définir les limites des classes pour l'âge, la taille et le poids
limites_age <- seq(0, 100, by = 10) # Classes d'âge de 10 ans
limites_taille <- seq(130, 200, by = 10) # Classes de taille de 10 cm
limites_poids <- seq(30, 120, by = 10) # Classes de poids de 10 kg

# Définir les labels pour chaque classe
labels_age <- paste("[", head(limites_age, -1), ";", tail(limites_age, -1), "]", sep = "")
labels_taille <- paste("[", head(limites_taille, -1), ";", tail(limites_taille, -1), "]", sep = "")
labels_poids <- paste("[", head(limites_poids, -1), ";", tail(limites_poids, -1), "]", sep = "")

# Créer des variables de classe pour l'âge, la taille et le poids
data$ClasseAge <- cut(data$Age, breaks = limites_age, include.lowest = TRUE, labels = labels_age)
data$ClasseTaille <- cut(data$Taille, breaks = limites_taille, include.lowest = TRUE, labels = labels_taille)
data$ClassePoids <- cut(data$Poids, breaks = limites_poids, include.lowest = TRUE, labels = labels_poids)


# Installer les packages nécessaires
install.packages(c("dplyr", "ggplot2"))

# Charger les packages
library(dplyr)
library(ggplot2)

# Calculer la fréquence de chaque classe d'âge
freq_age <- table(data$ClasseAge)

# Calculer la fréquence de chaque classe de taille
freq_taille <- table(data$ClasseTaille)

# Calculer la fréquence de chaque classe de poids
freq_poids <- table(data$ClassePoids)

# Créer un histogramme des classes d'âge
ggplot(data, aes(x=ClasseAge)) +
  geom_histogram(stat="count", fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(title="Distribution des classes d'âge", x="Classe d'âge", y="Fréquence")

# Créer un histogramme des classes de taille
ggplot(data, aes(x=ClasseTaille)) +
  geom_histogram(stat="count", fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(title="Distribution des classes de taille", x="Classe de taille", y="Fréquence")

# Créer un histogramme des classes de poids
ggplot(data, aes(x=ClassePoids)) +
  geom_histogram(stat="count", fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(title="Distribution des classes de poids", x="Classe de poids", y="Fréquence")

# Calculer le tableau de contingence
tableau <- table(data$Sexe, data$Fumeur)

# Afficher le tableau de contingence
print(tableau)

# Calculer les totaux marginaux
total_lignes <- rowSums(tableau)
total_colonnes <- colSums(tableau)
total_general <- sum(tableau)

# Calculer le tableau des fréquences attendues
attendu <- outer(total_lignes, total_colonnes) / total_general

# Afficher le tableau des fréquences attendues
print(attendu)

# Calculer la statistique du test du khi-2
khi2 <- sum((tableau - attendu)^2 / attendu)

# Afficher la statistique du test du khi-2
print(khi2)

