# Imporation de la base Table de coversion et cereales

# First step traitement de la base céréales

library(haven)
cereales <-read_dta("cereales.dta")
str(cereales)

## Renommer les variables 

colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")
## Gestion des NA

## Suppression des ménages qui ne consomment ni achétent de céréales

attach(cereales)
anyNA(Qtty_cons)
anyNA(Qtty_achat)

## Création de variables temporaires

cereales$t<-ifelse(is.na(Qtty_cons)==1,1,0) # Ménages qui ne consomment pas de céréales
table(cereales$t) 

cereales$k<-ifelse(is.na(Qtty_achat)==1,1,0) # Ménages qui n'achètent pas de céréales
table(cereales$k)

cereales_na1<-cereales[cereales$t==1,]
cereales_na2<-cereales[cereales$k==1,]

cereales<-cereales[cereales$t==0,]
cereales<-cereales[cereales$k==0,]

# Supression des variables temporaires

cereales$t<- NULL
cereales$k<- NULL

#Second step

library(readxl)
Table_de_conversion<- read_excel(
  "D:/Traitement Statistique avec R/Table de conversion phase 2.xlsx")

Table_de_conversion$...8 <- NULL
Table_de_conversion$...9 <- NULL

colnames(Table_de_conversion) <- c("cereales__id","Nom_Prod",
                                                "Unite_cons","Nom_Unite",
                                                "Taille_cons","Nom_Taille","poids_cons")
cereales <- merge(cereales, Table_de_conversion, 
               by = c("cereales__id", "Unite_cons", "Taille_cons"), all.x = TRUE)

colnames(Table_de_conversion) <- c("cereales__id","Nom_Prod",
                                        "Unite_achat","Nom_Unite",
                                        "Taille_achat","Nom_Taille","poids_achat")

# Base de données finale

cereales <- merge(cereales,Table_de_conversion, 
               by = c("cereales__id", "Unite_achat", "Taille_achat"), all.x = TRUE)


# Traitement de la base de données

library(data.table)
cereales_fin <- data.table(cereales)

cereales_fin[,poids_cons:=as.numeric(poids_cons)] # Conversion de la variable poids_cons
cereales_fin[,qtty_cons_kg:= poids_cons*Qtty_cons/1000] ## Quantité consommée en kg
cereales_fin[,summary(qtty_cons_kg)] # Quelques paramètres de tendances centrales

cereales_fin[,poids_achat:=as.numeric(poids_achat)] # Conversion de la variable poids_achat
cereales_fin[,qtty_achat_kg:= poids_achat*Qtty_achat/1000] ## Quantité achetée en kg
cereales_fin[,summary(qtty_achat_kg)] # Quelques paramètres de tendances centrales

cereales_fin[,prix_unit:= Value_achat/qtty_achat_kg] # Prix unitaire
cereales_fin[,summary(prix_unit)] # Quelques paramètres de tendances centrales

cereales_fin[,depen_cons:= prix_unit*qtty_cons_kg] ## Dépenses de consommations 
cereales_fin[,summary(depen_cons)] # Quelques paramètres de tendances centrales

# Normalisation des valeurs aberrantes

# Calcul de l'intervalle interquartile
Q1 <- quantile(cereales_fin$depen_cons, 0.25)
Q3 <- quantile(cereales_fin$depen_cons, 0.75)
IQR <- Q3 - Q1

# Définition des limites pour les valeurs aberrantes
upper_bound <- Q3 + 1.5 * IQR
lower_bound <- Q1 - 1.5 * IQR

# Suppression des valeurs aberrantes
cereales_fin <- cereales_fin[cereales_fin$depen_cons >= lower_bound 
                             & cereales_fin$depen_cons <= upper_bound, ]







