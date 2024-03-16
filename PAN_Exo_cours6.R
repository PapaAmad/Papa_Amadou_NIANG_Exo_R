

# Importing libraries
library("tidyverse")
library("haven")
library("questionr")

# Importer la base de données

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

## Suppression des ménages qui ne consomment pas de céréales

attach(cereales)
anyNA(Qtty_cons)

## Création d'une variable temporaire
cereales$t<-ifelse(is.na(Qtty_cons)==1,1,0) 
table(cereales$t)

cereales_na<-cereales[cereales$t==1,]

cereales<-cereales[cereales$t==0,]

# Supression de la variable temporaire

cereales$t<- NULL

## Renommer, créer, labeliser, recoder les variables

# On cherche les variables qui sont encodées
names(cereales)

Varlist <- list()
for (i in names(cereales)){
   if(is.null(attr(cereales[[i]], "labels"))== 0){ 
     Varlist <- c(Varlist, i)
   }
}

Varlist 


c_val <- names(attr(cereales[["Unite_achat"]], "labels"))
names(c_val) <- attr(cereales[["Unite_achat"]], "labels")
c_val

# Crétion des variables recodées
noms <- list()
for (i in to_rec ){
  
  
  # Inversion de label et d'étiquette : Liste où on a 100 = kg et non kg = 100__________
  c_val <- names(attr(cereales[[i]], "labels"))
  names(c_val) <- attr(cereales[[i]], "labels")

  
  nom <- paste0("Var_rec_", i) # nom_variable
  
  
  # Creation variable
  cereales <- mutate(cereales, 
                     A = unname(c_val[as.character(cereales[[i]])]))
  
  
  attr(cereales$A,"labels") <- c_val # codage...
  attr(cereales$A, "label") <- paste0(attr(cereales$Unite_achat, "label"),"_recoded") # label
  noms[[nom]] <- cereales$A # Ajout à la liste
  }

# noms est une liste de vecteurs, nos nouvelles variables
noms

# On supprime A
cereales$A <- NULL

# Transformation en dataframe puis merging with cereales
noms <- as.data.frame(noms)
View(cer)
cereales <- cbind.data.frame(cereales, noms)




## Découpage en classe 

# On crée un histogramme avec la règle de Sturges pour trouver le nombre de classes

H <- hist(cereales$Value_achat, breaks="Sturges")
length(H$breaks) - 1 # 15 classes

# Découpons en classes pour le Kg, le riz importé brisé et les deux.

rice <- cereales$Unite_cons[cereales$cereales__id == 3]
Kgr <- cereales$Unite_cons[cereales$Unite_cons == 100]
rice_Kgr <- cereales$Unite_cons[cereales$cereales__id == 3 & cereales$Unite_cons == 100]

rice <- cut(rice, breaks = length(H$breaks) - 1)
Kgr <- cut(Kgr, breaks = length(H$breaks) - 1)
rice_Kgr <- cut(rice_Kgr, breaks = length(H$breaks) - 1)

freq(rice)
freq(Kgr)
freq(rice_Kgr)


# Repérer les valeurs manquantes

val_maq <- which(is.na(cereales), arr.ind = TRUE)
dim(val_maq)
val_maq

## Détecter les individus aberrants, we use slice_min et slice_max

# Pour ceux qui ont acheté / consommé  du riz en kg...
#         Pour l'achat
View(slice_min(cereales[cereales$Unite_achat == 100 & cereales$cereales__id == 3,], Qtty_achat, n= 7)) #  7 valeurs aberrantes du haut
slice_min(cereales[cereales$Unite_achat == 100 & cereales$cereales__id == 3,], Qtty_achat , n= 7) #  7 valeurs aberrantes du bas

#         Pour la  consommation
slice_max(cereales[cereales$Unite_cons==100 & cereales$cereales__id == 3],cereales$Qtty_cons, n=7) 
slice_min(cereales[cereales$Unite_cons==100 & cereales$cereales__id == 3],cereales$Qtty_cons, n=7) 

# Merger la table de conversion
table_conv <- read_xlsx("Table de conversion phase 2.xlsx", sheet = "nationale")
names(table_conv)
names(cereales)
 cereales <- left_join(cereales, table_conv, by= c("cereales__id"= "produitID", "Unite_cons"="uniteID", "Taille_cons"="tailleID"))

View(cereales)





