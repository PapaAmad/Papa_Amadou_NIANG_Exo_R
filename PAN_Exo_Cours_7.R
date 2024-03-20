## Rev-QCM

## Suite MD
source("cours5.R")

# Renomer, cr´eer, labeliser les variables,  Recoder ;
glimpse(cereales)
cereales$t<-NULL

labprod <- c("Riz local brisé"	,"Riz local entier"	,"Riz importé brisé"
             ,"Riz importé entier"	,"Riz importé 3"	,"Maïs en épi","Maïs en grain"	,"Mil"	,"Sorgho"	,"Blé"	,"Fonio"	,"Autres céréales"	,"Farine de maïs"	,"semoule de mais"	,"Farine/semoule de mil"	,"semoule de mil"	,"Farine de blé local ou importé"	,"semoule de blé "	,"Autres farines de céréales"	,"Autres semoules de céréales"	,"Pâtes alimentaires"	,"Pain moderne"	,"Pain moderne type 2"	,"Pain traditionnel"	,"Pains traditionnel type 2"	,"Céréales de petit déjeuner"	
             ,"Croissants"	,"Biscuits"	,"Gâteaux"	,"Beignets, galettes")
levprod <- unique(cereales$cereales__id)
edit(levprod)
levprodN <- names(attr(cereales$cereales__id,"labels"))
levprodL <- unname(attr(cereales$cereales__id,"labels"))

cereales$produit1 <- as.factor(cereales$cereales__id)
glimpse(cereales)
table(cereales$produit1)
cereales$produit <- factor(cereales$cereales__id, 
                           levels = levprodL,
                           labels = levprodN )
table(cereales$produit)

glimpse(cereales)
edit(cereales$Unite_cons)
cereales$unite_cons <- factor(cereales$Unite_cons,
                     levels = unname(attr(cereales$Unite_cons,
                                                 "labels")),
                     labels =names(attr(cereales$Unite_cons,
                                       "labels")))
cereales$taille_cons <- factor(cereales$Taille_cons,
                              levels = unname(attr(cereales$Taille_cons,
                                                   "labels")),
                              labels =names(attr(cereales$Taille_cons,
                                                 "labels")))
# 5 changer de type ;
# 6 d´ecoupage en classe ;identifier une cereale et une unite standard;


cereales$classCereal <- cut(cereales$Qtty_cons, 
                            labels = c("Tres faible",
                                       "Faible",
                                       "Moyen",
                                       "Eleve"),
                            breaks = c(0,50,70,110,168))

table(cereales$classCereal)
cereales$classCereal_RizKg <- ifelse(cereales$cereales__id==1 & cereales$Unite_cons==100, 
                                     cut(cereales$Qtty_cons, 
                                  labels = c("Tres faible",
                                             "Faible",
                                             "Moyen",
                                             "Eleve"),
                                  breaks = c(0,50,70,110,168)),NA)
table(cereales$classCereal_RizKg)

c0 <- unique(cereales[cereales$Unite_cons==100,"Taille_cons"])
c1 <- cereales[cereales$cereales__id<5 & cereales$unite_cons==100, ]
## essayer de merger la base cereale avec la table de conversion 

library(readxl)
Table_de_conversion_phase_2 <- read_excel(
  "D:/Traitement Statistique avec R/Table de conversion phase 2.xlsx")

Table_de_conversion_phase_2$...8 <- NULL
Table_de_conversion_phase_2$...9 <- NULL
View(Table_de_conversion_phase_2)

colnames(Table_de_conversion_phase_2)[1:6] <- c("cereales__id","Nom_Prod",
                                                "Unite_cons","Nom_Unite",
                                                "Taille_cons","Nom_Taille")

merge <- merge(cereales, Table_de_conversion_phase_2, 
               by = c("cereales__id", "Unite_cons", "Taille_cons"), all.x = TRUE)


#$ STOP $# 