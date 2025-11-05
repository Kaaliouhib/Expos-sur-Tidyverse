# Expos-sur-Tidyverse


rm(list=ls())
setwd("C:/Users/SOW20/OneDrive/Desktop/Prochain_cours")
setwd("C:/Users/HP/Desktop/STATA_16/Prochain_cours")
library(readxl)
library(tidyr)
library(dplyr)
 donnee_grp=read_excel("donnee_grp.xlsx")
 View(donnee_grp)
 
 # Fonction tidyr
 
 # Pivot_wider() C'est une fonction du package de Tidyr de Tidyverse dans R qui permet de transformer  une table longue en une table large
 ?pivot_wider
 # Exemple pivot_wider()
 ?t.test
 # Base dans R
 fish_encounters
 
 fish_encounters %>%
   pivot_wider(names_from = station, values_from = seen)
 
 fish_encounters %>%
   pivot_wider(names_from = station, values_from = seen, values_fill = 0)
 
  Base_donne_grp=pivot_wider(donnee_grp,names_from = "region", values_from ="penses_sante" )

 View(Base_donne_grp)
 
 #Pivot_longer()  est l'inverse de Pivot_longer(). Elle est utilisé pour passer d'un format large à un format long
 ?pivot_longer
 #Exemple pivot_longer()
 
 relig_income
 
 relig_income %>%
   pivot_longer(!religion, names_to = "income", values_to = "count")
 
 
 billboard %>%
   pivot_longer(
     cols = starts_with("wk"),
     names_to = "week",
     names_prefix = "wk",
     values_to = "rank",
     values_drop_na = TRUE
   )
 
 Base_donnee_grp1= pivot_longer(donnee_grp, cols = 6:7, names_to = "taille_cm", values_to = "poid_kg")
 View(Base_donnee_grp1)
 Base_donnee_grp100= pivot_longer(donnee_grp, cols = 8:9, names_to = "fumeur", values_to = "sport")
 View(Base_donnee_grp100)
 
 # Unite() : C'est une fonction qui permet de combiner plusieurs colonnes en une seule, en les fusionnant avec un séparateur. 
 
 
 #Exemple Unite()
 
 setwd("C:/Users/SOW20/OneDrive/Desktop/Prochain_cours")
 
 Base_nvelle=read_excel("Base_Exposé.xlsx")
 View(Base_nvelle)
 
 ?col
 Base_donnee_grp2 = unite(Base_nvelle,Code_departement, Num_departement, col="Identifiant",sep="")
 View(Base_donnee_grp2)

 
 ?separate
 # Fonction separate()
 # Separate() , elle divise une colonne en plusieurs colonnes
 

 
 #Exemple 
 # importation de la novelle base
 
 Base_nvelle1=read_excel("Base_Exposé_1.xlsx")
 Base_prenom_nom = separate(Base_nvelle1,"Nationalité/Teint", sep = "/", into = c("Nationalité", "Teint"))
 View(Base_prenom_nom )
 
 # FOnction fill() qui permet de complèter les valeurs manquantes par propagation
 ?fill
 sales <- tibble::tribble(
   ~quarter, ~year, ~sales,
   "Q1",    2000,    66013,
   "Q2",      NA,    69182,
   "Q3",      NA,    53175,
   "Q4",      NA,    21001,
   "Q1",    2001,    46036,
   "Q2",      NA,    58842,
   "Q3",      NA,    44568,
   "Q4",      NA,    50197,
   "Q1",    2002,    39113,
   "Q2",      NA,    41668,
   "Q3",      NA,    30144,
   "Q4",      NA,    52897,
   "Q1",    2004,    32129,
   "Q2",      NA,    67686,
   "Q3",      NA,    31768,
   "Q4",      NA,    49094
 )
 # `fill()` defaults to replacing missing data from top to bottom
 sales %>% fill(year)
 # Dplyr
 library(dplyr)
         # dplyr est un package R très utilisé pour la manipulation de données. Il fait partie de l'écosystème tidyverse et fournit
 #un ensemble cohérent de fonctions (appelées "verbes") qui permettent de manipuler et résumer des tableaux de données (data frames ou tibbles) de manière claire et efficace.
 
 # Les principaux verbes de Dplyr sont: 
 
 # filter() Filtrer les lignes selon une condition
 # Select() selection des colonnes d'une base de données
 # mutate() , elle sert de l'ajout ou de modification des colonnes
 # rename() C'est pour renommer les colonnes d'un data frame
 # slice() Selectionne des lignes spécifiques par leur position
 # summarise() sert à résumer une ou plusieurs colonnes d’un tableau de données en une seule valeur par groupe (ou globalement si pas de groupe).
 # Elle est souvent utilisée avec group_by(), mais peut aussi être utilisée seule.
 
 # Exemple fonction filter()
 
 setwd("C:/Users/SOW20/OneDrive/Desktop/Prochain_cours")
 Base_nvelle_1=read_excel("Base_Exposé_2.xlsx")
 View(Base_nvelle_1)
 
 # Filtrer les personnes nées après 1997
 
 filter(Base_nvelle_1, as.numeric(année) > 1997)
 
 Base_nvelle_1%>% filter(as.numeric(année) > 1997)
 
 Base_nvelle_1%>% filter(as.numeric(année) > 1997,mois=="5")
 
 # Exemple fonction Select()
 
 Base_nvelle_1%>%
   select(nom,année)
 # Selectionner les variable en fonction de leur nom avec Starts_with.
 
 
 donnees_sep<- data.frame(
   nom = c("Diallo","Mbengue","Ba","Seck","Sy"),
   année = c("2000","1995","1998","2002","1997"),
   mois = c("5","11","1","9","12"),
   jour = c("31","12","20","15","3")) 
 
 View(donnees_sep)
 
donnees_sep%>%
 select(starts_with("n"))

  select(donnees_sep,starts_with("n"))
 donnees_sep%>%
   select(nom,starts_with("j"),starts_with("m"))
 donnees_sep%>%
   select(starts_with("j"),starts_with("m"))
 
 # # Selectionner les variable en fonction de leur nom avec ends_with().
 
 donnees_sep%>%
   select(ends_with("s"),ends_with("e"))        
 
 
 # Combinaison de starts_with et de end_with
 
 donnees_sep%>%
   select(starts_with("a"),ends_with("r"))  
 
 # mutate() , elle sert de l'ajout ou de modification des colonnes
 
 # Exemple avec la fonction mutate()
 
 donnees_sep<- data.frame(
   nom = c("Ndiaye","Mboup","Gadiaga","Mbodj","Diop"),
   année = c("2000","1995","1998","2002","1997"),
   mois = c("5","11","1","9","12"),
   jour = c("31","12","20","15","3")) 
 View(donnees_sep)
 # Ajoutons une colonne "age"(2025)
 
 donnees_sep%>%
   mutate(age=2025-(as.numeric(année)))
 
 # On peut créer aussi une colonne "date_complete" à partir de année/mois/jour
 
 donnees_sep%>%
   mutate(date_complete=paste(année,mois,jour,sep="-"))
 
 donnees_sep%>%
   mutate(date_complete=paste(jour,mois,année,sep="-"))
 # Créons une colonne "majeur" si l'age est supérieur à 18 ans
 
 donnees_sep%>%
   mutate(age=2025-(as.numeric(année)),majeur=age>18)
 

 # Foction Rename elle permet de renommer une ou plusieurs colonnes d'un tableau de données. Contrairement à select(), on écrit le nouveau nom à gauche, et l'ancien nom à droite 

 # Exple avec la base précédente 
 
 donnees_sep<- data.frame(
   nom = c("Ndiaye","Mboup","Gadiaga","Mbodj","Diop"),
   année = c("2000","1995","1998","2002","1997"),
   mois = c("5","11","1","9","12"),
   jour = c("31","12","20","15","3")) 
 
 donnees_sep%>%
   rename(Last_name=nom)
 
 donnees_sep%>%
   rename(Last_name = nom, Year = année, Month = mois, Day = jour)
 
 # La fonction  Slice
 # Contrairement à la fonction filter() (qui selectionne selon des conditions sur les valeurs), slice() selectionne des lignes par numéro (comme en excel)
 
 # Exemple
 
 donnees_sep%>%
   slice(1:3)
 donnees_sep%>%
   slice(1,4)
 
 # Alternatives plus specifiques
 donnees_sep%>%
   slice_head(n=2)
 
 donnees_sep%>%
   slice_tail(n=3)
 
 donnees_sep%>%
   slice_max(année,n=2)
 
 donnees_sep%>%
   slice_min(année,n=2)
 
 # La fonction summayrise 
 
 # summarise() sert à résumer une ou plusieurs colonnes d’un tableau de données en une seule valeur par groupe (ou globalement si pas de groupe).
 # Elle est souvent utilisée avec group_by(), mais peut aussi être utilisée seule.
 # Elle sert de faire la stat desc
 donnees_sep<- data.frame(
   nom = c("Camara","Aidarra","Ka","Ndaw","Sarr"),
   année = c("2000","1995","1998","2002","1997"),
   mois = c("5","11","1","9","12"),
   jour = c("31","12","20","15","3")) 
 
 # on ajoute l'age
 donnees_sep <-donnees_sep%>%
   mutate(age=2025-(as.numeric(année)))
 View(donnees_sep)
 donnees_sep%>%
   summarise(moyenne_age=mean(age))
 
 donnees_sep%>%
   summarise(ecart_type=sd(age))
 
 # Autres possibilités
 
 donnees_sep%>%
   summarise(
     age_moyen = mean(age),
     age_min = min(age),
     age_max = max(age),
     effects=n()
   )
 
 # summarise + group_by()
 
 donnees_sep1<- data.frame(
   nom = c("Camara","Aidarra","Ka","Ndaw","Sarr"),
   sexe= c("F","M","F","F","M"),
   age = c("25","30","27","23","28"),
   année = c("2000","1995","1998","2002","1997"),
   mois = c("5","11","1","9","12"),
   jour = c("31","12","20","15","3")) 
 
 donnees_sep1%>%
   group_by(sexe)%>%
   summarise(
     moyenne_d_age = mean(as.numeric(age)),
     effectif=n()
   )
 
 donnee_grp=read_excel("donnee_grp.xlsx")
 View(donnee_grp)
 
 donnee_grp%>%
   summarise(moyenne_poids=mean(poid_kg))
 
 
 donnee_grp%>%
   summarise_if(is.numeric, mean)
 
 donnee_grp%>%
   summarise_if(is.numeric, sd)
 
 donnee_grp%>%
   summarise_if(is.numeric, median)
 
 
 # La fonction arrange()
 
  # Elle permet de trier les lignes selon une ou plusieurs colonnes, en ordre croissant (par defaut) ou ordre décrossant(avec desc())
 
 donnees_sep1<- data.frame(
   nom = c("Camara","Aidarra","Ka","Ndaw","Sarr"),
   sexe= c("F","M","F","F","M"),
   age = c("25","30","27","23","28"),
   année = c("2000","1995","1998","2002","1997"),
   mois = c("5","11","1","9","12"),
   jour = c("31","12","20","15","3")) 
# Ordre croissant par defaut
 donnees_sep1%>%
   arrange(année)
 
 # Triage de l'age par ordre décroissant
 
 donnees_sep1%>%
   arrange(desc(age))
 
  # Join functions
 # 1. inner_join
 # Importation des bases dans excel
  setwd("C:/Users/HP/Desktop/STATA_16/Prochain_cours")
# inner_join ne garde que les lignes qui ont une correspondance dans les deux tables
 join_1=read_excel("inner_join_fonction.xlsx")
 
 join_2=read_excel("inner_join.xlsx")
 
 inner_join(join_1,join_2, by="id")
 
 # 2. left_join garde tout le tableau de gauche et fait les correspondances
 
 # Creation et importation de deux bases dans excel
 
 setwd("C:/Users/HP/Desktop/STATA_16/Prochain_cours")
 
 
 Base1=read_excel("Base_1.xlsx")
 Base2=read_excel("Base_2.xlsx")
 
 left_join(Base1,Base2,by="id")
 
 # 3. Right_join() garde tout le tableau de droite, plus les correspondances
 
 right_join(Base1,Base2,by="id")
 
 # 4. full_join  elle combine toutes les lignes des deux tables, en gardant tout, qu'il y ait correspondance ou nom
 
 
 
 full_join(Base1,Base2,by="id")
 
 # 5. bind_rows combine plusieurs data frames en empilant les linesn meme si les colonnes ne sont pas strictement identiques.
 
  
 Joueurs_Barça<- data.frame(
   nom= c("Lamine Yamal","Pedri","Dani Olmo","Lewandowski"),
   age= c("18","19","21","36")
 )
 
 Joueurs_Madrid<- data.frame(
   nom= c("Vinicus JR","Thibaut Courtois","Rodrigo","Rudiguer"),
   age= c("25","24","18","29")
 )
 
 bind_rows(Joueurs_Barça,Joueurs_Madrid)
 
 #6. bind_cols elle sert à combiner deux ou plusieurs data frames horizontalement, ligne par ligne
 
 
 Joueurs_Barça_Madrid<- data.frame(
   nom= c("Lamine Yamal","Pedri","Dani Olmo","Lewandowski","Vinicus JR","Thibaut Courtois","Rodrigo","Rudiguer"))
 View( Joueurs_Barça_Madrid)
 
 age_des_joueurs<- data.frame(age= c("18","19","21","36","25","24","18","29")
   
 )
 View(age_des_joueurs)
 bind_cols(Joueurs_Barça_Madrid,age_des_joueurs)
 library(dplyr)

 # FOnction distinct qui permet d'éliminer les doublons
 
 donnees_sep10<- data.frame(
   nom = c("Camara","Aidarra","Ka","Aidarra","Sarr"),
   age = c("25","30","27","30","28"))
 View(donnees_sep10)
 distinct(donnees_sep10)
 
 

 
