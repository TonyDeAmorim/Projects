# Partie 1 : Fonctions
# Question 1 
# 1.a : fonction minimum

fonction_min <- function(vecteur_étudié){                                # création de la fonction
  longeur_du_vecteur <- length(vecteur_étudié)                           # on définit une variable égale à la longeur du vecteur étudié
  if (longeur_du_vecteur == 0){                                          # on vérifie que le vecteur étudié comporte au moins une valeur
    stop("le vecteur est vide, merci d'en utiliser un autre")            # message d'erreur si ce n'est pas le cas
  }
  valeur_minimale <- vecteur_étudié[1]                                   # on initialise notre valeur minimale avec la première valeur du vecteur étudié
  for (i in 2:longeur_du_vecteur){                                       # on parcourt le vecteur étudié sur toutes ses valeurs
    if (vecteur_étudié[i] < valeur_minimale){                            # on vérifie pour chaque valeur si elle est inférieur à la valeur minimale actuelle
      valeur_minimale <- vecteur_étudié[i]                               # si c'est le cas on la substitue à la valeur minimale actuelle
    }
  } 
  return(valeur_minimale)                                                # la fonction renvoit la valeur minimale
} 

# 1.b : fonction maximum

fonction_max <- function(vecteur_étudié){                                # création de la fonction
  longeur_du_vecteur <- length(vecteur_étudié)                           # on définit une variable égale à la longeur du vecteur étudié
  if (longeur_du_vecteur == 0){                                          # on vérifie que le vecteur étudié comporte au moins une valeur
    stop("le vecteur est vide, merci d'en utiliser un autre")            # message d'erreur si ce n'est pas le cas
  }
  valeur_maximale <- vecteur_étudié[1]                                   # on initialise notre valeur maximale avec la première valeur du vecteur étudié
  for (i in 2:longeur_du_vecteur){                                       # on parcourt le vecteur étudié sur toutes ses valeurs
    if (vecteur_étudié[i] > valeur_maximale){                            # on vérifie pour chaque valeur si elle est supérieur à la valeur maximale actuelle
      valeur_maximale <- vecteur_étudié[i]                               # si c'est le cas on la substitue à la valeur maximale actuelle
    }
  } 
  return(valeur_maximale)                                                # la fonction renvoit la valeur maximale
} 

# Question 2 

statistiques_descriptives_du_vecteur <- function(vecteur_étudié){        # on crée la fonction qui prend en valeur le vecteur étudié
  longeur_du_vecteur <- length(vecteur_étudié)                           # on définit une variable égale à la longeur du vecteur étudié
  résumé_statistique <- data.frame(                                      # on crée le data frame
    "minimum" = fonction_min(vecteur_étudié),                            # on utilise la fonction de la question 1 pour trouver le minimum
    "premier_quartile" = quantile(vecteur_étudié, 0.25),                 # on utilise la fonction quantile pour obtenir le premier quantile
    "mediane" = quantile(vecteur_étudié, 0.5),                           # on utilise la fonction quantile pour obtenir la médiane
    "moyenne" = sum(vecteur_étudié)/longeur_du_vecteur,                  # on divise la somme des valeurs du vecteurs par le nombre de valeur pour trouver la moyenne
    "troisieme_quartile" = quantile(vecteur_étudié, 0.75),               # on utilise la fonction quantile pour obtenir le premier quantile
    "maximum" = fonction_max(vecteur_étudié)                             # on utilise la fonction de la question 1 pour trouver le maximum
  )
  rownames(résumé_statistique) <- c("vecteur étudié")                    # on renome la ligne du dataframe
  return(résumé_statistique)                                             # on renvoit le dataframe crée
}

# Question 3                                                             

tri_du_vecteur <- function(vecteur_étudié){                              # on crée la fonction
  longeur_du_vecteur <- length(vecteur_étudié)                           # on définit une variable égale à la longeur du vecteur étudié
  vecteur_trié <- sort(vecteur_étudié, decreasing = TRUE)                # on trie les valeurs du vecteur dans l'ordre décroissant
  matrice_triée <- matrix(c(1:longeur_du_vecteur, vecteur_trié), ncol=2) # on crée une matrice à deux colones avec le classement et les valeurs
  return(matrice_triée)                                                  # renvoit la matrice crée 
}

# Question 4

pondération <- function(matrice_étudiée){
  longeur_matrice <- nrow(matrice_étudiée)                           # on définit une variable égale à la longeur du vecteur étudié
  nombre_de_variable <- ncol(matrice_étudiée)
  liste_indicateur_pondéré <- c()
  for (n in 1:nombre_de_variable){
    valeur_pondérée <- 0                                               # on définit une variable qui prendra la valeur pondérée de chaque donnée
    indicateur_pondéré <- 0                                            # on définit une variable qui aggrège les valeurs pondérées
    for (i in 1:longeur_matrice){                                      # on parcourt la matrice dans la colone des valeurs qu'on étudie 
      valeur_pondérée <- matrice_étudiée[i,n]/longeur_matrice          # on pondère chaque valeur une par une
      indicateur_pondéré <- indicateur_pondéré + valeur_pondérée       # on aggrège les valeurs pondérées
    }
    liste_indicateur_pondéré <- c(liste_indicateur_pondéré, indicateur_pondéré) # on ajoute l'indice calculé à la liste
  }
  return(liste_indicateur_pondéré)                                         # on renvoit l'indicateur 
}


# Question 5

aggregation_additive <- function(vecteur_poids, matrice_de_variables){      # on crée la fonction
  longeur_matrice <- nrow(matrice_de_variables)                             # on crée un indicateur égale au nombre d'observations
  nombre_de_variables <- ncol(matrice_de_variables)                         # on crée un indicateur égale au nombre de variables
  if (length(vecteur_poids) != nombre_de_variables) {                       # on vérifie si les dimensions sont égales
    stop("Les dimensions des paramètres doivent être égales")               # si ce n'est pas lecas le programme s'arrête
  }
  aggrégation_additive <- c()                                 # on crée notre vecteur d'indice
  for (n in 1:nombre_de_variables){                            # on effectue le calcul pour chaque variable
    valeur_pondérée <- 0                                      # on définit une variable qui prendra la valeur pondérée de chaque donné                                             
    indicateur_pondéré <- 0                                   # on définit une variable qui aggrège les valeurs pondérées
    for (i in 1:longeur_matrice){                             # on parcourt la matrice pour la variable étudiée
      valeur_pondérée <- vecteur_poids[n]*matrice_de_variables[i,n]      # on pondère chaque valeur une par une
      indicateur_pondéré <- indicateur_pondéré + valeur_pondérée         # on aggrège les valeurs pondérées
    }
    aggrégation_additive <- c(aggrégation_additive, indicateur_pondéré)  # on ajoute l'indice calculé à la liste
  }
  return(aggrégation_additive)
}


# Question 6

aggregation_geometrique <- function(vecteur_poids, matrice_de_variables){           # on crée la fonction
  longeur_matrice <- nrow(matrice_de_variables)                                     # on crée un indicateur égale au nombre d'observations
  nombre_de_variables <- ncol(matrice_de_variables)                                 # on crée un indicateur égale au nombre de variables
  if (length(vecteur_poids) != nombre_de_variables) {                               # on vérifie si les dimensions sont égales
    stop("Les dimensions des paramètres doivent être égales")                       # si ce n'est pas lecas le programme s'arrête
  }
  aggregation_geometrique <- c()                                                    # on crée notre vecteur d'indice
  for (n in 1:nombre_de_variables){                                                  # on effectue le calcul pour chaque variable
    valeur_pondérée <- 0                                                            # on définit une variable qui prendra la valeur pondérée de chaque donné                                             
    indicateur_pondéré <- 1                                                         # on définit une variable qui aggrège les valeurs pondérées
    for (i in 1:longeur_matrice){                                                   # on parcourt la matrice pour la variable étudiée
      valeur_pondérée <- matrice_de_variables[i,n]**vecteur_poids[n]                # on pondère chaque valeur une par une
      indicateur_pondéré <- indicateur_pondéré * valeur_pondérée                    # on aggrège les valeurs pondérées
    }
    aggregation_geometrique <- c(aggregation_geometrique, indicateur_pondéré)       # on ajoute l'indice calculé à la liste
  }
  return(aggregation_geometrique)
}  
 
# Question 7

select_dim <- function(matrice_étudiée){
  library(FactoMineR)                                                           # on appelle les packages que l'on va utiliser
  library(factoextra)
  resultat_du_PCA <- PCA(matrice_étudiée, scale.unit=TRUE, graph=FALSE)         # on récupère les résultats de l'ACP sur le dataframe étudié
  eigenvalue_du_PCA <- get_eigenvalue(resultat_du_PCA)                          # on récupère la matrice des valeurs propres
  compteur_dim <- 0                                                             # on initialise le compteur des valeurs propres supérieure à 1
  for (i in 1:nrow(eigenvalue_du_PCA)){                                         # on parcourt la matrice des valeurs prorpes
    if (eigenvalue_du_PCA[i,1]>1){                                              # on regarde quelles valeurs propres sont supérieures à 1                    
      compteur_dim <- compteur_dim + 1                                          # on compte le nombre de dimensions concernées
    }
  }
  resultat_du_PCA_réduit <- PCA(matrice_étudiée, scale.unit=TRUE, graph=FALSE,  # on crée un nouveau dataframe qui ne garde que les dimensions où les valeurs propres sont supérieures à 1
                                ncp=compteur_dim)
  return(resultat_du_PCA_réduit)
}
res <- select_dim(matrices_normalisées[["IA"]])
cos2_PCA <- round(as.matrix(res$var$cos2), 2)               # on récupère la matrice des variabilités dans chaque dimension chaque variable
vecteur_somme_des_cos2 <- c()                                           # on crée le vecteur qui récupère la somme des cos2 pour chaque variable
nombre_de_variables <- ncol(cos2_PCA)                                   # on crée un indicateur égale au nombre de variables
for (n in 1:nombre_de_variables){                                       # on parcourt cos2 pour chaque variable
  somme_des_cos2 <- sum(cos2_PCA[,n])                                   # on fait la somme de tous les cos2 de la variable étudiée
  vecteur_somme_des_cos2 <- c(vecteur_somme_des_cos2, somme_des_cos2)   # on ajoute la valeur calculée au vecteur crée
}
print(vecteur_somme_des_cos2)
longeur_matrice <- nrow(cos2_PCA)                                       # on crée un indicateur égale au nombre d'observations
print(longeur_matrice)
matrice_contribution_maximale <- NULL                                   # on crée la matrice des contributions maximales, pour l'instant vide
fonction
for (i in 1:longeur_matrice){                                           # on parcourt chaque ligne de la matrice
  max_cos2 <- fonction_max(cos2_PCA[i,])                                # on trouve la contribution maximale
  position_max_cos2 <- which(cos2_PCA[i,] == max_cos2)                  # on trouve la variable associée à la contribution maximale
  max_cos2_pondéré <- round(((max_cos2*max_cos2)/vecteur_somme_des_cos2[position_max_cos2]), 2) # on pondère la valeur : son carré est divisé par la somme des coefficients de la variable calculé précédement
  nouveau_vecteur_contributions <- rep(0, nombre_de_variables)          # on crée un vecteurs de 0 de longeur égale au nombre de variable
  nouveau_vecteur_contributions[position_max_cos2] <- max_cos2_pondéré         # on remet la contribution maximale à sa place 
  matrice_contribution_maximale <- rbind(matrice_contribution_maximale, nouveau_vecteur_contributions) # on ajoute le vecteur à la matrice finale
  rownames(matrice_contribution_maximale)[nrow(matrice_contribution_maximale)] <- rownames(cos2_PCA)[i] # on renome la ligne par le nom de la variable concernée
}

rotation_des_facteurs <- function(dataframe_étudié){                      # on crée la fonction qui prend en argument le data frame étudié
  library(FactoMineR)                                                     # on appelle les packages que l'on va utiliser
  library(factoextra)
  resultat_du_PCA_réduit <- select_dim(dataframe_étudié)                        # on crée un nouveau dataframe qui ne garde que les dimensions où les valeurs propres sont supérieures à 1
  cos2_PCA <- round(as.matrix(resultat_du_PCA_réduit$var$cos2), 2)               # on récupère la matrice des variabilités dans chaque dimension chaque variable
  print(cos2_PCA)
  vecteur_somme_des_cos2 <- c()                                           # on crée le vecteur qui récupère la somme des cos2 pour chaque variable
  nombre_de_variables <- ncol(cos2_PCA)                                   # on crée un indicateur égale au nombre de variables
  for (n in 1:nombre_de_variables){                                       # on parcourt cos2 pour chaque variable
    somme_des_cos2 <- sum(cos2_PCA[,n])                                   # on fait la somme de tous les cos2 de la variable étudiée
    vecteur_somme_des_cos2 <- c(vecteur_somme_des_cos2, somme_des_cos2)   # on ajoute la valeur calculée au vecteur crée
  }
  longeur_matrice <- nrow(cos2_PCA)                                       # on crée un indicateur égale au nombre d'observations
  matrice_contribution_maximale <- NULL                                   # on crée la matrice des contributions maximales, pour l'instant vide
  for (i in 1:longeur_matrice){                                           # on parcourt chaque ligne de la matrice
    max_cos2 <- fonction_max(cos2_PCA[i,])                                # on trouve la contribution maximale
    position_max_cos2 <- which(cos2_PCA[i,] == max_cos2)                  # on trouve la variable associée à la contribution maximale
    max_cos2_pondéré <- round(((max_cos2*max_cos2)/vecteur_somme_des_cos2[position_max_cos2]), 2) # on pondère la valeur : son carré est divisé par la somme des coefficients de la variable calculé précédement
    nouveau_vecteur_contributions <- rep(0, nombre_de_variables)          # on crée un vecteurs de 0 de longeur égale au nombre de variable
    nouveau_vecteur_contributions[position_max_cos2] <- max_cos2_pondéré         # on remet la contribution maximale à sa place 
    matrice_contribution_maximale <- rbind(matrice_contribution_maximale, nouveau_vecteur_contributions) # on ajoute le vecteur à la matrice finale
    rownames(matrice_contribution_maximale)[nrow(matrice_contribution_maximale)] <- rownames(cos2_PCA)[i] # on renome la ligne par le nom de la variable concernée
  }
  return(matrice_contribution_maximale)
}





# Partie 2 : Récupération et retraitement des données
# Question 1

setwd("C:/Users/tonyd/OneDrive/Dauphine L3/Application R pour éco/Projet final")         # on s'assure que le répertoire de travail est correct
data_set_annuel <- read.csv("data_base.csv", sep=";", header = TRUE)                   # on importe le fichier csv dans un dataframe en séparant les données à chaque ";"

# Question 2

nombre_de_lignes <- nrow(data_set_annuel)                   # on récupère le nombre de lignes du dataframe 
nombre_de_pays <- 1                                         # on initialise le compteur de pays à 1 
liste_des_pays <- c(data_set_annuel$Country[1])             # on crée la liste des pays en y ajoutant le premier de la liste
nombre_annees <- c()                                        # on crée le vecteur qui collecte le nombre d'années par pays
compteur_annees <- 1 
for (i in 2:nombre_de_lignes){                              # on parcourt chaque ligne 
  verificateur <- FALSE                                     # on initialise un booléen qui sert à  vérifier si le pays est déjà dans la liste
  pays_en_cours <- data_set_annuel$Country[i]               # on attribue le pays de la ligne en cours au vecteur pays_en_cours                                     # on initialise pour ce pays le compteur d'années à 1
  for (j in 1:length(liste_des_pays)){                      # on parcourt la liste de pays 
    if (liste_des_pays[j]==pays_en_cours){                  # on vérifie si le pays y est déjà
      verificateur <- TRUE                                  # si c'est le cas on passe le booléen en TRUE
    }                                                       
  }
  if (verificateur == FALSE){                               
    liste_des_pays <- c(liste_des_pays, pays_en_cours)      # si le pays n'est pas dans la liste, on l'y ajoute
    nombre_de_pays <- nombre_de_pays + 1                    # on incrémente le compteur de pays
  }
  if (data_set_annuel$Country[i]==data_set_annuel$Country[i-1]){ # on vérifie si le pays en cours est le même que celui de la ligne précédente
    compteur_annees <- compteur_annees + 1                       # si ce le cas on incrémente le compteur d'années
  }else{
    nombre_annees <- c(nombre_annees, compteur_annees)           # si ce n'est pas le cas on peut ajouter le compteur pour le pays en cours à la liste
    compteur_annees <- 1                                         # on réinitialise le compteur pour le pays suivant
  }
}

matrice_pays_annees <- cbind(liste_des_pays, nombre_annees)     # on combine les deux listes en une matrice
print(matrice_pays_annees)                                      # on l'affiche 
cat("Il y a", nombre_de_pays, "pays\n")                         # on présente le nombre de pays

# Question 3 

nombre_de_colones <- ncol(data_set_annuel)                                              # on récupère le nombre de variables
tableau_stat_descriptives <- data.frame()                                               # on crée un dataframe vide
for (i in 6:nombre_de_colones){                                                         # on parcourt chaque colone des indices 
  stats_de_la_variable <- statistiques_descriptives_du_vecteur(data_set_annuel_complet[,i])     # pour chaque indice on récupère ses statistiques descriptives
  rownames(stats_de_la_variable) <- colnames(data_set_annuel)[i]                        # on renome les lignes par le nom de l'indice
  tableau_stat_descriptives <- rbind(tableau_stat_descriptives, stats_de_la_variable)   # on ajoute les stats de l'indice au dataframe final
}

# Question 4

library(Hmisc)                                                                               # on importe le package nécessaire à l'extrapolation
na_lignes <- which(is.na(data_set_annuel$ID1))                                               # on récupère le numéro de lignes des NA
NA1 <- na_lignes[1]                                                                          # on crée deux variables correspondantes au n° des lignes des NA
NA2 <- na_lignes[2]
dates_existantes <- c(data_set_annuel$Year[37:(NA1-1)],data_set_annuel$Year[(NA1+1):(NA2-1)]) # on crée un vecteur avec uniquement les années où les valeurs sont disponibles
valeurs_existantes <- c(data_set_annuel$ID1[37:(NA1-1)],data_set_annuel$ID1[(NA1+1):(NA2-1)]) # on crée un vecteur avec uniquement les ID1 où les valeurs sont disponibles
Database_Chili <- data.frame(dates_existantes,valeurs_existantes)                             # on fusionne les vecteurs en un dataframe
date_complete <- seq(2004,2014,1)                                                             # on définit la séquence à interpoler
ID1_Chili_interpolé <- approx(dates_existantes, valeurs_existantes, xout= date_complete, method="linear", ties="ordered") # on interpole 2012
date_extra <- seq(2004, 2015, 1)                                                              # on définit la séquence à extrapoler
ID1_Chili_complet <- approxExtrap(date_complete, ID1_Chili_interpolé$y, xout= date_extra, method="linear", ties="ordered") # on extrapole 2015
data_set_annuel_complet <- data_set_annuel
data_set_annuel_complet$ID1[37:48] <- ID1_Chili_complet$y                                             # on remplace dans notre base de données par les valeurs calculées



# Question 5  
 
noms_indicateurs <- substr(names(data_set_annuel_complet)[6:24], 1, 2)          # On extrait les préfixes des noms de colonnes
indicateurs <- unique(noms_indicateurs)                                         # On crée une liste avec les noms des indicateurs
matrices_complètes <- list()                                                    # On crée une liste de matrices

for (prefix in indicateurs) {
  colone_étudiée <- names(data_set_annuel_complet)[6:24][noms_indicateurs == prefix]   # On sélectionne les colonnes correspondant au nom de l'indicateure
  matrices_complètes[[prefix]] <- data_set_annuel_complet[, colone_étudiée, drop = FALSE]  # On stock la matrice crée dans la liste
}

# Question 6

IQR <- function(indice_étudié){                                                             # on crée une fonction qui sert à calculer l'écart interquartile d'un indice et de compter le nombre de valeurs en dehors
  Q1 <- tableau_stat_descriptives[indice_étudié,2]                                          # on trouve le premier quartile
  Q3 <- tableau_stat_descriptives[indice_étudié,5]                                          # on trouve le troisième quartile
  Intervalle <- Q3 - Q1                                                                     # on calcule l'écart interquartile
  Limite_inférieure <- Q1 - 1.5 * Intervalle                                                # on calcule la borne inférieure
  Limite_supérieure <- Q3 + 1.5 * Intervalle                                                # on calcule la borne supérieure
  nombre_lignes <- nrow(data_set_annuel_complet)                                                    # nombre de lignes
  compteur_val_aberante <- 0                                                                # on crée un compteur de valeur aberantes
  for (i in 1:nombre_lignes){                                                               # on parcourt toutes les valeurs de l'indice
    if (data_set_annuel_complet[i,indice_étudié+5] < Limite_inférieure | data_set_annuel_complet[i,indice_étudié+5] > Limite_supérieure){ # on vériifie pour chaque valeur si elle est dans l'intervalle
      compteur_val_aberante <- compteur_val_aberante + 1                                    # on incrémente si c'est le cas
    }
  }
  return(compteur_val_aberante)                                                             # la fonction renvoit le compteur
}

nombre_de_lignes <- nrow(tableau_stat_descriptives)                                         # nombre d'indices
tableau_val_aberrantes <- data.frame(Indice = character(0), Nombre_Valeurs_Aberrantes = integer(0)) # création d'un dataframe à deux colones
for (i in 1:nombre_de_lignes){                                                              # on parcourt tous les indices
  nouveau_resultat <- data.frame(Indice = rownames(tableau_stat_descriptives)[i],           # on obtient le compteur pour l'indice étudié
                                 Nombre_Valeurs_Aberrantes = IQR(i))
  tableau_val_aberrantes <- rbind(tableau_val_aberrantes, nouveau_resultat)                 # on l'ajoute au dataframe
}

# Question 7

data_set_annuel_normalisé <- data_set_annuel_complet                             # on crée un nouveau dataframe pour nos donées normalisées
nombre_indice <- nrow(tableau_stat_descriptives)                                 # nombre d'indices
for (j in 1:nombre_indice){                                                      # on étudie chaque indice
  min <- tableau_stat_descriptives[j,1]                                          # on trouve le min
  max <- tableau_stat_descriptives[j,6]                                          # on trouve le max
  diff_extremes <- max - min                                                     # on calcule l'étendue
  nombre_lignes <- nrow(data_set_annuel)                                         # nombre de lignes                                                                
  for (i in 1:nombre_lignes){                                                    # on parcourt chaque valeur
    valeur_étudiée <- data_set_annuel_normalisé[i,j+5]                           # on attribue quelle valeur est actuellement étudiée
    valeur_normalisée <- (valeur_étudiée - min)/diff_extremes                    # on calcule à chaque fois la valeur normalisée par la méthode min max
    data_set_annuel_normalisé[i,j+5] <- valeur_normalisée                        # on remplace dans le nouveau dataframe
  }
}

noms_indicateurs <- substr(names(data_set_annuel_normalisé)[6:24], 1, 2)        # On extrait les préfixes des noms de colonnes
indicateurs <- unique(noms_indicateurs)                                         # On crée une liste avec les noms des indicateurs
matrices_normalisées <- list()                                                  # On crée une liste de matrices

for (prefix in indicateurs) {
  colone_étudiée <- names(data_set_annuel_normalisé)[6:24][noms_indicateurs == prefix]   # On sélectionne les colonnes correspondant au nom de l'indicateure
  matrices_normalisées[[prefix]] <- data_set_annuel_normalisé[, colone_étudiée, drop = FALSE]  # On stock la matrice crée dans la liste
}

# Question 8
data_set_annuel_standardisé <- data_set_annuel_complet                             #on crée un nouveau dataframe pour nos donées standardisée
nombre_indice <- nrow(tableau_stat_descriptives)                                   # nombre d'indices
for (j in 1:nombre_indice){                                                        # on étudie chaque indice
  moyenne <- tableau_stat_descriptives[j,4]                                        # on trouve la moyenne
  variance <- sum((data_set_annuel_standardisé[,j+5]-moyenne)**2)/(length(data_set_annuel_standardisé[,j+5])-1) # on calcule la variance de l'indice                                       # on trouve le max
  ecart_type <- sqrt(variance)                                                     # on calcule l'écart type de l'indice
  nombre_lignes <- nrow(data_set_annuel)                                           # nombre de lignes                                                                
  for (i in 1:nombre_lignes){                                                      # on parcourt chaque valeur
    valeur_étudiée <- data_set_annuel_standardisé[i,j+5]                           # on attribue quelle valeur est actuellement étudiée
    valeur_standardisée <- (valeur_étudiée - moyenne)/ecart_type                   # on calcule à chaque fois la valeur standardisée
    data_set_annuel_standardisé[i,j+5] <- valeur_standardisée                      # on remplace dans le nouveau dataframe
  }
}

noms_indicateurs <- substr(names(data_set_annuel_standardisé)[6:24], 1, 2)      # On extrait les préfixes des noms de colonnes
indicateurs <- unique(noms_indicateurs)                                         # On crée une liste avec les noms des indicateurs
matrices_standardisées <- list()                                                # On crée une liste de matrices

for (prefix in indicateurs) {
  colone_étudiée <- names(data_set_annuel_standardisé)[6:24][noms_indicateurs == prefix]   # On sélectionne les colonnes correspondant au nom de l'indicateure
  matrices_standardisées[[prefix]] <- data_set_annuel_standardisé[, colone_étudiée, drop = FALSE]  # On stock la matrice crée dans la liste
}


# Partie 3 
# Question 3

matrices_correlation <- list()                                                  # On crée une liste pour stocker les matrices de corrélation
for (i in names(matrices_normalisées)) {                                        # On parcourt chaque indicateurs
  matrice_étudiée <- matrices_normalisées[[i]]                                  # On définit la matrice actuellement étudiée
  if (ncol(matrice_étudiée)>1){                                                 # On vérifié si il y a plus d'une variable dans la matrice
    matrice_de_corr <- cor(matrice_étudiée, use = "complete.obs", method = "pearson")  # Calcul de la matrice de corrélation
    matrices_correlation[[i]] <- matrice_de_corr                                # On ajoute la matrice de corrélation à la liste
  }
}

# Question 5
library(FactoMineR)
library(factoextra)
library(corrplot)

for (i in names(matrices_normalisées)) {                                        # On parcourt chaque indicateurs
  matrice_étudiée <- matrices_normalisées[[i]]                                  # On définit la matrice actuellement étudiée
  resultat_du_PCA <- PCA(matrice_étudiée, scale.unit=TRUE, graph=FALSE)
  eigenvalues <- get_eigenvalue(resultat_du_PCA)
  fviz_eig(resultat_du_PCA, addlabels = TRUE, ylim = c(0, 100))
  ggtitle(paste("Graphique des valeurs propres -", i))
  PCA_var <- get_pca_var(resultat_du_PCA)
  corrplot(PCA_var$coord, is.corr = FALSE, title = paste("Corrélogramme des variables -", i))
  }                                       

res <- rotation_des_facteurs(matrices_normalisées[["IA"]])


matrice_étudiée <- matrices_normalisées[["IA"]]                                  # On définit la matrice actuellement étudiée
res2 <- rotation_des_facteurs(matrice_étudiée)

eigenvalues <- get_eigenvalue(resultat_du_PCA)
fviz_eig(resultat_du_PCA, addlabels = TRUE, ylim = c(0, 100))
PCA_var <- get_pca_var(resultat_du_PCA)
corrplot(PCA_var$coord, is.corr = FALSE)







