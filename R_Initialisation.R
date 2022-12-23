
# Partie 1 - Initiation au logiciel de traitement de données et analyse statistique R----
  
  ## Les operateurs arithmetiques en R -----------------------------
  
    division_entiere = 5000 %/% 745
    division_entiere
    
    modulo = 5000 %% 745
    modulo
    
    puissance <- 5^6
    puissance
    
    puissance2 = 5**6
    puissance2
    
    notation_scientifique <- 5.375 * 1e3
    notation_scientifique
    
    egalite_conditionnelle = puissance == puissance2 # type LOGICAL
    egalite_conditionnelle
    
    operateur_ordre = puissance >= puissance2
    operateur_ordre
  
  ## Les operateurs logiques ---------------
  
    operateur_intersection1 = TRUE & TRUE
    operateur_intersection1
    
    operateur_intersection2 = TRUE & FALSE
    operateur_intersection2
    
    operateur_intersection3 = FALSE & FALSE
    operateur_intersection3
    
    operateur_union1 = TRUE | TRUE
    operateur_union1
    
    operateur_union2 = TRUE | FALSE
    operateur_union2
    
    operateur_union3 = FALSE | FALSE
    operateur_union3
  
  ## Instructions de controle IF, ELSE IF, ELSE ----
    ### Exemple 1.3 ............................................................................
      
      "
      auteur : Mohamed TOURE
      date : 26 Nov 2022 
      but du programme : relever les differentes pluies et indique a chaque fois si un nouveau
                         record maximal a ete atteint. L'idee sera de faire appel aux 
                         instructions de controle de flux.
      entrée : quantite_pluies_releve (quantite de pluies en mm releve)
      sortie : record_maximal (record maximal en mm de pluies)
      "
  
      record_maximal = 50
      quantite_pluies_releve = as.numeric(readline("Entrez la quantite de pluie releve :"))
      
      if (quantite_pluies_releve > record_maximal) {
        record_maximal = quantite_pluies_releve
        print(paste("Nous avons un nouveau record de", record_maximal, "mm de pluies"))
      
        }else if(quantite_pluies_releve == 0) {
        print("Pas de pluies")
      
          }else {
            print("Pas de nouveaux record")
            print(paste("Maximum retenu :", record_maximal))
          }
  
  ## Les instructions de repetitions FOR, WHILE ----
    ### Exemple 1.4 ............................................................................
  
      "
      auteur : Mohamed TOURE
      date : 01 Dec 2022 
      but du programme : programme qui saisit une valeur et retourne la somme des n premieres 
      valeurs
      entrée : n (valeur rentrée)
      sortie : res (resultat de la somme)
      "
  
      ma_sequence = seq(from = n, to = 1, by = -1.3)
      ma_sequence
      
      n = as.integer(readline("Entrer le nombre n : "))
      res = 0
      
      for (i in seq(n)){
          res = res + i
      }
      print(res)
  
  
    ### Exemple 1.5 ......................................................................
      "
      auteur : Mohamed TOURE
      date : 01 Dec 2022 
      but du programme : programme qui donne le nombre de pliages necessaires d'un tapis 
      pour entrer dans une porte d'hauteur definit
      entrée : epaisseur (epaisseur du tapis), 
               hauteur_porte_embarquement (hauteur de la porte d'embarquement),
      sortie : nombre_pliages (nombre de pliages necessaires)
      "
      
      epaisseur <- 3 # or 3e-2
      hauteur_porte_embarquement <- 500
      nombre_pliages <- 0
      
      
      while (epaisseur < hauteur_porte_embarquement) {
        epaisseur <- 2 * epaisseur
        nombre_pliages <- nombre_pliages + 1
        print(nombre_pliages)
      }
      
    print(paste(nombre_pliages, "pliages"))

  ## Les Fonctions dans R ------------------------------------------------------------------
    
    # Standardization   
    #                 vector <- [vector - min(vector)] %/% [max(vector) - min(vector)]
    # Standardization (Centrer_reduire)  
    #                 X_stand <- [x - mean(x)] / sd(x)    with    sd(x) == ecart type de x
    
    std_centrer_reduire <- function(x, y = 0){
      
      moyenne <- mean(x)
      ecart_type <- sd(x)
      y = (x - moyenne) %/% ecart_type
      
      return (y)
    }
    
    x1 = c(4,5,1,2,3)
    y1 = std_centrer_reduire(x1)
    
    
    # Dans une fonction on peut mettre 2 arguments et fixer l'une. Au moment d'appeler la
    # fonction, on ne renseignera que l'argument non fixe.
    
      "
      auteur : Mohamed TOURE
      date : 01 Dec 2022 
      but du programme : fonction qui calcul le factoriel
      entrée : n (nombre), 
      sortie : resultat (factoriel du nombre n)
      "
      
      factoriel_nombre <- function(n, resultat = 1){
        for (i in seq(n)){
          if (n == 0){
            return (1)
          }else{
            resultat = resultat * i
          }
        }
        return (resultat)
      }
      
      n = as.numeric(readline("Entrez le nombre : "))
      fact = factoriel_nombre(n)
      cat(paste("Factoriel de", n, "\n", fact))
    
      

      
      
# Partie 2 - Les structure de donnees dans R----
  
  ## Les types de variables dans R----
      
      pays <- "Senegal" # Type CHARACTER
      senegalais <- TRUE # Type LOGICAL
      is.character(pays) # Type LOGICAL   Return TRUE or FALSE
      age <- 5 # Type NUMERIC
      
      ### Exemple 2.4...................................
      
      class(age) # retourne le type de la variable
      typeof(senegalais) # retourne le type de variable
  
  ## Les donnees manquantes----
       "
      NA = Not Available
      NaN = Not an Number : donnee impossible comme 5/0; inf - inf
      "
      ### Exemple 2.5.............
      
      multiplication_NA <- 5*NA
      division_NA <- 5 %/% NA
      addition_NaN <- 5 + NaN
      valeur_inf <- log(0)
      
      is.nan(addition_NaN)
      is.na(multiplication_NA)
      is.infinite(valeur_inf)
      is.nan(2)
      
  ## Création et manipulation des vecteurs----
      ### Exemple 2.6 (vecteur).....................................
      
      # NB : Les elements d'un vecteurs doivent etre homogenes
      vecteur_numeric <- c(12,14,15)
      vecteur_nominal <- c("charles", "jean", "lwc")
      sequence_ou_suite_arithmetique <- 1:10 
      c.bis <- seq(from=65, to=100) # cmd pour suite arithmetique
      sequence_ou_suite_arithmetique2 <- seq(from=10, to=1000, by = 10)
      
      ### Exemple 2.7 (vecteur)...........................
      
      "
      En utilisant la commande 'seq(…)', 
      créer un vecteur de suite croissante appellée
      'suite_croissante' dont les valeurs sont paires 
      et comprises entre 0 et 10 (inclus), puis créez 
      un autre vecteur de suite décroissante à noter 
      'suite_decroissante' dont les valeurs sont
      impaires et comprises entre 0 et 9.
      "
      
      suite_croissante <- seq(from=0, to=10, by=2)
      suite_decroissante <- seq(from=9, to=0, by=-2)
      
      ### Exemple 2.8 (vecteur)..............................................
      
      vecteur <- c(10, 20,30)
      repetition_vecteur <- rep(vecteur, times=3)
      repeter_chaque_composant <- rep(vecteur, each=3)
      repeter_chaque_composant_differemment <- rep(vecteur, time=c(2,3,4))
      
  ## Cas pratique sur les Vecteurs ----
    ### Exemple 2.9
    
      nom_team_trash <- c("Mohamed Toure", "Mouhamed Sarr", "Mouhamed Massaly", "Abdoul Aziz Gaye",
                  "Ahmed Bachir Sarr", "Abdoul Aziz Niang", "Ali Fofana", "Elimane Tine", 
                  "Omar Gaye", "Ibrahima Sambe")
      age <- c(24, 23, 24, 24, 21, 22, 21, 23, 23, 22)
      
      #Variable encodee en binaire (0:F ; 1:M)
      sexe <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1) 
      sexe[c(2,5,10)] <- 0
      
      professions <- c("Instructeur", "Ouvrier", "Maçon", "Tiourayologue", "vendeur de parfum",
                       "Basketteur", "Influenceur", "Chef Tiourayologue", "Sapologue", "Tiaga")
      revenu <- c(50000, 150000, 120000, 80000, 250000, 30000, 300000, 130000, 90000, 10000)
      niveau_domeram <- c(1, 5, 5, 3, 4, 2, 1, 3, 3, 5)

      " Signification des modalités de la variable 'niveau_domeram'
      
      Les modalités de le niveau de domeram de team_trash (Echelle de Likert) :
      
      1 : Saint
      2 : un peu domeram
      3 : domeram
      4 : tres domeram
      5 : tiaz
      "
      
      ### Structure classique des variables .......................
      
        # QUALITATIF NOMINAL == FACTOR 
        # QUALITATIF ORDINAL == Ordered FACTOR
        # QUANTITATIF == NUMERIC
  
        nom_team_trash        # Qualitative Nominale de type 'Character'
        age                   # Quantitative de type 'Numeric'
        sexe                  # Qualitative Nominale de type 'Factor'
        professions           # Qualitative Nominale de type 'Factor'
        revenu                # Quantitative de type 'Numeric'
        niveau_domeram        # Qualitative Ordinale de type 'Ordered Factor'
        
      ### Recodage des classes
        
        "Il est question de recoder ou factoriser des vecteurs ou variables"
        
        class(nom_team_trash)
        class(age)
        class(sexe)           # VARIBALE NUMERIC ET ENCODEE => DOIT ETRE FACTORISER & LABELLISEE 
        class(professions)    # TYPE CHARACTER => DOIT ETRE FACTORISER
        class(revenu)
        class(niveau_domeram) # VARIBALE NUMERIC ET ENCODEE => DOIT ETRE FACTORISER & LABELLISEE
      
      ### Factoriser et Labelliser
        "levels() == modalites => nlevels() == nombres de modalites"
        
        sexe <- factor(sexe, labels=c("Femal", "Male")) # 0:F ; 1:M
        professions <- factor(professions)
        niveau_domeram <- factor(niveau_domeram, 
                                 labels=c("Saint",
                                          "Un peu domeram",
                                          "Domeram",
                                          "Tres Domeram",
                                          "Tiaz"),
                                 ordered = TRUE)
        levels(niveau_domeram)
        nlevels(niveau_domeram)

      
      ### Changer (mettre) un nom de colonne (vecteur)
        
      names(age) <- nom_team_trash
      names(sexe) <- nom_team_trash
      names(professions) <- nom_team_trash
      names(revenu) <- nom_team_trash
      names(niveau_domeram) <- nom_team_trash
      
      
      ### Filtrer les vecteurs
      
      age["Mohamed Toure"]
      niveau_domeram["Ibrahima Sambe"]
      revenu[c("Elimane Tine", "Ahmed Bachir Sarr")]
      mean(revenu)
      revenu[which(revenu >= mean(revenu))]
      mean(age)
      age[which(age <= mean(age))]
      revenu[which(revenu == max(revenu))]
      
      team_trash <- data.frame('Age' = age, "Sexe" = sexe, "Revenu" = revenu, "Profession" = professions, 
      "Niveau_domeram" = niveau_domeram)
      row.names(team_trash)<-(nom_team_trash)

  ## Création et manipulation des matrices----
      # Dim Matrice (4x2) == taille vecteur(8)
      matrix(1:8, nrow = 4, ncol = 2)
      # Remplissage par lignes et non par colonne (byrow)
      matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE)
      # taille vecteur > dim Matrice
      m<-matrix(1:20, nrow = 4, ncol = 3, byrow = TRUE)
      # taille vecteur < dim Matrice
      m2<-matrix(1:20, nrow = 5, ncol = 5, byrow = TRUE)
      
      ### Fonctions de requêtes sur les matrices .......................
      
      M <- matrix(1:20, nrow = 4, ncol = 5, byrow = TRUE)
      dim(M)      
      nrow(M)
      ncol(M)
      t(M)      
      diag(M)  
      M1 <- matrix(c(5,6,-1,8), nrow = 2, ncol = 2, byrow = TRUE)
      det(M1)      
      
      ### Opération sur les matrices ................................................................................
      
      x <- matrix(1:4, nrow = 2)
      y <- matrix(c(7,9,11,13), nrow = 2)
      
      x + y
      x - y
      5 * x
      x * y # Il s'agit d'une multiplication membre a membre et non une multiplication matricielle
      x %*% y # Il s'agit d'une multiplication matricielle ATTENTION : colonne matrice 1 == ligne matrice 2

      ### Indexer une ligne, une colonne et une sous matrice d'une matrice ..........................................      

      x <- seq(10, 200, by = 10)
      length(x)
      
      A <- matrix(x, nrow = 4, ncol = 5)
      
      #### Indexer une ligne ou une colonne
        
      A[1, ] # 1ere ligne
      A[ ,3] # 3e colonne

      #### Supprimer une ligne ou une colonne
        
      A[-4, ] #Supprimer 4e ligne
      A[ ,-5] # Supprimer 5e colonne      
        
      #### Sélectionner un élément ou une sous matrice
        
      A[4,2]
      A[c(1,4), 2] # Récupérer les éléments [1,2] et [4,2]

      ### Fonctions pour nommer les de la matrice ...................................................................
        
      row.names(A) <- c("L1", "L2", "L3", "L4")
      colnames(A) <- c("Col1", "Col2", "Col3", "Col4", "Col5")
      
      ### Fusion de deux matrices ................................................................................... 
        # En statistique on parle de MERGE
      
      M1 <- matrix(1:4, nrow = 2, ncol = 2)
      M2 <- matrix(c(5,6,7,8), nrow = 2, ncol = 2)
      
      #### Fusionner par lignes (VERTICAL)
      rbind(M1, M2)

      #### Fusionner par colonnes (HORIZONTAL)
      cbind(M1, M2)
        
      
    
  ## Cas pratique sur les matrices (DataFrames) ----


# Partie 3 - Les ensembles de donnees dans R
                                                        
  ## Importation d'une base de donnees (Data Frame) dans R
    ### Importer une base propre à R
      data() # Affiche tous les data frames propres a R
      data("mtcars") # Importation du data frame "mtcars"
      ?mtcars # Infos sur le data frame
      str(mtcars) # Structure du data frame (Variables, Type, modalités)
      
    ### Importer une base d'extension particulière
      #installed.packages("haven", dependencies = T)
      library(haven)
      
      ".dta (STATA)"
      data_credi_from_stata <- read_dta("data/credit.dta")
      
      " .sav (SPSS)" 
      data_tabaski_from_spss <- read_sav("data/Tabaski_Novembre_2016.sav")
      
      ".xls (EXCEL)"
      #installed.packages("readxl", dependencies = T)
      library("readxl")
      data_credi_from_excel <- read_excel("data/credit.xls")
      
    ### Importer une base d'extension '.txt'
# .txt (TEXTE)
# .csv (Classic)
  
  "Les fichiers CSV sont des fichiers excels avec comme separateur <,> ou <;>"
  
  # read.csv pour un fichier de séparateur <,>
  car_insurance <- read.csv(file = "data/car_insurance.csv", 
                              header = TRUE, 
                              sep = ',', 
                              dec = '.')
  
  # Application sur la base de donnees cardio_vascular_diseases.csv
  
    # importation du fichier cardio_vascular_diseases.csv avec read.csv2 pour un fichier de séparateur <;>
    cardio_vascular <- read.csv2(file = "data/cardio_vascular_diseases.csv", 
                              header = TRUE, 
                              sep = ';', 
                              dec = '.')
  
    # STRUCTURE
    str(cardio_vascular)
    
    "
    id  < Normal
    age < Normal, en jours >
    gender < 2 : Femme; 1 : Homme > < Need Factor and Label >
    height < Normal, en cm>
    weight < Normal, en kg>
    ap_hi < Normal >
    ap_lo < Normal >
    Cholesterol < 1 : Normal, 2 : Above Normal, 3 : Well above Normal > < Need Factor, Label, order >
    gluc < 1 : Normal, 2 : Above Normal, 3 : Well above Normal > < Need Factor, Label, order >
    smoke ...
    "
    # Selection d'un échantillon aléatoire de 10 000 observations
    
    # ATTENTION : EXECUTER LES 2 LIGNES EN MM TEMPS
    set.seed(123) # Fixer une graine aléatoire
    index <- sample(1:nrow(cardio_vascular), 10000)
    
    # Stockage de l’échantillon
    cardio_vascular_tb1 <- cardio_vascular[index,]
    
    # Pre-traitement des donnees
    
    cardio_vascular_tb1$gender <- factor(cardio_vascular_tb1$gender,
                                         labels = c('Female', 'Male'))
    cardio_vascular_tb1$cholesterol <- factor(cardio_vascular_tb1$cholesterol,
                                            labels = c("Normal", "Above Normal", "Well above Normal"),
                                            ordered = TRUE)
    cardio_vascular_tb1$gluc <- factor(cardio_vascular_tb1$gluc,
                                              labels = c("Normal", "Above Normal", "Well above Normal"),
                                              ordered = TRUE)
    cardio_vascular_tb1$smoke <- factor(cardio_vascular_tb1$smoke,
                                        labels = c("No", "Yes"))
    cardio_vascular_tb1$alco <- factor(cardio_vascular_tb1$alco,
                                        labels = c("No", "Yes"))
    cardio_vascular_tb1$active <- factor(cardio_vascular_tb1$active,
                                        labels = c("No", "Yes"))
    cardio_vascular_tb1$cardio <- factor(cardio_vascular_tb1$cardio,
                                        labels = c("No", "Yes"))

    # On met l'age (en jour) en age par années
    cardio_vascular_tb1$age <- round(cardio_vascular_tb1$age / 365) 
    
    # PARTIE 4 Analyse Statistique des donnees -----
    
    # STATISTIQUE UNIVARIEE
    
    "Varibale univariee : variable continue = distribution
     Variable univariee : variable qualitative = repartition"
    
    # Quelques fonctions de requetes

    # Moyenne (Indice de position)
    mean(cardio_vascular_tb1$age) # On fait un approximation par défaut (53 ans) 
    # Une moyenne peut être représentative ou pas.
    
    # Médiane (Indice de position)
    median(cardio_vascular_tb1$age) # Médiane = 54
    
    # Écart Type (Indice de dispersion)
    sd(cardio_vascular_tb1$age) # ecart type = 6.69
    
    # Les quartiles
    # Avec min et max aux extrémités # Les quartiles sont des quantiles d'ordre i (25% , 50%, 75%)
    quantile(cardio_vascular_tb1$age)
    
    # Répartition des sexes chez les individus (effectif)
    table(cardio_vascular_tb1$gender)

    # Répartition du sexe en % chez les individus (frequences)  
      # Mot clés : Proportion
      
    tab <- table(cardio_vascular_tb1$gender)
    percent <- prop.table(tab) * 100
    
    # Requêtes importantes pour questionner la base
    
    # Combien a t-on de fumeurs
    table(cardio_vascular_tb1$smoke == "Yes")
    
    # Combien y a t il d'individus atteint de maladie cardio
    table(cardio_vascular_tb1$cardio == "Yes")

    
    
    # La statistique descriptive avec Summary
    
    summary(cardio_vascular_tb1)[ ,-1]
    
    # La statisque descriptive avec describe
    #install.packages("prettyR", dependencies = T)
    library(prettyR)
    
    describe(cardio_vascular_tb1[, -1],
             num.desc = c("mean", "sd", "median", "min", "max", "valid.n"))
    
    