# qessmasteR
Quelques fonctions simplificatrices pour étudiants en sciences sociales

Pour l'installation :
```r
# installation de devtools si pas déjà installé
install.packages("devtools")

# Installation du package
devtools::install_github("jrdavalos/qessmasteR")
```

On y trouve pour l'instant :
  - multi_mp() qui sert à réaliser des modèles de probabilité linéaire avec des variables expliquées à plus de deux catégories.
  - stat_ajust() qui sert à obtenir les indicateurs d'ajustement des modèles log-linéaires. En effet, ces derniers étant construits sur des tableaux de contingence, les effectifs ne sont pas correctement pris en compte par les fonctions dédiées (AIC, BIC etc.).
  - lprop_pctot() qui sert à générer un tableau avec pourcentages en lignes mais indiquant les effectifs et le pourcentage sur le total de chaque ligne.

En prévision : 
  - une fonction permettant d'obtenir des statistiques descriptives rapidement
  - des fonctions de mise en forme de résultats pour ACM
  - des fonctions de mise en forme de résultats pour tableaux

L'idée est de faciliter la vie des étudiants dans leurs traitements de données, toute suggestion est la bienvenue ! Par ailleurs, comme le package est (vraiment) tout nouveau, il risque de beaucoup évoluer. Je vous conseille de le réinstaller de temps en temps ou de venir sur cette page pour suivre l'arrivée de nouvelles fonctions.
