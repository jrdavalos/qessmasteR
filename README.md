# qessmasteR
Quelques fonctions simplificatrices pour étudiants en sciences sociales

On y trouve pour l'instant :
  - multi_mp() qui sert à réaliser des modèles de probabilité linéaire avec des variables expliquées à plus de deux catégories.
  - stat_ajust() qui sert à obtenir les indicateurs d'ajustement des modèles. Elle a un intérêt notamment concernant les modèles log-linéaires sur tableau de contingence pour lesquels la fonction glm n'arrive pas à produire d'AIC ou de BIC.

En prévision : 
  - des fonctions de mise en forme de résultats (pour ACM et tableaux)
  - une fonction de calcul d'ajustements des modèles
Et peut-être (si cela n'est pas possible ailleurs) :
  - une nouvelle version de la fonction translate.logit() (package GDAtools) qui ne fonctionne plus

L'idée est de faciliter la vie des étudiants dans leurs traitements de données, toute suggestion est la bienvenue ! Par ailleurs, comme le package est (vraiment) tout nouveau, il risque de beaucoup évoluer. Je vous conseille de le réinstaller de temps en temps ou de venir sur cette page pour suivre l'arrivée de nouvelles fonctions.

Pour l'installation :
```r
# installation de devtools si pas déjà installé
install.packages("devtools")

# Installation du package
devtools::install_github("jrdavalos/qessmasteR")
```
