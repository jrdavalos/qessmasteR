# qessmasteR
Quelques fonctions simplificatrices pour étudiants, chercheurs et ingénieurs en sciences sociales (QESS étant le Master Quantifier en sciences sociales de l'ENS et de l'EHESS). 

On y trouve pour l'instant, des fonctions génériques pour la production de certains types de tableaux :
  - multi_mp() qui sert à réaliser des modèles de probabilité linéaire avec des variables expliquées à plus de deux catégories.
  - stat_ajust() qui sert à obtenir les indicateurs d'ajustement des modèles log-linéaires. En effet, ces derniers étant construits sur des tableaux de contingence, les effectifs ne sont pas correctement pris en compte par les fonctions dédiées.
  - lprop_pctot() qui sert à générer un tableau avec pourcentages en lignes mais indiquant les effectifs et le pourcentage sur le total de chaque ligne. Prend en compte les pondérations.
  - multi_croise() qui sert à obtenir un tableau descriptif croisant une variable avec une liste d'autres variables. Cela est utile pour décrire des clusters par exemple
  - multi_quanti() qui est comme multi_croise mais avec une variable quantitative en fonction de variables catégorielles, on peut obtenir un ensemble de statistiques synthétiques de la variable quantitative en fonction des catégories des variables catégorielles

De même, il y a des outils servant à la réalisation d'analyses géométriques de données :
  - exclure_agd() qui permet d'obtenir l'index des modalités à exclure dans une AGD
  - tab_quali_agd() qui permet d'obtenir toutes les aides à interprétation des modalités d'une AGD en vue de réaliser un graphique (par exemple avec ggplot à la façon de ce qu'on peut trouver ici : https://quanti.hypotheses.org/1871) ou un tableau qui les restitue une fois quelqes filtres et sélections d'axes opérés
  - separe_quali_afm() qui permet d'obtenir les mêmes données mais pour les analyses séparées d'une AFM

En prévision :
  - d'autres fonctions pour les AGD, notamment concernant les nuages d'individus
  - des fonctions de mise en forme de résultats (tableaux surtout), adaptable en html et LaTeX

L'idée est de faciliter la vie dans les traitements de données, c'est aussi pourquoi la documentation est en français. Toute suggestion est la bienvenue (-> mon adresse mail : julioricardo.davalos@ehess.fr) ! Par ailleurs, comme le package est tout récent, il risque de beaucoup évoluer. Je vous conseille de le réinstaller de temps en temps ou de venir sur cette page pour suivre l'arrivée de nouvelles fonctions.

Pour l'installation :
```r
# installation de devtools si pas déjà installé
install.packages("devtools")

# Installation du package
devtools::install_github("jrdavalos/qessmasteR")
```

Je pense bientôt mettre le package sur CRAN, mais cela demandera encore un peu plus de temps.

