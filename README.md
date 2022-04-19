# qessmasteR
Quelques fonctions simplificatrices pour étudiants, chercheurs et ingénieurs en sciences sociales (QESS étant le Master Quantifier en sciences sociales de l'ENS et de l'EHESS). 

On y trouve pour l'instant :
  - multi_mp() qui sert à réaliser des modèles de probabilité linéaire avec des variables expliquées à plus de deux catégories.
  - stat_ajust() qui sert à obtenir les indicateurs d'ajustement des modèles log-linéaires. En effet, ces derniers étant construits sur des tableaux de contingence, les effectifs ne sont pas correctement pris en compte par les fonctions dédiées.
  - lprop_pctot() qui sert à générer un tableau avec pourcentages en lignes mais indiquant les effectifs et le pourcentage sur le total de chaque ligne. Prend en compte les pondérations.
  - multi_croise() qui sert à obtenir un tableau descriptif croisant une variable avec une liste d'autres variables. Cela est utile pour décrire des clusters par exemple
  - multi_quanti() qui est comme multi_croise mais avec une variable quantitative en fonction de variables catégorielles, on peut obtenir un ensemble de statistiques synthétiques de la variable quantitative en fonction des catégories des variables catégorielles

En prévision : 
  - des fonctions de mise en forme de résultats (pour ACM et tableaux), adaptable en html et LaTeX

L'idée est de faciliter la vie dans les traitements de données, c'est aussi pourquoi la documentation est en français. Toute suggestion est la bienvenue (-> mon adresse mail : davalosjulioricardo@gmail.com) ! Par ailleurs, comme le package est tout récent, il risque de beaucoup évoluer. Je vous conseille de le réinstaller de temps en temps ou de venir sur cette page pour suivre l'arrivée de nouvelles fonctions.

Pour l'installation :
```r
# installation de devtools si pas déjà installé
install.packages("devtools")

# Installation du package
devtools::install_github("jrdavalos/qessmasteR")
```
