% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_croise.R
\name{multi_croise}
\alias{multi_croise}
\title{multi_croise}
\usage{
multi_croise(
  data,
  var_princ,
  ...,
  NR = FALSE,
  pct_ligne = TRUE,
  nb = 1,
  p.val = FALSE,
  cram.v = FALSE,
  sign = 0.05,
  tot = c("row", "col"),
  eff = TRUE,
  pourcent = FALSE,
  pond = NULL,
  norm_pond = TRUE
)
}
\arguments{
\item{data}{base de données}

\item{var_princ}{variable principale (en colonnes)}

\item{...}{variables à croiser avec la variable principale (en lignes)}

\item{NR}{FALSE par défaut. Compte les non-réponses.}

\item{pct_ligne}{TRUE par défaut. Pourcentages en ligne, sinon en colonne.}

\item{nb}{nombre de décimales pour les pourcentages. 1 par défaut}

\item{p.val}{FALSE par défaut. Si TRUE alors la p-value s'affiche en dernière colonne.}

\item{cram.v}{FALSE par défaut. Si TRUE alors le V de cramer s'affiche en dernière colonne. Afficher le V de Cramer implique d'afficher la p-value}

\item{sign}{0.05 par défaut. Si le test du Khi2 n'est pas significatif au seuil choisi alors pas de calcul du V de Cramer.}

\item{tot}{Affiche ou non les totaux en ligne et/ou en colonne. c("row", "col") par défaut, peut être aussi "row" ou "col" ou NULL.}

\item{eff}{TRUE par défaut. affiche les effectifs par case entre parenthèse.}

\item{pourcent}{TRUE par défaut. Met le signe \% après les pourcentages.}

\item{pond}{variable de pondération (facultatif), doit absolument être écrit sous la forme data$pond}

\item{norm_pond}{TRUE par défaut. Si TRUE alors normalise les poids de la pondération (la somme des poids est alors égale a la somme des effectifs).}
}
\value{
Un tabyl data.frame regroupant tous les tableaux croisés avec pourcentages et effectifs. Si les pourcentages sont en ligne et que les totaux sont activés alors la ligne de total est nommée "Ensemble" et la colonne de total est nommée "Total" et inversement pour les pourcentages en colonne.
}
\description{
Permet d'obtenir un tableau croisant une variable d'intérêt avec une liste d'autres variables.
}
\author{
Julio Ricardo Davalos
}
