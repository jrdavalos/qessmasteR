#' @title  multi_mpl
#' @author Julio Ricardo Davalos
#'
#' @description  Permet de réaliser une liste de modèles de probabilité linéaire. Cela a un intérêt dans le cas où la variable expliquée comporte plus de deux catégories. On a ainsi un modèle pour chaque modalité qui sera placée en référence. Cela permet d'obtenir des résultats aisément interprétables que les logit polytomiques sans passer par la traduction présentée par Jérôme Deauvieau dans "Comparer les résultats d'un modèle logit dichotomique ou polytomique entre plusieurs groupes à partir des probabilité́s estimées", Bulletin de Méthodologie sociologique, 2019.
#'
#' @param formula formule de la régression a appliquer, y doit être en factor ou character et non numérique
#' @param data base de données
#' @param subset optionnel, sous base des observations a utiliser
#' @param weights variable de pondération
#' @param na.action ce qui se passe en cas de présence de NA. Voir la documentation de lm
#' @param method méthode utilisée, "qr" est la valeur par défaut, voir la documentation de lm
#' @param model logique. Retourne les arguments si TRUE
#' @param x logique. Retourne les arguments si TRUE
#' @param y logique. Retourne les arguments si TRUE
#' @param qr logique. Retourne les arguments si TRUE
#' @param singular.ok logique. voir lm
#' @param offset arguments additionnels (voir lm)
#' @param ... variable de pondération ou sous ensemble
#'
#' @return une liste de modèle de probabilité linéaire dont on change a chaque fois la modalité de réference
#'
#' @importFrom stats na.omit as.formula lm update.formula
#'
#' @example
#' data <- data.frame(replicate(10,sample(0:3,1000,rep=TRUE)))
#' data$X1 <- as.factor(data$X1)
#' multi_mpl(X1 ~ X2 + X3 + X6 + X9:X10, data)

multi_mpl <- function(formula, data, subset = NULL, weights = NULL, na.action = na.omit, method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, offset = NULL,...) {
  # on prend les noms des arguments en ... s'il y en a
  if(length(list(...) != 0)) {
    argNames <- sapply(substitute(list(...))[-1L], deparse)
    # on regarde s'ils correspondent aux variables
    m <- match(names(data), argNames, 0L)
    # on ne garde que ceux qui n'y correspondent pas
    args <- list(eval(parse(text <- argNames[-m])))
    # on les nomme
    names(args) <- names(argNames[-m])
    # on stocke les valeurs completes dans args
    args[names(argNames)[m]] <- unlist(apply(data[, as.logical(m), drop <- FALSE],
                                          2, list), recursive <- FALSE)
  }
  else {
    args <- list()
  }
  # on met les arguments habituels de lm dans args
  args$data <- data
  args$weights <- weights
  args$subset <- subset
  args$na.action <- na.action
  args$method <- method
  args$model <- model
  args$x <- x
  args$y <- y
  args$qr <- qr
  args$singular.ok <- singular.ok
  args$offset <- offset
  # on récupère y
  y <- unlist(c(data[deparse(formula[[2]])]))
  if(!(is.character(y) | is.factor(y))) {
    stop(gettextf("y doit etre au format factor ou character"))
  }
  # on stocke les différentes catégories de la variable y
  mod_y <- levels(factor(unlist(c(data[deparse(formula[[2]])]))))
  # on cree une liste pour stocker les régressions
  reg <- list()
  # on cree une boucle pour faire une regression par categorie de y (en modalite de reference)
  for (i in mod_y) {
    # la formule va alors être (y == mod_ref) ~ x, (y == mod_ref) etant une valeur logique
    # elle renvoie 1 quand c'est vrai, on a bien mod_ref la modalite de reference
    # la formule est stockee dans args et change donc pour chaque regression
    args$formula <- update.formula(formula,as.formula(paste0(formula[[2]],"=='",i,"'~ .")))
    # on peut appliquer la fonction lm
    reg[[i]] <- do.call(lm, args)
    # on change le 'call' sinon la regression ne sera pas reconnue par summary() ou stargazer()
    reg[[i]][["call"]] <- str2lang(paste0('lm(formula=',paste0(deparse(args$formula), collapse = ""),
                                          ',data =',deparse(substitute(data)),')'))
  }
  reg
}
