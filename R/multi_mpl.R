#' multi_mpl
#'
#' @param formula formule de la regression a appliquer
#' @param data base de donnees
#' @param subset optionnel, sous base des observations a utiliser
#' @param weights variable de ponderation
#' @param na.action ce qui se passe en cas de presence de NA. Voir la documentation de lm
#' @param method methode utilisee, "qr" est la valeur par defaut, voir la documentation de lm
#' @param model logique. Retourne les arguments si TRUE
#' @param x logique. Retourne les arguments si TRUE
#' @param y logique. Retourne les arguments si TRUE
#' @param qr logique. Retourne les arguments si TRUE
#' @param singular.ok logique. voir lm
#' @param offset arguments additionnels (voir lm)
#' @param ... variable de ponderation ou sous ensemble
#'
#' @return une liste de modele de probabilite lineaire dont on change a chaque fois la modalite de reference
#' @export
#'
#' @importFrom stats na.omit, as.formula, lm, update.formula
#'
#' @examples
#' data(Music)
#' multi_mpl(Age ~ Rap + Rock + Jazz + Jazz:Rock, Music)

multi_mpl <- function(formula, data, subset = NULL, weights = NULL, na.action = na.omit, method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, offset = NULL,...) {
  # on prend les noms des arguments en ...
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
  # on stocke les differentes categories de la variable y
  mod_y <- levels(unlist(c(data[deparse(formula[[2]])])))
  # on cree une liste pour stocker les regressions
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
    reg[[i]][["call"]] <- str2lang(paste0('lm(formula=',deparse(args$formula),
                                          ',data =',deparse(substitute(data)),')'))
  }
  reg
}
