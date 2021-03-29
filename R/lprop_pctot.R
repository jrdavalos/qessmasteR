#' @title lprop_pctot
#' @author Julio Ricardo Davalos
#' @description Permet d'obtenir un tableau croisé avec les pourcentages en lignes, mais ajoute une colonne avec le pourcentage total de la modalité.
#'
#' @param x un tableau
#' @param m logique. Si TRUE (valeur par défaut) alors les effectifs sont affichés
#' @param digits nombre de chiffres après la virgule
#'
#' @return un data.frame
#' @importFrom questionr lprop
#'
#' @export
#' @examples
#' lprop_pctot(as.table(rbind(c(1,2,3),c(4,5,6),c(7,8,9))))
lprop_pctot <- function(x, m = TRUE, digits = 1){
  a <- lprop(x, n = TRUE)
  b <- nrow(a)
  c <- ncol(a)
  colnames(a)[c-1] <- "% tot."
  colnames(a)[c] <- "Effectifs"
  for (i in 1:b) {
    a[i,c-1] <- a[i,c]/a[b,c]*100
  }
  a <- as.data.frame.array(round(a,digits))
  a[,ncol(a)] <- round(a[,ncol(a)])
  if (m == TRUE) {
    a <- a[,c(1:(c-2), c, c-1)]
    return(a)
  }
  if (m == FALSE) {
    a <- a[,-c]
    return(a)
  }
}
