#' @include generation.R
NULL
#' Afficher une grille Slitherlink
#'
#' Cette fonction permet de visualiser une grille Slitherlink en affichant :
#' \itemize{
#'   \item la grille de base,
#'   \item la boucle (solution),
#'   \item les indices (nombres de segments autour de chaque case).
#' }
#'
#' La grille est représentée dans un repère cartésien avec des segments
#' reliant les points du réseau.
#'
#' @param obj Liste contenant :
#' \describe{
#'   \item{n}{Entier. Taille de la grille (\eqn{n \times n}).}
#'   \item{segments}{Liste de segments définissant la boucle. Chaque segment
#'   est un vecteur \code{(x1, y1, x2, y2)}.}
#'   \item{indices}{Matrice \code{n x n} contenant les indices du puzzle.}
#' }
#'
#' @return Aucun retour. La fonction produit uniquement un graphique.
#'
#' @details
#' La fonction utilise les fonctions graphiques de base de R :
#' \code{plot()}, \code{segments()} et \code{text()}.
#'
#' Les coordonnées sont affichées avec une origine en bas à gauche.
#'
#' @examples
#' \dontrun{
#' boucle <- generer_boucle_slitherlink(5)
#' slither <- convertir_en_indices(boucle)
#' afficher_slitherlink(slither)
#' }
#'
#' @import graphics
#' @export
afficher_slitherlink <- function(obj) {

  n <- obj$n
  segments <- obj$segments
  indices <- obj$indices

  # fenêtre graphique
  plot(NULL,
       xlim = c(1, n+1),
       ylim = c(1, n+1),
       asp = 1,
       xlab = "",
       ylab = "",
       axes = FALSE)

  # dessiner la grille
  for (i in 1:(n+1)) {
    segments(1, i, n+1, i, col="grey80")  # horizontales
    segments(i, 1, i, n+1, col="grey80")  # verticales
  }

  # dessiner la boucle
  for (seg in segments) {
    segments(seg[1], seg[2], seg[3], seg[4], lwd=3, col="blue")
  }

  # afficher les indices dans les cases
  for (i in 1:n) {
    for (j in 1:n) {
      text(j + 0.5, i + 0.5, labels = indices[i,j], cex = 1.5)
    }
  }
}
