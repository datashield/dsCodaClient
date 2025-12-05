#' Build sequential ILR basis matrix (sequential binary partition)
#'
#' Construct an orthonormal contrast matrix `V` for ILR coordinates using the
#' sequential binary partition (SBP): at step `j` compare part `j` against
#' the remaining parts `j+1..D`.
#'
#' @param D An integer giving the number of parts in the composition (>= 2).
#' @param part_names Optional character vector of part names to use as row names.
#' @return A numeric `D x (D - 1)` matrix whose columns form an orthonormal basis
#'   for ILR coordinates under the sequential SBP. Row names are taken from 
#'   `part_names` when provided; column names are `ilr1`, ..., `ilr(D-1)`.
#' @details The resulting `V` can be supplied to `compositions::ilr(x, V = V)` to
#'   perform the isometric log-ratio (ILR) transformation based on a sequential
#'   binary partition. Column `j` contrasts part `j` with the remaining parts `j+1..D`,
#'   with appropriate normalizing constants to ensure orthonormality.
#' @export
#' @examples
#' # Using number of parts only
#' V3 <- build_sequential_ilr_V(3)
#' 
#' # Using number of parts with custom names
#' part_names <- c('vpa', 'mpa', 'lipa', 'sleep', 'sb')
#' V5 <- build_sequential_ilr_V(5, part_names)
#' 
#' # Use with compositions::ilr
#' # ilr_vals <- compositions::ilr(X, V = V5)
#' 
build_sequential_ilr_V <- function(D, part_names = NULL) {
  # Accept number of parts as primary argument
  if (length(D) != 1 || !is.numeric(D)) {
    stop("'D' must be a single integer specifying the number of parts")
  }
  D <- as.integer(D)
  if (is.na(D) || D < 2) stop("Number of parts D must be >= 2")
  
  V <- matrix(0, nrow = D, ncol = D - 1)
  for (j in seq_len(D - 1)) {
    positive_idx <- j
    negative_idx <- (j + 1):D
    m <- length(positive_idx)     # size of positive group
    n <- length(negative_idx)     # size of negative group
    V[positive_idx, j] <- sqrt(n / (m * (m + n)))
    V[negative_idx, j] <- -sqrt(m / (n * (m + n)))
  }
  rownames(V) <- part_names
  colnames(V) <- paste0("ilr", seq_len(D - 1))
  V
}