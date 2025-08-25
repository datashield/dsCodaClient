#' @title Compute mean composition via ILR mean and inverse transform
#' @description Convenience wrapper that creates ILR coordinates server-side, computes
#' the mean of each ILR coordinate, and returns the inverse ILR (geometric mean composition)
#' on the client. Same as `compositions::mean()` does.
#'
#' @details This function automates the typical sequence:
#' 1) compute ILR coordinates server-side; 2) convert to tibble for column access;
#' 3) compute means of the ILR coordinates ; and
#' 4) invert the ILR means client-side using compositions::ilrInv to obtain the
#' geometric mean composition.
#' @param X character string specifying the name of the server-side dataset, matrix,
#' data.frame, or acomp object containing the compositional data (parts in columns).
#' @param ilr_objectname optional character string for the intermediate ILR object name on the server.
#' If NULL, a temporary name is generated.
#' @param tibble_objectname optional character string for the intermediate tibble name on the server.
#' If NULL, a temporary name is generated.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' If not specified, the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#'
#' @return A named numeric vector containing the estimated mean composition (parts sum to 1).
#' Part names are taken from the input (when available), otherwise generic names are used.
#'
#' @author DataSHIELD Development Team
#' @examples
#' \dontrun{
#'   # Assuming a server-side object D with compositional parts in columns
#'   mean_comp <- dsCodaClient::ds.meanComp("D")
#'   print(mean_comp)
#' }
#'
#' @export

ds.meanComp <- function(
  X,
  datasources = NULL
) {
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  if (is.null(X) || !is.character(X) || length(X) != 1) {
    stop("X must be a single character string naming a server-side object", call. = FALSE)
  }

  ilr_objectname <- "tmp_ilr"
  tibble_objectname <- "tmp_ilr_tbl"
  tibble_og_objectname <- "tmp_og_tbl"

  dsCodaClient::ds.ilr(X = X, objectname = ilr_objectname, datasources = datasources)
  dsTidyverseClient::ds.as_tibble(x = X, newobj = tibble_og_objectname, datasources = datasources)
  dsTidyverseClient::ds.as_tibble(x = ilr_objectname, newobj = tibble_objectname, datasources = datasources)

  coord_names <- dsBaseClient::ds.colnames(tibble_objectname)[[1]]

  ilr_means <- NULL
  for (i in seq_along(coord_names)) {
    col_expr <- paste0(tibble_objectname, "$", coord_names[i])
    mean_obj <- dsBaseClient::ds.mean(col_expr, type = "combine")
    ilr_means[i] <- as.numeric(mean_obj$Global.Mean[1])
  }

  mean_comp <- compositions::ilrInv(ilr_means)
  mean_comp_num <- as.numeric(mean_comp)

  part_names <- dsBaseClient::ds.colnames(tibble_og_objectname)[[1]]

  names(mean_comp_num) <- part_names

  dsBaseClient::ds.rm(c(ilr_objectname, tibble_objectname, tibble_og_objectname), datasources = datasources)

  return(mean_comp_num)
} 
