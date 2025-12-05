#' @title Creates server-side ILR coordinates for compositional data analysis
#' @description Creates a server-side object containing isometric log-ratio (ILR)
#' coordinates using the compositions::ilr() function.
#' @details This function creates a server-side ILR-transformed object that can be
#' used for compositional data analysis. It wraps the compositions::ilr() function
#' for use in DataSHIELD.
#'
#' Server function called: \code{ilrDS}.
#'
#' @param X character string specifying the name of the server-side dataset, matrix,
#' data.frame, or acomp object containing the compositional data.
#' @param V an orthonormal basis matrix to be used for the ILR transform. If NULL
#' (default), the package default basis is used on the server.
#' @param objectname character string specifying the name of the new server-side object
#' which will store the ILR result.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' If the \code{datasources} argument is not specified, the default set of connections will be used:
#' see \code{\link{datashield.connections_default}}.
#'
#' @return \code{ds.ilr} returns (assigns) a server-side object containing the ILR-transformed
#' coordinates for use in compositional data analysis.
#' @author DataSHIELD Development Team
#'
#' @examples
#' \dontrun{
#'
#'   # connecting to the DataSHIELD servers
#'
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'   require('dsCodaClient')
#'
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1",
#'                  url = "http://192.168.56.100:8080/",
#'                  user = "administrator", password = "datashield_test&",
#'                  table = "COMPOSITIONAL.DATA1", driver = "OpalDriver")
#'   builder$append(server = "study2",
#'                  url = "http://192.168.56.100:8080/",
#'                  user = "administrator", password = "datashield_test&",
#'                  table = "COMPOSITIONAL.DATA2", driver = "OpalDriver")
#'   logindata <- builder$build()
#'
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#'
#'   # Create a server-side ILR object using default basis
#'   dsCodaClient::ds.ilr(X = 'D',
#'                        objectname = 'ilr_object_default',
#'                        datasources = connections)
#'
#'   # Create a server-side ILR object using a custom basis matrix V
#'   # (V must be an orthonormal basis of appropriate dimension)
#'   V <- diag(4) # example only; replace with a valid ILR basis
#'   dsCodaClient::ds.ilr(X = 'D',
#'                        V = V,
#'                        objectname = 'ilr_object_customV',
#'                        datasources = connections)
#'
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.ilr <- function(
  X = NULL,
  V = NULL,
  objectname = NULL,
  datasources = NULL
) {

  # look for DS connections
  # if one not provided then get current
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # verify that 'X' was set
  if (is.null(X)) {
    stop("Please provide a valid compositional dataset parameter (X)", call. = FALSE)
  }

  # handle objectname - use default if not provided
  if (is.null(objectname)) {
    objectname <- "ilr_object"
    warning("No objectname provided, using default: 'ilr_object'", call. = FALSE)
  }

  # call the server side function
  if (is.null(V)) {
    # Call without V matrix
    calltext <- call("ilrDS", X)
  } else {
    # Flatten V matrix and send as separate arguments
    if (!is.matrix(V)) {
      stop("V must be a matrix", call. = FALSE)
    }
    
    # Flatten the matrix by columns (default behavior of as.vector for matrices)
    V_flattened <- as.vector(V)
    V_ncol <- ncol(V)
    
    # Create call with flattened V values and ncol
    calltext <- do.call("call", c(list("ilrDS", X), as.list(V_flattened), list(V_ncol = V_ncol)))
  }

  # call assign function to create the object on the server
  output <- DSI::datashield.assign(conns = datasources, value = calltext, symbol = objectname)

  # return the output
  return(output)

} 