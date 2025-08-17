#' @title Creates a server-side log-ratio EM algorithm object for compositional data imputation
#' @description Creates a server-side object using the zCompositions::lrEM() function
#' @details This function creates a server-side log-ratio EM algorithm object that can be used
#' for imputing left-censored data (e.g. values below detection limit, rounded zeros) in 
#' compositional data sets. It wraps the zCompositions::lrEM() function for use in DataSHIELD.
#' 
#' Server function called: \code{lrEMDS}. 
#' 
#' @param X character string specifying the name of the server-side dataset or matrix 
#' containing the compositional data.
#' @param label unique label (numeric or character) used to denote unobserved values in X. 
#' Default is NULL.
#' @param dl numeric vector or matrix of detection limits/thresholds. These must be given on 
#' the same scale as X. Default is NULL.
#' @param rob logical value. FALSE provides maximum-likelihood estimates of model parameters, 
#' TRUE provides robust parameter estimates. Default is FALSE.
#' @param ini.cov initial estimation of either the log-ratio covariance matrix (ML estimation) 
#' or unobserved data (robust estimation). It can be based on either complete observations 
#' ("complete.obs") or multiplicative simple replacement ("multRepl"). Default is "complete.obs".
#' @param frac if ini.cov="multRepl", fraction parameter for initial multiplicative simple 
#' replacement of left-censored data expressed in proportions. Default is 0.65.
#' @param tolerance convergence criterion for the EM algorithm. Default is 0.0001.
#' @param max.iter maximum number of iterations for the EM algorithm. Default is 50.
#' @param rlm.maxit if rob=TRUE, maximum number of iterations for the embedded robust 
#' regression estimation. Default is 150.
#' @param imp.missing if TRUE then unobserved data identified by label are treated as 
#' missing data. Default is FALSE.
#' @param suppress.print suppress printed feedback. Default is FALSE.
#' @param closure closure value used to add a residual part if needed when ini.cov="multRepl" 
#' is used. Default is NULL.
#' @param z.warning threshold for the proportion of modified zeros per part above which 
#' a warning is issued. Default is 0.8.
#' @param z.delete logical value indicating whether rows containing only zeros should be 
#' deleted prior to imputation. Default is TRUE.
#' @param objectname character string specifying the name of the new server-side object 
#' which will store the log-ratio EM imputation result.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified, the default set of connections will be used: 
#' see \code{\link{datashield.connections_default}}.
#' @return \code{ds.lrEM} returns a server-side object containing the imputed compositional 
#' data set for use in compositional data analysis.
#' @author DataSHIELD Development Team
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
#'   # Create a server-side log-ratio EM imputation
#'   # Using default parameters
#'   dsCodaClient::ds.lrEM(X = 'D', 
#'                         label = 0,
#'                         objectname = 'imputed_data',
#'                         datasources = connections)
#'
#'   # Using specific detection limits
#'   dsCodaClient::ds.lrEM(X = 'D', 
#'                         label = 0,
#'                         dl = c(0.1, 0.1, 0.1),
#'                         rob = FALSE,
#'                         tolerance = 0.001,
#'                         objectname = 'imputed_data_custom',
#'                         datasources = connections)
#'
#'   # Using robust estimation
#'   dsCodaClient::ds.lrEM(X = 'D', 
#'                         label = 0,
#'                         dl = c(0.1, 0.1, 0.1),
#'                         rob = TRUE,
#'                         ini.cov = "multRepl",
#'                         frac = 0.5,
#'                         objectname = 'robust_imputed_data',
#'                         datasources = connections)
#'   
#'   # clear the DataSHIELD R sessions and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.lrEM <- function(
  X = NULL,
  label = NULL,
  dl = NULL,
  rob = FALSE,
  ini.cov = c("complete.obs", "multRepl"),
  frac = 0.65,
  tolerance = 0.0001,
  max.iter = 50,
  rlm.maxit = 150,
  imp.missing = FALSE,
  suppress.print = FALSE,
  closure = NULL,
  z.warning = 0.8,
  z.delete = TRUE,
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
    objectname <- "lrEM_object"
    warning("No objectname provided, using default: 'lrEM_object'", call. = FALSE)
  }

  # match ini.cov argument
  ini.cov <- match.arg(ini.cov)

  # call the server side function
  calltext <- call("lrEMDS", X, label, dl, rob, ini.cov, frac, tolerance, 
                   max.iter, rlm.maxit, imp.missing, suppress.print, closure,
                   z.warning, z.delete)

  # call assign function to create the object on the server
  output <- DSI::datashield.assign(conns = datasources, value = calltext, symbol = objectname)

  # return the output
  return(output)

} 