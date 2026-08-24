# Creates a server-side Aitchison composition object for compositional data analysis

Creates a server-side Aitchison composition object using the
compositions::acomp() function

## Usage

``` r
ds.acomp(
  X = NULL,
  parts = NULL,
  total = 1,
  warn.na = FALSE,
  detectionlimit = NULL,
  BDL = NULL,
  MAR = NULL,
  MNAR = NULL,
  SZ = NULL,
  objectname = NULL,
  datasources = NULL
)
```

## Arguments

- X:

  character string specifying the name of the server-side dataset or
  vector containing the compositional data.

- parts:

  vector containing the indices or names of the columns to be used. If
  NULL (default), all columns will be used.

- total:

  numeric value specifying the total amount to be used, typically 1
  or 100. Default is 1.

- warn.na:

  logical value indicating whether the user should be warned in case of
  NA, NaN or 0 coding different types of missing values. Default is
  FALSE.

- detectionlimit:

  a number, vector or matrix of positive numbers giving the detection
  limit of all values, all columns or each value, respectively. Default
  is NULL.

- BDL:

  the code for 'Below Detection Limit' in X. Default is NULL.

- MAR:

  the code for 'Missing At Random' in X. Default is NULL.

- MNAR:

  the code for 'Missing Not At Random' in X. Default is NULL.

- SZ:

  the code for 'Structural Zero' in X. Default is NULL.

- objectname:

  character string specifying the name of the new server-side object
  which will store the Aitchison composition object.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified, the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.acomp` returns a server-side Aitchison composition object for use in
compositional data analysis.

## Details

This function creates a server-side Aitchison composition object that
can be used for compositional data analysis. It wraps the
compositions::acomp() function for use in DataSHIELD.

Server function called: `acompDS`.

## Author

DataSHIELD Development Team

## Examples

``` r
if (FALSE) { # \dontrun{

  # connecting to the DataSHIELD servers

  require('DSI')
  require('DSOpal')
  require('dsBaseClient')
  require('dsCodaClient')

  builder <- DSI::newDSLoginBuilder()
  builder$append(server = "study1", 
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "COMPOSITIONAL.DATA1", driver = "OpalDriver")
  builder$append(server = "study2", 
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "COMPOSITIONAL.DATA2", driver = "OpalDriver")
  logindata <- builder$build()
  
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
  
  # Create a server-side Aitchison composition object
  # Using all columns of the dataset
  dsCodaClient::ds.acomp(X = 'D', 
                         objectname = 'comp_object',
                         datasources = connections)

  # Using specific columns
  dsCodaClient::ds.acomp(X = 'D', 
                         parts = c(1, 2, 3),
                         total = 100,
                         objectname = 'comp_object_subset',
                         datasources = connections)

  # Using column names
  dsCodaClient::ds.acomp(X = 'D', 
                         parts = c('component1', 'component2', 'component3'),
                         objectname = 'comp_object_named',
                         datasources = connections)
  
  # clear the DataSHIELD R sessions and logout
  datashield.logout(connections)
} # }
```
