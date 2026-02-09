#' Install Lightcast API Credentials in Your `.Renviron` File for Repeated Use
#' 
#' @description This function will add Lightcast API credentials to your `.Renviron` 
#' file so that they can be used to authenticate requests without being hardcoded. 
#' THESE API CREDENTIALS SHOULD NOT BE SHARED OUTSIDE THE ORGANIZATION.
#' @details The client_id is stored using the environmental variable `LIGHTCAST_API_ID` and the 
#' client_secret is stored using the variable LIGHTCAST_API_SECRET. These can be accessed
#' at any time by using `Sys.getenv("LIGHTCAST_API_ID")` or by using
#' `Sys.getenv(LIGHTCAST_API_SECRET)`. To access and return both as a character vector,
#' use `Sys.getenv(c("LIGHTCAST_API_ID", "LIGHTCAST_API_SECRET"))` (see examples).
#' @param id string, Lightcast API client_id.
#' @param secret string, Lightcast API client_secret.
#' @examples
#' 
#' \dontrun{
#' set_api_creds(id = "orgname", secret = "abcd")
#' # Check that credentials have been added with:
#' Sys.getenv(c("LIGHTCAST_API_ID", "LIGHTCAST_API_SECRET"))
#' }
#' @export

set_api_creds <- function(id, secret) {

  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")

  if(file.exists(renv)){
    # Backup original .Renviron.
    file.copy(renv, file.path(home, ".Renviron_backup"))
    message("Backup of .Renviron saved to home directory as .Renviron_backup.\n")
    }
  
  if(!file.exists(renv)){
    file.create(renv)
  }
  
  vars_concat <- paste0(
    "LIGHTCAST_API_ID='", 
    id, 
    "'\nLIGHTCAST_API_SECRET='",
    secret,
    "'\n"
  )
  write(vars_concat, renv, sep = "\n", append = TRUE)
  message(
      'Your API credentials have been stored in your .Renviron and can be ', 
      'accessed by Sys.getenv(c("LIGHTCAST_API_ID", "LIGHTCAST_API_SECRET)).',
      '\n\nTo use now, restart R or run readRenviron("~/.Renviron")'
  )

}


#' Authorize Access to Lightcast API
#' 
#' @description This function uses stored API credentials to create a client
#' containing the token necessary to authorize any requests to the Lightcast
#' API.
#' 
#' @details NOTE: ensure that your Lightcast API credentials have been
#' added to your `.Renviron` by using the built-in `set_api_creds`
#' function prior to using this function and making requests.
#' 
#' In order to use the Lightcast API, a `Bearer` token must 
#' accompany every request in order to provide authentication. This function
#' requests a token via an OAuth 2.0 Client Credentials flow using API 
#' credentials previously stored in `.Renviron` as `LIGHTCAST_API_ID`
#' and `LIGHTCAST_API_SECRET` and returns a `client` object 
#' generated using `httr2::oauth_client`. This `client` object, which 
#' contains the `Bearer` token, is attached to each API request via
#' `httr2::req_oath_client_credentials()`. This process benefits from the
#' token management features provided by the `httr2` package,
#' including caching and automatic token refresh. 
#' 
#' Since this function caches the token in memory, it will not persist across
#' sessions - this function will need to be run at the start of each session 
#' to request a fresh token. Each token also expires after 1 hour, however
#' the automatic refresh behavior means that you will not need to run this 
#' function multiple times per session (just once at the start).
#' 
#' For more information about Lightcast's authentication flow, see:
#' https://docs.lightcast.io/lightcast-api/docs/authentication-guide.
#' For more information about using `httr2` to manage authorization
#' using OAuth, see https://httr2.r-lib.org/articles/oauth.html.
#' @return A client object containing the token required for authenticating an
#' API request.
#' @examples
#' # Ensure API credentials have been saved to .Renviron before running
#' \dontrun{
#' authenticate_api()
#' }
#' # Simply running the function will retrieve the token and save it to an
#' # environment that persists until you end the R session.
#' 
#' @export

authenticate_api <- function() {

  client_id <- Sys.getenv("LIGHTCAST_API_ID")
  client_secret <- Sys.getenv("LIGHTCAST_API_SECRET")

  if (client_id=="" | client_secret=="") {
    stop("'LIGHTCAST_API_ID' and 'LIGHTCAST_API_SECRET' must be set in ",
         ".Renviron using set_api_creds()", call. = FALSE
    )
  } 

  try( 
    { the$client <- httr2::oauth_client(
      id = client_id,
      secret = client_secret,
      token_url = "https://auth.emsicloud.com/connect/token",
      auth = "body",
      name = "lightcast-postings"
    )
    
    message("OAuth client succesfully created.")
    }
  )
}