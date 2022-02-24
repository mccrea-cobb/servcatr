#' Query ServCat reference profiles
#'
#' @param references a vector of numeric reference codes for ServCat records
#'
#' @return a list containing the contents of the parsed ServCat ari request and the api response
#'
#' @import httr
#' @import jsonlite
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sc_profiles(87544)
#' sc_profiles(c(87544, 87545))
#' }
sc_profiles <- function(references){

  # Send a request
  comp_url <- "https://ecos.fws.gov/ServCatServices/servcat/v4/rest/Profile"
  refs <- paste0(references, collapse = ", ")
  query <- list(q = refs)
  res <- httr::GET(comp_url, query = query)

  # Stop if the request doesn't return a json
  if (httr::http_type(res) != "application/json") {
    stop("ServCat API did not return json", call. = FALSE)
  }

  # Parse the response
  parsed <- jsonlite::fromJSON(rawToChar(res$content))

  # Stop message if request fails
  if (httr::http_error(res)) {
    stop(
      sprintf(
        "ServCat API request failed [%s]\n%s",
        status_code(res),
        parsed$message
      ),
      call. = FALSE
    )
  }

  # Return an S3 object
  structure(
    list(
      content = parsed,
      #path = path,
      response = res
    ),
    class = "servcat_api"
  )
}

