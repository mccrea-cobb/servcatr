#' Query ServCat using the public REST Api
#'
#' @param references a vector of numeric reference codes for ServCat records
#'
#' @return a list containing the contents of the parsed ServCat ari request and the api response
#' @export
#'
#' @examples
#' dontrun{
#' servcat_api(87544)
#' servcat_api(c(87544, 87545))
#' }
servcat_api <- function(references = c(87544, 87545)){
  library(httr)
  library(jsonlite)

  ### Send a request
  ## Reference Profiles
  #https://ecos.fws.gov/ServCatServices/servcat/v4/rest/Profile?q=87544%2C%2087545

    comp_url <- "https://ecos.fws.gov/ServCatServices/servcat/v4/rest/Profile"
    refs <- paste0(references, collapse = ", ")
    query <- list(q = refs)
    res <- GET(comp_url, query = query)

  # if(quick_search) {
  #   ## Quick search
  #   comp_url <- "https://ecos.fws.gov/ServCatServices/servcat/v4/rest/ReferenceCodeSearch/Composite"
  #   query <- list(q = 87544)
  #   res <- GET(comp_url, query = query)
  # }

  # Stop if the request doesn't return a json
  if (http_type(res) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }


  ### Parse the response
  # Translate json to usable data frame
  parsed <- fromJSON(rawToChar(res$content))

  # Add error message if request fails
  if (http_error(res)) {
    stop(
      sprintf(
        "ServCat API request failed [%s]\n%s",
        status_code(res),
        parsed$message
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      #path = path,
      response = res
    ),
    class = "servcat_api"
  )

}

