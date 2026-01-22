#' Read a sensor instance TTL record and create a DataCite/Fabrica DOI record
#' @description `r lifecycle::badge("stable")`
#' Reads a **sensor instance** described in an RDF/Turtle (.ttl) file and uses the
#' extracted metadata to create or update a DOI record in **DataCite/Fabrica**
#' via the REST API.
#'
#' The sensor instance is treated as a DataCite **Instrument** resource.
#' By default, the function creates a **draft** DOI record, letting DataCite
#' automatically generate the DOI suffix using the provided prefix.
#'
#' Metadata are mapped from the TTL as follows:
#' \itemize{
#'   \item \code{rdfs:label} → DataCite \emph{Titles}
#'   \item \code{rdfs:comment} → DataCite \emph{Descriptions (Abstract)}
#'   \item \code{dct:identifier} → Alternate Identifier (UUID)
#'   \item \code{schema:serialNumber} → Alternate Identifier (SerialNumber)
#'   \item \code{foaf:givenName}, \code{foaf:familyName}, \code{foaf:account} →
#'         DataCite \emph{Creators} (Person with ORCID)
#' }
#'
#' @details
#' The function performs a \code{POST} request to the DataCite API endpoint
#' (\code{/dois}) to create a new DOI record.
#' If \code{method = "PUT"} is used, an existing DOI record can be updated
#' (e.g. to change state from \emph{draft} to \emph{publish}).
#'
#' Authentication credentials must be provided via the environment variables
#' \code{DATACITE_USER} and \code{DATACITE_PASS}.
#'
#' @param ttl_file Character. Path to the Turtle (.ttl) file describing a single
#'   sensor instance.
#' @param env Character. DataCite environment to use: \code{"test"} or \code{"prod"}.
#' @param prefix Character. DOI prefix (e.g. \code{"10.82159"}). The DOI suffix
#'   is generated automatically by DataCite.
#' @param publisher Character. Name of the publishing organisation
#'   (e.g. \code{"LTER-Italy"}).
#' @param publisherIdentifier Character. Persistent identifier of the publisher,
#'   typically a ROR URI (e.g. \code{"https://ror.org/05ma8mw15"}).
#' @param state Character. Desired DataCite record state
#'   (e.g. \code{"draft"}, \code{"register"}, \code{"publish"}).
#' @param method Character. HTTP method to use: \code{"POST"} (create a new DOI)
#'   or \code{"PUT"} (update an existing DOI).
#' @param url_override Character. Optional landing page URL for the DOI.
#'   If \code{NULL}, the sensor instance URI from the TTL file is used.
#' @param doi Character. Existing DOI to update (required only if
#'   \code{method = "PUT"}).
#' @return
#' Invisibly returns a list containing:
#' \itemize{
#'   \item the parsed sensor instance metadata,
#'   \item the JSON payload sent to DataCite,
#'   \item the DataCite API response,
#'   \item the minted DOI (if returned by DataCite).
#' }
#' @examples
#' \dontrun{
#' sensors_to_datacite(
#'   ttl_file = "sensor_instance_20260114_0d57829e-f780-47c3-bfc7-f36c0c983b3f.ttl",
#'   env = "prod",
#'   prefix = "10.82159",
#'   publisher = "LTER-Italy",
#'   publisherIdentifier = "https://ror.org/05ma8mw15",
#'   state = "draft",
#'   method = "POST"
#' )
#' }
#' @importFrom rdflib rdf_parse rdf_query
#' @importFrom httr2 request req_auth_basic req_headers req_body_json req_perform req_method
#' @export
#' 
### function sensors_to_datacite
sensors_to_datacite <- function(
    ttl_file,
    env = c("test", "prod"),
    prefix,
    publisher,
    publisherIdentifier,
    state = c("draft", "register", "publish"),
    url_override = NULL,
    method = c("POST", "PUT"),
    doi = NULL,
    datacite_username = Sys.getenv("DATACITE_USER"),
    datacite_password = Sys.getenv("DATACITE_PASS")
) {
  env <- match.arg(env)
  state <- match.arg(state)
  method <- match.arg(method)
  
  if (!file.exists(ttl_file)) stop("ttl_file not found: ", ttl_file)
  if (missing(prefix) || is.null(prefix) || !nzchar(prefix)) stop("Provide 'prefix' (e.g. 10.12345).")
  if (missing(publisher) || is.null(publisher) || !nzchar(publisher)) stop("Provide 'publisher' (e.g. CNR-IREA).")
  if (!nzchar(datacite_username) || !nzchar(datacite_password)) {
    stop("Missing DataCite credentials. Set DATACITE_USER and DATACITE_PASS in env (or pass them explicitly).")
  }
  if (!requireNamespace("rdflib", quietly = TRUE)) stop("Install rdflib.")
  if (!requireNamespace("httr2", quietly = TRUE)) stop("Install httr2.")
  
  # --- Parse TTL ---
  g <- rdflib::rdf_parse(ttl_file, format = "turtle")
  
  # Helper: get first literal/object for a predicate
  get_first_obj <- function(subject, predicate) {
    q <- sprintf(
      "SELECT ?o WHERE { <%s> <%s> ?o } LIMIT 1",
      subject, predicate
    )
    res <- rdflib::rdf_query(g, q)
    if (NROW(res) == 0) return(NA_character_)
    as.character(res$o[1])
  }
  
  # 1) Find the main subject (sensor instance URI): take the first subject that has dct:identifier
  q_subj <- "
    SELECT ?s WHERE {
      ?s <http://purl.org/dc/terms/identifier> ?id .
    } LIMIT 1
  "
  subj_res <- rdflib::rdf_query(g, q_subj)
  if (NROW(subj_res) == 0) stop("Cannot find a subject with dct:identifier in TTL.")
  subject_uri <- as.character(subj_res$s[1])
  
  # 2) Extract fields
  instance_uuid <- get_first_obj(subject_uri, "http://purl.org/dc/terms/identifier")
  label <- get_first_obj(subject_uri, "http://www.w3.org/2000/01/rdf-schema#label")
  creator_orcid <- get_first_obj(subject_uri, "http://purl.org/dc/terms/creator")
  serial_number <- get_first_obj(subject_uri, "https://schema.org/serialNumber")
  created_dt <- get_first_obj(subject_uri, "http://purl.org/dc/terms/created")
  comment <- get_first_obj(subject_uri, "http://www.w3.org/2000/01/rdf-schema#comment")
  
  # contactPoint blank node: extract givenName + familyName
  q_cp <- sprintf("
    SELECT ?given ?family ?account WHERE {
      <%s> <http://www.w3.org/ns/dcat#contactPoint> ?cp .
      OPTIONAL { ?cp <http://xmlns.com/foaf/0.1/givenName> ?given . }
      OPTIONAL { ?cp <http://xmlns.com/foaf/0.1/familyName> ?family . }
      OPTIONAL { ?cp <http://xmlns.com/foaf/0.1/account> ?account . }
    } LIMIT 1
  ", subject_uri)
  cp_res <- rdflib::rdf_query(g, q_cp)
  
  given <- if (NROW(cp_res) > 0 && !is.na(cp_res$given[1])) as.character(cp_res$given[1]) else NA_character_
  family <- if (NROW(cp_res) > 0 && !is.na(cp_res$family[1])) as.character(cp_res$family[1]) else NA_character_
  account <- if (NROW(cp_res) > 0 && !is.na(cp_res$account[1])) as.character(cp_res$account[1]) else NA_character_
  
  creator_orcid <- if (!is.na(account) && nzchar(account)) account else creator_orcid
  if (!is.na(creator_orcid) && nzchar(creator_orcid) && !startsWith(creator_orcid, "https://orcid.org/")) {
    creator_orcid <- paste0("https://orcid.org/", creator_orcid)
  }
  
  # Build creator name "Family, Given" if possible
  creator_name <- if (!is.na(family) && nzchar(family) && !is.na(given) && nzchar(given)) {
    paste(family, given, sep = ", ")
  } else if (!is.na(family) && nzchar(family)) {
    family
  } else if (!is.na(given) && nzchar(given)) {
    given
  } else {
    "Unknown"
  }
  
  creators <- list(list(
    name = creator_name, # "Doe, John"
    nameType = "Personal", # DataCite expects "Personal"
    givenName = if (!is.na(given) && nzchar(given)) given else NULL, # "John"
    familyName = if (!is.na(family) && nzchar(family)) family else NULL, # "Doe"
    nameIdentifiers = if (!is.na(creator_orcid) && nzchar(creator_orcid)) {
      list(list(
        nameIdentifier = creator_orcid, # https://orcid.org/...
        nameIdentifierScheme = "ORCID",
        schemeUri = "https://orcid.org"
      ))
    } else NULL
  ))
  
  # Title: use rdfs:label if present, otherwise build one
  title <- if (!is.na(label) && nzchar(label)) label else paste("Sensor instance", instance_uuid)
  
  # Description
  descriptions <- NULL
  if (!is.na(comment) && nzchar(comment)) {
    descriptions <- list(list(
      description = comment,
      descriptionType = "Abstract",
      lang = "en-us"
    ))
  }
  
  # URL: override or default to subject URI
  url <- if (!is.null(url_override) && nzchar(url_override)) url_override else subject_uri
  
  publication_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # identifiers (UUID mandatory, Serial optional)
  identifiers <- list(list(identifier = instance_uuid, identifierType = "UUID"))
  if (!is.na(serial_number) && nzchar(trimws(serial_number))) {
    identifiers <- c(identifiers, list(list(
      identifier = trimws(serial_number),
      identifierType = "SerialNumber"
    )))
  }
  
  # Drop NULL recursively (keeps JSON clean)
  drop_nulls <- function(x) {
    if (is.list(x)) {
      x <- lapply(x, drop_nulls)
      x <- x[!vapply(x, is.null, logical(1))]
      if (length(x) == 0) return(NULL)
    }
    x
  }
  
  # --- Build JSON:API payload ---
  attributes <- drop_nulls(list(
    prefix = prefix,
    state = state,
    url = url,
    creators = creators,
    titles = list(
      list(
        title = title,
        lang = "en-us"
      )
    ),
    publisher = publisher,
    publisherIdentifier = publisherIdentifier,
    publisherIdentifierScheme = "ROR",
    schemeUri = "https://ror.org",
    publicationYear = publication_year,
    types = list(
      resourceTypeGeneral = "Instrument",
      resourceType = "Sensor instance"
    ),
    identifiers = identifiers,
    descriptions = descriptions,
    language = "English"
  ))
  
  payload <- list(
    data = list(
      type = "dois",
      attributes = attributes
    )
  )
  
  # --- API call ---
  base <- if (env == "test") "https://api.test.datacite.org" else "https://api.datacite.org"
  endpoint <- if (method == "POST") {
    paste0(base, "/dois")
  } else {
    if (is.null(doi) || !nzchar(doi)) stop("PUT requires 'doi'.")
    paste0(base, "/dois/", URLencode(doi, reserved = TRUE))
  }
  
  req <- httr2::request(endpoint) |>
    httr2::req_auth_basic(datacite_username, datacite_password) |>
    httr2::req_headers("Content-Type" = "application/vnd.api+json") |>
    httr2::req_body_json(payload, auto_unbox = TRUE)
  
  if (method == "PUT") req <- httr2::req_method(req, "PUT")
  
  resp <- httr2::req_perform(req)
  resp_json <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  
  # Try to extract minted DOI (for POST with prefix)
  minted_doi <- NA_character_
  if (!is.null(resp_json$data$attributes$doi)) minted_doi <- resp_json$data$attributes$doi
  
  invisible(list(
    subject_uri = subject_uri,
    instance_uuid = instance_uuid,
    serial_number = if (!is.na(serial_number)) serial_number else NULL,
    created = if (!is.na(created_dt)) created_dt else NULL,
    payload = payload,
    response = resp_json,
    minted_doi = minted_doi
  ))
}
