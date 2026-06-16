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
#'   \item \code{dct:created} → DataCite \emph{Dates} (type: Created)
#'   \item \code{schema:serialNumber} → Alternate Identifier (SerialNumber)
#'   \item \code{foaf:givenName}, \code{foaf:familyName}, \code{foaf:account} →
#'         DataCite \emph{Creators} (Person with ORCID)
#' }
#'
#' The following fields are set as constants for all records:
#' \itemize{
#'   \item \emph{Subject}: "Earth and related Environmental sciences"
#'         (FOS classification, code 1.5)
#'   \item \emph{Rights}: Creative Commons Attribution 4.0 International
#'         (CC-BY-4.0, SPDX)
#'   \item \emph{Version}: "1.0"
#'   \item \emph{Resource type}: Instrument / Sensor instance
#' }
#'
#' @details
#' The function performs a \code{POST} request to the DataCite API endpoint
#' (\code{/dois}) to create a new DOI record.
#' If \code{method = "PUT"} is used, an existing DOI record can be updated
#' (e.g. to change state from \emph{draft} to \emph{publish}).
#'
#' Authentication credentials must be provided via the environment variables
#' \code{DATACITE_USER} and \code{DATACITE_PASS}, or passed explicitly via
#' the \code{datacite_username} and \code{datacite_password} arguments.
#'
#' The \code{dates} field in the DataCite record is populated from
#' \code{dct:created} in the TTL file, with \code{dateType = "Created"} and
#' the description \emph{"Creation date of the sensor instance"}.
#' If \code{dct:created} is absent, the \code{dates} field is omitted.
#'
#' The \code{subjects} field is set to the FOS (Fields of Science and
#' Technology) classification entry for Earth and related Environmental
#' sciences (\href{https://skos.um.es/unesco6/}{UNESCO FOS}, code 1.5),
#' consistent with DataCite best practices for environmental monitoring data.
#'
#' Rights are set to \strong{CC BY 4.0} and apply to the metadata record.
#' The SPDX identifier \code{CC-BY-4.0} is used, as recommended by the
#' \href{https://spdx.org/licenses/}{SPDX license list}.
#'
#' @param ttl_file \code{character}. Path to the Turtle (.ttl) file describing
#'   a single sensor instance.
#' @param env \code{character}. DataCite environment to target:
#'   \code{"test"} (sandbox at \code{api.test.datacite.org}) or
#'   \code{"prod"} (production at \code{api.datacite.org}).
#' @param prefix \code{character}. DOI prefix assigned to your repository
#'   (e.g. \code{"10.82159"}). The DOI suffix is generated automatically
#'   by DataCite when \code{method = "POST"}.
#' @param publisher \code{character}. Name of the publishing organisation
#'   (e.g. \code{"LTER-Italy"}).
#' @param publisherIdentifier \code{character}. Persistent identifier of the
#'   publisher, typically a ROR URI
#'   (e.g. \code{"https://ror.org/05ma8mw15"}).
#' @param state \code{character}. Desired DataCite record state. One of
#'   \code{"draft"} (default), \code{"register"}, or \code{"publish"}.
#' @param method \code{character}. HTTP method for the API call.
#'   \code{"POST"} creates a new DOI record (default);
#'   \code{"PUT"} updates an existing record (requires \code{doi}).
#' @param url_override \code{character} or \code{NULL}. Optional landing page
#'   URL for the DOI. If \code{NULL} (default), the sensor instance URI
#'   extracted from the TTL file (\code{dct:identifier} subject) is used.
#' @param doi \code{character} or \code{NULL}. The existing DOI to update.
#'   Required only when \code{method = "PUT"}; ignored for \code{"POST"}.
#' @param datacite_username \code{character}. DataCite repository account
#'   username. Defaults to the value of the \code{DATACITE_USER} environment
#'   variable.
#' @param datacite_password \code{character}. DataCite repository account
#'   password. Defaults to the value of the \code{DATACITE_PASS} environment
#'   variable.
#'
#' @return
#' Invisibly returns a named \code{list} with the following elements:
#' \describe{
#'   \item{subject_uri}{\code{character}. The sensor instance URI extracted
#'     from the TTL file.}
#'   \item{instance_uuid}{\code{character}. The UUID from \code{dct:identifier}.}
#'   \item{serial_number}{\code{character} or \code{NULL}. The serial number
#'     from \code{schema:serialNumber}, or \code{NULL} if absent.}
#'   \item{created}{\code{character} or \code{NULL}. The creation datetime
#'     from \code{dct:created}, or \code{NULL} if absent.}
#'   \item{payload}{\code{list}. The full JSON:API payload sent to DataCite,
#'     including \code{subjects}, \code{dates}, \code{rightsList}, and
#'     \code{version}.}
#'   \item{response}{\code{list}. The parsed JSON response returned by the
#'     DataCite API.}
#'   \item{minted_doi}{\code{character}. The DOI minted by DataCite
#'     (for \code{method = "POST"} with automatic suffix generation), or
#'     \code{NA_character_} if not returned.}
#'   \item \code{org:memberOf / foaf:name} → DataCite \emph{Creators}
#'         (Organizational affiliation)
#'   \item \code{org:memberOf / owl:sameAs} → DataCite \emph{Creators}
#'         (affiliation ROR identifier)
#' }
#'
#' @note
#' The \code{version} field is set to \code{"1.0"} for all records.
#' If versioning of sensor instance records is required in the future,
#' expose this as a function parameter.
#'
#' Credentials are never included in the return value or logged.
#' 
#' If \code{org:memberOf} and \code{owl:sameAs} are present in the TTL
#' (populated by \code{sensors_instance_ttl()} when \code{owner_institution}
#' and \code{owner_ror} are supplied), they are mapped to the DataCite
#' \code{affiliation} block of the creator, including the ROR identifier
#' and scheme URI.
#'
#' @seealso
#' \href{https://support.datacite.org/docs/api}{DataCite REST API documentation},
#' \href{https://schema.datacite.org/meta/kernel-4/}{DataCite Metadata Schema 4.x},
#' \href{https://spdx.org/licenses/}{SPDX license list},
#' \href{https://skos.um.es/unesco6/}{UNESCO FOS classification}
#'
#' @importFrom rdflib rdf_parse rdf_query
#' @importFrom httr2 request req_auth_basic req_headers req_body_json
#'   req_perform req_method resp_body_json
#' @importFrom rdflib rdf_parse rdf_query
#' @importFrom httr2 request req_auth_basic req_headers req_body_json
#'   req_perform req_method resp_body_json
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a draft DOI record from a TTL file (test environment)
#' result <- sensors_to_datacite(
#'   ttl_file = "output/instances/sensor_instance_20260422_ff13cf7e-87f9-4b47-8866-4ad35822d5af.ttl",
#'   env = "prod",
#'   prefix = "10.82159",
#'   publisher = "LTER-Italy",
#'   publisherIdentifier = "https://ror.org/05ma8mw15",
#'   state = "draft",
#'   method = "POST"
#' )
#'
#' # Inspect the minted DOI
#' result$minted_doi
#'
#' # Publish an existing draft DOI (production environment)
#' sensors_to_datacite(
#'   ttl_file = "sensor_instance_20260122_ce6d89f9-a51b-4b1a-8347-81ffd9f4f509.ttl",
#'   env = "prod",
#'   prefix = "10.82159",
#'   publisher = "LTER-Italy",
#'   publisherIdentifier = "https://ror.org/05ma8mw15",
#'   state = "publish",
#'   method = "PUT",
#'   doi = result$minted_doi
#' )
#' }
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
  
  # Inform user which DataCite environment is being targeted
  message("Targeting DataCite environment: ", toupper(env),
          if (env == "prod") " ⚠️  — changes will be permanent." else " (sandbox, changes are reversible).")
  
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
      ?s <http://purl.org/dc/terms/identifier> [] .
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
  
  # contactPoint → org:memberOf: extract institution name and ROR URI
  q_org <- sprintf("
    SELECT ?org_name ?ror WHERE {
      <%s> <http://www.w3.org/ns/dcat#contactPoint> ?cp .
      ?cp <http://www.w3.org/ns/org#memberOf> ?org .
      OPTIONAL { ?org <http://xmlns.com/foaf/0.1/name> ?org_name . }
      OPTIONAL { ?org <http://www.w3.org/2002/07/owl#sameAs> ?ror . }
    } LIMIT 1
  ", subject_uri)
  org_res <- rdflib::rdf_query(g, q_org)
  
  org_name <- if (NROW(org_res) > 0 && !is.na(org_res$org_name[1]))
    as.character(org_res$org_name[1]) else NA_character_
  org_ror  <- if (NROW(org_res) > 0 && !is.na(org_res$ror[1]))
    as.character(org_res$ror[1]) else NA_character_
  
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
  
  # Build affiliation block (institution + ROR, if available)
  affiliation <- NULL
  if (!is.na(org_name) && nzchar(org_name)) {
    aff_entry <- list(name = org_name)
    if (!is.na(org_ror) && nzchar(org_ror)) {
      aff_entry$affiliationIdentifier       <- org_ror
      aff_entry$affiliationIdentifierScheme <- "ROR"
      aff_entry$schemeUri                   <- "https://ror.org"
    }
    affiliation <- list(aff_entry)
  }
  
  creators <- list(list(
    name       = creator_name,
    nameType   = "Personal",
    givenName  = if (!is.na(given)  && nzchar(given))  given  else NULL,
    familyName = if (!is.na(family) && nzchar(family)) family else NULL,
    nameIdentifiers = if (!is.na(creator_orcid) && nzchar(creator_orcid)) {
      list(list(
        nameIdentifier       = creator_orcid,
        nameIdentifierScheme = "ORCID",
        schemeUri            = "https://orcid.org"
      ))
    } else NULL,
    affiliation = affiliation          # NULL → dropped by drop_nulls()
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
  # Date block: use created_dt from TTL dct:created
  dates <- NULL
  if (!is.na(created_dt) && nzchar(created_dt)) {
    dates <- list(list(
      date            = created_dt,
      dateType        = "Created",
      dateInformation = "Creation date of the sensor instance"
    ))
  }
  
  # --- Build JSON:API payload ---
  attributes <- drop_nulls(list(
    prefix = prefix,
    state  = state,
    url    = url,
    creators = creators,
    titles = list(
      list(
        title = title,
        lang  = "en-us"
      )
    ),
    publisher                  = publisher,
    publisherIdentifier        = publisherIdentifier,
    publisherIdentifierScheme  = "ROR",
    schemeUri                  = "https://ror.org",
    publicationYear            = publication_year,
    types = list(
      resourceTypeGeneral = "Instrument",
      resourceType        = "Sensor instance"
    ),
    # Subject: constant Earth & Environmental sciences (FOS classification)
    subjects = list(
      list(
        subject              = "Earth and related Environmental sciences",
        subjectScheme        = "Fields of Science and Technology (FOS)",
        schemeUri            = "https://skos.um.es/unesco6/",
        classificationCode   = "1.5"        # FOS code for Earth sciences
      )
    ),
    identifiers  = identifiers,
    dates        = dates,           # Dates from dct:created
    descriptions = descriptions,
    # CC BY 4.0 applied to the metadata record
    rightsList = list(
      list(
        rights            = "Creative Commons Attribution 4.0 International",
        rightsUri         = "https://creativecommons.org/licenses/by/4.0/",
        rightsIdentifier  = "CC-BY-4.0",    # SPDX identifier
        rightsIdentifierScheme = "SPDX",
        schemeUri         = "https://spdx.org/licenses/"
      )
    ),
    version  = "1.0",               # Version
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
  
  # Debug: log endpoint and url before performing the request
  message("Endpoint: ", endpoint)
  message("Landing page URL: ", url)
  message("Subject URI: ", subject_uri)
  message("Instance UUID: ", instance_uuid)
  
  resp <- httr2::req_perform(req)
  resp_json <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  
  # Try to extract minted DOI (for POST with prefix)
  minted_doi <- NA_character_
  if (!is.null(resp_json$data$attributes$doi)) minted_doi <- resp_json$data$attributes$doi
  
  invisible(list(
    subject_uri   = subject_uri,
    instance_uuid = instance_uuid,
    serial_number = if (!is.na(serial_number)) serial_number else NULL,
    created       = if (!is.na(created_dt))    created_dt    else NULL,
    institution   = if (!is.na(org_name))      org_name      else NULL,  # <-- NUOVO
    ror           = if (!is.na(org_ror))        org_ror       else NULL,  # <-- NUOVO
    payload       = payload,
    response      = resp_json,
    minted_doi    = minted_doi
  ))
}
