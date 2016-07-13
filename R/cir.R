################################################################################
#
# Source code of functions to query the insolvency registry in the Netherlands.
# Part of the "econometer" package.
# 
# This code is copyrighted and made available under the license specified in the
# LICENSE file in the root of this package.
#
################################################################################

.cir_url <- "http://webservice.rechtspraak.nl/CIR.asmx"

.cir_credentials <- new.env()

.cir_request_template <- function() {
  
  if (!exists("username", envir = .cir_credentials) || 
      !exists("password", envir = .cir_credentials)) {
    stop("no credentials found: please provide username and password with cir_login()")
  }
  
  sprintf('<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope 
  xmlns:wsa="http://schemas.xmlsoap.org/ws/2004/08/addressing" 
  xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd" 
  xmlns:cir="http://www.rechtspraak.nl/namespaces/cir01" 
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
  xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
   <soap:Header>
      <wsse:Security mustUnderstand="true">
         <wsse:UsernameToken>
            <wsse:Username>%s</wsse:Username>
            <wsse:Password>%s</wsse:Password>
         </wsse:UsernameToken>
      </wsse:Security>
      <wsa:Action>%%s</wsa:Action>
      <wsa:To>http://webservice.rechtspraak.nl/CIR.asmx</wsa:To>
   </soap:Header>
   <soap:Body>
      %%s
   </soap:Body>
</soap:Envelope>',
          gsub("\\%", "%%", get("username", envir = .cir_credentials)),
          gsub("\\%", "%%", get("password", envir = .cir_credentials)))
}

.cir_publication_types <- c("Uitspraken faillissement",
                            "Uitspraken schuldsanering",
                            "Uitspraken surseance",
                            "Einde faillissementen",
                            "Einde schuldsaneringen",
                            "Einde surseances",
                            "Overig")

.cir_courts <- c("Amsterdam",
                 "Noord-Holland",
                 "Midden-Nederland",
                 "Noord-Nederland",
                 "Oost-Nederland",
                 "Den Haag",
                 "Rotterdam",
                 "Limburg",
                 "Oost-Brabant",
                 "Zeeland-West-Brabant",
                 "Gelderland",
                 "Overijssel")


#' Get a vector of court names/locations
#' 
#' @return A character vector with the available courts in the Netherlands.
#' @export
cir_getCourtNames <- function() {
  .cir_courts
}


#' Provide login credentials for the insolvency registry
#' 
#' @param username a single character string with the username
#' @param password a single character string with the password
#' @details Register your login credentials once at the start of your session
#' and all subsequent queries to the registry will use these.
#' @return \code{TRUE} (invisibly) in case of success
#' @export
cir_login <- function(username, password) {
  if (is.character(username)) {
    assign("username", username, envir = .cir_credentials)
  } else {
    stop("'username' must be a character string")
  }
  if (is.character(password)) {
    assign("password", password, envir = .cir_credentials)
  } else {
    stop("'password' must be a character string")
  }
  invisible(TRUE)
}


#' Retrieve the date of the last update
#' 
#' @return A \code{Date} object with the date of the last update to the Dutch
#'   insolvency registry.
#' @importFrom httr POST upload_file stop_for_status content
#' @importFrom xml2 xml_text xml_find_one xml_ns
#' @export
cir_GetLastUpdate <- function() {
  
  action <- "http://www.rechtspraak.nl/namespaces/cir01/GetLastUpdate"
  body <- "<cir:GetLastUpdate/>"
  request_body <- sprintf(.cir_request_template(), action, body)
  
  # Write the SOAP request body to a temporary file:
  f <- tempfile(pattern = "GetLastUpdate", fileext = ".xml")
  cat(request_body, file = f, append = FALSE)
  
  # Send the request to the webservice:
  r <- POST(url = .cir_url, body = upload_file(f, type = "text/xml"))
  
  # Clean up the temporary file:
  file.remove(f)
  
  stop_for_status(r)
  
  envelope <- content(r)
  # Return the result as a 'Date' object:
  as.Date(xml_text(xml_find_one(envelope, "//d2:lastUpdateDate", xml_ns(envelope))))
}


#' Retrieve publication numbers by date
#' 
#' @param date a \code{Date} object which should not be in the future, nor more 
#'   than one year in the past
#' @param court an optional character string with the name of the court, e.g. 
#'   \sQuote{Den Haag}
#' @param type a character vector with publication types to retrieve, accepted
#'   values are \sQuote{Uitspraken faillissement}, \sQuote{Uitspraken
#'   schuldsanering}, \sQuote{Uitspraken surseance}, \sQuote{Einde
#'   faillissementen}, \sQuote{Einde schuldsaneringen}, \sQuote{Einde
#'   surseances} and \sQuote{Overig}. If no value is supplied then all
#'   publication types are retrieved.
#' @details The function name \sQuote{searchByDate} is actually quite a misnomer
#'   as the function in fact returns all publications since the date provided.
#' @seealso \code{\link{cir_getCourtNames}} to get a vector of court names
#' @return A character vector with case names or \code{NULL} if no cases are 
#'   found or some other error has occurred.
#' @importFrom httr POST upload_file stop_for_status content
#' @importFrom xml2 xml_text xml_attr xml_find_one xml_ns as_list
#' @export
cir_searchByDate <- function(date, court, type = NULL) {
  
  # 'date' must be a Date object which is not in the future:
  stopifnot(inherits(date, "Date"), date <= Sys.Date())
  
  court.nr <- switch(match.arg(court, choices = cir_getCourtNames()),
                     "Amsterdam" = 40,
                     "Noord-Holland" = 41,
                     "Midden-Nederland" = 42,
                     "Noord-Nederland" = 43,
                     "Oost-Nederland" = 44,
                     "Den Haag" = 45,
                     "Rotterdam" = 46,
                     "Limburg" = 47,
                     "Oost-Brabant" = 48,
                     "Zeeland-West-Brabant" = 49,
                     "Gelderland" = 50,
                     "Overijssel" = 51
  )
  
  action <- "http://www.rechtspraak.nl/namespaces/cir01/searchByDate"
  body <- if (is.null(type)) {
    sprintf("<cir:searchByDate>
  <cir:date>%s</cir:date>
  <cir:court>%d</cir:court>
</cir:searchByDate>",
            format(date, "%Y-%m-%d"), court.nr)
  } else {
    # Only accept valid publication types:
    stopifnot(all(type %in% .cir_publication_types))
    sprintf("<cir:searchByDate>
  <cir:date>%s</cir:date>
  <cir:court>%d</cir:court>
  <cir:pubType>
%s
  </cir:pubType>
</cir:searchByDate>",
            format(date, "%Y-%m-%d"), court.nr, paste(sprintf("    <cir:string>%s</cir:string>", type), collapse = "\n"))
  }
  
  request_body <- sprintf(.cir_request_template(), action, body)
  
  # Write the SOAP request body to a temporary file:
  f <- tempfile(pattern = "searchByDate", fileext = ".xml")
  cat(request_body, file = f, append = FALSE)
  
  # Send the request to the webservice:
  message("retrieving case publications from the court in ", court, " since ", date)
  tries <- 3
  repeat {
    tries <- tries - 1
    success <- TRUE
    r <- tryCatch(stop_for_status(POST(url = .cir_url,
                                       body = upload_file(f, type = "text/xml"))),
                  http_500 = function(c) {
                    message("an error occurred: ", conditionMessage(c))
                    success <<- FALSE
                  }
    )
    if (success || tries < 1L) {
      break
    } else {
      message("retrieval of publications on ", date, " failed, retrying...")
    }
  }
  # Clean up the temporary file:
  file.remove(f)
  
  result <- content(r)
  
  exception <- try(xml_find_one(result, "//d2:exceptie", xml_ns(result)), silent = TRUE)
  if (!inherits(exception, "try-error")) {
    warning("No results due to error '", xml_text(exception), "' with code ", xml_attr(exception, "errorcode"))
    return(NULL)
  }
  envelope <- as_list(result)
  
  # Return the case publication numbers in a character vector:
  sapply(envelope$Body$searchByDateResponse$searchByDateResult$publicatieLijst, function(x) x[[1]])
}


#' Retrieve case details
#' 
#' @param publication.nr a character vector with publication numbers
#' @return A named list with the details for each publication in
#'   \code{publicationNumber}.
#' @importFrom httr POST upload_file stop_for_status content
#' @importFrom xml2 as_list
#' @export
cir_getCase <- function(publication.nr) {
  
  stopifnot(is.character(publication.nr))
  
  action <- "http://www.rechtspraak.nl/namespaces/cir01/getCase"
  
  sapply(publication.nr, function(pubnr) {
    
    body <- sprintf("<cir:getCase>
  <cir:publicationNumber>%s</cir:publicationNumber>
</cir:getCase>", pubnr)
    
    request_body <- sprintf(.cir_request_template(), action, body)
    
    # Write the SOAP request body to a temporary file:
    f <- tempfile(pattern = "getCase", fileext = ".xml")
    cat(request_body, file = f, append = FALSE)
    
    # Send the request to the webservice:
    message("retrieving case details for '", pubnr, "'")
    tries <- 3
    repeat {
      tries <- tries - 1
      success <- TRUE
      r <- tryCatch(stop_for_status(POST(url = .cir_url,
                                         body = upload_file(f, type = "text/xml"))),
                    http_500 = function(c) {
                      message("an error occurred: ", conditionMessage(c))
                      success <<- FALSE
                    }
      )
      if (success || tries < 1L) {
        break
      } else{
        message("retrieval of '", pubnr, "' failed, retrying...")
      }
    }
    # Clean up the temporary file:
    file.remove(f)
    
    envelope <- content(r)
    # Return the result as a list (the deeply nested 'insolvente' field):
    case <- as_list(envelope)
    case$Body$getCaseResponse$getCaseResult$inspubWebserviceInsolvente$insolvente
  }, simplify = FALSE)
}


#' Extract a selection of case details into a data frame
#' 
#' @param cases a list with one or more case details obtained using \code{\link{cir_getCase}}
#' @return A data frame with a selection of case details required for the
#'   Econometer
#' @details The typical workflow will be to retrieve multiple cases from the
#'   register and to convert the resulting list of cases into a data frame. See
#'   the example.
#' @examples
#' \dontrun{
#' ids <- cir_searchByDate(Sys.Date() - 1) # find case publications from yesterday
#' cases <- cir_getCase(ids)
#' cases.df <- cir_extractCaseDetails(cases)
#' }
#' @export
cir_extractCaseDetails <- function(cases) {
  
  stopifnot(is.list(cases))
  
  # Local function to extract a (nested) field from a list:
  get.field <- function(x, field) {
    if (is.null(x) || is.null(x[[field]])) {
      NA
    } else {
      x[[field]][[1]]
    }
  }
  
  res <- do.call(rbind, lapply(cases, function(case) {
    # dig down into the list to retrieve the data on the insolvent party
    insolvency <- case
    entity <- insolvency$persoon
    
    insolvencyType <- get.field(insolvency$persoon, "rechtspersoonlijkheid")
    
    address <- switch(insolvencyType,
                      "rechtspersoon" = {
                        # find the "VEST" address
                        addressTypes <- sapply(insolvency$adressen, function(adres) adres$adresType[[1]])
                        i <- which(addressTypes == "VEST")
                        if (length(i)) {
                          insolvency$adressen[[i[1]]]
                        } else {
                          NULL
                        }
                      },
                      "natuurlijk persoon" = {
                        # find the "WOON" address
                        addressTypes <- sapply(insolvency$adressen, function(adres) adres$adresType[[1]])
                        i <- which(addressTypes == "WOON")
                        if (length(i)) {
                          insolvency$adressen[[i[1]]]
                        } else {
                          NULL
                        }
                      },
                      stop("unknown insolvency type"))
    
    publications <- insolvency$publicatiegeschiedenis[
      names(insolvency$publicatiegeschiedenis) == "publicatie"
      ]
    
    pub <- data.frame(
      publicatie.kenmerk = sapply(publications, get.field, "publicatieKenmerk"),
      publicatie.datum = sapply(publications, get.field, "publicatieDatum"),
      cir.code = as.numeric(sapply(publications, get.field, "publicatieSoortCode")),
      insolventie.nr = get.field(insolvency, "insolventienummer"),
      insolventie.soort = insolvencyType,
      insolventie.naam = trimws(paste(entity$voornaam, entity$achternaam)),
      insolventie.kvknr = get.field(entity, "KvKNummer"),
      insolventie.straat = get.field(address, "straat"),
      insolventie.huisnr = get.field(address, "huisnummer"),
      insolventie.plaats = get.field(address, "plaats"),
      insolventie.postcode = get.field(address, "postcode"),
      stringsAsFactors = FALSE
    )
  }))
  row.names(res) <- NULL
  res
}
