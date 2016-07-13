################################################################################
#
# Source code of functions to query the Dutch registry with energy labels for 
# buildings. Part of the "econometer" package.
# 
# This code is copyrighted and made available under the license specified in the
# LICENSE file in the root of this package.
#
################################################################################

.epbd_url <- "https://webapplicaties.agro.nl/DownloadMutationFile/EpbdDownloadMutationFile.asmx"

.epbd_credentials <- new.env()

.epbd_request_template <- function() {
  
  if (!exists("username", envir = .epbd_credentials) || 
      !exists("password", envir = .epbd_credentials)) {
    stop("no credentials found: please provide username and password with epbd_login()")
  }
  
  sprintf('<?xml version="1.0" encoding="utf-8"?>
<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
 xmlns:xsd="http://www.w3.org/2001/XMLSchema" 
 xmlns:soap12="http://www.w3.org/2003/05/soap-envelope"
 xmlns:epbd="http://schemas.ep-online.nl/EpbdDownloadMutationFileService">
  <soap12:Header>
    <EpbdDownloadMutationFileHeader xmlns="http://schemas.ep-online.nl/EpbdDownloadMutationFileHeader">
      <username>%s</username>
      <password>%s</password>
    </EpbdDownloadMutationFileHeader>
  </soap12:Header>
  <soap12:Body>
    <epbd:DownloadMutationFile>
      <epbd:request>
        %%s
      </epbd:request>
    </epbd:DownloadMutationFile>
  </soap12:Body>
</soap12:Envelope>',
          gsub("\\%", "%%", get("username", envir = .epbd_credentials)),
          gsub("\\%", "%%", get("password", envir = .epbd_credentials)))
}


#' Provide login credentials for the energy label registry
#' 
#' @param username a single character string with the username
#' @param password a single character string with the password
#' @details Register your login credentials once at the start of your session
#' and all subsequent queries to the registry will use these.
#' @return \code{TRUE} (invisibly) in case of success
#' @export
epbd_login <- function(username, password) {
  if (is.character(username)) {
    assign("username", username, envir = .epbd_credentials)
  } else {
    stop("'username' must be a character string")
  }
  if (is.character(password)) {
    assign("password", password, envir = .epbd_credentials)
  } else {
    stop("'password' must be a character string")
  }
  invisible(TRUE)
}


#' Retrieve label mutations
#' 
#' @param date a Date object
#' @details The registry with \sQuote{energieprestatielabels} uses a webservice 
#'   based on the SOAP protocol. There are two types of queries which you can 
#'   send to the webservice: \itemize{ \item retrieve all existing labels at the
#'   beginning of the month or \item retrieve the mutations (i.e. changes, 
#'   updates, etc.) on a given date. } This function implements the second 
#'   query: retrieve all changes on a given date.
#'   
#'   Unfortunately, the webservice doesn't actually return an XML document with
#'   the mutations, it returns an XML document with only a link to the (zipped)
#'   XML document on the server. So this function first queries the webservice
#'   to retrieve the URL to the archive, then downloads and extracts the
#'   archive, and finally reads the contents into an \code{xml_document} object.
#' @note The webservice only provides access to the information for the current 
#'   month. In other words: it is not possible to retrieve mutations in other 
#'   months except for the current.
#' @references The registry can be accessed online at \url{http://www.ep-online.nl/}.
#' @return An object of class \code{xml_document} which you can pass to 
#'   \code{\link{epbd_extractLabelMutations}} to convert to a data frame.
#' @importFrom xml2 read_xml xml_ns xml_find_one xml_text
#' @importFrom httr POST stop_for_status content upload_file
#' @export
epbd_getLabelMutations <- function(date) {
  
  body <- sprintf("<epbd:mutationType>Mutation</epbd:mutationType>
  <epbd:date>%s</epbd:date>", format(date, "%Y-%m-%d"))
  
  request_body <- sprintf(.epbd_request_template(), body)
  
  # Write the SOAP request body to a temporary file:
  f <- tempfile(pattern = "getLabelMutations", fileext = ".xml")
  cat(request_body, file = f, append = FALSE)
  
  # Send the request to the webservice:
  message("retrieving label mutations on ", date, "...")
  tries <- 3
  repeat {
    tries <- tries - 1
    success <- TRUE
    r <- tryCatch(stop_for_status(POST(url = .epbd_url,
                                       body = upload_file(f, type = "text/xml"))),
                  http_500 = function(c) {
                    message("an error occurred: ", conditionMessage(c))
                    success <<- FALSE
                  }
    )
    if (success || tries < 1L) {
      break
    } else{
      message("retrieval of mutations on '", date, "' failed, retrying...")
    }
  }
  # Clean up the temporary file:
  file.remove(f)
  
  envelope <- content(r, type = "text/xml", encoding = "UTF-8")
  file.url <- xml_text(xml_find_one(envelope, "//d2:downloadURL", xml_ns(envelope)))
  
  message("downloading zipped XML file with mutations for ", date, "...")
  
  file.zipped <- tempfile(pattern = sprintf("d%s_", format(date, "%Y%m%d")), fileext = ".zip")
  
  download.file(file.url, destfile = file.zipped, quiet = FALSE)
  
  # Name of XML file in ZIP archive:
  file.name <- sprintf("d%s.xml", format(date, "%Y%m%d"))
  
  res <- system2(command = getOption("unzip"),
                 args = c("-o", "-d .", file.zipped, file.name),
                 stdout = TRUE)
  
  # Full path to unzipped file:
  file.unzipped <- file.path(".", file.name)
  
  xml <- try(read_xml(file.unzipped), silent = TRUE)
  
  # Clean up the temporary files:
  unlink(c(file.zipped, file.unzipped))
  
  if (inherits(xml, "try-error")) {
    stop("failed to load the XML file with the following message: ", geterrmessage())
  }
  
  xml
}


#' Extract details of EP label mutations
#' 
#' @param xml a character string with the name of an XML file containing changes
#'   (i.e. mutations) in the EP-label registry or an object of class
#'   \code{xml_document} with the same data already read from the file system
#' @return A data frame with a selection of fields extracted from the mutations
#'   file.
#' @importFrom xml2 xml_find_all xml_find_one xml_text read_xml
#' @export
epbd_extractLabelMutations <- function(xml) {
  
  xml <- if (inherits(xml, "xml_document")) {
    # do nothing:
    xml
  } else if (is.character(xml) && length(xml) == 1L && file.exists(xml)) {
    # read XML file first:
    read_xml(xml)
  } else {
    stop("invalid input")
  }
  
  datum <- as.Date(xml_text(xml_find_one(xml, ".//Mutatiedatum")), format = "%Y%m%d")
  
  changes <- xml_find_all(xml, ".//Mutatiebericht")
  
  do.call(rbind, lapply(changes, function(change) {
    
    getField <- function(field) {
      res <- try(xml_find_one(change, sprintf(".//%s", field)), silent = TRUE)
      if (inherits(res, "try-error")) {
        NA
      } else {
        xml_text(res)
      }
    }
    
    stuurcode <- getField("Stuurcode")
    
    data.frame(id = as.numeric(getField("Mutatievolgnummer")),
               code = as.numeric(stuurcode),
               postcode = switch(stuurcode,
                                 "1" = getField("PandVanMeting_postcode"),
                                 "2" = getField("Pand_postcode")),
               huisnummer = switch(stuurcode,
                                   "1" = getField("PandVanMeting_huisnummer"),
                                   "2" = getField("Pand_huisnummer")),
               toevoeging = switch(stuurcode,
                                   "1" = getField("PandVanMeting_huisnummer_toev"),
                                   "2" = getField("Pand_huisnummer_toev")),
               energieklasse = switch(stuurcode,
                                      "1" = getField("PandVanMeting_energieklasse"),
                                      "2" = NA_character_),
               datum.meting = switch(stuurcode,
                                     "1" = as.Date(getField("PandVanMeting_opnamedatum"),
                                                   format = "%Y%m%d"),
                                     "2" = NA_character_),
               datum.mutatie = datum,
               stringsAsFactors = FALSE)
  }))
}


#' Energy label HEX color code
#' 
#' @param a character vector with energy label codes (i.e. \sQuote{A}, 
#'   \sQuote{B}, etc.)
#' @return A character vector with HEX color codes. \code{NA} values are given a
#'   light grey color.
#' @export
epbd_labelColor <- function(label) {
  
  table <- data.frame(label = c(    "A++",      "A+",       "A",        "B",
                                      "C",       "D",       "E",        "F",
                                      "G",        NA),
                      color = c("#33A357", "#33A357", "#33A357", "#79B752",
                                "#C3D545", "#FFF12C", "#EDB731", "#D66F2C",
                                "#CC232A", "#D3D3D3"),
                      stringsAsFactors = FALSE)
  
  stopifnot(all(label %in% table$label))
  
  table$color[match(label, table$label)]
}


#' Download energy certificates at the start of the current month
#' 
#' @param postcode.min integer value representing the smallest postcode to 
#'   download (default: 1000)
#' @param postcode.max integer value representing the largest postcode to
#'   download (default: 9999); must be greater than \code{postcode.min}
#' @details The registry with \sQuote{energieprestatielabels} uses a webservice 
#'   based on the SOAP protocol. There are two types of queries which you can 
#'   send to the webservice: \itemize{ \item retrieve all existing certificates 
#'   at the beginning of the month or \item retrieve the mutations (i.e. 
#'   changes, updates, etc.) on a given date. } This function implements the 
#'   first query: retrieve all certificates at the start of the month. The 
#'   arguments \code{postcode.min} and \code{postcode.max} can be used to 
#'   download certificates from a specific region. These arguments accept only 
#'   the 4 digit numeric part of Dutch postal codes.
#'   
#'   Unfortunately, the webservice doesn't actually return an XML document with 
#'   the mutations, it returns an XML document with only a link to the (zipped) 
#'   XML document on the server. So this function first queries the webservice 
#'   to retrieve the URL to the archive, then downloads and extracts the 
#'   archive, and finally reads the contents into an \code{xml_document} object.
#' @note This functions calls a Python script in the \sQuote{inst/tools} folder 
#'   which uses a SAX-based XML parser to extract the certificate data from the 
#'   huge XML file which is returned by this rather stupid service.
#' @references The registry can be accessed online at 
#'   \url{http://www.ep-online.nl/}.
#' @return A data frame with the contents of the energy certificates.
#' @importFrom httr POST stop_for_status content upload_file
#' @export
epbd_downloadCertificates <- function(postcode.min = 1000L, postcode.max = 9999L) {
  
  stopifnot(postcode.min >= 1000, postcode.min < postcode.max, postcode.max < 10000L)
  
  # What's the first day of the current month?
  TODAY <- as.POSIXlt(Sys.Date())
  date <- as.Date(sprintf("%s-%s-01", TODAY$year + 1900, TODAY$mon + 1))
  
  body <- "<epbd:mutationType>Complete</epbd:mutationType>"
  
  request_body <- sprintf(.epbd_request_template(), body)
  
  # Write the SOAP request body to a temporary file:
  f <- tempfile(pattern = "downloadCertificates", fileext = ".xml")
  cat(request_body, file = f, append = FALSE)
  
  # Send the request to the webservice:
  message("retrieving certificates...")
  tries <- 3
  repeat {
    tries <- tries - 1
    success <- TRUE
    r <- tryCatch(stop_for_status(POST(url = .epbd_url,
                                       body = upload_file(f, type = "text/xml"))),
                  http_500 = function(c) {
                    message("an error occurred: ", conditionMessage(c))
                    success <<- FALSE
                  }
    )
    if (success || tries < 1L) {
      break
    } else{
      message("retrieval of certificates failed, retrying...")
    }
  }
  # Clean up the temporary file:
  file.remove(f)
  
  envelope <- content(r, type = "text/xml", encoding = "UTF-8")
  file.url <- xml_text(xml_find_one(envelope, "//d2:downloadURL", xml_ns(envelope)))
  
  message("downloading zipped XML file with certificates...")
  
  file.zipped <- tempfile(pattern = sprintf("v%s_", format(date, "%Y%m%d")),
                          fileext = ".zip")
  
  download.file(file.url, destfile = file.zipped, quiet = TRUE)
  
  # Name of XML file in ZIP archive:
  file.name <- sprintf("v%s.xml", format(date, "%Y%m%d"))
  
  res <- system2(command = getOption("unzip"),
                 args = c("-o", "-d .", file.zipped, file.name),
                 stdout = TRUE)
  
  # Full path to unzipped file:
  file.unzipped <- file.path(".", file.name)
  
  # Extract certificates in The Hague:
  message("extracting certificates in The Hague...")
  res <- system2(command = "python2.7",
                 args = c(system.file("tools", "epbdparser.py",
                                      package = "econometer"),
                          file.unzipped, "certificates.csv", postcode.min, postcode.max),
                 stdout = TRUE)
  
  certificates <- read.csv2("certificates.csv")
  certificates$X <- NULL
  
  # Clean up the temporary files:
  unlink(c(file.zipped, file.unzipped, "certificates.csv"))
  
  data.frame(
    postcode = as.character(certificates$PandVanMeting_postcode),
    huisnummer = as.character(certificates$PandVanMeting_huisnummer),
    toevoeging = as.character(certificates$PandVanMeting_huisnummer_toev),
    energieklasse = as.character(certificates$PandVanMeting_energieklasse),
    datum.meting = as.Date(as.character(certificates$PandVanMeting_opnamedatum),
                           format = "%Y%m%d"),
    stringsAsFactors = FALSE
  )
}
