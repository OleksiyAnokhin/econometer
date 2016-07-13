################################################################################
#
# Source code of functions to query the BAG geocoder. Part of the "econometer" 
# package.
# 
# This code is copyrighted and made available under the license specified in the
# LICENSE file in the root of this package.
#
################################################################################

#' Retrieve the (RD) coordinates of a street address in The Hague
#' 
#' @param postcode a character verctor with postal code
#' @param huisnummer a character vector with house numbers
#' @param straatnaam a character vector with street names
#' @param plaatsnaam a character vector with city or location name
#' @param max.retries an integer with the maximum number of retries if a request
#'   times out (default: 2)
#' @return A data frame with the coordinates of each address in two columns
#'   named \sQuote{rdx} and \sQuote{rdy}. Unknown addresses have NA as
#'   coordinates.
#' @importFrom xml2 xml_find_all xml_text xml_ns
#' @export
bag_geocode <- function(postcode, huisnummer, straatnaam, plaatsnaam, max.retries = 2L) {
  
  stopifnot(is.integer(max.retries) && max.retries > 0L)
  
  # if 'postcode' is provided, ignore 'straatnaam' and 'plaatsnaam':
  if (!missing(postcode)) {
    # postcode is provided, we ignore the street and city names
    if (!missing(huisnummer)) {
      adressen <- Map(list, postcode = postcode, nummer = huisnummer)
    } else {
      adressen <- Map(list, postcode = postcode)
    }
  } else {
    # postcode is not provided, city name is required
    if (missing(plaatsnaam)) {
      stop("if 'postcode' is not provided, then 'plaatsnaam' is required")
    }
    
    if (!missing(huisnummer) & !missing(straatnaam)) {
      adressen <- Map(list, straat = straatnaam, nummer = huisnummer, plaats = plaatsnaam)
    }
    if (missing(huisnummer) & !missing(straatnaam)) {
      adressen <- Map(list, straat = straatnaam, plaats = plaatsnaam)
    }
    if (missing(huisnummer) & missing(straatnaam)) {
      adressen <- Map(list, plaats = plaatsnaam)
    }
  }
  
  do.call(rbind, lapply(adressen, function(adres) {
    coord <- data.frame(
      rdx = NA,
      rdy = NA,
      stringsAsFactors = FALSE
    )
    
    query <- paste(adres, collapse = " ")
    
    tries <- 2
    while (tries) {
      response <- .query_bag_geocoder(query, max.retries)
      position_node <- xml_find_all(response, "//gml:pos", xml_ns(response))
      
      if (length(position_node) > 0L) {
        if (length(position_node) > 1L) {
          message(length(position_node), " matches for '", query, "', using first one")
        } else {
          message("1 match found for '", query, "'")
        }
        xy <- strsplit(xml_text(position_node[[1]]), " ")[[1]]
        coord$rdx = round(as.numeric(xy[1]))
        coord$rdy = round(as.numeric(xy[2]))
        tries <- 0
      } else {
        if ("nummer" %in% names(adres)) {
          # address apparently not found, retry without street number:
          old_query <- query
          tries <- tries - 1
          adres$nummer <- NULL
          query <- paste(adres, collapse = " ")
          message("no match for '", old_query, "', trying '", query, "'")
        } else {
          message("no match for '", query, "'")
          tries <- 0
        }
      }
    }
    coord
  }))
}

#' Send a query to the BAG geocoding service
#' 
#' @param query the query string
#' @param max.retries an integer with the maximum number of retries
#' @return the response in the form of an XML document
#' @importFrom xml2 read_xml url_escape
.query_bag_geocoder <- function(query, max.retries) {
  
  url_template <- "https://geodata.nationaalgeoregister.nl/geocoder/Geocoder?zoekterm=%s"
  
  url <- sprintf(url_template, url_escape(query))
  
  message("requesting ", url)
  
  while (max.retries >= 0L) {
    response <- try(read_xml(url))
    max.retries <- if (inherits(response, "try-error")) {
      # request failed, retry
      max.retries - 1L
    } else {
      # request succeeded, continue
      -1L
    }
  }
  
  if (inherits(response, "try-error")) {
    stop("failed to query the BAG geocoding service")
  }
  
  response
}
