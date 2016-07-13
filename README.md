# econometer

A packaged set of R functions to query two (semi) public registries in the 
Netherlands:

 1. Insolvency registry (Dutch: [Centraal Insolventieregister](http://insolventies.rechtspraak.nl/)) and
 2. Energy certificates for building registry (Dutch: [EP-online](http://www.ep-online.nl/ep-online/Default.aspx))

These two registries can be accessed by requesting a username and password and 
queried using the lovely [SOAP protocol](https://en.wikipedia.org/wiki/SOAP). 
This R package saves you from having to deal with this arcane protocol, but you 
still need to request permission to access the registries and to accept their 
terms of service.

Also included is a function `bag_geocode()` to geocode a Dutch street address to [EPSG:28992](http://spatialreference.org/ref/epsg/amersfoort-rd-new/) coordinates. 
No login is required for this service.

This package was created as part of a project called "Econometer" for the
Economics department of the [city of The Hague](http://www.denhaag.nl), hence
the name of the package.

## Dutch insolvency registry

This registry publishes details on insolvency cases in the Netherlands. Once you 
have received login credentials from the registry, a typical workflow 
in R will be the following:

```r
library("econometer")
# save login credentials for this session:
cir_login(username = "foo", password = "bar")
# retrieve the identifier of all publications in the last 4 weeks:
publications <- cir_searchByDate(Sys.Date() - 28, court = "Den Haag")
# retrieve details of each publication:
cases <- cir_getCase(publications)
# convert the list of XML objects in 'cases' to a data.frame:
cases.df <- cir_extractCaseDetails(cases)
```

The `cir_searchByDate` and `cir_getCase` implement the registry (SOAP) commands 
with the same name. Note that the `cir_searchByDate()` searches by court. A list 
of courts in the Netherlands is returned by `cir_getCourtNames()`.

## Dutch registry with energy certificates for buildings

The acronym *EPBD* seems to stem from the European [Energy Performance of Buildings Directive](https://ec.europa.eu/energy/en/topics/energy-efficiency/buildings).

Once you have received login credentials from the registry, a typical workflow 
in R will be the following:

```r
library("econometer")
# save login credentials for this session:
epbd_login(username = "foo", password = "bar")
# download all certificates in The Hague at the start of the current month into a data frame:
certificates <- epbd_downloadCertificates(postcode.min = 2491, postcode.max = 2599)
# or download all new certificates published yesterday:
certificates <- epbd_getLabelMutations(Sys.Date() - 1)
```

## Shapefiles

The shapefiles for *Buurten*, *Wijken* and *Stadsdelen* are provided by Antoine 
Gribnau from the gemeente Den Haag as the shapefiles on their
[geo-portal](http://geoportaal.ddh.opendata.arcgis.com/) 
have been tranformed to WGS84, but with a substantial error.

These shapefiles now line up exactly with the
[Wijk and buurtkaart](http://www.cbs.nl/nl-NL/menu/themas/dossiers/nederland-regionaal/publicaties/geografische-data/archief/2016/wijk-en-buurtkaart-2015.htm)
from the CBS.

# License

The code in this package is released under the MIT license, see [LICENSE](LICENSE).

