#-------------------------------------------------------------------------------
# EpbdParser
#
# Python programma waarmee het totaalbestand van EPBD kan worden omgezet van
# xml naar csv. Het programma kan worden aangepast door de waarde InputFile en
# OutputFile naar de gewenste bestanden te laten wijzen.
#
# Voor het draaien van het programma is Python 2.7 nodig.
#
# Het programma dient als voorbeeld voor hoe de verwerking van zeer grote
# xml bestanden kan worden opgezet. Aan het gebruik van dit programma kunnen
# geen rechten worden ontleend.
#-------------------------------------------------------------------------------
# Aangepast door Maarten-Jan Kallen op 19 april 2016 om alleen energielabels van
# panden in Den Haag weg te schrijven. Het resultaat is een veel kleiner 
# bestand.
#-------------------------------------------------------------------------------

import sys
import xml.sax
import re

pc4 = re.compile("\d{4}")

#-------------------------------------------------------------------------------
# Configuratie
#-------------------------------------------------------------------------------
try:
    InputFile = sys.argv[1]
    OutputFile = sys.argv[2]
except IndexError:
    raise ValueError("in- of uitvoerbestand ontbreekt, gebruik 'python epbdparser.py invoer.xml uitvoer.csv'")

try:
    pc4_lower = int(sys.argv[3])
    pc4_upper = int(sys.argv[4])
except IndexError:
    raise ValueError("ongeldige invoer voor de onder- en bovengrens voor postcodes")

#-------------------------------------------------------------------------------
# EpbdErrorHandler
#-------------------------------------------------------------------------------
class EpbdErrorHandler(xml.sax.ErrorHandler):
    def error(self, exception):
        print(exception)
        pass
    def fatalError(self, exception):
        print(exception)
        pass
#-------------------------------------------------------------------------------
# EpbdContentHandler
#-------------------------------------------------------------------------------
class EpbdContentHandler(xml.sax.ContentHandler):
    Kolommen = ["PandVanMeting_postcode",
                "PandVanMeting_huisnummer",
                "PandVanMeting_huisnummer_toev",
                "PandVanMeting_bagverblijfsobjectid",
                "PandVanMeting_opnamedatum",
                "PandVanMeting_berekeningstype",
                "PandVanMeting_energieprestatieindex",
                "PandVanMeting_energieklasse",
                "PandVanMeting_gebouwklasse",
                "Meting_geldig_tot",
                "Pand_registratiedatum",
                "Pand_postcode",
                "Pand_huisnummer",
                "Pand_gebouwtype",
                "Pand_gebouwsubtype"]
    #---------------------------------------------------------------------------
    # aangeroepen bij de start van het document
    #---------------------------------------------------------------------------
    def startDocument(self):
        # gebruik het begin van het document om een csv bestand te openen en een
        # buffer aan te maken
        self.output = open(OutputFile, "w")
        # als deze vlag waar wordt dan wordt data weg geschreven
        self.isdata = False
        # deze waarde wordt gebruikt om te bepalen welk element nu verwerkt wordt
        # wordt gezet bij start element events, wordt gewist bij end element events
        self.current = ""
        # gebruik een dictionary object als buffer aangezien sommige tags niet altijd voorkomen
        self.data = {}
        for name in self.Kolommen:
            self.data[name] = ""
        buffer = ""
        # schrijf de namen van de kolommen (in de volgorde zoals ze voorkomen in de buffer)
        # als eerste regel weg in het csv bestand
        for name in self.data.iterkeys():
            buffer += name + "; "
        self.output.write(buffer + "\n")
        self.output.flush()
        pass
    #---------------------------------------------------------------------------
    # aangeroepen bij de start van een nieuwe tag
    #---------------------------------------------------------------------------
    def startElement(self, name, attrs):
        if (name == "LaatstVerwerkteMutatieVolgnummer"):
            pass
        elif (name in self.Kolommen):
            # alleen bij deze tags schrijven we data echt weg naar de csv file
            self.isdata = True
            self.current = name
        elif (name == "Pandcertificaat"):
            # begin van een nieuwe rij in het csv bestand
            # buffer opnieuw initialiseren
            self.buffer = ""
        pass
    #---------------------------------------------------------------------------
    # aangeroepen na lezen content van een tag
    #---------------------------------------------------------------------------
    def characters(self, content):
        # schrijf de waarde weg in de buffer indien het mag
        if (self.isdata):
            self.data[self.current] = content
        pass
    #---------------------------------------------------------------------------
    # aangeroepen bij het einde van een tag
    #---------------------------------------------------------------------------
    def endElement(self, name):
        if (name == "Pandcertificaat"):

            # aangepassing tbv de Haagse Econometer (alleen energielabels in
            # Den Haag wegschrijven):
            postcode = pc4.match(self.data['PandVanMeting_postcode'])
            if (postcode is None):
                postcode = pc4.match(self.data['Pand_postcode'])
            postcode = int(postcode.group(0))

            if (postcode >= pc4_lower and postcode <= pc4_upper):
                # zet alle data in een buffer
                buffer = ""
                for value in self.data.itervalues():
                    buffer += value + "; "
                # print de buffer naar het scherm (visuele controle)
                # print(buffer)
                # schrijf de buffer weg naar de csv file
                self.output.write(buffer + "\n")
                self.output.flush()
                # initialiseer de buffer opnieuw door alle waardes leeg te maken
                for name in self.data.iterkeys():
                    self.data[name] = ""
        # na sluiten van een tag altijd de current waarde leeg maken
        self.current = ""
        # na sluiten van een tag altijd de vlag voor wegschrijven van data uitzetten
        self.isdata = False
        pass
    #---------------------------------------------------------------------------
    # aangeroepen bij het einde van het document
    #---------------------------------------------------------------------------
    def endDocument(self):
        # gebruik het einde van het document om het csv bestand te sluiten
        self.output.close()
        pass

#-------------------------------------------------------------------------------
# start programma
#-------------------------------------------------------------------------------
def main():
    # parser object aanmaken
    parser = xml.sax.make_parser()
    # voeg objecten toe voor verwerking van de tags en error afhandeling
    parser.setContentHandler(EpbdContentHandler())
    parser.setErrorHandler(EpbdErrorHandler())
    # parse het bron bestand
    with open(InputFile, "rb") as f:
        src = xml.sax.xmlreader.InputSource()
        src.setByteStream(f)
        src.setEncoding("utf-8")
        parser.parse(src)
    pass

if __name__ == '__main__':
    main()
