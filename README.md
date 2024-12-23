# ICONE (deutsche Version)
Kontaktnetzwerke zur interaktiven Analyse von Infektionsketten in Krankenhäusern (Infectious diseases COntact NEtworks)

(es handelt sich um eine Erweiterung der Version von ICONE, die für das Universitätsklinikum Münster UKM, entwickelt wurde; eine englische Version ist unter sandmanns/icone verfügbar)

<p align="center">
    <img height="600" src="https://uni-muenster.sciebo.de/s/uHSv5T3rxzzWJIA/download">
</p>



## Voraussetzungen
Um ICONE zu nutzen, benötigen Sie R (Version 4.1.0 oder höher) und R Shiny.

##  Installation
Um ICONE zu installieren, laden Sie einfach das Repository herunter. Alle erforderlichen Pakete werden installiert, alle Funktionen werden automatisch mit Hilfe des global.R-Skripts geladen.

## ICONE ausführen
ICONE ist als R Shiny GUI verfügbar. Um die Software auszuführen: 1) navigieren Sie zu dem Ordner, in dem die folgenden Dateien gespeichert sind: `global.R`, `ui.R`, `server.R`, `www/UKM.png` und `www/white.png`. 2) Führen Sie Run App aus.

## Beispielhafte Nutzung von ICONE

## Input
m Analysen mit ICONE durchzuführen, muss der Input 1) eingelesen und 2) konfiguriert werden.

### Datei einlesen

* Fachabteilungen:
    * `Datei hochladen` Wählen Sie eine tabellarische Datei zum Hochladen aus, die Informationen über die Aufnahme und Entlassung von Fällen auf Fachabteilungen enthält.
    * `Trennzeichen` Wählen Sie das Feldtrennzeichen aus, das in der Eingabedatei verwendet wird (eine von: Komma, Semikolon, Tabulator).
* Stationen:
    * `Datei hochladen` Wählen Sie eine tabellarische Datei zum Hochladen aus, die Informationen über die Aufnahme und Entlassung von Fällen auf Stationen enthält.
    * `Trennzeichen` Wählen Sie das Feldtrennzeichen aus, das in der Eingabedatei verwendet wird (eine von: Komma, Semikolon, Tabulator).

Hinweis: es muss mindestens eine Datei hochgeladen werden. Eine zweite Datei, die z.B. eine feingliedrigere Verlegung von Fällen zwischen Stationen, also innerhalb einer Fachabteilungen, beschreibt, kann hochgeladen werden.

### Input konfigurieren

* `Wählen Sie die Spalte, die Informationen enthält zu...`
  * `...Fallnummer` Name der Spalte, die Informationen über die betrachteten Fälle enthält (Fall-IDs).
  * `...Fachabteilung/Station` Name der Spalte, die Informationen über die Fachabteilung, bzw. die Station enthält.
  * `...Aufnahme` Name der Spalte, die Informationen über das Aufnahmedatum des Falls auf der Fachabtielung, bzw. der Station enthält (Formatvorgabe: JJJJ-MM-TT Std:Min:Sek).
  * `...Enthlassung` Name der Spalte, die Informationen über das Aufnahmedatum des Falls auf der Fachabtielung, bzw. der Station enthält (Formatvorgabe: JJJJ-MM-TT Std:Min:Sek).
  * `...Hauptdiagnose` Name der Spalte, die Informationen zur Hauptdiagnose enthält (ICD-Kode).

      

### Beispiel

Wählen Sie `Demo Daten laden` um die simulierten Beispieldateien `Fachabteilung_V2.txt`und `Station_V2.txt` einzulesen. Beide Dateien sind im www-Ordner verfügbar und können alternativ auch per Hand eingelesen werden.

Wenn Sie die Demodaten automatisch laden, sind die Spalten zur Konfiguration des Inputs bereits korrekt gesetzt ('Fallnummer' für `Fallnummer`, 'Fachabteilung' für `Fachabteilung`, 'Station' für `Station`, 'Aufnahme' für `Aufnahme`, 'Entlassung' für `Entlassung` und 'Hauptdiagnose' für `Hauptdiagnose`. Führen Sie `Input konfigurieren` aus.


## Kontaktanalyse

Es kann zwischen Analysen pro Fachabteilung und pro Station gewechselt werden. 

### Fall im Fokus
Für jede Analyse stehen eine Fachabteilung (bzw. Station) und ein Fall auf dieser Abteilung im Fokus. Der Fall stellt das Zentrum eines Infektionsausbruchs dar. Optional können nur ausgewählte ICD-Kodes oder ICD-Kapitel betrachtet werden.

### Auswahl Beobachtungszeitraum
Für die Analyse wird ein Beobachtungszeitraum gewählt. 

Als frühestes Datum für den Beginn des Beobachtungszeitraums kann der Tag der Aufnahme des Falls im Fokus auf der Abteilung im Fokus gewählt werden. Als spätestes Datum für den Beginn des Beobachtungszeitraums kann der Tag der Entlassung des Falls im Fokus auf der Abteilung im Fokus gewählt werden. 

Als frühestes Datum für das Ende des Beobachtungszeitraums kann der für den Beginn des Beobachtungszeitraums gewählte Tag gewählt werden. Als spätestes Datum für das Ende des Beobachtungszeitraums kann der letzte Tag gewählt werden, für den in der Input Datei für die Abteilung im Fokus Daten vorliegen. 

Beachten Sie, dass bei einem großen Beobachtungszeitraum die Berechnung je nach Rechenleistung bis zu 1 Minute dauern kann.

### Kontaktanalyse
Für die Kontaktanalyse müssen 3 notwendige Parameter gesetzt werden:

* Latenzzeit: Die Zeit von der Infektion eines Falls bis dieser selbst ansteckend ist. Ist davon auszugehen, dass der Fall im Fokus sofort, ab Beginn des Beobachtungszeitraums, ansteckend ist, sollte `Latenzzeit auch für Fall im Fokus` auf inaktiv gesetzt werden. Andernfalls sollte der Parameter aktiviert werden.
* Minimale Expositionszeit: Die Zeit, die zwei Fälle in Kontakt miteinander sein müssen, damit eine Übertragung der Infektion stattfindet.
* Infektiöse Phase: Die Zeit, die ein Fall ansteckend ist.

* Maximale Länge der Infektionskette: zur Analyse kann die Länge der betrachteten Infektionsketten vorgegeben werden. Ein Wert zwischen 1 und 10, bzw. unbegrenzt kann gewählt werden.








