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


## Ergebnisse
Das primäre Ergebnis der Analyse mit ICONE stellt ein interaktives Kontaktnetzwerk dar (R-Paket 'NetworkD3').

### Basis-Analyse

<b> Darstellung: </b>

Im Zentrum des Netzwerks steht der Fall im Fokus (schwarz, blaue Umrandung). 

Jeder Kontakt, der zu einer Infektion geführt hat, ist mit dem Fall im Fokus durch eine graue Kante verbunden. Die Dicke der Kante korreliert mit der Länge der Kontaktzeit. Die Größe der Knoten korreliert mit der Länge der Aufenthaltsdauer auf der gewählten Fachabteilung, bzw. Station. 

Wird eine Kettenlänge >1 gewählt, werden auch für Kontakte 1. Grades weitere Kontakte analysiert, die mutmaßlich zu einer Weitergabe der Infektion entsprechend der Vorgaben geführt haben (Latenzzeit, minimale Expositionszeit, infektiöse Phase); und so weiter für höhere Grade. Der Grad der Infektion/Kettenlänge ist farblich kodiert. Ein hellerer Farbton signalisiert einen höheren Grad.

Ist ein Knoten schwarz umrandet, zeigt dies an, dass der Fall mindestens eine weitere Fachabteilung, bzw. Station während seiner infektiösen Phase besucht hat. Folglich besteht das Risiko einer Ausbreitung der Infektion auf andere Abteilungen. 

Hat der Fall im Fokus während seiner infektiösen Phase eine (oder mehrere) weitere Fachabteilungen, bzw. Stationen besucht, wird für jede weitere Abteilung ein weiterer schwarzer Knoten dargestellt. Dieser ist mit dem zentralen Knoten durch eine schwarze Kante verbunden. Die weiteren Knoten unterscheiden sich farblich von den Knoten der Abteilung im Fokus (erste weitere Abteilung orange, zweite grün, dritte rot etc.).

<b> Pop-up Fenster: </b>

Über jeden Knoten können Detailinformationen abgefragt werden. Klickt man auf einen Knoten, öffnet sich ein Pop-up Fenster. Dort sind Informationen gegeben zu:

* Abteilung
* Fall
* Hauptdiagnose
* Infektion
    * Am (Datum + Uhrzeit)
    * Durch (Fallnummer, Grad der Infektion)
* Ansteckend auf Abteilung
    * Von - Bis (Datum + Uhrzeit)
    * Aufenthaltsdauer
    * Fälle angesteckt
* Weitere besuchte Abteilungen während ansteckend


<b> Allgemeine Informationen: </b>

Oberhalb der Visualisierung sind Basis-Informationen zusammengefasst:

* Konfiguration der Analyse
    * Abteilung
    * ICD-Filter
    * Fall im Fokus
    * Beobachtungszeitraum
    * Latenzzeit
    * Minimale Expositionszeit
    * Infektiöse Phase
    * Maximale Länge der Infektions-Kette
* Weiterführende Informationen
    * Mutmaßlich infizierte Fälle auf Abteilung im Fokus
    * Infektionsrisiko für x/y weitere Ateilungen: Auflistung der Abteilungen + Anzahl der Verlegungen, geordnet nach Anzahl der Verlegungen (Hinweis auf größtes Risiko einer Infektionsausbreitung)

<b> Beispiel: </b>

Wählen Sie:

* Fachabteilungen
* Auswahl Fall
    * Fachabteilung im Fokus: Abteilung 1
    * Hauptdiagnose nach ICD-Code-Filtern: nein
    * Fall im Fokus: 1000001
* Auswahl Beobachtungszeitraum
    * Beginn des Beobachtungszeitraums: 2023-03-22
    * Ende des Beobachtungszeitraums: 2023-04-21
* Kontaktanalyse
    * Latenzzeit: 2 Stunden
    * Latenzzeit auch für Fall im Fokus: nein
    * Minimale Expositionszeit: 1 Stunde
    * Infektiöse Phase: 9 Tage
    * Maximale Länge der Infektionskette: 3


### Erweiterte Analyse: + weitere besuchte Abteilungen (Fall: Fokus, Länge Infektionskette: Basic-Analyse)
<b> Darstellung: </b>

Besucht der Fall im Fokus mindestens eine weitere Abteilung während der infektiösen Phase, kann die Analyse auf diese Abteilung(en) erweitert werden. Dazu wird der Graph aus der Basis-Analyse entsprechend erweitert:

Für jeden schwarzen Knoten mit nicht-blauer Umrandung werden die Kontakte auf allen weiteren besuchten Abteilungen während der infektiösen Phase bestimmt. Pro Abteilung wird eine eindeutige Farbe gewählt. Die Länge der Infektionsketten ist für alle vom Fall im Fokus besuchten Abteilungen gleich, entsprechend der Definition für die Basis-Analyse.

Für jeden neuen Kontakt auf den weiteren Abteilungen wird erneut bestimmt, ob dieser eine oder mehrere weitere Abteilungen besucht hat (schwarzer Rand).

<b> Pop-up Fenster: </b>

Analog.

<b> Allgemeine Informationen: </b>

Analog. 

Mutmaßlich infizierte Fälle auf Abteilung und Infektionsrisiko für x/y weitere Abteilungen entsprechend aktualisiert (Anzahl Verlegungen).


<b> Beispiel: </b>

Wählen Sie:

* Fachabteilungen
* Auswahl Fall
    * Fachabteilung im Fokus: Abteilung 6
    * Hauptdiagnose nach ICD-Code-Filtern: nein
    * Fall im Fokus: 1000001
* Auswahl Beobachtungszeitraum
    * Beginn des Beobachtungszeitraums: 2023-03-12
    * Ende des Beobachtungszeitraums: 2023-04-11
* Kontaktanalyse
    * Latenzzeit: 2 Stunden
    * Latenzzeit auch für Fall im Fokus: nein
    * Minimale Expositionszeit: 1 Stunde
    * Infektiöse Phase: 13 Tage
    * Maximale Länge der Infektionskette: 2
 
 Führen Sie die Basic-Analyse für das Beispiel fort. 

### Erweitere Analyse: + weitere besuchte Abteilungen (Fall: alle, Länge Infektionskette: 1)
<b> Darstellung: </b>

Hat mindestens ein Kontakt mindestens eine weitere Fachabteilung, bzw. Station besucht, kann die Analyse auf diese Abteilungen erweitert werden. Dazu wird der Graph aus der Basis-Analyse entsprechend erweitert:

Für jeden Knoten mit schwarzer Umrandung werden die Kontakte auf auf allen weiteren besuchten Abteilungen während der infektiösen Phase bestimmt. Gleiche Abteilungen, die z.B. von unterschiedlichen Fällen besucht wurden, werden dabei farblich gleich kodiert. 

Für jeden neuen Kontakt auf den weiteren Abteilung wird erneut bestimmt, ob dieser eine oder mehrere weitere Abteilungen besucht hat (schwarzer Rand).

Um die Übersichtlichkeit der Ergebnisse zu gewährleisten, ist die Länge der Infektionsketten für diese erweiteren Abteilungen auf 1 beschränkt. Werden bei der Analyse neue Risiko-Abteilungen identifiziert, wird eine erneute Analyse mit neuer Abteilung im Fokus empfohlen.

<b> Pop-up Fenster: </b>

Analog.

<b> Allgemeine Informationen: </b>

Analog. 

Mutmaßlich infizierte Fälle auf Abteilung und Infektionsrisiko für x/y weitere Abteilungen entsprechend aktualisiert (Anzahl Infetktionen + Anzahl Verlegungen).

<b> Beispiel: </b>

Führen Sie die Basis-Analyse für das oben beschriebene Beispiel fort.







