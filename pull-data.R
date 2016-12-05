#De nødvendige biblioteker
library(RCurl)
library(XML)
library(rvest)

#Hvor ligger tingene:
personale_tal_url <- 'http://tal.ku.dk/c_personale/'
stud_tal_url <- 'http://us.ku.dk/studiestatistik/studiestatistikker/bestand/KU_BESTAND_2016.pdf'

#Henter den rå side med tal for personale
pers_tal_raw <- read_html(personale_tal_url)

#Trækker de relevante tekststrenge ud:
pers_tal <- html_nodes(pers_tal_raw,"div") %>%
  html_nodes("table") %>%
  html_nodes("strong") %>%
  html_text

#Og så de helt rigtige af dem:
VIP <- pers_tal[2]
DVIP <- pers_tal[4]
TAP <- pers_tal[6]

#Vi fjerner et kedeligt punktum midt i det hele:
VIP <- gsub('\\.','',VIP)
DVIP <- gsub('\\.','',DVIP)
TAP <- gsub('\\.','',TAP)

#Og konverterer det fra tekst til tal:
VIP <- as.numeric(VIP)
DVIP <- as.numeric(DVIP)
TAP <- as.numeric(TAP)

#Herefter er det trivielt at finde ud af om fordelingen af personaletyper blandt vores 
#respondenter er repræsentativ.

#Og så til de studerende. KU er nogle snøbler. De har lagt tallene i en pdf. 
#Så den skal vi starte med at have ekstraheret.

#Vi introducerer dette bibliotek:
library(pdftools)

#Så downloader vi filen
download.file(stud_tal_url, "destfile.pdf", mode="wb")

#Og konverterer den til text. 
txt <- pdf_text("destfile.pdf")

#vi får det ud som en masse skrammel. Men. Hver linie i tabellen afsluttes med \r\n.

lines <- strsplit(txt[1],"\r\n")

#Nej hvor ser det pænt ud!

# I første omgang er vi egentlig bare interesseret i fordelingen mellem kønnene. 
#Men egentlig kan vi jo ligeså godt trække det hele ud. Det kan være vi kan bruge det til
#noget andet.
#Det vi derfor har brug for er: Hum, bac, arabisk, 34, 16, 50

#lad os starte med at samle de enkelte linier.

for(i in 1:length(txt)){
  lines[i] <- strsplit(txt[i], "\r\n")
}

linier <- unlist(lines, recursive=FALSE)
str(linier)
linier <- as.list(linier)
str(linier)
linier[1] <- NULL


linier[1]

#Nu har jeg alle linierne i en variabel. Så skal de skilles ad...
#Den første linie er uinteressant.
#De næste tre kan jeg bruge som test på at jeg får de rigtige resultater senere.
#og til at øve mig på.


#Den skal vi have splittet op. linierne er karakteriseret af at der er en tekststreng.
#Så kommer der nogen tal, der også ligger som tekststrenge
#De er adskilt af mellemrum.
#Så vi skal bruge nogen regulære udtryk.
#Vi skal nok starte med at kunne finde positionen af et tal.
#Og endelig skal vi have fjernet punktummer. det så vi ovenfor hvordan man gjorde

#Alt det skriver vi en funktion til at håndtere. Og det er vist bedst at den pakkes væk
#i en funktion, for den er satme ikke køn.
datafunktion <- function(test_linie){
  regexp <- "([[:digit:]])"
  regexpr(pattern = regexp, test_linie)[1]
  teksten <- substr(test_linie,1 , regexpr(pattern = regexp, test_linie)[1]-1)
  resten <- substr(test_linie, regexpr(pattern = regexp, test_linie)[1], nchar(test_linie))
  resten <- gsub('\\.','',resten)
  reg2 <- "([[:blank:]])+"
  noget <- strsplit(resten, reg2)
  noget[[1]][2] 
  result <- list(teksten, noget[[1]][1], noget[[1]][2], noget[[1]][3])
  return(result)
}

#anyways, nu får jeg fire ting ud. Teksten, antallet af kvinder, antallet af mænd. Og summen.
total_stud_kvinder <- datafunktion(linier[1])[2]
total_stud_male    <- datafunktion(linier[1])[3]
total_stud <- as.numeric(total_stud_kvinder) + as.numeric(total_stud_male)

total_bac_kvinder <- datafunktion(linier[2])[2]
total_bac_male    <- datafunktion(linier[2])[3]
total_bac <- as.numeric(total_stud_kvinder) + as.numeric(total_stud_male)

total_cand_kvinder <- datafunktion(linier[3])[2]
total_cand_male    <- datafunktion(linier[3])[3]
total_cand <- as.numeric(total_stud_kvinder) + as.numeric(total_stud_male)


#Så vil jeg godt af med de første tre linier, og de sidste tre.

udsnit <- linier[4:(length(linier)-3)]

udsnit <- linier

#OK. Nu vil jeg så gå dem igennem linie for linie.
#Jeg vil lave en tabel med følgende struktur:
#streng1   streng2  streng3  talK talM talT
#Når jeg kommer til en linie, ser jeg om den indeholder strengen "fakultet". Hvis den gør
#det, sættes streng1 = tekststregen.
#Hvis den indeholder strengen "bachelor", sættes streng2 = bachelor
#Hvis den indeholder strengen "kandidat", sættes streng2 = kandidat
#Herefter skrives der følgende til tabellen:
#De ovenfor definerede streng1 og streng2. Streng3 er lig tekststrengen. tallene giver sig selv.

#For hver linie
#hvis fakultet indgår i teksten, streng1 = teksten. Ellers uændret
#Hvis bachelor indgår i teksten, streng2 = bachelor. Ellers uændret
#hvis kandidat indgår i teksten, streng2 = kandidat. Ellers uændret

#vær her obs på, at det skal være kandidat med stort K. Faktisk at strengen, trimmet,
#skal være præcist bachelor eller kandidat med stort b eller k.
#Der er en uddannelse der hedder noget med kandiat nemlig.


#Tre funktioner kunne være nyttige.

test_fak <- function(streng){
  regexp = "(Fakultet)"
  result = grepl(pattern=regexp, x=streng)
  return(result)
}

test_bac <- function(streng){
  regexp = "(Bachelor)"
  result = grepl(pattern=regexp, x=streng)
  return(result)
}

test_cand <- function(streng){
  regexp = "(Kandidat)"
  result = grepl(pattern=regexp, x=streng)  
  return(result)
}

tallene <- data.frame("Fakultet" = character(), Niveau = character(), Fag = character(), Kvinder = numeric(), Maend = numeric(), Total = numeric(), stringsAsFactors=FALSE)
colnames(tallene) <- c("Fakultet", "Niveau", "Fag", "Kvinder", "Maend", "Total")


niveau <- "Bachelor"
fakultet <- "Hele KU"
require(stringr)

for(i in 1:length(udsnit)){
  if(test_fak(datafunktion(udsnit[i])[1])){
    fakultet <- str_trim(datafunktion(udsnit[i])[1])
  }
  if(test_bac(datafunktion(udsnit[i])[1])){
    niveau <- "Bachelor"
  }
  else if(test_cand(datafunktion(udsnit[i])[1])){
    niveau <- "Kandidat"
  }
  fag <- str_trim(datafunktion(udsnit[i])[1])
  hun <- as.numeric(datafunktion(udsnit[i])[2])
  han <- as.numeric(datafunktion(udsnit[i])[3])
  tot <- as.numeric(datafunktion(udsnit[i])[4])
  
  if(identical(fag, fakultet)) {
    fag <- "Alle"
    niveau <- "Alle"
  }
  if(identical(niveau,fag)){
    fag <- "Alle"
  }
  
  tallene[nrow(tallene)+1,] <- list(fakultet,niveau,fag,hun,han,tot)
}

str(hun)
str(tallene)
#rasghu! tallene bliver nu til strenge...

#Hvis teksten indeholder strengen "fakultet", så starter vi med et ny fakultet


View(tallene)
#Nice. Nu er jeg tæt på. Jeg skal blot håndtere overgangene.
