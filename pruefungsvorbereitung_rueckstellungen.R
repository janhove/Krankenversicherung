#' ---
#' author: 'Jan Vanhove'
#' title: 'Prüfungsvorbereitung Rückstellungen'
#' output: 
#'  html_document:
#'    toc: true
#'    toc_float: true
#'    number_sections: true
#'    theme: sandstone
#'    highlight: tango
#'    dev: svg
#'    df_print: paged
#' ---
#' 
#' # Aufgabe 1
#' _Erstellen Sie aus dem Datensatz ein Abwecklungsdreieck zu den Nettoleistungen (netto)._

library(here)
library(tidyverse)
library(kRnk) # eigenes R-Package

# Uses ";" between cells but "." as decimal separator.
d <- read_delim(here("aufgaben_pruefung", "daten_vrl.csv"),
                delim = ";")
(X <- abw3eck(d, netto, behj, abrj))
X <- shift_left(X)
S <- X |> to_S() |> shift_left()

#' # Aufgabe 2
#' _Rückstellungen nach Chain Ladder und erweitertem Bornhuetter-Ferguson-Verfahren._
#' _Volumenmass $\pi$ aus Datensatz gewinnen._
#' 
#' ## Volumenmass
#' Wir nehmen das Prämienvolumen pro Jahr.
#' Unklar ist, ob auch noch a priori $\alpha$- und 
#' $\gamma$-Schätzungen verwendet werden sollten.
volumen <- tapply(d$praemie, d$behj, sum, na.rm = TRUE)
#' ## Chain ladder
#' Verwende die Funktionen `chain_ladder()`
#' und `rueckstellung()` aus dem `kRnk`-Package.
S |> chain_ladder() |> rueckstellung()
zsf <- tibble(
  Methode = "Chain ladder",
  Rückstellungsbedarf = S |> 
    chain_ladder() |> 
    rueckstellung()
)
#' ## Additives Verfahren
#' Verwende die Funktion `additiv()`.
S |> additiv(pis = volumen) |> rueckstellung()
zsf <- zsf |> 
  add_row(Methode = "Additives Verfahren (bf)",
          Rückstellungsbedarf = S |> additiv(pis = volumen) |> rueckstellung())

#' ## Panning-Verfahren
S |> panning() |> rueckstellung()
zsf <- zsf |> 
  add_row(Methode = "Panning (bf)",
          Rückstellungsbedarf = S |> panning() |> rueckstellung())
#' # Aufgabe 3
#' _Notwendige Rückstellungen berechnen._
#' 
#' Mit etwa 236'000 ist man auf der sicheren Seite.
zsf
