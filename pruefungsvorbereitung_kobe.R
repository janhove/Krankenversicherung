#' ---
#' author: 'Jan Vanhove'
#' title: 'Prüfungsvorbereitung Kostenbeteiligung'
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
#' Überprüfe die Kostenbeteiligung.
#' 
#' Differenzen von mehr als einem Rappen flaggen.
#' Solche gibt es nicht.
library(tidyverse)
library(kRnk)
d_per_vers <- read_csv("vrl_per_vers.csv")
d_per_vers$kobe_kontrolle <- NA
for (i in 1:nrow(d_per_vers)) {
  d_per_vers$kobe_kontrolle[[i]] <-
    k(d_per_vers$betrag[[i]],
      d_per_vers$franchise[[i]],
      sb = 0.1,
      m = ifelse(d_per_vers$agr[i] == "K", 350, 700))
}
d_per_vers |> 
  mutate(difference = kobe - kobe_kontrolle) |> 
  filter(abs(difference) > 0.01)
plot(kobe_kontrolle ~ kobe, data = d_per_vers)

#' # Aufgabe 2
#' Vier Varianten:
#' 
#' * Alle Franchisen um 500 Sfr. erhöhen.
#' * Erhöhung Selbstbehalt auf 20%.
#' * Erhöhung Max. Selbstbehalt auf 1000 bzw. 500 Sfr.
#' * Alle zusammen.

d_per_vers$variante1 <- NA
d_per_vers$variante2 <- NA
d_per_vers$variante3 <- NA
d_per_vers$variante4 <- NA
for (i in 1:nrow(d_per_vers)) {
  d_per_vers$variante1[[i]] <-
    k(d_per_vers$betrag[[i]],
      d_per_vers$franchise[[i]] + 500,
      sb = 0.1,
      m = ifelse(d_per_vers$agr[i] == "K", 350, 700))
  
  d_per_vers$variante2[[i]] <-
    k(d_per_vers$betrag[[i]],
      d_per_vers$franchise[[i]],
      sb = 0.2,
      m = ifelse(d_per_vers$agr[i] == "K", 350, 700))
  
  d_per_vers$variante3[[i]] <-
    k(d_per_vers$betrag[[i]],
      d_per_vers$franchise[[i]],
      sb = 0.1,
      m = ifelse(d_per_vers$agr[i] == "K", 500, 1000))
  
  d_per_vers$variante4[[i]] <-
    k(d_per_vers$betrag[[i]],
      d_per_vers$franchise[[i]] + 500,
      sb = 0.2,
      m = ifelse(d_per_vers$agr[i] == "K", 500, 1000))
}

#' Ergebnis:

d_per_vers |> 
  summarise(
    total_kobe0 = sum(kobe),
    total_kobe1  = sum(variante1),
    total_kobe2  = sum(variante2),
    total_kobe3  = sum(variante3),
    total_kobe4  = sum(variante4)
  )

#' # Aufgabe 3
#' Berechne Rabatt für Franchise 2500 vs. Franchise 300
#' über alle 4 Jahre zusammen ungeachtet des gesetzlich vorgegebenen 
#' Maximumrabatts.
#' 
#' **Frage:** Nur Erwachsene berücksichtigen oder nicht?
#' 
#' **Vorgehen:** Kobeanteil berechnen, wenn alle Franchise 300 bzw. 2500 hätten.

d_per_vers$kobe300 <- NA
d_per_vers$kobe2500 <- NA
for (i in 1:nrow(d_per_vers)) {
  d_per_vers$kobe300[[i]] <- k(d_per_vers$betrag[[i]],
                               300, 0.1, ifelse(d_per_vers$agr[[i]] == "K", 350, 700))
  d_per_vers$kobe2500[[i]] <- k(d_per_vers$betrag[[i]],
                               2500, 0.1, ifelse(d_per_vers$agr[[i]] == "K", 350, 700))
}

d_per_vers |> 
  summarise(
    kobe_anteil300 = sum(kobe300) / sum(betrag),
    kobe_anteil2500 = sum(kobe2500) / sum(betrag),
    praemie_300 = 1 - kobe_anteil300,
    praemie_2500 = 1 - kobe_anteil2500
  )

#' Wenn wir die Daten von Kindern in die Berechnung einfliessen lassen:
#' Die Prämie für Franchise 300 würde $0.897P$ betragen (mit $P$ 
#' der fiktiven Grundprämie); jene für Franchise 2500 $0.669P$.
#' Somit sollten Versicherte mit Franchise 2500
#' einen Rabatt von $1 - 0.653 / 0.890 \approx 26.6\%$ erhalten.

d_per_vers |> 
  filter(agr != "K") |> 
  summarise(
    kobe_anteil300 = sum(kobe300) / sum(betrag),
    kobe_anteil2500 = sum(kobe2500) / sum(betrag),
    praemie_300 = 1 - kobe_anteil300,
    praemie_2500 = 1 - kobe_anteil2500
  )

#' Ohne Kinder: $1 - 0.669 / 0.897 \approx 25.4\%$.
#' 
#' # Aufgabe 4
#' Obacht: Nur sehr wenige Datenpunkte!
#' Der gewährte Rabatt scheint ungefähr $1 - 3588/5005 \approx 28\%$
#' zu sein.
d_per_vers |> 
  filter(agr == "A", model == "trad", unfc == "S") |> 
  group_by(franchise) |> 
  summarise(
    n = n(),
    durchschnittsprämie = mean(praemie)
  )

