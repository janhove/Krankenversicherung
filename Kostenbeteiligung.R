#' ---
#' author: 'Jan Vanhove'
#' title: 'Aufgabenserie Kostenbeteiligung'
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

library(tidyverse)

#' # Aufgabe 1

# Formel 31, S. 184
k <- function(s, f, sb, m) {
  if (s < f) return(s)
  if (f <= s && s < f + m / sb) return(f + (s-f)*sb)
  f + m
}
# Formel 34, S. 186
k_avg <- function(e, s, f, sb, m) {
  avg <- 0
  for (i in 1:length(e)) {
    avg <- avg + e[i]*k(s[i], f, sb, m)
  }
  avg
}
kobe_anteil <- function(e, s, f, sb, m) {
  k_avg(e, s, f, sb, m) / sum(e * s)
}

#' (1) Die Gesamtkosten betragen $323K$, wo $K$ die Anzahl Erkrankter ist.
#' Alternativ, $258.4L$, wo $L$ die Anzahl Versicherter ist.

kosten <- seq(100, 1000, by = 100)
anteil <- c(0.32, 0.18, 0.13, 0.10, 0.09, 0.06, 0.05, 0.04, 0.02, 0.01) * 0.8
(gesamtkosten <- sum(kosten * anteil))

#' Es ist nicht klar, ob die Franchisewahl mit der Kostenklasse zusammenhängt oder
#' nicht. Hier gehe ich davon aus, dass gemeint ist, dass pro Kostenklasse
#' genau 2/3 die niederigere Franchise gewählt haben. Sämtliche Erkrankte
#' in den tiefsten drei Kostenklassen übernehmen die Kosten komplett.
#' In den Preisklassen 400 und 500 übernimmt 1/3 ihre Kosten komplett,
#' während diese bei 2/3 der Erkrankten 100 bzw. 200 Sfr. über der Franchise liegen.
#' Analog für die anderen Preisklassen.
(in_franchise_300 <- pmin(300, kosten))
(in_franchise_500 <- pmin(500, kosten))
#' Insgesamt beträgt der Franchise-Anteil somit $239.\bar{3}K$ oder $191.4\bar{6}L$.
sum((2/3 * in_franchise_300 + 1/3 * in_franchise_500) * anteil)

#' Nirgends betragen die Kosten überhalb der Franchise mehr als 7'000 Sfr,
#' also gilt für alle einen Selbstbehalt von 10%.
selbstbehalt_300 <- 0.1 * (kosten - in_franchise_300)
selbstbehalt_500 <- 0.1 * (kosten - in_franchise_500)
#' Insgesamt beträgt der Selbstbehalt-Anteil somit $8.3\bar{6}K$ oder $6.69\bar{3}L$.
sum((2/3 * selbstbehalt_300 + 1/3 * selbstbehalt_500) * anteil)
#' Die totale Kostenbeteiligung beträgt also $247.7K$ oder $198.16L$..
#' Der Anteil der Kostenbeteiligung ist daher $247.7/323 = 198.16/258.4 \approx 77\%$.
#' 
#' Mit der Funktion `k_avg()` kommt man zum gleichen Resultat:
# Kosten
k_avg(anteil, kosten, Inf, 1, 0)
# Selbstbehalt
2/3 * k_avg(anteil, kosten, 300, 0.1, 700) + 1/3 * k_avg(anteil, kosten, 500, 0.1, 700)
2/3 * kobe_anteil(anteil, kosten, 300, 0.1, 700) + 1/3 * kobe_anteil(anteil, kosten, 500, 0.1, 700)

#' (2) Vgl. Formeln 34--36. 
#' Mit Franchise 300 sollte man eine Prämie, die 29\% der fiktiven
#' Grundprämie $p$ beträgt, zahlen, mit Franchise 500 eine, die 11\% von $p$ beträgt.
#' Also sollte die Prämie für diejenigen mit Franchise 500 $39\%$ der Prämie
#' derjenigen mit Franchise 300 betragen. (Beide überschreiten 50%? Wie weiter?)
(reduktionsfaktor300 <- 1 - k_avg(anteil, kosten, 300, 0.1, 700)/gesamtkosten)
(reduktionsfaktor500 <- 1 - k_avg(anteil, kosten, 500, 0.1, 700)/gesamtkosten)
reduktionsfaktor500 / reduktionsfaktor300

#' # Aufgabe 2
#' Zur Kontrolle:
kosten <- c(500, 2000, 5000, 10000)
anteil <- c(0.2, 0.4, 0.3, 0.1)
k_avg(anteil, kosten, 0, 0.1, 400) / sum(anteil * kosten)
#' (a, b, c) Erhalte folgende Tabelle. Der Anteil der Kobe am gesamten Leistungsvolumen beträgt
#' 36.6\%.
anzahl <- anteil * 1000
franchise <- pmin(1200, kosten)
selbstbehalt <- pmin(0.1*(kosten - franchise), 400)
kobe_pro_kopf <- franchise + selbstbehalt
kosten_total <- kosten * anzahl
kobe_total <- kobe_pro_kopf * anzahl
tibble(
  Kosten = kosten,
  Anzahl = anzahl,
  Franchise = franchise,
  Selbstbehalt = selbstbehalt,
  `Kobe pro Erkrankten` = kobe_pro_kopf,
  `Gesamtkosten` = kosten_total,
  `Gesamtkobe` = kobe_total
) |> knitr::kable()
sum(anzahl * kosten) # Gesamtkosten
sum(kobe_total) # Gesamtkobe
k_avg(anteil, kosten, 1200, 0.1, 400) / sum(anteil * kosten)

#'
#' (d) Verglichen mit einer Grundprämie $p$ (fiktive Variante ohne Selbsterhalt) sollen
#' die Prämien 92% bzw. 63% betragen. Also gewährt man die Variante mit Franchise 1200
#' einen Rabatt von 32% verglichen zur Variante mit Franchise 0.
(reduktionsfaktor0 <- 1 - k_avg(anteil, kosten, 0, 0.1, 400) / sum(anteil * kosten))
(reduktionsfaktor1200 <- 1 - k_avg(anteil, kosten, 1200, 0.1, 400) / sum(anteil * kosten))
1 - reduktionsfaktor1200 / reduktionsfaktor0

#' (e) Wie folgt. Totalkosten: 3'900'000. Gesamtkobe: 1'372'000. Kobeanteil: 35%. Rabatt: 30%.
kosten <- kosten + 500
anzahl <- anteil * 1000
franchise <- pmin(1200, kosten)
selbstbehalt <- pmin(0.1*(kosten - franchise), 400)
kobe_pro_kopf <- franchise + selbstbehalt
kosten_total <- kosten * anzahl
kobe_total <- kobe_pro_kopf * anzahl
tibble(
  Kosten = kosten,
  Anzahl = anzahl,
  Franchise = franchise,
  Selbstbehalt = selbstbehalt,
  `Kobe pro Erkrankten` = kobe_pro_kopf,
  `Gesamtkosten` = kosten_total,
  `Gesamtkobe` = kobe_total
) |> knitr::kable()
sum(anzahl * kosten) # Gesamtkosten
sum(kobe_total) # Gesamtkobe
k_avg(anteil, kosten, 1200, 0.1, 400) / sum(anteil * kosten)
(reduktionsfaktor0 <- 1 - k_avg(anteil, kosten, 0, 0.1, 400) / sum(anteil * kosten))
(reduktionsfaktor1200 <- 1 - k_avg(anteil, kosten, 1200, 0.1, 400) / sum(anteil * kosten))
1 - reduktionsfaktor1200 / reduktionsfaktor0

#' # Aufgabe 3
decum <- function(x) {
  x - c(0, x[-length(x)])
}
d <- read_table("serie-241108_aufgabe-3.txt") |> 
  mutate(
    cum_prop_erkr = as.numeric(sub("%", "", anteil_erkr))/100,
    prop_erkr = decum(cum_prop_erkr),
    avg_kosten = case_when(
      kostenklasse == 40000 ~ 40000,
      .default = (2*kostenklasse + 100)/2
    )) 

#' (a) Der Kobeanteil beträgt 16% des Leistungsvolumens.
kobe_anteil(d$prop_erkr, d$avg_kosten, 400, 0.1, 700)
#' (b) Der Kobeanteil steigt um 2 Prozentpunkte auf 18% des Leistungsvolumens.
kobe_anteil(d$prop_erkr, d$avg_kosten, 500, 0.1, 700)
#' (c) Der Kobeanteil steigt um 2.5 Prozentpunkte auf 18.5% des Leistungsvolumens.
kobe_anteil(d$prop_erkr, d$avg_kosten, 400, 0.2, 700)
#' (d) Der Kobeanteil senkt um einen Prozentpunkt auf 15% des Leistungsvolumens.
kobe_anteil(d$prop_erkr, 1.1 * d$avg_kosten, 400, 0.1, 700)