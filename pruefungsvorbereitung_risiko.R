#' ---
#' author: 'Jan Vanhove'
#' title: 'Prüfungsvorbereitung Risikotheoretischer Ansatz'
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
#' _Ermitteln Sie aus den aggregierten Daten für jedes der 4 Behandlungsjrahre die Anzahl der Versicherten, die Anzahl der Erkrankten, die Erkrankungswsk. Nimm an, die Anzahl $K$ der Erkrankten sei biniomialverteilt. Berechne die Varianz von $K$._
library(tidyverse)
d_per_vers <- read_csv("vrl_per_vers.csv")
d1 <- d_per_vers |> 
  group_by(behj) |> 
  summarise(
    n_vers = n(),
    n_krank = sum(belege == 0),
    p_krank = n_krank / n_vers,
    var_krank = n_vers * p_krank * (1 - p_krank)
  )
d1
#' # Aufgabe 2
#' _Berechne (Schätze?) Erwartungswert, Varianz der Einzelleistungen pro Erkrankten._
d2 <- d_per_vers |> 
  filter(belege > 0) |> 
  group_by(behj) |> 
  summarise(
    mittel_leistung = mean(betrag),
    var_leistung = var(betrag)
  )
d2
d_jahr <- full_join(d1, d2)  

#' # Aufgabe 3
#' _Berechne (Schätze?) Erwartungswert, Varianz, Variationskoeffizienten der Gesamtleistungen pro Jahr._
d_jahr <- d_jahr |> 
  mutate(
    ErwartungswertS = n_krank * mittel_leistung,
    VarianzS = var_krank * mittel_leistung^2 + n_krank * var_leistung,
    VarKoeffS = ErwartungswertS / sqrt(VarianzS)
  )
d_jahr |> 
  select(behj, ErwartungswertS, VarianzS, VarKoeffS)

#' # Aufgabe 4
#' Gammaverteilungen haben Parameter $\mu$ (shape) und $\gamma$ (scale).
#' Dabei gilt
#' 
#' $$\mu = \textrm{Varianz}/\textrm{Erwartungswert}$$
#' 
#' und
#' 
#' $$\gamma = \textrm{Erwartungswert}^2/\textrm{Varianz}.$$
#' 
#' Verwende die Stichprobenschätzungen:
d_jahr <- d_jahr |> 
  mutate(
    mu = VarianzS / ErwartungswertS,
    gamma = ErwartungswertS^2 / VarianzS
  )
d_jahr |> select(behj, mu, gamma)
#' # Aufgabe 5
#' _Value at Risk und Expected Shortfall._
#' 
#' Diese Konzepte wurden noch nicht besprochen, aber
#' siehe Skript S. 236--237.
#' 
#' Frage: Sollten wir auch noch berücksichtigen,
#' dass die Abrechnungen für 2015, 2016 noch nicht komplett sind?

expected_shortfall <- function(alpha, qdist, rdist, ..., B = 1e6) {
  VaR <- qdist(alpha, ...)
  sim_data <- rdist(B, ...)
  mean(sim_data[sim_data > VaR])
}

alpha <- 0.95
d_jahr <- d_jahr |> 
  mutate(
    ValueAtRisk = qgamma(alpha, shape = mu, scale = gamma)#,
    # ExpectedShortfall = expected_shortfall(alpha, qgamma, rgamma, shape = mu, scale = gamma)
  )
ExpectedShortfall <- vector(length = nrow(d_jahr))
for (i in 1:length(ExpectedShortfall)) {
  ExpectedShortfall[[i]] = expected_shortfall(alpha, qgamma, rgamma, shape = d_jahr$mu[[i]], scale = d_jahr$gamma[[i]])
}
d_jahr$ExpectedShortfall <- ExpectedShortfall
d_jahr |> select(behj, ValueAtRisk, ExpectedShortfall)