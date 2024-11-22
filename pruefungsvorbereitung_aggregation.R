#' ---
#' author: 'Jan Vanhove'
#' title: 'Prüfungsvorbereitung Datenaggregation'
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

library(here)
library(tidyverse)
# Uses ";" between cells but "." as decimal separator.
d <- read_delim(here("aufgaben_pruefung", "daten_vrl.csv"),
                delim = ";")

d$bestand <- as.numeric(d$bestand)
d$bestand[is.na(d$bestand)] <- 0
d$praemie[is.na(d$praemie)] <- 0
d$ra[!is.na(d$ra)] <- 0

d$mediflag <- as.character(d$mediflag)
d$mediflag[is.na(d$mediflag)] <- "n"

d$todesjahr <- as.character(d$todesjahr)
d$todesjahr[is.na(d$todesjahr)] <- "n"

d_per_vers <- d |> 
  group_by(vnr, sex, gebj, alter, ag, agr, einjahr, ausjahr, verweildauer, todesjahr,
           franchise, model, unfc, spitalflag, mediflag, behj) |> 
  summarise(
    bestand = sum(bestand),
    praemie = sum(praemie),
    ra = sum(ra),
    betrag = sum(betrag),
    kobe = sum(kobe),
    netto = sum(netto),
    belege = sum(belege),
    .groups = "drop"
  ) |> 
  arrange(vnr, behj)
d_per_vers |> write_csv("vrl_per_vers.csv")
