#' ---
#' author: 'Jan Vanhove'
#' title: 'Prüfungsvorbereitung Standardstatistiken'
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
#' Morbiditätsstatistiken auf Bruttoleistungen:
#' 
#' * Behandlungsjahr
#' * Franchisestufe (für Erwachsene)
#' * Modell
#' * Neueintritte vs. Bisherige
library(tidyverse)
library(kRnk)
d_per_vers <- read_csv("vrl_per_vers.csv")

#' Nach Behandlungsjahr

d_per_vers |> 
  ggplot(aes(x = alter,
             colour = sex,
             y = betrag)) +
  geom_smooth(se = FALSE) +
  # scale_colour_manual(name = "Geschlecht") +
  facet_wrap(facets = vars(behj)) +
  xlab("Alter") +
  ylab("Durchschnittliche Leistung/Versicherte(n)") +
  labs(
    title = "Morbiditätsstatistik je nach Behandlungsjahr",
    caption = "Trendlinie mit LOESS"
  )
  
d_per_vers |> 
  filter(agr != "K") |> 
  ggplot(aes(x = alter,
             colour = sex,
             y = betrag)) +
  geom_smooth(se = FALSE) +
  facet_wrap(facets = vars(franchise)) +
  xlab("Alter") +
  ylab("Durchschnittliche Leistung/Versicherte(n)") +
  labs(
    title = "Morbiditätsstatistik je nach Franchise",
    caption = "Trendlinie mit LOESS"
  )

d_per_vers |> 
  # filter(agr != "K") |> 
  ggplot(aes(x = alter,
             colour = sex,
             y = betrag)) +
  geom_smooth(se = FALSE) +
  facet_wrap(facets = vars(model)) +
  xlab("Alter") +
  ylab("Durchschnittliche Leistung/Versicherte(n)") +
  labs(
    title = "Morbiditätsstatistik je nach Modell",
    caption = "Trendlinie mit LOESS"
  )

d_per_vers |> 
  mutate(neueintritt = ifelse(einjahr == behj, "ja", "nein")) |> 
  ggplot(aes(x = alter,
             colour = sex,
             y = betrag)) +
  geom_smooth(se = FALSE) +
  facet_wrap(facets = vars(neueintritt)) +
  xlab("Alter") +
  ylab("Durchschnittliche Leistung/Versicherte(n)") +
  labs(
    title = "Morbiditätsstatistik je nach Neueintritt oder nicht",
    caption = "Trendlinie mit LOESS"
  )

#' Interpretation?
#' 
#' # Aufgabe 2
#' Lorenzkurve

d_per_vers |> 
  group_by(behj, sex) |> 
  arrange(betrag) |> 
  mutate(n = 1:n()) |> 
  mutate(
    cumprop_n = cumsum(n) / sum(n),
    cumprop_leis = cumsum(betrag) / sum(betrag)
  ) |> 
  ggplot(aes(x = cumprop_n,
             y = cumprop_leis,
             linetype = sex)) +
  geom_line() +
  scale_linetype_discrete(
    name = "Geschlecht",
    labels = c("Männer", "Frauen")
  ) +
  xlab("Anteil Erkrankter") +
  ylab("Anteil Kosten") +
  facet_wrap(facets = vars(behj)) +
  ggtitle("Lorenzkurve") +
  theme(legend.position = "bottom")
