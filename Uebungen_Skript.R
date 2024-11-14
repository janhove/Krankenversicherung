#' ---
#' author: 'Jan Vanhove'
#' title: 'Übungen Skript'
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
#' # Übung 1
#' Mit $B$ der Anzahl Belege pro Versicherten ($\mathbb{E}(B) = 7$) gilt
#' \begin{align*}
#'  \mathbb{E}(s)
#'  &= \mathbb{E}(s_i) \\
#'  &= \mathbb{E}(\mathbb{E}(s_i | B)) \\
#'  &= \mathbb{E}(300B) \\
#'  &= 2100.
#' \end{align*}
#' Weiter
#' \begin{align*}
#'  \textrm{Var}(s)
#'  &= \textrm{Var}(s_i) \\
#'  &= \textrm{Var}(s_i | B))
#'      + \mathbb{E}(\textrm{Var}(s_i | B)) \\
#'  &= \textrm{Var}(300B) + \mathbb{E}(100B) \\
#'  &= 300^2 \cdot \textrm{Var}(B) + 700.
#' \end{align*}
#' Ich gehe davon aus, dass $B \equiv 7$ gemeint ist (statt $\textrm{Var}(B) > 0$). 
#' Sonst ergibt der Rest nicht wahnsinnig viel Sinn.
#' 
#' Tschebyscheff: Setze $k = 1/\sqrt{20}$ und erhalte

k <- 1 / sqrt(20)
L <- 50000
L * 2100 + c(-1, 1) * k * sqrt(L) * 700

#' Also 
#' 
#' $$\mathbb{P}\left(S \ni [104965000, 105035000]\right) \geq 0.95.$$
#' 
#' Mit einer Normalverteilung: $\mathbb{E}(S) = L\mathbb{E}(s) = 50000 \cdot 2100$
#' und
#' 
#' \begin{align*}
#'  \textrm{Var}(S) 
#'  &= \textrm{Var}\left(\sum_{i=1}^{L} s_i\right) \\
#'  &= \sum_{i=1}^L \textrm{Var}(s_i) \\
#'  &= 700 \cdot L.
#' \end{align*}

qnorm(c(0.025, 0.975), L * 2100, sqrt(700*L))

#' Also 
#' 
#' $$\mathbb{P}\left(S \ni [104988405, 105011595]\right) = 0.95.$$
#' 
#' # Übung 2

library(tidyverse)
theme_set(theme_bw(12))
d <- read_table("uebung_02.txt")

# Je nachdem, ob wir Proportionen oder Dichten brauchen:
d |> 
  ggplot(aes(x = leis,
             # y = after_stat(density))) +
             y = after_stat(count / sum(count)))) + 
  geom_histogram(
    fill = NA,
    col = "black",
    breaks = seq(0, max(d$leis), by = 50)
    ) +
  xlab("Leistungen pro Erkrankten") +
  # ylab("Wsk.-Dichte")
  ylab("Proportion")

d |> 
  ggplot(aes(x = leis,
             y = after_stat(density))) +
  geom_histogram(
    fill = NA,
    col = "black",
    breaks = seq(0, max(d$leis), by = 50)
  ) +
  facet_wrap(facets = vars(sex)) +
  xlab("Leistungen pro Erkrankten") +
  ylab("Wsk.-Dichte")

d |> 
  ggplot(aes(x = leis,
             linetype = sex)) +
  stat_ecdf() +
  scale_linetype_discrete(
    name = "Geschlecht",
    labels = c("Frauen", "Männer")
  ) +
  xlab("Leistung pro Erkrankten") +
  ylab("kumulative Wsk.\n(Anteil Erkrankter)")

d |> 
  ggplot(aes(x = leis,
             linetype = sex)) +
  stat_ecdf() +
  scale_x_log10() +
  scale_linetype_discrete(
    name = "Geschlecht",
    labels = c("Frauen", "Männer")
  ) +
  xlab("Leistungen pro Erkrankten") +
  ylab("kumulative Wsk.\n(Anteil Erkrankter)") +
  labs(caption = "x-Achse log-transformiert")

d |> 
  group_by(sex) |> 
  arrange(leis) |> 
  mutate(cumprop_leis = cumsum(leis) / sum(leis)) |> 
  ggplot(aes(x = leis, y = cumprop_leis,
             linetype = sex)) +
  geom_line() +
  scale_linetype_discrete(
    name = "Geschlecht",
    labels = c("Frauen", "Männer")
  ) +
  xlab("Leistungen pro Erkrankten") +
  ylab("Kumulativer Anteil Kosten")

d |> 
  group_by(sex) |> 
  arrange(leis) |> 
  mutate(cumprop_leis = cumsum(leis) / sum(leis)) |> 
  ggplot(aes(x = leis, y = cumprop_leis,
             linetype = sex)) +
  geom_line() +
  scale_x_log10() +
  scale_linetype_discrete(
    name = "Geschlecht",
    labels = c("Frauen", "Männer")
  ) +
  xlab("Leistungen pro Erkrankten") +
  ylab("Kumulativer Anteil Kosten") +
  labs(caption = "x-Achse log-transformiert")

d |> 
  group_by(sex) |> 
  arrange(leis) |> 
  mutate(n = 1:n()) |> 
  mutate(
    cumprop_n = cumsum(n) / sum(n),
    cumprop_leis = cumsum(leis) / sum(leis)
    ) |> 
  ggplot(aes(x = cumprop_n,
             y = cumprop_leis,
             linetype = sex)) +
  geom_line() +
  scale_linetype_discrete(
    name = "Geschlecht",
    labels = c("Frauen", "Männer")
    ) +
  xlab("Anteil Erkrankter") +
  ylab("Anteil Kosten") +
  ggtitle("Lorenzkurve") +
  theme(legend.position = "bottom")

#' Leistungszunahme von 10%:
#' Einfach überall `leis` durch `1.1*leis` ersetzen.

d |> 
  ggplot(aes(x = 1.1*leis,
             y = after_stat(density))) +
  geom_histogram(
    fill = NA,
    col = "black",
    breaks = seq(0, max(1.1*d$leis), by = 50)
  ) +
  xlab("Leistungen pro Erkrankten") +
  ylab("Wsk.-Dichte")

d |> 
  ggplot(aes(x = 1.1*leis,
             y = after_stat(density))) +
  geom_histogram(
    fill = NA,
    col = "black",
    breaks = seq(0, max(1.1*d$leis), by = 50)
  ) +
  facet_wrap(facets = vars(sex)) +
  xlab("Leistungen pro Erkrankten") +
  ylab("Wsk.-Dichte")

d |> 
  ggplot(aes(x = 1.1*leis,
             linetype = sex)) +
  stat_ecdf() +
  ylab("kumulative Wsk.\n(Anteil Erkrankter)") +
  ylab("kumulative Wsk.")

d |> 
  ggplot(aes(x = 1.1*leis,
             linetype = sex)) +
  stat_ecdf() +
  scale_x_log10() +
  xlab("Leistungen pro Erkrankten") +
  ylab("kumulative Wsk.\n(Anteil Erkrankter)") +
  labs(caption = "x-Achse log-transformiert")

d |> 
  group_by(sex) |> 
  arrange(leis) |> 
  mutate(n = 1:n()) |> 
  mutate(
    cumprop_n = cumsum(n) / sum(n),
    cumprop_leis = cumsum(1.1*leis) / sum(1.1*leis)
  ) |> 
  ggplot(aes(x = cumprop_n,
             y = cumprop_leis,
             linetype = sex)) +
  geom_line() +
  scale_linetype_discrete(
    name = "Geschlecht",
    labels = c("Frauen", "Männer")
  ) +
  xlab("Anteil Erkrankter") +
  ylab("Anteil Kosten") +
  ggtitle("Lorenzkurve") +
  theme(legend.position = "bottom")

#' # Übung 3
#' Die Warnungen betreffen trailing spaces.
d <- read_table("uebung_03.txt")
d |> 
  ggplot(aes(x = apoth,
             y = sd,
             label = kt)) +
  geom_text() +
  xlab("Anzahl Apotheken pro 1'000 Einw.") +
  ylab("Anteil Medikamentenkosten")

d <- d |> 
  mutate(
    region = case_when(
      kt %in% c("FR", "GE", "JU", "NE",
                "TI", "VD", "VS") ~ "lateinische Schweiz",
      .default = "Deutschschweiz"
    )
  )

d |> 
  ggplot(aes(x = apoth,
             y = sd)) +
  geom_text(aes(label = kt)) +
  facet_wrap(facets = vars(region)) +
  geom_smooth(method = "lm") +
  xlab("Anzahl Apotheken pro 1'000 Einw.") +
  ylab("Anteil Medikamentenkosten")

ueb3.lm <- lm(sd ~ region * apoth, d)
par(mfrow = c(2, 2))
plot(ueb3.lm)
par(mfrow = c(1, 1))

#' Abgesehen von Aargau (1) und Basel-Stadt (6)
#' ist der Fit noch recht okay.
#' 
#' # Restliche Übungen
#' Noch nicht gemacht.