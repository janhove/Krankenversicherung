#' # Aufgabenserie Rückstellungen
#' 
#' ## Rückstellungsberechnung mit Chain-Ladder
#' Beobachtungen einlesen:

M <- rbind(
  c(58000, 276100, 111700, 20400),
  c(NA,    100900, 480319, 194319),
  c(NA,        NA, 120800, 575050),
  c(NA,        NA,     NA, 130900)
)

#' (a) Berechnen Sie den Rückstellungsbedarf mit 
#' Hilfe des Chain-Ladder Verfahrens.
#' 
#' Zuerst ein paar Hilfefunktionen definieren,
#' die zusammen den Chain-Ladder-Algorithmus bilden.
#' Dieser Algorithmus wird auf S. 150ff. erklärt.

shift_row_left <- function(row) {
  if (!is.na(row[[1]])) {
    return(row)
  }
  shift_row_left(c(row[-1], NA))
}
shift_left <- function(M) {
  for (i in 1:nrow(M)) {
    M[i, ] <- shift_row_left(M[i, ])
  }
  M
}
cum_row <- function(M) {
  apply(M, 1, cumsum) |> t()
}
# Formel S. 151
phi <- function(S) {
  ell <- ncol(S)
  phis <- rep(NA, ell)
  for (i in 2:ell) {
    phis[i] <- sum(S[1:(ell-i+1), i]) / sum(S[1:(ell-i+1), i-1])
  }
  phis
}
# Formel S. 152
fill_in <- function(S) {
  phis <- phi(S)
  ell <- ncol(S)
  for (row in 1:ell) {
    for (col in 1:ell) {
      if (is.na(S[row, col])) {
        S[row, col] <- S[row, col - 1] * phis[col]
      }
    }
  }
  S
}
decum_row <- function(S) {
  ell <- ncol(S)
  for (row in 1:ell) {
    for (col in 2:ell) {
      S[row, col] <- S[row, col] - sum(S[row, 1:(col-1)])
    }
  }
  S
}
shift_row_right <- function(row, leading, trailing) {
  c(rep(NA, leading), row, rep(NA, trailing))
}
shift_right <- function(M) {
  ell <- ncol(M)
  M_shifted <- matrix(nrow = ell, ncol = 2 * ell - 1)
  for (i in 1:ell) {
    M_shifted[i, ] <- shift_row_right(M[i, ], i - 1, ell - i)
  }
  colnames(M_shifted) <- 1:(2*ell-1)
  rownames(M_shifted) <- 1:ell
  M_shifted
}
chain_ladder <- function(M) {
  M |> 
    shift_left() |> 
    cum_row() |> 
    fill_in() |> 
    decum_row() |> 
    shift_right()
}
#' Resultat:
(M_cl <- chain_ladder(M))

#' (b) Angenommen, in den darauf folgenden Behandlungsmonaten 5 und 6
#' fallen Leistung von total 1'000'000 bzw. 1'100'000 an. Das Abwicklungsmuster
#' bleibe dasselbe wie in den vorangehenden Monaten 1 bis 4.
#' Wie viele Leistungen werden in den Abrechnungsmonaten 5 und 6 abgerechnet.
#' 
#' **Antwort:** Berechne nochmals die $\varphi$-Werte:
(phis <- M |> shift_left() |> cum_row() |> phi())

#' Wir erwarten also, dass die Leistungen aus einem bestimmten Monat sich wie 
#' folgt auf die Folgemonate verteilen (Proportionen):
phis[1] <- 1
(props <- phis / sum(phis))

#' Folglich werden in Abrechnungsmonat 5 etwa 109'404 der 1'000'000 Kosten 
#' abgerechnet. (Total mit den Vormonaten: 1'000'666.)
props[1] * 1e6

#' In Abrechnungsmonat 6 fallen 750'549.1 zusätzliche Kosten an.
#' (Total mit den Vormonaten: 1'045'132.)
props[2] * 1e6 + props[1] * 11e5

#' ## Rückstellungen mit Benktander-Hovinen
#' Berechnen Sie ausgehend vom Zahlenbeispiel auf S. 154
#' die Rückstellungen gemäss Bornhuetter-Ferguson und gemäss Benktander-Hovinen.
#' 
#' **Antwort:** Tabelle 8w (S. 160) enthält bereits die kumulativen Rückstellungen.
#' Es reicht also, diese zu dekumulieren. Für Benktander-Hovinen kann man
#' den Bornhuetter-Ferguson-Algorithmus rezyklieren und lediglich die $\alpha$-Werte
#' anpassen.

S <- rbind(
  c(0,       0,    0,    0,    0, 3483),
  c(0,       0,    0,    0, 3844, 4043),
  c(0,       0,    0, 3977, 4393, 4624),
  c(0,       0, 3880, 4729, 5238, 5521),
  c(0,    4261, 5379, 6310, 6869, 7180),
  c(1889, 3472, 4611, 5560, 6130, 6447)
)
X <- S |> decum_row() |> shift_right()
X[2:6, 7:11]
sum(X[2:6, 7:11], na.rm = TRUE) # Gesamtrückstellungen: 9'964

#' Bornhuetter-Ferguson-Algorithmus definieren. (S. 160)
bf <- function(S, gamma, alpha) {
  ell <- ncol(S)
  for (row in 2:ell) {
    for (col in (ell - row + 2):ell) {
      S[row, col] <- S[row, col - 1] + alpha[row] * (gamma[col] - gamma[col - 1])
    }
  }
  S
}
antidiag <- c(1889, 4261, 3880, 3977, 3844, 3483)
S <- diag(antidiag)[length(antidiag):1, ]
gamma <- c(0.28, 0.53, 0.71, 0.86, 0.95, 1)
alpha <- c(3520, 3980, 4620, 5660, 6210, 6330)
bf(S, gamma, alpha) # Stimmt

#' Für den Benktander-Hovinen-Algorithmus muss man lediglich die $\alpha$-Werte
#' anders berechnen, S. 166. Ich erhalte die 
bh <- function(S, gamma, alpha) {
  alpha_bf <- bf(S, gamma, alpha)[, ncol(S)]
  bf(S, gamma, alpha_bf)
}
(S_bh <- bh(S, gamma, alpha))
X <- S_bh |> decum_row() |> shift_right()
X[2:6, 7:11]
sum(X[2:6, 7:11], na.rm = TRUE) # Gesamtrückstellungen: 10'467

#' ## Rückstellungen nach Mack's Methode
#' Berechnen Sie ausgehend vom Zahlenbeispiel auf S. 159 (Tabelle 8t)
#' die Rückstellungen nach Mack's Methode.
#' 
#' Für Macks Methode müssen wir die $\alpha$- und $\gamma$-Werte neu berechnen:
X <- rbind(
  c(1001,  854,  568, 565, 347, 148),
  c(1113,  990,  671, 648, 422,  NA),
  c(1265, 1168,  800, 744,  NA,  NA),
  c(1490, 1383, 1007,  NA,  NA,  NA),
  c(1725, 2536,   NA,  NA,  NA,  NA),
  c(1889,   NA,   NA,  NA,  NA,  NA)
)
gamma_ad <- c(0.263, 0.543, 0.709, 0.862, 0.960, 1) # S. 159

# Formel S. 158
compute_sigmas <- function(X, pis) {
  ell <- ncol(X)
  sigmas <- rep(0, ell)
  for (i in 1:ell) {
    sigmas[i] <- sum(X[1:(ell-i+1), i]) / sum(pis[1:(ell-i+1)])
  }
  sigmas
}
mack <- function(S, gamma_ad) {
  X <- S |> decum_row()
  alpha_ld <- rowSums(X, na.rm = TRUE) / rev(gamma_ad) # S. 161
  sigmas_ad <- compute_sigmas(X, alpha_ld)
  gamma_mack <- cumsum(sigmas_ad) / sum(sigmas_ad) # S. 159
  alpha_mack <- alpha_ld * sum(sigmas_ad) # S. 163; verwende alpha_ld für pi
  bf(S, gamma_mack, alpha_mack)
}
#' Resultat:
(S_mack <- X |> 
    cum_row() |> 
    mack(gamma_ad)) 
X_mack <- S_mack |> decum_row() |> shift_right()
X_mack[2:6, 7:11]
sum(X_mack[2:6, 7:11], na.rm = TRUE) # Gesamtrückstellungen: 11'696

#' ## Rückstellungsberechnung mit erweitertem Bornhuetter-Ferguson-Verfahren
#' **Chain Ladder:**
S <- rbind(
  c(100, 150, 180),
  c(NA, 120, 170),
  c(NA, NA, 110)
) |> shift_left()
gamma <- c(0.6, 0.9, 1) # gegeben
(phi_cl <- phi(S))      # S. 156
(gamma_cl <- c(rev(1/cumprod(rev(phi_cl)))[2:length(phi_cl)], 1)) # S. 157
(alpha_ld <- rev(apply(S, 1, max, na.rm = TRUE) / gamma_cl)) # S. 161
bf(S, gamma_cl, alpha_ld) |> 
  decum_row() |> 
  shift_right()

#' **Panning**

# S. 158
gamma_pa <- function(beta) {
  cumsum(beta) / sum(beta)
}
# S. 157
beta_pa <- function(X) {
  ell <- ncol(X)
  betas <- vector(length = ell)
  for (i in 1:ell) {
    betas[i] <- sum(X[1:(ell-i+1), i] * X[1:(ell-i+1), 1]) / sum(X[1:(ell-i+1), 1]^2)
  }
  betas
}
# S. 164
alpha_pa <- function(X) {
  X[, 1] * sum(beta_pa(X))
}
S |> 
  decum_row() |> 
  beta_pa() |> 
  gamma_pa()
S |> 
  decum_row() |> 
  alpha_pa()
bf(S, gamma_pa(beta_pa(decum_row(S))), alpha_pa(decum_row(S))) |> 
  decum_row() |> 
  shift_right()

#' ## Rückstellungsverfahren mit erweitertem Bornhuetter-Ferguson-Verfahren
#' Bissl zu mühsam.
#' 
#' # Kostenbeteiligung
#' 
#' ## Aufgabe 1

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
2/3*k_avg(anteil, kosten, 300, 0.1, 700) + 1/3*k_avg(anteil, kosten, 500, 0.1, 700)
#' 
#' (2) Vgl. Formeln 34--36. 
#' Mit Franchise 300 sollte man eine Prämie, die 29\% der fiktiven
#' Grundprämie $p$ beträgt, zahlen, mit Franchise 500 eine, die 11\% von $p$ beträgt.
#' Also sollte die Prämie für diejenigen mit Franchise 500 $11/29 = 38\%$ der Prämie
#' derjenigen mit Franchise 300 betragen. (Beide überschreiten 50%? Wie weiter?)
(reduktionsfaktor300 <- 1 - k_avg(anteil, kosten, 300, 0.1, 700)/gesamtkosten)
(reduktionsfaktor500 <- 1 - k_avg(anteil, kosten, 500, 0.1, 700)/gesamtkosten)

#' ## Aufgabe 2
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
#' (d) Eine Verbilligung von 36.6\% (cf. Anteil Kobe am Leistungsvolumen).
(rabatt1200 <- k_avg(anteil, kosten, 1200, 0.1, 400)/sum(anteil * kosten))

#' (e) Wie folgt. Totalkosten: 3'900'000. Gesamtkobe: 1'372'000. Kobeanteil und Rabatt: 35%.
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
(rabatt1200 <- k_avg(anteil, kosten, 1200, 0.1, 400)/sum(anteil * kosten))

#' ## Aufgabe 3
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
k_avg(d$prop_erkr, d$avg_kosten, 400, 0.1, 700) / sum(d$prop_erkr * d$avg_kosten)
#' (b) Der Kobeanteil steigt um 2 Prozentpunkte auf 18% des Leistungsvolumens.
k_avg(d$prop_erkr, d$avg_kosten, 500, 0.1, 700) / sum(d$prop_erkr * d$avg_kosten)
#' (c) Der Kobeanteil steigt um 2.5 Prozentpunkte auf 18.5% des Leistungsvolumens.
k_avg(d$prop_erkr, d$avg_kosten, 400, 0.2, 700) / sum(d$prop_erkr * d$avg_kosten)
#' (d) Der Kobeanteil senkt um einen Prozentpunkt auf 15% des Leistungsvolumens.
k_avg(d$prop_erkr, 1.1 * d$avg_kosten, 400, 0.1, 700) / sum(d$prop_erkr * 1.1 * d$avg_kosten)

#' # Risikoausgleich
#' ## Aufgabe 1
#' (a) Leistungsvolumen: `sum(bestand_A * kosten_A)` bzw. `_B`.
#' Der Ausgleich beträgt 735'778, wobei Versicherer A zahlt
#' und Versicherer B empfängt. 
#' Ich verstehe nicht, wieso der Aufwand als Summe berechnet werden sollte?
#' Vielleicht Vorzeichen ändern?
bestand_A <- c(500,200,2000,700)
bestand_B <- c(300,200,1600,1300)
kosten_A <- c(600,1200,2000,4000)
kosten_B <- c(400,1000,1800,5000)
ausgleich <- c(0, -1670,-858.889,1880)
sum(bestand_A * ausgleich)
sum(bestand_B * ausgleich)
#' (b) Verstehe das mit der Summe eben nicht, daher hier als Differenz?
(aufwand_A <- kosten_A - ausgleich)
(aufwand_B <- kosten_B - ausgleich)
(aufwand_branche <- ((kosten_A - ausgleich) * bestand_A +  (kosten_B - ausgleich) * bestand_B)/(bestand_A + bestand_B))
#' (c) Vielleicht $2770-525 = 2245$ Kinderrabatt gewähren? 
#' Jugendrabatt ist nicht sinnvoll,
#' weil diese unter dem Strich (mit der Solidarität) eh gleich viel kosten
#' wie der Rest.
#' 
#' ## Aufgabe 2
#' (a) Auf Papier gelöst. Daten sind konsistent.
#' 
#' (b) 
(leistungen_alt <- c(150000000 + 112000000, 105000000 + 330000000))
(bestand_alt <- c(100000+80000, 50000 + 150000))
(durchschnitt_alt <- leistungen_alt / bestand_alt)
(durchschnitt_alt_total <- sum(leistungen_alt) / sum(bestand_alt))
(ausgleich_alt <- durchschnitt_alt - durchschnitt_alt_total)

(leistungen_neu <- c(50000000+30000000, 100000000+82000000, 30000000+95000000, 75000000+235000000))
(bestand_neu <- c(80000+60000, 20000+20000, 40000+120000, 10000+30000))
(durchschnitt_neu <- leistungen_neu / bestand_neu)
(durchschnitt_neu_total <- sum(leistungen_neu) / sum(bestand_neu))
(ausgleich_neu <- durchschnitt_neu - durchschnitt_neu_total)

#' (c) Mit dem alten Ausgleich zahlt Kasse A 20'826'023. Kasse B erhält den
#' gleichen Betrag. Mit dem neuen Ausgleich zahlt Kasse A 29'667'293.
#' Also ist der neue Ausgleich etwa 9 Millionen besser für B.
(c(100000, 50000) * ausgleich_alt)
(c(100000, 50000) * ausgleich_alt) |> sum()
c(80000, 150000) * ausgleich_alt
(c(80000, 150000) * ausgleich_alt) |> sum()

(c(80000,20000,40000,10000) * ausgleich_neu)
(c(80000,20000,40000,10000) * ausgleich_neu) |> sum()
c(60000,20000,120000,30000) * ausgleich_neu
(c(60000,20000,120000,30000) * ausgleich_neu) |> sum()

#' ## Aufgabe 3
#' Ke Luscht :)