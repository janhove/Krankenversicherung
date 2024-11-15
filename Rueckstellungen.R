#' ---
#' author: 'Jan Vanhove'
#' title: 'Aufgabenserie Rückstellungen'
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

#' # Rückstellungsberechnung mit Chain-Ladder
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

#' **Falsch, korrigieren:** Wir erwarten also, dass die Leistungen aus einem bestimmten Monat sich wie 
#' folgt auf die Folgemonate verteilen (Proportionen):
phis[1] <- 1
(props <- phis / sum(phis))

#' Folglich werden in Abrechnungsmonat 5 etwa 109'404 der 1'000'000 Kosten 
#' abgerechnet. (Total mit den Vormonaten: 1'000'666.)
props[1] * 1e6

#' In Abrechnungsmonat 6 fallen 750'549.1 zusätzliche Kosten an.
#' (Total mit den Vormonaten: 1'045'132.)
props[2] * 1e6 + props[1] * 11e5

#' (c) Cf. GLM-Implementierung.

#' # Rückstellungen mit Benktander-Hovinen
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

#' # Rückstellungen nach Mack's Methode
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

#' **Irgendwo muss ich einen kleinen Fehler gemacht haben. Die tatsächliche Antwort ist 11'706. Vllt. liegt's daran, dass man die $\alpha^{LD}$-Werte genauer berechnen kann.**

#' # Rückstellungsberechnung mit erweitertem Bornhuetter-Ferguson-Verfahren
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
#' Auch hier irgendwo noch ein Fehler zu korrigieren.

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

#' # Rückstellungsverfahren mit erweitertem Bornhuetter-Ferguson-Verfahren
#' Bissl zu mühsam.