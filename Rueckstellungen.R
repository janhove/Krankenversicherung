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
chain_ladder <- function(S) {
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
rueckstellung <- function(S) {
  last_cols <- (ncol(S)+1):(2*ncol(S)-1)
  X <- S |> 
    decum_row() |> 
    shift_right()
  X[, last_cols] |> sum(na.rm = TRUE)
}
#' Resultat: Der Rückstellungsbedarf laut chain ladder beträgt 1'231'886.
(S_cl <- M |> shift_left() |> cum_row() |> chain_ladder())
(S_cl |> decum_row() |> shift_right())
rueckstellung(S_cl)

#' (b) Angenommen, in den darauf folgenden Behandlungsmonaten 5 und 6
#' fallen Leistung von total 1'000'000 bzw. 1'100'000 an. Das Abwicklungsmuster
#' bleibe dasselbe wie in den vorangehenden Monaten 1 bis 4.
#' Wie viele Leistungen werden in den Abrechnungsmonaten 5 und 6 abgerechnet.
#' 
#' Antwort: Berechne nochmals die $\varphi$-Werte:
(phis <- M |> shift_left() |> cum_row() |> phi())

#' Es gelten $S_{5,0} \cdot \phi_1 \dots \phi_3 = 1000000$ 
#' und $S_{6,0} \cdot \phi_1 \dots \phi_3 = 1100000$.
#' Daraus $S_{5,0} = 124410.1$, $S_{6,0} = 136851.2$.
#' Daraus dann wieder $S_{5,1} = 124410.1 \cdot \phi_1 = 716645.5$.
#' Somit $X_{5,1} = 716645.5 - 124410.1 = 592235.4$.
phis[1] <- 1
1e6 / prod(phis)
11e5 / prod(phis)
1e6 / prod(phis) * cumprod(phis[2:3])
11e5 / prod(phis) * cumprod(phis[2:3])

#' Folglich werden im Abrechnungsmonat 5 etwa 124'410 der 1'000'000 Kosten 
#' abgerechnet. (Total mit den Vormonaten: 1'015'672.)
#' Im Abrechnungsmonat 6 fallen 136'851.2 + 592'235.4 = 729'087 zusätzliche Kosten an.
#' (Total mit den Vormonaten: 1'023'670.)
#' 
#' (c) Cf. GLM-Implementierung. Siehe S. 169.
#' 
#' # Rückstellungen mit Benktander-Hovinen
#' Berechnen Sie ausgehend vom Zahlenbeispiel auf S. 154
#' die Rückstellungen gemäss Bornhuetter-Ferguson und gemäss Benktander-Hovinen.
#' 
#' Tabelle 8w (S. 160) enthält bereits die kumulativen Rückstellungen.
#' Es reicht also, diese zu dekumulieren. Für Benktander-Hovinen kann man
#' den Bornhuetter-Ferguson-Algorithmus rezyklieren und lediglich die $\alpha$-Werte
#' anpassen.
#' 
#' Die Gesamtrückstellungen betragen 9'964.

S <- rbind(
  c(0,       0,    0,    0,    0, 3483),
  c(0,       0,    0,    0, 3844, 4043),
  c(0,       0,    0, 3977, 4393, 4624),
  c(0,       0, 3880, 4729, 5238, 5521),
  c(0,    4261, 5379, 6310, 6869, 7180),
  c(1889, 3472, 4611, 5560, 6130, 6447)
)
rueckstellung(S) # Gesamtrückstellungen: 9'964

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
#' anders berechnen, S. 166. Ich erhalte einen Rückstellungsbedarf von 10'467.
bh <- function(S, gamma, alpha) {
  alpha_bf <- bf(S, gamma, alpha)[, ncol(S)]
  bf(S, gamma, alpha_bf)
}
(S_bh <- bh(S, gamma, alpha))
rueckstellung(S_bh)

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

# Formel S. 158
# (Vermutlich ist zeta (ζ) gemeint, steht aber ς (= Variante von sigma).)
compute_sigmas <- function(X, pis) {
  ell <- ncol(X)
  sigmas <- rep(0, ell)
  for (i in 1:ell) {
    sigmas[i] <- sum(X[1:(ell-i+1), i]) / sum(pis[1:(ell-i+1)])
  }
  sigmas
}
mack <- function(S, pi) {
  X <- S |> decum_row()
  sigma_ad <- colSums(X, na.rm = TRUE) / rev(cumsum(pi)) # S. 158
  gamma_ad <- cumsum(sigma_ad) / sum(sigma_ad)           # S. 159
  alpha_ld <- rowSums(X, na.rm = TRUE) / rev(gamma_ad)   # S. 161
  sigmas_ad <- compute_sigmas(X, alpha_ld)
  gamma_mack <- cumsum(sigmas_ad) / sum(sigmas_ad)       # S. 159
  alpha_mack <- alpha_ld * sum(sigmas_ad)                # S. 163; verwende alpha_ld für pi
  bf(S, gamma_mack, alpha_mack)
}
#' Resultat: Die Gesamtrückstellungen betragen 11'706.
(S_mack <- X |> 
    cum_row() |> 
    mack(c(4000, 4500, 5300, 6000, 6900, 8200))) 
rueckstellung(S_mack)

#' # Rückstellungsberechnung mit erweitertem Bornhuetter-Ferguson-Verfahren
#' **Chain Ladder:** Rückstellungsbedarf: 111.
S <- rbind(
  c(100, 150, 180),
  c(NA, 120, 170),
  c(NA, NA, 110)
) |> shift_left()
chain_ladder_bf <- function(S) {
  phi_cl <- phi(S) # S. 156
  gamma_cl <- c((1 / cumprod(rev(phi_cl[2:length(phi_cl)]))) |> rev(), 1) # S. 157
  alpha_ld <- rev(apply(S, 1, max, na.rm = TRUE) / rev(gamma_cl)) # S. 161
  bf(S, gamma_cl, alpha_ld)
}
S |> chain_ladder_bf() |> rueckstellung()

#' **Panning:** Rückstellungsbedarf: 119.
panning <- function(S) {
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
  bf(S, gamma_pa(beta_pa(decum_row(S))), alpha_pa(decum_row(S)))
}
S |> panning() |> rueckstellung()

#' # Rückstellungsverfahren mit erweitertem Bornhuetter-Ferguson-Verfahren
#' Zunächst weitere Funktionen definieren.

loss_dev <- function(S, gamma) {
  alpha_ld <- apply(S, 1, max, na.rm = TRUE) / rev(gamma)
  bf(S, gamma, alpha_ld)
}
X <- rbind(
  c(1001,  854,  568, 565, 347, 148),
  c(1113,  990,  671, 648, 422,  NA),
  c(1265, 1168,  800, 744,  NA,  NA),
  c(1490, 1383, 1007,  NA,  NA,  NA),
  c(1725, 2536,   NA,  NA,  NA,  NA),
  c(1889,   NA,   NA,  NA,  NA,  NA)
)
X |> 
  shift_left() |> 
  cum_row() |> 
  loss_dev(c(0.28, 0.53, 0.71, 0.86, 0.95, 1)) # OK

additiv <- function(S, pis) {
  X <- S |> 
    decum_row()
  sigma_ad <- colSums(X, na.rm = TRUE) / rev(cumsum(pis)) # S. 158
  gamma_ad <- cumsum(sigma_ad) / sum(sigma_ad) # S. 159
  alpha_ad <- pis * sum(sigma_ad)
  bf(S, gamma_ad, alpha_ad)
}

X |> 
  shift_left() |> 
  cum_row() |> 
  additiv(c(4000, 4500, 5300, 6000, 6900, 8200)) |> 
  decum_row() # ok

cape_cod <- function(S, gamma, pis) {
  kappa_cc <- sum(apply(S, 1, max, na.rm = TRUE)) / sum(rev(gamma) * pis)
  alpha_cc <- pis * kappa_cc
  bf(S, gamma, alpha_cc)
}
X |> 
  shift_left() |> 
  cum_row() |> 
  cape_cod(c(0.28, 0.53, 0.71, 0.86, 0.95, 1),
           c(4000, 4500, 5300, 6000, 6900, 8200)) # OK

#' Zur Sache:
S <- rbind(
  c(100, 150, 180),
  c(NA, 120, 170),
  c(NA, NA, 110)
) |> shift_left()
gamma <- c(0.6, 0.9, 1)
pis <- c(185, 205, 195)
alpha <- c(190, 200, 190)

S_cl <- chain_ladder(S)
S_cl_bf <- chain_ladder_bf(S)
S_bf <- bf(S, gamma, alpha)
S_bh <- bh(S, gamma, alpha)
S_mack <- mack(S, pi)
S_ld <- loss_dev(S, gamma)
S_cc <- cape_cod(S, gamma, pis)
S_ad <- additiv(S, pis)
S_pa <- panning(S)

dreiecke <- list(
  chainladder = S_cl,
  chainladder_bf = S_cl_bf, 
  bornhuetter_ferguson = S_bf,
  benktander_hovinen = S_bh,
  mack = S_mack,
  loss_dev = S_ld,
  cape_cod = S_cc,
  additiv = S_ad,
  panning = S_pa
)

# Übersicht
(uebersicht <- sapply(dreiecke, rueckstellung))
dotchart(sort(uebersicht),
         xlab = "Rückstellungen")
summary(uebersicht)

#' Das 75. Perzentil (115) scheint mir ein gutes Kompromiss zu sein.