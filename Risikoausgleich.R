#' ---
#' author: 'Jan Vanhove'
#' title: 'Aufgabenserie Risikoausgleich'
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
#' # Aufgabe 2
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

#' # Aufgabe 3
#' Ke Luscht :)