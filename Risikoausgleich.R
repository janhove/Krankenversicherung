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
#' (a) Die Daten sind konsistent.
#' 
#' | Kasse 1, alt   	| Leistungen 	| Bestand 	| Durchschnittsleitungen 	|
#' |----------------	|------------:	|---------:	|------------------------:	|
#' | Risikoklasse A 	| $150 \times 10^6$  	| $100 \times 10^3$| $1500$                       	|
#' | Risikoklasse B 	| $105 \times 10^6$   | $50 \times 10^3$ | $2100$                       	|
#' | Total          	| $255 \times 10^6$   | $150 \times 10^3$  | $1700$                       	|
#'
#' | Kasse 2, alt   	| Leistungen 	| Bestand 	| Durchschnittsleitungen 	|
#' |----------------	|------------:	|---------:	|------------------------:	|
#' | Risikoklasse A 	| $112 \times 10^6$  	| $80 \times 10^3$| $1400$                       	|
#' | Risikoklasse B 	| $330 \times 10^6$   | $150 \times 10^3$ | $2200$                       	|
#' | Total          	| $442 \times 10^6$   | $230 \times 10^3$  | $1922$                       	|
#' 
#' | Kasse 1, neu   	| Leistungen 	| Bestand 	| Durchschnittsleitungen 	|
#' |----------------	|------------:	|---------:	|------------------------:	|
#' | Risikoklasse A1 	| $50 \times 10^6$  	| $80 \times 10^3$| $625$                       	|
#' | Risikoklasse A2 	| $100 \times 10^6$  	| $20 \times 10^3$| $5000$                       	|
#' | Risikoklasse B 	| $30 \times 10^6$   | $40 \times 10^3$ | $750$                       	|
#' | Risikoklasse A1 	| $75 \times 10^6$  	| $10 \times 10^3$| $7500$                       	|
#' | Total          	| $225 \times 10^6$   | $150 \times 10^3$  | $1700$                       	|
#'
#' | Kasse 2, neu   	| Leistungen 	| Bestand 	| Durchschnittsleitungen 	|
#' |----------------	|------------:	|---------:	|------------------------:	|
#' | Risikoklasse A1 	| $30 \times 10^6$  	| $60 \times 10^3$| $500$                       	|
#' | Risikoklasse A2 	| $82 \times 10^6$  	| $20 \times 10^3$| $4100$                       	|
#' | Risikoklasse B1 	| $95 \times 10^6$   | $120 \times 10^3$ | $792$                       	|
#' | Risikoklasse B2 	| $235 \times 10^6$  	| $30 \times 10^3$| $7833$                       	|
#' | Total          	| $442 \times 10^6$   | $230 \times 10^3$  | $1922$                       	|
#'  
#' (b) 
(leistungen_alt <- c(150000000 + 112000000, 105000000 + 330000000))
(bestand_alt <- c(100000+80000, 50000 + 150000))
(durchschnitt_alt <- leistungen_alt / bestand_alt)
(durchschnitt_alt_total <- sum(leistungen_alt) / sum(bestand_alt))
(ausgleich_alt <- durchschnitt_alt - durchschnitt_alt_total)

#' | Branch, alt   	| Leistungen 	| Bestand 	| Durchschnittsleitungen 	| Risikoausgleich Beitrag |
#' |----------------	|------------:	|---------:	|------------------------:	|------------------------:	|
#' | Risikoklasse A 	| $262 \times 10^6$  	| $180 \times 10^3$| $1456$ | $-379$ |
#' | Risikoklasse B 	| $435 \times 10^6$  	| $200 \times 10^3$| $2175$ | $341$	|
#' | Total          	| $697 \times 10^6$   | $380 \times 10^3$  | $1834$ | |
#'  

(leistungen_neu <- c(50000000+30000000, 100000000+82000000, 30000000+95000000, 75000000+235000000))
(bestand_neu <- c(80000+60000, 20000+20000, 40000+120000, 10000+30000))
(durchschnitt_neu <- leistungen_neu / bestand_neu)
(durchschnitt_neu_total <- sum(leistungen_neu) / sum(bestand_neu))
(ausgleich_neu <- durchschnitt_neu - durchschnitt_neu_total)


#' | Branch, neu   	| Leistungen 	| Bestand 	| Durchschnittsleitungen 	| Risikoausgleich Beitrag |
#' |----------------	|------------:	|---------:	|------------------------:	|------------------------:	|
#' | Risikoklasse A1 	| $80 \times 10^6$  	| $140 \times 10^3$| $571$ | $-1263$ |
#' | Risikoklasse A2 	| $182 \times 10^6$  	| $40 \times 10^3$| $4550$ | $2716$	|
#' | Risikoklasse B1 	| $125 \times 10^6$  	| $160 \times 10^3$| $781$ | $-1053$	|
#' | Risikoklasse B2 	| $310 \times 10^6$  	| $40 \times 10^3$| $7750$ | $5916$	|
#' | Total          	| $697 \times 10^6$   | $380 \times 10^3$  | $1834$ | |
#'  

#' (c) Mit dem alten Ausgleich zahlt Kasse A 20'826'023. Kasse B erhält den
#' gleichen Betrag. Mit dem neuen Ausgleich zahlt Kasse A 29'667'293.
#' Also ist der neue Ausgleich etwa 9 Millionen besser für B.
(c(100000, 50000) * ausgleich_alt) # Kasse 1, alt
(c(100000, 50000) * ausgleich_alt) |> sum()
c(80000, 150000) * ausgleich_alt   # Kasse 2, alt
(c(80000, 150000) * ausgleich_alt) |> sum()

(c(80000,20000,40000,10000) * ausgleich_neu) # Kasse 1, neu
(c(80000,20000,40000,10000) * ausgleich_neu) |> sum()
c(60000,20000,120000,30000) * ausgleich_neu  # Kasse 2, neu
(c(60000,20000,120000,30000) * ausgleich_neu) |> sum()

#' # Aufgabe 3
#' (a)

#' | Anzahl Versicherte | Kasse A | Kasse B | Kasse C | alle Kassen |
#' |--------------------|--------:|--------:|--------:|------------:|
#' | Risikoklasse I     | $1000$  | $2000$  | $1600$  | $4600$      |
#' | Risikoklasse II    | $1500$  | $1400$  | $2000$  | $4900$      |
#' | Risikoklasse III   | $1200$  | $1200$  | $1500$  | $3900$      |
#' | Total              | $3700$  | $4600$  | $5100$  | $13400$     |
#' 
#' Leistungen sind geteilt durch 1000.
#' 
#' | Leistungen         | Kasse A | Kasse B | Kasse C | alle Kassen |
#' |--------------------|--------:|--------:|--------:|------------:|
#' | Risikoklasse I     | $500$   | $1200$  | $960$   | $2660$      |
#' | Risikoklasse II    | $1650$  | $1400$  | $2100$  | $5150$      |
#' | Risikoklasse III   | $2280$  | $2400$  | $2940$  | $7620$      |
#' | Total              | $4430$  | $5000$  | $6000$  | $15430$     |
#' 
#' | $\emptyset$-Leistungen | Kasse A | Kasse B | Kasse C | alle Kassen | Risikoausgl. |
#' |------------------------|--------:|--------:|--------:|------------:|-------------:|
#' | Risikoklasse I         | $500$   | $600$   | $600$   | $578$       | $-573$       |
#' | Risikoklasse II        | $110$   | $1000$  | $1050$  | $1051$      | $-100$       |
#' | Risikoklasse III       | $1900$  | $2000$  | $1960$  | $1954$      | $802$        |
#' | Total                  | $1197$  | $1087$  | $1176$  | $1151$      |              |
#' 
#' Beiträge in den Ausgleichstopf:
#' 
#' | Risikoausgleich    | Kasse A | Kasse B | Kasse C | alle Kassen |
#' |--------------------|--------:|--------:|--------:|------------:|
#' | Risikoklasse I     | $573232$   | $1146463$ | $917171$   | $2636866$  |
#' | Risikoklasse II    | $150708$   | $140661$  | $200944$   | $492313$   |
#' | Risikoklasse III   | $-962824$  | $-962824$ | $-1203530$ | $-3129179$ |
#' | Total              | $-238884$  | $324300$  | $-85415$   | $0$        |
#' 
#' | Aufwand            | Kasse A    | Kasse B   | Kasse C    | alle Kassen |
#' |--------------------|-----------:|----------:|-----------:|------------:|
#' | Risikoklasse I     | $1073232$  | $2346463$ | $1877171$  | $5296866$   |
#' | Risikoklasse II    | $1800708$  | $1540661$ | $2300944$  | $5642313$   |
#' | Risikoklasse III   | $1317176$  | $1437176$ | $1736470$  | $4490821$   |
#' | Total              | $4191116$  | $5324300$ | $5914585$  | $15430000$  |
#' 
#' | $\emptyset$-Aufwand | Kasse A    | Kasse B   | Kasse C    | alle Kassen |
#' |--------------------|------------:|----------:|-----------:|------------:|
#' | Risikoklasse I     | $1073$      | $1173$    | $1173$     | $1151$      |
#' | Risikoklasse II    | $1200$      | $1100$    | $1150$     | $1151$      |
#' | Risikoklasse III   | $1098$      | $1198$    | $1158$     | $1151$      |
#' | Total              | $1497$      | $1157$    | $1160$     | $1151$      |
#' 
#' (b) Kasse A erhält 238'884, Kasse B zahlt 324'300, Kasse C erhält 85'415.
#' (mit Rundungsfehler).
4191116 - 4430e3
5324300 - 5000e3
5914585 - 6000e3

#' (c) Ist hiermit die Summe der Absolutbeträge der Zahlen aus der letzten
#' Antwort gemeint? Wenn ja, dann 648'599.
sum(abs(c(4191116 - 4430e3, 5324300 - 5000e3, 5914585 - 6000e3)))

#' (d) Gleiche Frage. Wenn ja, dann 6'258'358.
sum(abs(c(2636866, 492313, -3129179)))

#' (e) Siehe oben. Ohne weitere Informationen zu berücksichtigen:
#' Kasse A sollte ihre Prämie auf 1497 setzen,
#' Kasse B auf 1157, 
#' Kasse C auf 1160.