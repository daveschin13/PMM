# Aufgabe KM7-02 — Der Chi-Quadrat-Test

Name: David Schindl &nbsp;&nbsp;&nbsp; Abgabe: pmm-km7-02

**Lektüre:** Navarro, [Statistik für Human- und Sozialwissenschaftler](https://learningstatisticswithr.com/book/)
(Pflicht) — **Kap. 12, Chi-Quadrat-Tests**. Aufgaben 4 und 6 beziehen sich
auf das Kapitel. Voraussetzung ist
[Lektion KM5-02](../KM5-02-binomialverteilung-und-annahmeschwelle/lesson.html)
(Binomialverteilung) — der Anpassungstest ist ihre Anwendung.

> **Setup-Ritual:** RStudio-Projekt `pmm-km7-02`, Skript `analyse.R`.
> **Daten:** `assets/betriebsdaten.csv` — Sollmaß 10,00 mm, Toleranz
> ± 0,15 mm. Gültige Messungen: 116 (vier fehlen und werden NA).

---

## 0. Der Datensatz

```r
library(tidyverse)
daten <- read_csv(".../assets/betriebsdaten.csv") |>
  mutate(iok = if_else(is.na(masse_mm), NA,
              if_else(abs(masse_mm - 10) > 0.15,
                      "ausserhalb", "in_Toleranz")))
tab <- table(daten$maschine, daten$iok)
tab
```

```
          ausserhalb  in_Toleranz
M1                0           37
M2                0           39
M3                6           34
Sum               6          110
```

---

## 1. Vorhersagen (ohne R — erst hinschreiben!)

**a)** Wie viele gültige Messungen hat die Tabelle insgesamt?
116 Warum nicht 120? 4 Messungen haben NA

**b)** Wie viele Ausschusteile wären bei **Unabhängigkeit** (H0) in Zeile
M1 zu erwarten? Rechne die Formel von Hand:

$$\text{erwartet}_{ij} = \frac{\text{Zeilensumme}_i \cdot \text{Spaltensumme}_j}{\text{Gesamtsumme}}$$

Erwartet M1 außen: 37*6/116 = 1,91

**c)** Wie viele der sechs Zellen liegen erwartungsgemäß unter 5? Die Regel
verlangt: mindestens 80 % müssen &ge; 5 sein. Ist sie hier erfüllt?
Unter 5% liegen 3 von 6 Zellen (50 %). Welches Testverfahren folgt daraus? Die 80-%-Regel ist nicht erfüllt, daher sollte man den Fisher-Test verwenden.

---

## 2. Die Teststatistik von Hand

**a)** Berechne den Beitrag jeder der drei Zellen in der Spalte
„außerhalb":

$$\frac{(\text{beobachtet} - \text{erwartet})^2}{\text{erwartet}}$$

| Zelle | beobachtet | erwartet | Beitrag |
|-------|-----------|----------|---------|
| M1 außerhalb |0 | 1.914 | 1.914 |
| M2 außerhalb |0 | 2.017 | 2.017 |
| M3 außerhalb |6 | 2.069 | 7.469 |

**b)** Berechne dieselbe Tabelle mit R und vergleiche Zelle für Zelle.
Wo weicht deine Handrechnung am stärksten ab, und warum?

```r
erwartet <- outer(rowSums(tab), colSums(tab)) / sum(tab)
round((tab - erwartet)^2 / erwartet, 4)
```
Die Handrechnung weicht am stärksten bei M3 außen ab. Dort wird die Differenz quadriert, daher wirken sich Rundungsfehler im Erwartungswert dort am stärksten aus.
Summe der Beiträge: 12,02 &nbsp; `chisq.test(tab)$statistic`: 12,02

**c)** Welche Zelle trägt den größten Beitrag bei? Ist das die Zelle mit
der größten relativen Abweichung, oder die mit dem kleinsten Nenner?
Begründe. M3 außen trägt am meisten bei. Der Nenner ist bei allen drei Zellen etwa gleich (≈ 2). Entscheidend ist die quadrierte Differenz: M3 hat die größte absolute Abweichung (+3,93).

---

## 3. Der Unabhängigkeitstest

```r
chisq.test(tab)
fisher.test(tab)
```

- X&sup2;: 12,02 &nbsp; df: 2
- p-Wert (Chi): 0,0025 &nbsp; p-Wert (Fisher): 0,0032
- Warum weichen die beiden leicht ab? Chi-Quadrat ist nur eine Näherung und bei kleinen erwarteten Häufigkeiten ungenau. Fisher rechnet exakt.

**a)** Wie berechnet man die df? Zeige die Formel und die Zahlen:

df = ( 3 − 1) · ( 2 − 1) = 2

**b)** R gibt eine Warnung aus. Schreib sie wörtlich auf:

"Chi-squared approximation may be incorrect"

**c)** Übersetze den Chi-p-Wert in einen deutschen Satz, in dem die Zahl
0,0025 vorkommt. Dann sag, was daraus folgt (H0 verwerfen oder
behalten). Wären Maschine und Ausschuss unabhängig, würde man eine so große oder größere Abweichung nur in etwa 0,0025 (0,25 %) der Fälle sehen. H0 wird bei α = 0,05 **verworfen**, wegen der verletzten Voraussetzung aber mit Vorsicht.

---

## 4. Der Anpassungstest

Der Kunde gibt eine Sollquote von **10 % Ausschuss** vor.

**a)** Rechne die Anpassungstests für alle drei Maschinen:

```r
quoten <- daten |> filter(!is.na(masse_mm)) |>
  group_by(maschine) |>
  summarise(n = n(), aus = sum(iok == "ausserhalb"),
            .groups = "drop") |>
  mutate(in_toleranz = n - aus)

# Anpassungstest je Maschine gegen 10 % Ausschuss
lapply(split(quoten, quoten$maschine), function(q) {
  suppressWarnings(
    chisq.test(c(q$in_toleranz, q$aus), p = c(0.9, 0.1)))
})
```

| Maschine | beobachtet | erwartet | X&sup2; | p-Wert |
|----------|-----------|----------|--------|--------|
| M1 | 37 / 0 | 37 / 3,7 |4,11 |0,043 |
| M2 | 39 / 0 | 39 / 3,9 |4,33 |0,037 |
| M3 | 34 / 6 | 36 / 4,0 |1,11 |0,292 |

> Warum `suppressWarnings()`? Der Test warnt auch hier vor der
> Näherung — in Aufgabe 5 siehtst du, ob die Warnung berechtigt ist.

**b)** Welche Maschine fällt statistisch auf? In <strong>welche
Richtung</strong> weicht sie ab — und wie erkennst du das, wenn der Test
nur einen p-Wert liefert? Auffällig sind **M1 und M2** (p < 0,05). Sie weichen in die *günstige* Richtung ab: 0 Ausschuss beobachtet gegenüber 3,7 bzw. 3,9 erwartet. Die Richtung sieht man am Vergleich von `$observed`, `$expected` und `$residuals`. Der p-Wert allein zeigt sie nicht.

**c)** Erkläre in 3 Sätzen, warum ein Anpassungstest gegen eine
Sollverteilung <em>kein</em> Güftest für den Prozess ist.

Ein Anpassungstest prüft nur, ob die Häufigkeiten zu einer vorgegebenen Verteilung passen. Er ist zweiseitig und schlägt auch bei "zu gutem" Ausschuss an. Über Lage, Streuung oder Stabilität des Prozesses sagt er nichts, deshalb ist er kein Gütetest.

**d)** M3 hat 6 von 40 Ausschusteilen, M1 und M2 keine. Welche Frage
beantwortet der Anpassungstest, und welche <em>nicht</em>?

Beantwortet: ob die Aufteilung in/außerhalb bei *einer* Maschine mit den 10 % Sollquote vereinbar ist. Beantwortet nicht: ob sich die Maschinen untereinander unterscheiden und ob M3 schlechter ist als M1 und M2.

---

## 5. Voraussetzungen prüfen

**a)** Welche der sechs erwarteten Häufigkeiten liegen unter 5? Trage sie ein:

Unter 5 liegen: 1,91 (M1), 2,02 (M2), 2,07 (M3), jeweils in der Spalte "außerhalb" Wie viele Prozent der Zellen sind das? Das sind 3 von 6 =50 %.

**b)** Die 80-%-Regel verlangt also: erfüllt / nicht erfüllt
(Die Regel ist nicht erfüllt (50 % < 80 %).). Was bedeutet das für die Verlässlichkeit von
`p = 0,0025`? Der Wert p = 0,0025 ist deshalb nur eine Näherung und nicht verlässlich. Der exakte Fisher-Test (≈ 0,0032) führt aber zur gleichen Entscheidung.

**c)** R gibt <em>auch</em> beim Anpassungstest aus Aufgabe 4 eine
Warnung aus. Führe ihn ohne `suppressWarnings()` aus und notiere den Wortlaut.
Warum? Überlege, wie viele Fälle je Zelle erwartet werden — bei M1 sind
das 37 in Toleranz gegen 3,7 außerhalb. Was folgt daraus für die
Verlässlichkeit aller p-Werte in Aufgabe 4? Wortlaut: *"Chi-squared approximation may be incorrect"*. Beim Anpassungstest werden in der Ausschuss-Zelle nur 3,7 (M1), 3,9 (M2) und 4,0 (M3) erwartet, also jeweils unter 5. Damit sind alle p-Werte aus Aufgabe 4 nur Näherungen. Sauberer wäre `binom.test()`.

---

## 6. Kapitel-Check (Navarro Kap. 12)

**a)** Nenne die **drei** Voraussetzungen des Chi-Quadrat-Tests aus dem
Kapitel und prüfe, welche in diesem Datensatz erfüllt sind:

1. Die Daten sind Häufigkeiten (Zähldaten) → erfüllt.
2. Die Beobachtungen sind voneinander unabhängig (jedes Teil wird nur einmal gezählt) → vermutlich erfüllt.
3. Die erwarteten Häufigkeiten sind groß genug (≥ 5) → nicht erfüllt.

**b)** Erkläre den Unterschied zwischen Unabhängigkeitstest und
Anpassungstest in eigenen Worten: Welche Frage stellt welcher, und wie
wird in R der Unterschied am Aufruf sichtbar? Der Unabhängigkeitstest fragt, ob zwei Merkmale zusammenhängen (hier Maschine und Ausschuss). In R bekommt `chisq.test()` eine Kreuztabelle: `chisq.test(tab)`. Der Anpassungstest fragt, ob die Verteilung eines Merkmals zu vorgegebenen Wahrscheinlichkeiten passt. In R bekommt `chisq.test()` einen Vektor plus `p = ...`.


**c)** Ein Kollege sagt: „Der Chi-Test sagt, dass M3 schlecht ist, weil
0,0025 < 0,05." Hat er recht? Begründe in 2 Sätzen — denke an Richtung
und daran, was ein Anpassungstest gerade <em>nicht</em> kann.

Nein, der Kollege hat nicht recht. Der Unabhängigkeitstest zeigt nur, dass die Quoten zwischen den Maschinen verschieden sind, nicht welche Maschine in welche Richtung abweicht. Erst die Residuen zeigen, dass M3 mehr Ausschuss hat als erwartet. Der Anpassungstest gegen die Sollquote findet bei M3 mit p ≈ 0,29 nichts Auffälliges, und die 0,0025 sind wegen der verletzten Voraussetzung nur eine Näherung.
