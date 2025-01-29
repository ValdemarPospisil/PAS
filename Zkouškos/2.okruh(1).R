## (1) Bodové a intervalové odhady střední hodnoty a rozdílu středních hodnot

# Bodový odhad je jedna konkrétní hodnota, např. průměr nebo rozptyl
# Intervalový odhad určuje interval, ve kterém se s určitou pravděpodobností (např. 95%) nachází skutečná střední hodnota

# Načtení potřebné knihovny pro intervalový odhad
library(DescTools)

# Práce s daty - příklad datasetu `cars`
(price <- cars$Price)  

# Výpočet bodového odhadu - aritmetický průměr
(prumer <- mean(price))  # Průměrná cena

# Počet pozorování (velikost vzorku)
(n <- length(price))

# Výpočet směrodatné odchylky
(sd_price <- sd(price))

# Výpočet střední chyby průměru (Standard Error of Mean, SEM)
(SEM <- sd_price / sqrt(n))  # SEM = směrodatná odchylka / odmocnina z n

# Výpočet 95% intervalového odhadu pro střední hodnotu
(alpha <- 0.05)  # Úroveň spolehlivosti 95%
(q.n <- qnorm(1 - alpha / 2))  # Kvantil normálního rozdělení pro 95% interval

# Dolní a horní mez intervalu spolehlivosti
(lower_bound <- prumer - q.n * (sd_price / sqrt(n)))
(upper_bound <- prumer + q.n * (sd_price / sqrt(n)))

# Alternativní výpočet intervalu spolehlivosti pomocí funkce MeanCI
(ci <- MeanCI(price, conf.level = 0.95))

# Výstup intervalu
ci

# Bodový odhad rozdílu dvou středních hodnot
# Příklad datasetu 'Stulong' s dvěma skupinami
syst1 <- Stulong$syst1  # První skupina
syst2 <- Stulong$syst2  # Druhá skupina

# Výpočet bodových odhadů průměrů pro obě skupiny
(prumer1 <- mean(syst1))
(prumer2 <- mean(syst2))

# Rozdíl středních hodnot
(rozdil_prumeru <- prumer1 - prumer2)

# Intervalový odhad rozdílu středních hodnot pomocí t-testu
# Používáme Studentův t-test (Welchův t-test pro nerovné rozptyly)
(t_test_result <- t.test(syst1, syst2, var.equal = FALSE))

# Interpretace výsledků t-testu:
# t = testovací statistika (jak daleko jsou data od nulové hypotézy, že rozdíl = 0)
# df = počet stupňů volnosti
# p-value = pravděpodobnost, že rozdíl mezi průměry je způsoben náhodou
# Pokud p < 0.05, zamítáme nulovou hypotézu a rozdíl mezi průměry je statisticky významný
# Dolní a horní mez intervalu spolehlivosti

# Výhody a nevýhody bodového a intervalového odhadu
# Bodový odhad:
#  + Jednoduchý na interpretaci
#  + Rychlý výpočet
#  - Neudává míru nejistoty
#  - Může být nepřesný, pokud je vzorek malý

# Intervalový odhad:
#  + Udává míru nejistoty
#  + Přesnější interpretace dat
#  - Komplexnější výpočet
#  - Šířka intervalu závisí na velikosti vzorku

# Použití:
# Bodový odhad se používá pro rychlou analýzu, kde není potřeba odhadovat přesnost.
# Intervalový odhad se používá tam, kde chceme určit míru nejistoty, například při statistickém testování hypotéz.
