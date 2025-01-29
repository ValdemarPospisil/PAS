library(DescTools)
library(ggplot2)
library(dplyr)

## Číselná proměnná
weight <- Policie$weight

# Aritemetický průměr
#   součet hodnot dělěno počtem hodnot
mean(weight)

# Vážený průměr
#   každé hodnotě je přirazena váha, která určuje její relativní důležitost.
#   součet každé hodnoty krát její váha děleno součtem všech vah
#   použití: když různé hodnoty mají odlišnou důležistost (např. výpočet průměrné známky, každá známka jiná váha)
weighted.mean(weight)

# Upravený průměr
#   aritmetický průměr po odstranění určitého procenta nejvyšších a nejnižších hodnot 
#   uměle se zbavím outlierům
#   použít: když je potřeba omezit vliv extrémních hodnot 
mean(weight,trim=0.10)

# Střední hodnota
#   prostřední hodnota seřazených dat
#   rozděluje data na dvě stejné poloviny
median(weight)
# Polovina policistů má váhu nižší než 77.95 kg a polovina vyšší.

# Huberův odhad
#   robustní metoda výpočtu průměru
#   méně ovlivněn outliery
#   kombinuje prvky aritmetického průměru a mediánu
#   váhování dat na základě jejich vzdálenosti od střední hodnoty
#   kombinuje kvadratickou ztrátovou funkci (pro malé odchylky) a lineární ztrátovou funkci (pro velké odchylky)
#   hodnoty blízko střední hodnoty mají větší vliv než odlehlé
#   použití: když jsou v datech přítomny odlehlé hodnoty, které by mohli ovlivnit aritmetický průměr,
#            ale chcete zahrnout informace z extrémů omezeným způsobem 
#   vzorec viz. finale
HuberM(weight)
# Robustní odhad průměru, který je méně ovlivněn extrémy, je 78.17315 kg.

# Modus
#   různé definice pro různé typy proměnných
#   spojitá: hodnota, kde je maximální hustota pravděpodobnosti
#   diskrétní: proměnná s málo hodnotami
#   kategorická: nejčasteji se objevující hodnota

# Tukeyho čísla
#   min, Q1 (25%), Q2 (median), Q3 (75%), max
fivenum(weight)

# Variabilita
#   rozdílnost nebo rozptyl dat
#   měří, jak jsou data rozptýlená kolem centrální hodnoty
#   tyto metriky poskytují informaci o tom, zda jsou hodnoty blízko sebe (nízká variabilita)
#   nebo rozprostřeny široce (vysoká variabilita)

# IQR
#   rozdíl mezi Q1 a Q3 -> Q3 - Q1
#   udává variabilitu středních 50 % dat
IQR(weight)
# Střední 50 % dat má rozptyl 17.15 kg.

# Rozptyl
#   průměrný kvadratický rozdíl hodnot od aritmetického průměru
#   vysoký rozptyl znamená, že jsou hodnoty daleko od průměru
#   nízký rozdíl značí, že hodnoty jsou blízko průměru
#   je vyjádřen ve čtvercích pvůdních jednotek (na druhou)
var(weight)
# Průměrný kvadratický rozdíl od průměru je 131.1711 kg².

# Směrodatná odchylka
#   průměrná odchylka hodnot od průměru v původních jednotkách měření
#   vysoká hodnota = aritmetický průměr je k ničemu
#   odmocnina rozptylu
sd(weight)

# Variační koeficient
#   poměr sd k průměru vyjádřený v procentech
#   vyjadřuje relativní rozptyl hodnot
#   je to bezrozměrná veličina vhodná pro srovnání rozptylu mezi různými soubory dat.
(sd(weight) / mean(weight)) * 100
# Relativní rozptyl hodnot je 14.6 %, což naznačuje střední variabilitu.

# Mediánová absolutní odchylka
#   robustní vůči odlehlým hodnotám
#   měří variabilitu dat na základě odchylek od mediánu
mad(weight)

# Šikmost
#   průměr ze třetích mocnin z-skorů
#   asymetrie rozdělení
#   pozitivní šikmost (Pravostranná)
#       hodnoty jsou soustředěnné vlevo (v nižších hodnotách)
#   negatvní šikmost (Levostranná)
#       hodnoty jsou soustředěnné vlevo (ve vyšších hodnotách)
#   hodnota koeficientu šikmosti (S) určuje o který druh jde
Skew(weight)

# Špičatost
#   označuje soustředění hodnot proměnné kolem svého modu spolu s vyšším nebo nižším výskytem hodnot v chvostu distribuce
#   průměr ze čtrvtých mocnin z-skorů mínus 3
#   měří se pomocí: směrodatná odchylka, střední hodnota a rozptyl
#   koeficient označen K
#   "Těžké konce" (heavy/fat tails) označují distribuci s vyšším výskytem hodnot v odlehlých oblastech
#   "Lehké konce" (light/thin tails) indikují distribuci s menším výskytem hodnot v odlehlých oblastech
Kurt(weight)

# Histogram
# ukazuje frekveční rozdělení
ggplot(Policie, aes(x = weight)) +
  geom_histogram(binwidth = 5, fill = "pink", color = "black") +
  labs(title = "Histogram váhy policistů", x = "Váha", y = "Frekvence") +
  theme_minimal()

# BoxPlot
#   používá Tukeyho čísla
ggplot(Policie, aes(y = weight)) +
  geom_boxplot(fill = "pink", color = "black") +
  labs(title = "Box Plot pro váhu", y="váha") +
  theme_minimal()



## Kategorická proměnná
#   n(i) - Absolutní četnost = počet výsktytů konkrétní hodnoty v datové sadě
#   N(i) - Kumulativní absolutní četnost = součet všech počtů výskytů konkrétní hodnoty a všech hodnot před ní
#   f(i) - Běžná relativní četnost = procentuální podíl počtu výskytů konkrétní hodnoty ze všech hodnot v sadě.
#   F(i) - Kumulativní relativní četnost = procentuální podíl počtu výskytů konkrétní hodnoty a všech hodnot před ní



# Absolutní četnost
table(typ)
# Kumulativní absolutní četnost
cumsum(table(typ))
# Běžná relativní četnost
round(prop.table(table(typ)), 4)
# Kumulativní relativní četnost
cumsum(round(prop.table(table(typ)),4))

# Sloupcový graf
ggplot(cars, aes(x = Type)) +
  geom_bar(fill = 'violet', color = 'black') +
  labs(title = "Sloupcový graf typu aut", x = "Typ", y = "Frekvence") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

# Výpočet procent pro koláčový graf
cars_summary <- cars %>%
  group_by(Type) %>%
  summarise(Frekvence = n()) %>%
  mutate(Procenta = Frekvence / sum(Frekvence) * 100)

# Koláčový graf s popisky a procenty
ggplot(cars_summary, aes(x = "", y = Frekvence, fill = Type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Procenta, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Koláčový graf typu aut", fill = "Typ") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# Frekvenční křivka
ggplot(cars, aes(x = Type)) +
  geom_line(stat = "count", aes(group = 1), color = "blue", linewidth = 1) +
  geom_point(stat = "count", color = "red", size = 2) +
  labs(title = "Frekvenční křivka typu aut", x = "Typ", y = "Frekvence") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

# Sloupcový graf s frekvenční křivkou
ggplot(cars, aes(x = Type)) +
  geom_bar(aes(y = after_stat(count)), fill = 'skyblue', color = 'black') +
  geom_line(stat = "count", aes(y = after_stat(count), group = 1), color = "red", size = 1) +
  geom_point(stat = "count", aes(y = after_stat(count)), color = "black", size = 2) +
  labs(title = "Kombinovaný barplot a frekvenční křivka typu aut", x = "Typ", y = "Frekvence") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
