library(ggplot2)
library(readxl)
library(MASS)
library(asbio)
library(moments)
library(DescTools)
VysledkyGEO <- read_excel("Data/prij.xlsx", 
                          sheet = "Sheet1")

# -----------------------------------------------------------------------------------
# 1. Je možné považovat proměnnou ss3 za normálně rozdělenou? K posouzení použijte vhodné grafy
# a v případě odchylky od normality popište.
mean_ss3 <- mean(VysledkyGEO$ss3, na.rm = TRUE)
sd_ss3 <- sd(VysledkyGEO$ss3, na.rm = TRUE)

# Histogram s přidanou normální křivkou
ggplot(VysledkyGEO, aes(x=ss3)) +
  geom_histogram(aes(y=..density..), binwidth = 0.1, color = "black", fill="blue") +
  stat_function(fun = dnorm, args = list(mean = mean_ss3, sd = sd_ss3), color = "red", size = 1) +
  labs(title = "Histogram průměrná známka z 3. ročníku", x="ss3", y="Hustota") +
  theme_minimal()

ggplot(VysledkyGEO, aes(sample = ss3)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot pro ss3", x="Teoretické kvantily", y="Vzorkové kvantily") +
  theme_minimal()

ggplot(VysledkyGEO, aes(y = ss3)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box Plot pro ss3", y="ss3") +
  theme_minimal()

ggplot(VysledkyGEO, aes(x=ss3)) +
  geom_density(color = "blue", fill = "lightblue") +
  stat_function(fun = dnorm, args = list(mean = mean(VysledkyGEO$ss3, na.rm = TRUE), sd = sd(VysledkyGEO$ss3, na.rm = TRUE)), color = "red") +
  labs(title = "Density Plot pro ss3", x="ss3", y="Hustota") +
  theme_minimal()


ss3 <- VysledkyGEO$ss3
# Q-Q plot
qqnorm(ss3)
qqline(ss3, col = "red")
# shapiro
shapiro.test(ss3)
# Kolmogorov_smirnov
ks.test(ss3, "pnorm", mean(ss3), sd(ss3))


Skew(ss3)
Kurt(ss3)

# Na základě provedených testů  lze dojít k závěru že promměnná ss3 není normálně rozdělená.
# Shapirův ukázal hodnotu p hodnotu 0.01336, což je nižší než hranice 0.05.
# V grafu vidíme odchylky a Skew a Kurt které se bliží 1 dále potvrzují asymetrii od standartního tvaru
# normální kčivky.

------------------------------------------------------------------------------------------------------

# Spočtěte a interpretujte 95%-ní interval spolehlivosti pro celkový počet bodů u přijjímaček.

celprij <- VysledkyGEO$celprij
sd_celprij <- sd(celprij)

MeanCI(celprij,sd_celprij=sd_celprij)

# Na základě dat je 95%-ní pravděpodobnost že skutečný průměr celkový počet bodů u přijímaček
# se nachází mezi dolní hodnotou (135.0274) a horní hodnotou (146.35260).
# Tento interval vyjadřuje míru nejistoty ohledně odhadu skutečného průměru.

-------------------------------------------------------------------------------------------------------
  
# Liší se výsledky u přijímaček mezi pohlavími? A pokud ano, je rozdíl statisticky významný? 

Pohlavi <- VysledkyGEO$Pohlavi
DescStat <- Desc(celprij ~ Pohlavi)

shapiro.test(celprij[Pohlavi == "m"])
shapiro.test(celprij[Pohlavi == "z"])

t.test(celprij ~ Pohlavi)
MeanDiffCI(celprij ~ Pohlavi, conf.level=0.99)

ggplot(VysledkyGEO, aes(x = Pohlavi, y = celprij, fil = Pohlavi)) +
  geom_violin(trim= FALSE)

# na základě výsledků a grafu vidíme že rozdíl ve výsledcích mezi pohlavím je.
# ale jelikož p-hodnota t.testu vyšla vyšší než 0.05 (0.517)
# a v intervalu spolehlivosti mezi těmito dvěma proměnnými se nachází nula (dolní mez -11.5 a horní mez 18.9)
# tak můžeme říci že rozdíl není statisticky významný

------------------------------------------------------------------------------
# Existuje nějaký vztah mezi oborem studia a pohlavím? Volí studenti stejné obory v závislosti na pohlaví,
# nebo jsou mezi muži a ženami rozdíly? 
# Pokud si myslíte, že jsou rozdíly, zkuste je popsat vlastními slovy.
  
# Kontingenční tabulka
Pohlavi <- VysledkyGEO$Pohlavi
Obor <- VysledkyGEO$Obor
table_obor_pohlavi <- table(Pohlavi, Obor)
print(table_obor_pohlavi)

# Chí-kvadrát test nezávislosti
chi_square_result <- chisq.test(table_obor_pohlavi)
print(chi_square_result)

# Mozaikový graf
mosaicplot(table_obor_pohlavi, main = "Mozaikový graf: Pohlaví vs. Obor studia", color = TRUE)

# P-hodnota: Pokud je p-hodnota větší než 0.05, neexistuje dostatek důkazů pro odmítnutí nulové hypotézy, 
# která předpokládá nezávislost mezi pohlavím a volbou oboru studia.
# Mozaikový graf: I když chí-kvadrát test nenaznačuje statisticky významný vztah, 
# může mozaikový graf ukázat určité vizuální rozdíly mezi skupinami.

#  Na základě chí-kvadrát testu nebyl nalezen statisticky významný vztah mezi pohlavím a volbou oboru studia (p > 0.05).
# To znamená, že podle těchto dat není pohlaví studentů spolehlivým prediktorem výběru oboru studia.
# Nicméně vizuální analýza prostřednictvím mozaikového grafu naznačuje určité rozdíly,
# které mohou být zajímavé pro další kvalitativní nebo kvantitativní analýzu.
-------------------------------------------------------------------------------------------

# Existuje souvislost mezi známkami  ze třetích ročníků SŠ 
#  a celkovým počtem bodů u přijímaček (proměnné ss3 a celprij)? 
#  Případnou závislost popište: je silná, nebo slabá a přímá, nebo nepřímá? O co se opíráte?

# Rozptylový diagram
ss3 <- VysledkyGEO$ss3
celprij <- VysledkyGEO$celprij

correlation <- cor(ss3, celprij, method = "pearson")
correlation

ggplot(data.frame(ss3, celprij), aes(x = ss3, y = celprij)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter plot známek ze SŠ a počtu bodů u přijímaček", 
       x = "Známky ze SŠ", y = "Počet bodů u přijímaček") +
  theme_minimal()
model <- lm(celprij ~ ss3, data = VysledkyGEO)
summary(model)

# Korelační koeficient rr se pohybuje v rozmezí od -1 do 1.
# r>0r>0: Přímá závislost (zvyšování jedné proměnné zvyšuje druhou).
# r<0r<0: Nepřímá závislost (zvyšování jedné proměnné snižuje druhou).
# ∣r∣≈0∣r∣≈0: Slabá nebo žádná lineární závislost.
# ∣r∣>0.7∣r∣>0.7: Silná závislost.
# na základě pearson testu můžeme říci že souvislot je slabá a nepřímá jelikož korelace nám vyšla
# -0.303 což je pud nulou tudíž je nepřímá a je to blízko nule tudíž je slabá.
# graf pouze potvrzuje toto tvrzení jelikož tam není vidět skoro žádná souvislost.

--------------------------------------------------------------------------------------------------

# Porovnejte známky z matematické geografie a z geologie (proměné matzem a geol)
# pomocí jejich tří kvartilů. Liší se od sebe kvartily těchto proměných?

matzem <- VysledkyGEO$matzem
geol <- VysledkyGEO$geol
# Výpočet kvartilů pro obě proměnné
quartiles_matzem <- quantile(matzem, probs = c(0.25, 0.5, 0.75))
quartiles_geol <- quantile(geol, probs = c(0.25, 0.5, 0.75))

# Zobrazení výsledků
quartiles_matzem
quartiles_geol

# Připrava dat pro graf
data <- data.frame(
  Znamky = c(matzem, geol),
  Predmet = rep(c("Matematická geografie", "Geologie"), each = length(matzem))
)

# Vytvoření boxplotu
ggplot(data, aes(x = Predmet, y = Znamky, fill = Predmet)) +
  geom_boxplot() +
  labs(title = "Porovnání známek z matematické geografie a geologie", x = "Předmět", y = "Známky") +
  theme_minimal()

ggplot(data.frame(matzem), aes(x = matzem)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Histogram známek z matematické geografie", x = "Známky", y = "Frekvence") +
  theme_minimal()

# Histogram pro geol
ggplot(data.frame(geol), aes(x = geol)) +
  geom_histogram(binwidth = 0.5, fill = "pink", color = "black") +
  labs(title = "Histogram známek z geologie", x = "Známky", y = "Frekvence") +
  theme_minimal()

# I když u obou předmětů převldádá 2 jako známka, známky z geologie mají spíše 3 a a z matematické
# geografie jsou spíše jedničky
# tudíž se dá říci že geologie je těžší než matematická geografie aspoň co se známek studentů týče.

-------------------------------------------------------------------------------------------
  