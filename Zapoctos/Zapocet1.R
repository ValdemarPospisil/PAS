library(ggplot2)

#Je monžné považovat proměnnou ss3 za normálně rozdělenou?
#histogram
ggplot(VysledkyGEO, aes(x=ss3)) +
  geom_histogram(binwidth = 0.1, color = "black", fill="blue") +
  labs(title = "Histogram průměrná známka z 3. ročníku", x="ss3", y="Frekvence")

ss3 <- VysledkyGEO$ss3
# Q-Q plot
qqnorm(ss3)
qqline(ss3, col = "red")
# shapiro
shapiro.test(ss3)
# Kolmogorov_smirnov
ks.test(ss3, "pnorm", mean(ss3), sd(ss3))

library(moments)
Skew(ss3)
Kurt(ss3)

# Na základě provedených testů  lze dojít k závěru že promměnná ss3 není normálně rozdělená.
# Shapirův ukázal hodnotu p hodnotu 0.01336, což je nižší než hranice 0.05.
# V grafu vidíme odchylky a Skew a Kurt které se bliží 1 dále potvrzují asymetrii od standartního tvaru
# normální kčivky.

------------------------------------------------------------------------------------------------------

# Spočtěte a interpretujte 95%-ní interval spolehlivosti pro celkový počet bodů u přijjímaček.
library(DescTools)
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

# hodnota kontingeční tabulky je větší než 0.05 tudíž není statisticky významný
# vztah mezi pohlavím a volbou oboru.
# podle grafu tam rozdíy vidět jsou

-------------------------------------------------------------------------------------------

# Existuje souvislost mezi známkami  ze třetích ročníků SŠ 
#  a celkovým počtem bodů u přijímaček (proměnné ss3 a celprij)? 
#  Případnou závislost popište: je silná, nebo slabá a přímá, nebo nepřímá? O co se opíráte?

# Rozptylový diagram
ss3 <- VysledkyGEO$ss3
celprij <- VysledkyGEO$celprij
plot(ss3, celprij, main = "Rozptylový diagram: Známky ze třetích ročníků vs. Celkové body", 
    xlab = "Známky ze třetích ročníků (ss3)", ylab = "Celkové body u přijímaček (celprij)")

# Pearsonův korelační koeficient
pearson_corr <- cor.test(ss3, celprij, method = "pearson")
print(pearson_corr)

# Spearmanův korelační koeficient
spearman_corr <- cor.test(ss3, celprij, method = "spearman")
print(spearman_corr)


  