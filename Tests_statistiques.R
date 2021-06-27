################################################################################################
################# TESTS STATISTIQUES COMPARAISON MOYENNE A VALEUR DE REFERENCE #################
################################################################################################

################################################################################################
### On étudie le nb annuel de sorties en boîte des 20-25 ans dans la région parisienne.
### On souhaiterait le comparer à la moyenne nationale qui est de 50.
### Sur un échantillon de 24 jeunes de cette région, on obtient une moyenne de Xobs = 54.5.
### On suppose que l'écart type est le même que dans la pop nationale: 10
### Le seuil de signification est fixé à 0.05.

nb_sorties_boite <- c(46, 64, 50, 56, 52, 64, 42, 56, 70, 44, 52, 56, 44, 42, 38, 68, 68, 52, 52, 66, 62, 64, 44, 56)

muref <- 50
muobs <- mean(nb_sorties_boite)
ecart_type <- 10
alpha <- 0.05
n <- 24


# 1) Quel test faire? Test de comparaison d'une moyenne à une valeur de référence (Gauss) car variance connue et estimée et 
# la variable suit une Loi Normale:

# Test de la normalité d'une distribution
shapiro.test(nb_sorties_boite) # pvalue > 0.05 donc on accepte H0: la variable dont provient l'échantillon suit une
# Loi Normale

qqnorm(nb_sorties_boite)
qqline(nb_sorties_boite)


# 2) Quelles sont les hypothèses du test? Hypothèse bilatérale: H0: muobs = muref = 50        HA: muobs ≠ µref ≠ 50


# 3) Quelle est la formule de la statistique de test et quelle est sa valeur?
Sobs = (muobs - muref)/(ecart_type/sqrt(n)) # 2.20


# 4) Quelle est la conclusion du test? 
# Il s'agit d'un test bilatéral, je calcule donc les quantiles (1-alpha/2) et (alpha/2) de la loi Normale:
qnorm(1-alpha/2) # 1.96
qnorm(alpha/2) # -1.96`


# Sobs non compris dans l'intervalle des quantiles de la Loi Normale donc je rejette H0.
# Les jeunes de 20/25 ans en région parisienne ne se comportent pas comme la moyenne nationale 


# 5) Avec la pvaleur
2*(1-pnorm(abs(Sobs))) # 0.03 < 0.05 Le test est significatif et je rejette donc H0




################################################################################################
### 16 footballers d'un club sont tirés au sort afin d'être contrôlés pour leur dosage d'EPO.
### Ainsi la variable X de moyenne inconnue mu, mesure le dosage d'EPO.
### On veut tester, avec un risque de première espèce de 5%, si le taux moyen dans le sang des joueurs de ce club 
### est au dessus du seuil de positivité au dopage EPO qui est de 0.4.
### La variable X est supposée normale avec une variance connue 0.04.

dosage_epo <- c(0.35, 0.4, 0.65, 0.27, 0.24, 0.59, 0.73, 0.13, 0.24, 0.48, 0.5, 0.70, 0.35, 0.28, 0.74, 0.45)

muobs <- mean(dosage_epo)
muref <- 0.4
sigma <- sqrt(0.04)
alpha <- 0.05
n <- 16


# 1) Quel test? Test de comparaison d'une moyenne à une valeur de référence car variance connue et estimée et 
#    la variable suit une Loi Normale:

# Test de la normalité d'une distribution
shapiro.test(dosage_epo) # pvalue > 0.05 donc on accepte H0: la variable dont provient l'échantillon suit une Loi Normale

qqnorm(dosage_epo)
qqline(dosage_epo)


# 2) Quelles hypothèses? Hypothèse unilatérale droite: H0: muobs <= muref donc <= 0.4     HA: muobs > muref donc > 0.4


# 3) Quelle est la formule de la statistique de test et quelle est sa valeur?
Sobs = (muobs - muref)/(sigma/sqrt(n)) # 0.875


# 4) Quelle est la conclusion du test? Comme il s'agit d'un test unilatéral à droite, je calcule le quantile 1-alpha de la Loi Normale
qnorm(1-alpha) # 1.64 => Sobs < 1.64 donc je valide H0. Le dosage moyen EPO des 16 footballers est < au seuil de positivité


# 5) Avec la pvaleur
(1-pnorm(abs(Sobs))) # 0.19 > 0.05 donc je valide H0.


# 6) Même chose mais en supposant que variance inconnue
#    Pour une variance inconnue la statistique de test est:
variance_relle <- var(dosage_epo)
Sobs <- (muobs - muref)/(sqrt(variance_relle)/sqrt(n)) # 0.90 < qnorm(1-alpha) donc je valide H0


# Avec la pvaleur
(1-pnorm(abs(Sobs))) # 0.18 > 0.05 donc je valide H0.


# Test de Student:
t.test(dosage_epo, mu = muref, alternative = "greater", conf.level = 0.95)




################################################################################################
################ TESTS STATISTIQUES COMPARAISON PROPORTION A VALEUR DE REFERENCE ###############
################################################################################################


### Le Gouvernement français se demande si le taux de létalité du covid en france est le même qu'en Chine (d'où est parti le virus)
### ou si le virus a mutué
### Risque alpha = 0.05
### Une étude Chinoise indique un taux de létalité de 0.023
### En France, sur les 191 cas recensés, 3 sont décédés.

alpha <- 0.05
prop_ref_chine <- 0.023
prop_fr <- 3/191
n <- 191


# 1) Quel test faire ? Il faut faire un test de proportion (comparaison d'une proportion à une valeur de référence)
binom.test(x = 3, n = 191, p = 0.023, alternative = "two.sided", conf.level = 0.95)


# 2) Quelles sont les hyp du test? Hypothèse bilatérale:
#    H0 : taux de létalité en France identique à celui de la Chine    H0: prop_fr = 0.023
#    HA: taux de létalité en France différent de celui de la Chine    H0: prop_fr != 0.023


# 3) Quelle est la formule de la statistique de test et quelle est sa valeur?
Sobs = (prop_fr - prop_ref_chine)/(sqrt((prop_ref_chine*(1-prop_ref_chine))/n)) # -0.67


# 4) Quelle est la conclusion du test? 
#    Il s'agit d'un test bilatéral, je calcule donc les quantiles (1-alpha/2) et (alpha/2) de la loi Normale:
qnorm(1-alpha/2) # 1.96
qnorm(alpha/2) # -1.96
# Sobs compris dans l'intervalle des quantiles de la Loi Normale donc j'accepte H0.  
# Le taux de létalité du covid en France est le même qu'en Chine


# 5) Avec la pvaleur:
(1-pnorm(abs(Sobs))) # 0.25 > 0.05 donc test non significatif et on accepte H0.




################################################################################################
### Quand approprié, on suppose que les variables numériques suivent une distribution normale et qu’il y a 
### égalité des variances dans les différents groupes.

### A/ Une coach sportive cherche à évaluer l’efficacité de son programme sur la prise de masse chez les sportifs. 
### Pour se faire, elle suit pendant 6 mois 30 clients et évalue à la fin du programme si leur objectif de prise 
### de masse a été atteint. Pour que le programme soit considéré comme efficace, elle souhaiterait montrer que 
### plus de 70% des clients ont atteint leur objectif.


# 1) Définir la population et l’échantillon:
#    population: les sportifs
#    échantillon: 30 clients de la coach sportives


# 2) Quelles sont les variables étudiées ? 
#    Atteinte de l'objectif : OUI / NON


# 3) De quels types sont-elles ?
#    variable qualitative norminale


# 4) Comment présenteriez-vous les données (avec et sans l’aide d’un graphique) ?
#    Proportion d'échec et de réussite au programme
#    Diagramme circulaire ou en barres


# 5) Quel test envisagerez-vous ?
#    Test de comparaison d'une proportion à une valeur théorique


# 6) Quelles hypothèses? Hypothèse unilatérale droite
#    H0: la proportion des sportifs suivis par la coach ayant atteint l'objectif est <= 0.70
#    HA: la proportion des sportifs suivis par la coach ayant atteint l'objectif est > 0.70





################################################################################################
######################## TESTS STATISTIQUES COMPARAISON DE DEUX MOYENNES #######################
################################################################################################


################################################################################################
### Dans le cadre d’une étude clinique, on cherche à savoir si les sujets diagnostiqués positifs 
### pour une pathologie ont des âges similaires ou différents que les sujets négatifs. 
### Sur les 154 sujets, 77 sont négatifs et 77 positifs. α= 0.05

setwd("C:/Users/milie/Desktop/MBA MBDCDO/COURS/DATAMINING/PROJETS_R/04_STAT_TEST/data")
posneg <- read.table("posneg.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

# 1) Quels indicateurs et quelle représentation? 
#    Indicateurs: âge moyen (variable quantitative continue) en fonction du diagnostic (variable qualitative nominale).
#                 on peut donc calculer la moyenne, variance, écart type, les quartiles, le max, le min par diagnostic.
library(dplyr)
posneg %>%
  group_by(statut) %>%
  summarize(min(age), max(age), mean(age), var(age), sd(age), quantile(age, 0.25), median(age), quantile(age,0.75))


#    Représentation: boxplot
qplot(statut, age, data = posneg, ylab= "Âge", 
      main = "Distribution de l’âge selon le statut", geom = "boxplot", fill = statut) 


# 2) Quel test statistique? 
#    Comparaison de 2 positions avec n >=30 et la variable dont provient l'échantillon suit une Loi Normale

# Test de la normalité d'une distribution
library(rstatix)
posneg %>%
  group_by(statut) %>%
  shapiro_test(age) # Les pvalues de chaque statut sont > 0.05 donc on accepte H0: la normalité est supposée

qqnorm(posneg$age)
qqline(posneg$age)


# 3) Quelles sont les hypothèses? Hypothèse bilatérale
#    H0: âge moyen des sujets positifs = âge moyen des sujets négatifs
#    HA: âge moyen des sujets positifs != âge moyen des sujets négatifs


# 4) Quelles sont les conditions du test?
#    n >= 30 et Loi Normale => conditions respectées
#    Il faut tester l'égalité des variances:

# On pose H0 : le rapport entre les variances est = 1 et HA: le rapport entre les variances ≠ est de 1
var.test(age ~ statut, data = posneg, var.equal = T)  # => Fisher
# pvalue = 0.1283 
# pvalue > 0.05 donc H0 d'égalité des variances validée


# 5) Effectuez le test: toutes conditions sont respectées, on peut faire le test paramétrique de Student:
t.test(age ~ statut, data = posneg, var.equal = T) # 0.0009778
# pvalue < 0.05 donc le test est significatif et on rejette HO. L'âge moyen des sujets positifs est != de celui des sujets négatifs.
# Les sujets positifs sont plus âgés que les sujets négatifs.




################################################################################################
## Une barmaid vient de créer un cocktail et elle aimerait montrer à son responsable qu’il est plus apprécié que 
## le cocktail maison actuel. Pour se faire, elle décide de relever sur 1 mois, le nombre de ventes quotidiennes 
## de chacun des deux types de cocktails et de les comparer.

# 1) Définir la population et l’échantillon
#    Population: consommation des deux cocktails
#    Echantillon: ventes quotidiennes sur un mois des deux types de cocktails


# 2) Quelles sont les variables étudiées ? De quels types sont-elles ?
#    Variables quantitatives discrètes (nombre de ventes du nouveau cocktail sur un mois et nombre de ventes du cocktail maison sur 1 mois)


# 3) Comment présenterez-vous les données?
#    Box plot pour chacune des deux séries


# 4) Quelles sont les hypothèses? 
#    Hypothèse unilatérale à droite: 
#    H0: moyenne des ventes du nouveau cocktail <= moyenne des ventes du cocktail maison
#    HA: moyenne des ventes du nouveau cocktail > moyenne des ventes du cocktail maison


# 5) Quelles sont les conditions du test?
#    Comme on suppose que les variables suivent une distribution normale et que les variances sont identiques : t test 




################################################################################################
#################### TESTS STATISTIQUES COMPARAISON DE PLUS DE DEUX MOYENNES ###################
################################################################################################

################################################################################################
### En 1933,Ronald Aylmer Fisher,le «père» de la statistique «moderne», s’est posé une question essentielle:
### est ce que la longueur des sépales des espèces setosa, versicolor et virginica (qui sont des iris) est 
### identique ou est ce qu’il y a des différences. Pour se faire il a récupéré 50 iris de chaque espèce et a
### mesuré la longueur de leur sépale. α=0.05


# 1) Quels indicateurs?
library(dplyr)
iris %>%
  group_by(Species) %>%
  summarize(min(Sepal.Length), max(Sepal.Length), mean(Sepal.Length), var(Sepal.Length), sd(Sepal.Length), 
            quantile(Sepal.Length, 0.25), median(Sepal.Length), quantile(Sepal.Length,0.75))



# 2) Quelle représentation?
#    Boxplot
qplot(Species, Sepal.Length, data = iris, ylab= "Longueur du sépale", 
      main = "Distribution de la longueur du sépale selon l'espèce", geom = "boxplot", fill = Species)



# 3) Quel test statistique?  
#    Test de comparaison de plus de deux positions 



# 4) Quelles sont les hypothèses?
#    Hypothèse bilatérale
#    H0: égalité des moyennes de longueur de sépales pour les 3 espèces: museto = muversi = muvirg
#    HA : ] (i,j) tel que µi =  µj


# 5) Quelles sont les conditions du test?
#    n > 30 et qui suit une Loi Normale => test paramétrique de l'ANOVA à un facteur
#    Test de la normalité d'une distribution
library(rstatix)
iris %>%
  group_by(Species) %>%
  shapiro_test(Sepal.Length) # Les pvalues de chaque espèce sont > 0.05 donc on accepte H0: la normalité est supposée

qqnorm(iris$Sepal.Length)
qqline(iris$Sepal.Length)

#    Pour vérifier l'égalité des variances: 
bartlett.test(residuals(fit) ~ iris$Species)
plot(fit,3)


# 6) Effectuez le test

fit <- aov(Sepal.Length ~ Species, data = iris)
summary(fit) # pvalue < 0.05 donc on rejette H0. Au moins une espèce d'iris a une longueur moyenne de sépale différente des autres.


#    On poursuit par un test post hoc pour comparer les groupes par paire.
pairwise.t.test(iris$Sepal.Length, iris$Species, "bonferroni")


#    Toutes les pvalues sont < 0.05 donc toutes les espèces ont des moyennes différentes.

library(dplyr)
iris %>%
  group_by(Species) %>%
  summarize(mean(Sepal.Length))


#    Virginica a la longueur de sépale la plus imp suivie de versicolor puis Setosa







################################################################################################
################################## TESTS STATISTIQUES MULTIPLES ################################
################################################################################################

################################################################################################

# Sur la base Ronfleurs qui recense pour 100 sujets leur âge (AGE), Indice de Masse Corporelle (IMC), 
# nombre de verres par semaine (ALCOOL), sexe (SEXE, 1 si femme 0 si homme), s’il ronfle (RONFLE, 1 si oui, 0 si non), 
# s’il fume (TABAC, 1 si oui, 0 si non), type de dormeur (TYPE_DORMEUR, petit, moyen ou gros).
# Voici les questions que l'on se pose : 


ronfleurs <- read.table("ronfleurs.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

###############################################################################
# A/ Y-a-t-il des différences d’âge entre les ronfleurs et les non ronfleurs ?# 
###############################################################################

# Indicateurs: variable quantitative continue (âge) et qualitative nominale
library(dplyr)
ronfleurs %>%
  group_by(RONFLE) %>%
  summarize(min(AGE), max(AGE), mean(AGE), var(AGE), sd(AGE), quantile(AGE, 0.25), median(AGE), quantile(AGE,0.75))


# Représentation: boxplot 
# Je transforme la variable RONFLE en OUI / NON
ronfleurs[which(ronfleurs$RONFLE == 0), "RONFLE"] = "NON"
ronfleurs[which(ronfleurs$RONFLE == 1), "RONFLE"] = "OUI"


qplot(RONFLE, AGE, data = ronfleurs, ylab= "Age", 
      main = "Distribution de l'âge selon le fait de ronfler", geom = "boxplot", fill = RONFLE)

# Test: test de comparaison de 2 moyennes avec n>30, Normalité et égalité des variances supposées
n <- nrow(ronfleurs)

# Test de la Normalité: Test de Shapiro Wilk
# H0: la variable dont provient l'échantillon suit une Loi Normale 
# HA: la variable dont provient l'échantillon ne suit pas une Loi Normale
library(rstatix)
ronfleurs %>%
  group_by(RONFLE) %>%
  shapiro_test(AGE) # Les pvalues de chaque statut sont > 0.05 donc on accepte H0: la normalité est supposée

qqnorm(ronfleurs$AGE)
qqline(ronfleurs$AGE)

# Test de l'égalité des variances:
# On pose H0 : le rapport entre les variances est = 1 et HA: le rapport entre les variances ≠ est de 1
var.test(AGE ~ RONFLE, data = ronfleurs, var.equal = T)  #=> pvalue = 0.8871 > 0.05 donc H0 validée, les variances sont supposées égales


# Hypothèses: hypothèse bilatérale
# H0: µ1 = µ2 <=> µ1 - µ2 = 0
# HA: µ1 ≠ µ2  <=> µ1 - µ2 ≠ 0
# avec µ1 l'âge moyen des non ronfleurs et µ2 l'âge moyen des ronfleurs

# Test paramétrique de Student car les conditions ont été vérifiées précédemment:
t.test(AGE ~ RONFLE, data = ronfleurs, var.equal = T) # pvalue = 0.01556 < 0.05, test significatif, on rejette H0, les âges 
# sont différents entre les ronfleurs et non ronfleurs.
# Les sujets qui ronflent sont plus âgés.






###############################################################################
# B/ Est-ce que les ronfleurs consomment plus d’alcool que les non ronfleurs ?#
###############################################################################

# Indicateurs: variable quantitative discrète (verres d'alcool) et qualitative nominale
library(dplyr)
ronfleurs %>%
  group_by(RONFLE) %>%
  summarize(min(ALCOOL), max(ALCOOL), mean(ALCOOL), var(ALCOOL), sd(ALCOOL), quantile(ALCOOL, 0.25), median(ALCOOL), quantile(ALCOOL,0.75))


# Représentation: boxplot 
qplot(RONFLE, ALCOOL, data = ronfleurs, ylab= "Verres d'alcool", 
      main = "Distribution des verres d'alcool selon le fait de ronfler", geom = "boxplot", fill = RONFLE)

# Test: test de comparaison de 2 moyennes avec n>30, Normalité et égalité des variances supposées
n <- nrow(ronfleurs)

# Test de la Normalité: Test de Shapiro Wilk
# H0: la variable dont provient l'échantillon suit une Loi Normale 
# HA: la variable dont provient l'échantillon ne suit pas une Loi Normale
qqnorm(ronfleurs$AGE)
qqline(ronfleurs$AGE)

# Test de l'égalité des variances:
# On pose H0 : le rapport entre les variances est = 1 et HA: le rapport entre les variances ≠ est de 1
var.test(ALCOOL ~ RONFLE, data = ronfleurs, var.equal = T)  #=> pvalue = 0.5482 > 0.05 donc H0 validée, les variances sont supposées égales


# Hypothèses: hypothèse unilatérale à gauche
# H0: µ1 >= µ2 
# HA: µ1 < µ2  
# avec µ1 le nombre de verres d'alcool moyen des non ronfleurs et µ2 le nombre de verres d'alcool moyen des ronfleurs

# Test paramétrique de Student car les conditions ont été vérifiées précédemment:
t.test(ALCOOL ~ RONFLE, data = ronfleurs, var.equal = T, alternative = "less") # pvalue = 0.00894 < 0.05, test significatif, on rejette H0, 
# les ronfleurs consomment plus d'alcool que les non ronfleurs








###############################################################  A VERIFIER (pas le même resultat et dans quel sens mettre les proportions) )
# C/ Est-ce que les fumeurs ronflent plus que les non-fumeurs?#
###############################################################

# Indicateurs: effectifs / proportion

# Représentation: diagramme en barres
# Je transforme la variable TABAC en OUI / NON
ronfleurs[which(ronfleurs$TABAC == 0), "TABAC"] = "NON"
ronfleurs[which(ronfleurs$TABAC == 1), "TABAC"] = "OUI"

# Effectifs
qplot(data=ronfleurs, x = RONFLE, fill = TABAC,geom="bar", 
      main ="Distribution des fumeurs selon le fait de ronfler", ylab = "Effectifs")

# Proportion
library(scales)
ggplot(ronfleurs) +
  aes(x = TABAC, fill = RONFLE) +
  geom_bar(position = "fill") +
  xlab("Fumeur") +
  ylab("Proportion") +
  labs(fill = "RONFLE") +
  ggtitle("Proportion de ronfleurs / non ronfleurs \n parmis les fumeurs") +
  scale_y_continuous(labels = percent)

# Test: comparaison de deux proportions

# Hypothèses: Hyp unilatérale à gauche
# H0: pNF >= pF
# HA: pNF < pF avec p la proportion de ronfleurs

# J'ordonne mon facteur RONFLE: OUI en premier car c'est ce qu'on analyse
ronfleurs$RONFLE = relevel(as.factor(ronfleurs$RONFLE),"OUI")

TC =table(TABAC = ronfleurs$TABAC, RONFLE = ronfleurs$RONFLE)
prop.table(TC, 1)*100 # 42% des non fumeurs ronflent et 31% des fumeurs ronflent

prop.test(TC, correct = FALSE, alternative = "less")
# pvalue > 0.05 donc on accepte H0, les proportions ne sont pas assez significativement différentes. Les non fumeurs ronflent
# plus que les fumeurs




#################################################
# D/ Y-a-t-il des différences d’âges entre les types de dormeurs ?


# Indicateurs: moyenne d'âge par types de dormeur
library(dplyr)
ronfleurs %>%
  group_by(TYPE_DORMEUR) %>%
  summarize(min(AGE), max(AGE), mean(AGE), var(AGE), sd(AGE), quantile(AGE, 0.25), median(AGE), quantile(AGE,0.75))


# Représentation: boxplot 
qplot(TYPE_DORMEUR, AGE, data = ronfleurs, ylab= "Age", 
      main = "Distribution de l'âge selon le type de dormeur", geom = "boxplot", fill = TYPE_DORMEUR)

# Test: test de comparaison de plus de 2 moyennes avec n>30, Normalité et égalité des variances supposées
n <- nrow(ronfleurs)

# Test de la Normalité: Test de Shapiro Wilk
# H0: la variable dont provient l'échantillon suit une Loi Normale 
# HA: la variable dont provient l'échantillon ne suit pas une Loi Normale

library(rstatix)
ronfleurs %>%
  group_by(TYPE_DORMEUR) %>%
  shapiro_test(AGE)

qqnorm(ronfleurs$AGE)
qqline(ronfleurs$AGE)


# Hypothèses: hypothèse bilatérale
# H0: µP = µM = µG
# HA: ∃(i,  j)  tel  que  μi≠  μj

# On se sait pas s'il y a égalité des variances: soit ANOVA à un facteur soit test de Kruskall-Wallis
fit <- aov(AGE ~ TYPE_DORMEUR, data = ronfleurs)
summary(fit) # pvalue = <2e-16 < 0.05, test significatif donc on rejette HO: il y a au moins un groupe qui a une moyenne 
# d'âge différente des deux autres


# Test de l'égalité des variances:
# On pose H0: les variances sont identiques et HA: au moins une des variances est ≠ d'une autre
bartlett.test(residuals(fit) ~ ronfleurs$TYPE_DORMEUR) # pvalue > 0.05 donc on accepte H0 d'égalité des variances.
# Le test de l'ANOVA à un facteur est indiqué.

plot(fit,3)

# Test post hoc pour savoir quels groupes diffèrent: TUKEY
TukeyHSD(fit) # tous les groupes sont différents: les petis dormeurs sont plus âgés que les moyens dormeurs qui sont
# plus âgés que les gros dormeurs

ronfleurs %>%
  group_by(TYPE_DORMEUR) %>%
  summarize(mean(AGE))



################################################################################################
############################# ASSOCIATION ENTRE 2 VARIABLES (KHI2) #############################
################################################################################################

##############################################
########### 2 VARIABLES QUALITATIVES #########
##############################################


################################################################################################
## Pour 600 individus tirés au sort dans une population représentative en âge de voter, on leur a demandé
# leur âge et de choisir entre 4 partis politiques duquel ils se sentaient le plus proche.
# On se demande si la répartition des votes est la même entre chaque classe d’âge.

politique <- read.table("politique.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
colnames(politique) <-c("parti", "classe_age")

# 1) Quelle représentation graphique: il s'agit de deux variables qualitatives (classe d'âge et parti politique).
# Les données peuvent être représentées à l'aide d'un diagramme en barres avec pour chaque classe d'âge, le % de personnes
# ayant voté pour tel ou tel parti
TC = table(politique$classe_age, politique$parti)
TC_ligne <- round(prop.table(TC, 1) * 100)


library(scales)
ggplot(politique) +
  aes(x = classe_age, fill = parti) +
  geom_bar(position = "fill") +
  xlab("Classe d'âge") +
  ylab("Proportion") +
  labs(fill = "Partis politiques") +
  scale_y_continuous(labels = percent)


# 2) Calculez les effectifs observés pour chaque tranche d'âge
effectifs_observes = table(politique$classe_age, politique$parti)
addmargins(TC)

# 3) Calculez les effectifs théoriques pour chaque tranche d'âge
test_politique <- chisq.test(effectifs_observes, correct = FALSE)
effectifs_theoriques <- test_politique$expected

# Hypothèses: hypothèse bilatérale avec 
# H0: indépendance entre les 2 variables qualitatives
# HA: association entre les 2 variables qualitatives


# 3) Calculer la statistique de test du Khi2 d'indépendance avec alpha = 0.05.
# Suit une loi du Khi2 à 6 degrés de liberté
S = sum((effectifs_observes - effectifs_theoriques)^2/(effectifs_theoriques)) # 203.25


# Calcul des degrés de liberté?
test_politique$parameter

# Calcul de la zone de rejet: [12.59 ; + inf[
qchisq(1-0.05, 6)
# Stat de test comprise dans la zone de rejet donc on rejette H0: il y a association entre les deux variables qualitatives


# 4) Calcul de la pvaleur
test_politique$p.value # 3.860076e-41 < 0.05: test significatif, on rejette H0.



################################################################################################
# On veut comparer la répartition de médecins qui font un burn out en fonction de s'ils exercent en 
# cabinet de ville, hôpital privé ou hôpital public. Sur 800 médecins étudiés, on obtient le tableau 
# de contingence suivant. Y a t-il un lien entre le fait de faire un burn out et le lieu d'exercice du métier?


## Table de contingence
a <- c(128, 175, 72)
b <- c(272, 75, 78)

tableau <- data.frame(a, b)

colnames(tableau) <- c("Burn_out", "Pas_Burn_out")
row.names(tableau) <- c("Cabinet_ville", "Hopital_public", "Hopital_prive")

## Représentation graphique: il s'agit de deux variables qualitatives (lieu d'exercice et fait de faire un Burn Out).
# Les données peuvent être représentées à l'aide d'un diagramme en barres avec pour chaque lieu d'exercice, le % de personnes
# ayant fait ou non un burn out


## Hypothèses: hypothèse bilatérale avec:
# H0: indépendance entre les 2 variables qualitatives
# HA: association entre les 2 variables qualitatives

# Test du KHI2 d'indépendance car chaque effectif du tableau est >= 5 et les deux variables sont qualitatives
test_burn_out <- chisq.test(tableau, correct = FALSE)

# Effectifs observés et théoriques:
eff_observes <- test_burn_out$observed
eff_theoriques <- test_burn_out$expected

# Statistique de test suit une loi du KHI2 à 2 degrés de liberté
test_burn_out$parameter
S = sum((eff_observes - eff_theoriques)^2/(eff_theoriques)) # 89.30

# Calcul de la zone de rejet: [5.99 ; + inf[
qchisq(1-0.05, 2)
# Stat de test comprise dans la zone de rejet donc on rejette H0, il y a association entre les 2 variables

# Avec la pvalue:
test_burn_out$p.value # pvalue = 4.054309e-20 < 0.05 donc le test est significatif et on rejette H0




################################################################################################
## Est-ce que le fait d'être fumeur est associé au ronflement? 


ronfleurs <- read.table("ronfleurs.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

# 1) Quelle représentation graphique: il s'agit de deux variables qualitatives (fait de fumer et fait de ronfler).
# Les données peuvent être représentées à l'aide d'un diagramme en barres avec pour chaque fait de fumer OUI/NON, 
# le % de personnes qui ronflent ou non

ronfleurs[which(ronfleurs$RONFLE == 0), "RONFLE"] = "NON"
ronfleurs[which(ronfleurs$RONFLE == 1), "RONFLE"] = "OUI"
ronfleurs[which(ronfleurs$TABAC == 0), "TABAC"] = "NON"
ronfleurs[which(ronfleurs$TABAC == 1), "TABAC"] = "OUI"

library(scales)
ggplot(ronfleurs) +
  aes(x = RONFLE, fill = TABAC) +
  geom_bar(position = "fill") +
  xlab("Fait de ronfler") +
  ylab("Proportion") +
  labs(fill = "Fait de fumer") +
  scale_y_continuous(labels = percent)


# 2) Calculez les effectifs observés
effectifs_observes = table(RONFLE = ronfleurs$RONFLE, FUME = ronfleurs$TABAC)
addmargins(effectifs_observes)

# 3) Calculez les effectifs théoriques
test_ronfleurs <- chisq.test(effectifs_observes, correct = FALSE)
effectifs_theoriques <- test_ronfleurs$expected

# Hypothèses: hypothèse bilatérale avec 
# H0: indépendance entre les 2 variables qualitatives
# HA: association entre les 2 variables qualitatives


# 3) Calculer la statistique de test du Khi2 d'indépendance avec alpha = 0.05.
# Suit une loi du Khi2 à 1 degré de liberté
S = sum((effectifs_observes - effectifs_theoriques)^2/(effectifs_theoriques)) # 1,10


# Calcul des degrés de liberté?
test_ronfleurs$parameter

# Calcul de la zone de rejet: [3.84 ; + inf[
qchisq(1-0.05, 1)
# Stat de test non comprise dans la zone de rejet donc on accepte H0: il y a indépendance entre les deux variables qualitatives
# Le fait de fumer n'est pas lié au fait de ronfler

# 4) Calcul de la pvaleur
test_ronfleurs$p.value # 0.2945074 > 0.05: test non significatif, on accepte H0.




##############################################
########### 2 VARIABLES QUANTITATIVES ########
##############################################

x = c(0,5,10,15,20)
y = c(45, 42, 33, 31, 29)

plot(x,y) # linéaire (décroissante) donc calcul du coef de Pearson

cor(x,y, method = "pearson")


################################################################################################
## On va travailler sur la base poids_taille qui recense pour 318 sujets leur âge(age), poids(weight), taille(height)

poids_taille <- read.table("poids_taille.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

# 1) Y’a-t-il une association entre le poids et la taille des 318 sujets d’une étude?
# Quelle représentation graphique? Il s'agit de deux variables quantitatives donc représentation à l'aide d'un nuage de points
plot(poids_taille$weight,poids_taille$height, ylab = "Taille", xlab = "Poids", 
     main = "Taille en fonction du poids")

RL = lm(height ~ weight, poids_taille)
summary(RL)
abline(RL)



# 2) Quel coefficient et quel test? Coef de Pearson avec test de Pearson car lien linéaire et n>= 30
sum(is.na(poids_taille$weight)) # 1
sum(is.na(poids_taille$height)) # 0


# Afficher les lignes dont la valeur dans la colonne "colonne" est NA
poids_taille[is.na(poids_taille$weight),]


# Supprimer les lignes dont la valeur de cette colonne est NA
poids_taille <- subset(poids_taille,
                       !is.na(poids_taille$weight))


# Calcul des coefficients de Pearson
Rxyp <- cor(poids_taille$weight, poids_taille$height, method = "pearson")


# 3) Hypothèses (test bilatéral): H0: rxy = 0   HA: rxy != 0


# 4) Statistique de test:
N = nrow(poids_taille)
Sp <- (abs(Rxyp - 0)) / sqrt((1-Rxyp)/(N-2)) # 19,12

qt(0.95, 315) # zone de rejet [1.64 ; + inf[
# Sp compris dans la zone de rejet donc on rejette H0, il y a une corrélation


test <- cor.test(poids_taille$weight, poids_taille$height, method = "pearson")
# pvalue = 2.2e-16< 0.05 donc on rejette H0, il y a une corrélation. Il y a donc une association




################################################################################################
# Est ce que la consommation de l'alcool est liée à l'IMC

ronfleurs <- read.table("ronfleurs.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, dec = ",")

# 1) Y’a-t-il une association entre l'IMC et l'alcool?
# Quelle représentation graphique? nuage de points
# IMC à mettre en numéric
ronfleurs$IMC <- as.numeric(ronfleurs$IMC)
plot(ronfleurs$IMC,ronfleurs$ALCOOL) # relation exponentielle


# Nombre de NA dans les colonnes IMC et ALCOOL
sum(is.na(ronfleurs$IMC)) # 0
sum(is.na(ronfleurs$ALCOOL)) # 0


# Calcul des coefficients de Spearman
# Rxyp <- cor(ronfleurs$IMC, ronfleurs$ALCOOL, method = "pearson") # 0.02
RxyS <- cor(ronfleurs$IMC, ronfleurs$ALCOOL, method = "spearman") # 0.03

# 4) Hypothèses (test bilatéral): H0: rxy = 0   HA: rxy != 0

# 5) Statistique de test:
N = nrow(ronfleurs)
Sp <- (abs(RxyS - 0)) / sqrt((1-RxyS)/(N-2)) # 0.34

# Degré de liberté: Nombre de lignes sans NA - 2

qt(0.95, 98) # zone de rejet [1.66 ; + inf[
# Sp pas dans la zone de rejet donc on accepte H0, il n'y a pas de corrélation

test <- cor.test(ronfleurs$IMC, ronfleurs$ALCOOL, method = "spearman")
# pvalue = 0.7372 > 0.05 donc on accepte H0, il n'y a pas de corrélation.






web <- read.table("data/web.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, dec = ",")

table(web)
