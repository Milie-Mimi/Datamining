setwd("C:/Users/milie/Desktop/MBA MBDCDO/COURS/DATAMINING/PROJETS_R/03_PROJET_TRANSVERSE/data")

entetes_ticket <- read.table("ENTETES_TICKET_V4.CSV", header = TRUE, sep = "|", stringsAsFactors = FALSE)
ligne_ticket <- read.table("LIGNES_TICKET_V4.CSV", header = TRUE, sep = "|", stringsAsFactors = FALSE)
ref_articles <- read.table("REF_ARTICLE.CSV", header = TRUE, sep = "|", stringsAsFactors = FALSE)
ref_magasin <- read.table("REF_MAGASIN.CSV", header = TRUE, sep = "|", stringsAsFactors = FALSE)
client <- read.table("CLIENT.CSV", header = TRUE, sep = "|", stringsAsFactors = FALSE)

# PARTIE 1 : Le point sur votre société à vendre.
# 1/ Nettoyez vos données et identifiez les données incohérentes.
library(dplyr)

# COLONNE CIVILITE

# Nous regardons les valeurs à harmoniser.
table(client$CIVILITE)

client = mutate(client,CIVILITE=case_when(CIVILITE=="madame"~"MADAME",
                                          CIVILITE=="MADAME"~"MADAME",
                                          CIVILITE=="Mme"~"MADAME",
                                          CIVILITE=="monsieur"~"MONSIEUR",
                                          CIVILITE=="MONSIEUR"~"MONSIEUR",
                                          CIVILITE=="Mr"~"MONSIEUR"))
# Verification : 
table(client$CIVILITE)

# HARMONISER LES DATES + CREATION COLONNE AGE, 

str(client)

client$DATENAISSANCE = as.Date(client$DATENAISSANCE, format = "%d/%m/%Y")

client$DATEDEBUTADHESION = as.Date(client$DATEDEBUTADHESION, format = "%d/%m/%Y")

client$DATEREADHESION = as.Date(client$DATEREADHESION, format = "%d/%m/%Y")

client$DATEFINADHESION = as.Date(client$DATEFINADHESION, format = "%d/%m/%Y")

str(client)


client$AGE = difftime(Sys.Date(), client$DATENAISSANCE, units = "days")/365.25
client$AGE = round(as.numeric(client$AGE))

# ON décide de garder les clients entre 18 (majeur) et 100 ans.

install.packages("tidyr")
library(tidyr)

client_ok = client%>%
  filter((AGE>=18 & AGE<=100) %>% replace_na(TRUE))

#Verification clients qui ont mois de 18 ans et plus de 100 : 
client_aberant = client%>%
  filter((AGE<18 | AGE>100))

rm(client_aberant)

# CREATION COLONNE DUREE ADHESION

client_ok$DUREADHESION = difftime(client_ok$DATEFINADHESION, client_ok$DATEDEBUTADHESION, units = "days")/365.25
client_ok$DUREADHESION = round(as.numeric(client_ok$DUREADHESION))

# CREATION COLONNE REPARITION CLIENT

client_ok = mutate(client_ok,REPARTION =case_when(VIP =="1" ~ "VIP",
                                                  format(DATEDEBUTADHESION, "%Y") == 2016 ~ "NEW_N2",
                                                  format(DATEDEBUTADHESION, "%Y") == 2017 ~ "NEW_N1",
                                                  DATEFINADHESION > "2018/01/01"~"ADHERENT",
                                                  DATEFINADHESION < "2018/01/01"~"CHURNER"))

table(client_ok$REPARTION)

#VERIFICATION DES AUTRES TABLES
# ENTETES TICKET
str(entetes_ticket)

# On harmonise les dates tickets :

entetes_ticket$TIC_DATE = as.Date(entetes_ticket$TIC_DATE, format = "%Y-%m-%d")

# On transforme les totaux ttc en numeric

entetes_ticket$TIC_TOTALTTC  = gsub(",",".",entetes_ticket$TIC_TOTALTTC)
entetes_ticket$TIC_TOTALTTC = as.numeric(entetes_ticket$TIC_TOTALTTC)

str(entetes_ticket)

# LIGNES TICKET : on travaille d'abord avec un échantillon
str(ligne_ticket)
#ligne_ticket$QUANTITE2 = ligne_ticket$QUANTITE
ligne_ticket_echantillon = ligne_ticket[c(1:150),c(1:7)]

str(ligne_ticket_echantillon)

# On transforme les variables en numerique 
#Quantité
ligne_ticket_echantillon$QUANTITE  = gsub(",",".",ligne_ticket_echantillon$QUANTITE)
ligne_ticket_echantillon$QUANTITE = as.numeric(ligne_ticket_echantillon$QUANTITE)

#Montant remise
ligne_ticket_echantillon$MONTANTREMISE   = gsub(",",".",ligne_ticket_echantillon$MONTANTREMISE )
ligne_ticket_echantillon$MONTANTREMISE  = as.numeric(ligne_ticket_echantillon$MONTANTREMISE )

# Total
ligne_ticket_echantillon$TOTAL   = gsub(",",".",ligne_ticket_echantillon$TOTAL )
ligne_ticket_echantillon$TOTAL  = as.numeric(ligne_ticket_echantillon$TOTAL )

# Marge de sortie
ligne_ticket_echantillon$MARGESORTIE   = gsub(",",".",ligne_ticket_echantillon$MARGESORTIE )
ligne_ticket_echantillon$MARGESORTIE  = as.numeric(ligne_ticket_echantillon$MARGESORTIE )

str(ligne_ticket_echantillon)

### Une fois qu'on a vérifié que notre échantillon est correctement traité, on applique le code sur la table orgininale.

#Quantité
ligne_ticket$QUANTITE  = gsub(",",".",ligne_ticket$QUANTITE)
ligne_ticket$QUANTITE = as.numeric(ligne_ticket$QUANTITE)

#Montant remise
ligne_ticket$MONTANTREMISE   = gsub(",",".",ligne_ticket$MONTANTREMISE )
ligne_ticket$MONTANTREMISE  = as.numeric(ligne_ticket$MONTANTREMISE )

# Total
ligne_ticket$TOTAL   = gsub(",",".",ligne_ticket$TOTAL )
ligne_ticket$TOTAL  = as.numeric(ligne_ticket$TOTAL )

# Marge de sortie
ligne_ticket$MARGESORTIE   = gsub(",",".",ligne_ticket$MARGESORTIE )
ligne_ticket$MARGESORTIE  = as.numeric(ligne_ticket$MARGESORTIE )

str(ligne_ticket)

rm(ligne_ticket_echantillon)

#REF ARTICLES
str(ref_articles)

#REF MAGASIN
str(ref_magasin)

table(ref_magasin$LIBELLEREGIONCOMMERCIALE)

ref_magasin = mutate(ref_magasin,LIBELLEREGIONCOMMERCIALE =case_when(LIBELLEREGIONCOMMERCIALE =="RhÃ´ne-Alpes" ~ "Rhone-Alpes",
                                                                     LIBELLEREGIONCOMMERCIALE == "Alsace-Est" ~ "Alsace-Est",
                                                                     LIBELLEREGIONCOMMERCIALE == "Centre-Paris" ~ "Centre-Paris",
                                                                     LIBELLEREGIONCOMMERCIALE == "Littoral" ~ "Littoral",
                                                                     LIBELLEREGIONCOMMERCIALE == "Vente en ligne" ~ "Vente en ligne"))




# 2/ Présentez le bilan de votre société sur ces deux dernières années à votre futur acheteur en utilisant 
# indicateurs /graphiques appropriés. Mettez en valeur l’essentiel.
library(dplyr)
# On vérifie que les idclients sont uniques : 
length(unique(client_ok$IDCLIENT))

###############################
### Présentation générale : ###
###############################

# Nombre de magasins par REGION pour montrer que tout le territoire est couvert : 
magasins_region = ref_magasin %>%
  group_by(LIBELLEREGIONCOMMERCIALE)%>%
  summarise(nb_magasins=length(ï..CODESOCIETE))

magasins_region

# Representation du nombre de clients par magasin (TOP 20 en ordre décroissant)
library("RColorBrewer")
coul = brewer.pal(12, "Paired") 

client_magasin =  client_ok%>%
  group_by(MAGASIN) %>%
  summarise(nb_client=length(IDCLIENT))%>%
  arrange(desc(nb_client))

client_magasin = client_magasin[1:20,]

barplot(client_magasin$nb_client, names.arg = client_magasin$MAGASIN, main = "Representation du nombre de clients \n pour le TOP 20 des magasins.", 
        xlab = "Magasin", ylab = "Nb de clients", col = coul )

#   Ancienneté des clients: clients fidèles


################################
### Présentation financière : ##
################################

# 1 :   CA, Marge, taux marge par année et par mois.


# Création colonne CA HT dans entetes_ticket
entetes_ticket <- mutate(entetes_ticket, CA_HT = TIC_TOTALTTC / 1.2)

# Inner Join entetes et lignes pour avoir une base comparable
ligne_ticket2 <- ligne_ticket %>%
  group_by(IDTICKET) %>%
  summarise(sum(MARGESORTIE))

merge_entete_ligne = merge(entetes_ticket, ligne_ticket2, by.x="IDTICKET", by.y="IDTICKET", all=F)

# Transformation colonne TIC_DATE au format Date
merge_entete_ligne$TIC_DATE = as.Date(merge_entete_ligne$TIC_DATE, format = "%Y/%m/%d")

# Création colonnes Mois/Année
merge_entete_ligne <- mutate(merge_entete_ligne, MMAAAA = format(merge_entete_ligne$TIC_DATE, format = "%m/%Y"))

# Création de la table CA et Marge
table_CA_MB <- merge_entete_ligne %>%
  group_by(MMAAAA) %>%
  summarise(CA_HT = sum(CA_HT), MARGE = sum(`sum(MARGESORTIE)`))

# Calcul du taux de Marge 
table_CA_MB <- mutate(table_CA_MB, TX_MB = MARGE/CA_HT)

# Export au format csv pour dataviz sous excel
write.table(table_CA_MB, "table_CA_MB.csv", row.names=FALSE, sep=";",dec=",", na=" ")


# Création colonne CA HT dans entetes_ticket
entetes_ticket <- mutate(entetes_ticket, CA_HT = TIC_TOTALTTC / 1.2)

# Inner Join entetes et lignes pour avoir une base comparable
ligne_ticket2 <- ligne_ticket %>%
  group_by(IDTICKET) %>%
  summarise(sum(MARGESORTIE))

merge_entete_ligne = merge(entetes_ticket, ligne_ticket2, by.x="IDTICKET", by.y="IDTICKET", all=F)

# Transformation colonne TIC_DATE au format Date
merge_entete_ligne$TIC_DATE = as.Date(merge_entete_ligne$TIC_DATE, format = "%Y/%m/%d")

# Création colonnes Mois/Année
merge_entete_ligne <- mutate(merge_entete_ligne, MMAAAA = format(merge_entete_ligne$TIC_DATE, format = "%m/%Y"))

# Création de la table CA et Marge
table_CA_MB <- merge_entete_ligne %>%
  group_by(MMAAAA) %>%
  summarise(CA_HT = sum(CA_HT), MARGE = sum(`sum(MARGESORTIE)`))

# Calcul du taux de Marge 
table_CA_MB <- mutate(table_CA_MB, TX_MB = MARGE/CA_HT)

# Export au format csv pour dataviz sous excel
write.table(table_CA_MB, "table_CA_MB.csv", row.names=FALSE, sep=";",dec=",", na=" ")

# 2 : Representation de la repartition des clients selon : 
# VIP : client étant VIP (VIP = 1)
# NEW_N2 : client ayant adhéré au cours de l'année N-2 (date début adhésion)
# NEW_N1 : client ayant adhéré au cours de l'année N-1 (date début adhésion)
# ADHÉRENT : client toujours en cours d'adhésion (date de fin d'adhésion > 2018/01/01)
# CHURNER : client ayant churné (date de fin d'adhésion < 2018/01/01)
repartition = round(prop.table(table(client_ok$REPARTION))*100,2)

lbls = paste(names(repartition), "\n", repartition, "%", sep="")

pie(repartition, labels = lbls, main="Repartition client")

#   3 : Comportement du CA Global par client N-2 vs N-1 (SQL)
ca_client_annee=read.csv(file="ca_client_annee.csv", dec = ".", sep = ",", h=T, stringsAsFactors = F)

# Création colonne CA HT dans ca_client_annee
ca_client_annee = mutate(ca_client_annee, total_HT = total_ttc / 1.2)
str(ca_client_annee)
ca_client_annee$date = as.character(ca_client_annee$date)

qplot(date, total_HT, data = ca_client_annee , xlab = "Année", ylab= "CA total TTC par client", ylim = c(150,800), 
      main = "Comportement du CA global HT par client N-2 vs N-1",  group = date, geom = "boxplot", nclass = 2,
      fill = date)

#   4: Etude par magasin (SQL) avec la marge par magasin et le taux de marge. 

#   5: CA par univers et année

# 6: Top marge par univers





##############################################################################################
####################### ECHANTILLONNAGE STRATIFIE SELON LE SEXE ET LA REGION #################
##############################################################################################

# Inner join entre client_ok et ref_magasin pour ne prendre que les clients associés à un magasin
merge_client_mag = merge(client_ok, ref_magasin, by.x="MAGASIN", by.y="ï..CODESOCIETE", all=F)


# Table prop stratifiée REGION/SEXE (on concatene les 2 variables pour pouvoir les traiter ensuite dans l'échantillonnage)
merge_client_mag = mutate(merge_client_mag, region_sexe = paste(merge_client_mag$LIBELLEREGIONCOMMERCIALE, merge_client_mag$CIVILITE))

prop_region_sexe <- round(prop.table(table(merge_client_mag$region_sexe))*300)


ech_ALS_MMME <- sample(x = which(merge_client_mag$region_sexe == "Alsace-Est MADAME"), size = prop_region_sexe["Alsace-Est MADAME"], replace = T)
ech_ALS_M <- sample(x = which(merge_client_mag$region_sexe == "Alsace-Est MONSIEUR"), size = prop_region_sexe["Alsace-Est MONSIEUR"], replace = T)
ech_CEN_MME <- sample(x = which(merge_client_mag$region_sexe == "Centre-Paris MADAME"), size = prop_region_sexe["Centre-Paris MADAME"], replace = T)
ech_CEN_M <- sample(x = which(merge_client_mag$region_sexe == "Centre-Paris MONSIEUR"), size = prop_region_sexe["Centre-Paris MONSIEUR"], replace = T)
ech_LIT_MME <- sample(x = which(merge_client_mag$region_sexe == "Littoral MADAME"), size = prop_region_sexe["Littoral MADAME"], replace = T)
ech_LIT_M <- sample(x = which(merge_client_mag$region_sexe == "Littoral MONSIEUR"), size = prop_region_sexe["Littoral MONSIEUR"], replace = T)
ech_RHA_MME <- sample(x = which(merge_client_mag$region_sexe == "Rhone-Alpes MADAME"), size = prop_region_sexe["Rhone-Alpes MADAME"], replace = T)
ech_RHA_M <- sample(x = which(merge_client_mag$region_sexe == "Rhone-Alpes MONSIEUR"), size = prop_region_sexe["Rhone-Alpes MONSIEUR"], replace = T)
ech_VEL_MME <- sample(x = which(merge_client_mag$region_sexe == "Vente en ligne MADAME"), size = prop_region_sexe["Vente en ligne MADAME"], replace = T)
ech_VEL_M <- sample(x = which(merge_client_mag$region_sexe == "Vente en ligne MONSIEUR"), size = prop_region_sexe["Vente en ligne MONSIEUR"], replace = T)


echST <- merge_client_mag[c(ech_ALS_MMME, ech_ALS_M, ech_CEN_MME, ech_CEN_M, ech_LIT_MME, ech_LIT_M, ech_RHA_MME, ech_RHA_M, ech_VEL_MME, ech_VEL_M), ]

# 298 valeurs au lieu de 300??

# Vérification des % dans l'échantillon vs merge_client_mag => OK
round(prop.table(table(echST$region_sexe))*100)
round(prop.table(table(merge_client_mag$region_sexe))*100)

##############################################################################################
###############################☺######## ANALYSE BI_VARIEE  ##################################
##############################################################################################

# Création variable Groupe Age
echST = mutate(echST,AGE_GROUP = case_when(AGE >= 71 ~"RETRAITE",
                                          AGE <= 70 & AGE >= 61 ~"JEUNE RETRAITE",
                                          AGE <= 60 & AGE >= 51 ~"SENIOR",
                                          AGE <= 50 & AGE >= 31 ~"ADULTE",
                                          AGE <= 30 & AGE >= 18 ~"JEUNE ADULTE",
                                          AGE < 18 ~"MINEUR"))

########
## Groupe d’âge par région (2 variables qualitatives) :
########


# Renommage variable LIBELLEREGIONCOMMERCIALE
echST <- echST %>% rename(REGION = LIBELLEREGIONCOMMERCIALE)


# Représentation graphique
barplot(age_region, ylab = "Effectifs", xlab = "Tranches d'âge", beside = T, legend = T)

library(ggplot2)
qplot(data = subset(echST, !is.na(REGION) & !is.na(AGE_GROUP)), 
      x = AGE_GROUP, fill = REGION, geom = "bar", main = "Groupe d'âge par région")


library(ggplot2)
qplot(data = subset(echST, !is.na(REGION) & !is.na(AGE_GROUP)), 
      x = AGE_GROUP, fill = REGION, geom = "bar", main = "Groupe d'âge par région")

# Groupe d'âge par région
age_region <-table(echST$REGION, echST$AGE_GROUP)

# Profils lignes
prop.table(age_region,1)*100

# Profils colonnes
prop.table(age_region,2)*100


########
## Âge vs. CA par client (2 Variables Quantitatives) :
########

# Création variable CA par client
ca_client = ca_client_annee %>%
  group_by(idclient) %>%
  summarise(CA = sum(total_HT))
ca_client  

echST = merge(echST, ca_client, by.x="IDCLIENT", by.y="idclient", all.x = T, all.y = F)

# CA Moyen par age :
age_ca = echST %>%
  group_by(AGE) %>%
  summarise(CA_moyen=mean(na.omit(CA)))%>%
  arrange(desc(CA_moyen))

age_ca = na.omit(age_ca)
age_ca

# Représentation graphique
library("RColorBrewer")
coul = brewer.pal(11, "Spectral") 

plot(age_ca$AGE, age_ca$CA_moyen, xlab = "Âge", ylab = "CA HT moyen", main = "Âge moyen vs. CA moyen", 
     col = coul, pch = 16)

# Indicateurs :
#Coef de pearson (lineare)
cor(age_ca$AGE, age_ca$CA_moyen, method = "pearson")

# 0.2945 : petite correlation entre l'âge et le CA moyen

########
##  Âge vs. marge (2 Variables Quantitatives) :
########

# Inner Join entetes et lignes pour avoir une base comparable
ligne_ticket2 <- ligne_ticket %>%
  group_by(IDTICKET) %>%
  summarise(sum(MARGESORTIE))

merge_entete_ligne = merge(entetes_ticket, ligne_ticket2, by="IDTICKET", all=F)


#Jointure
echST <- merge(echST, merge_entete_ligne, by='IDCLIENT')

#Creation Marge
age_marge = echST %>%
  group_by(AGE) %>%
  summarise(MARGE =sum(`sum(MARGESORTIE)`))

#Representation graphique
library("RColorBrewer")
coul = brewer.pal(11, "Spectral") 

plot(age_marge$AGE, age_marge$MARGE, xlab = "Âge", ylab = "Marge", main = "Âge moyen vs. MArge", ylim=c(0,3000),
     col = coul, pch = 16)

#Indicateurs
cor(age_marge$AGE, age_marge$MARGE, method = "spearman")

# NA : pas de corrélation entre l'âge et la marge

########
# Marge total/CA par tranche d´âge (1 Variable Quantitative + 1 Variable Qualitative)
########


##############################################################################################
######################################## TEST_STATISTIQUES ###################################
##############################################################################################

# Calcul du CA HT par client sur les 2 années
CAHT_IDCLIENT <- entetes_ticket %>% group_by(IDCLIENT) %>% summarise(CA_HT_CLIENT = sum(CA_HT))

# Left join de l'échantillon avec CAHT_IDCLIENT afin de voir pour tous les clients s'ils ont acheté ou non entre 2016 et 2017
merge_echST_CAHT_IDCLIENT <- merge(echST, CAHT_IDCLIENT, by.x = "IDCLIENT", by.y = "IDCLIENT", all.x = T, all.y = F) 


# Je remplace les NA par 0 car cela signifie que les clients n'ont rien acheté
merge_echST_CAHT_IDCLIENT[which(is.na(merge_echST_CAHT_IDCLIENT$CA_HT_CLIENT)), "CA_HT_CLIENT"] <-0

# Création colonne "PLUS DE 40 ANS" et "MOINS DE 40 ANS"
merge_echST_CAHT_IDCLIENT <- mutate(merge_echST_CAHT_IDCLIENT, PLUSouMOINS40 = case_when(AGE >= 40 ~"Plus de 40 ans",
                                                                                         AGE < 40 ~"Moins de 40 ans"))

# 1: Les plus de 40 ans ont-il dépensé le même montant que les moins de 40 ans? 


# Quel test statistique? Test de comparaison de 2 positions (moyennes)

# Quelles sont les hypothèses? Hypothèse bilatérale
## H0: µ1 = µ2 <=> µ1 - µ2 = 0
## HA: µ1 ≠ µ2  <=> µ1 - µ2 ≠ 0
## avec µ1 le CA moyen des plus de 40 ans sur les deux années
## avec µ2 le CA moyen des moins de 40 ans sur les deux années

# Quelles sont les conditions du test?
## 1 var quanti * 1 var quali à 2 modalités avec n >= 30. Il faut se poser la question de l'égalité ou non des variances.

## On pose H0 : égalité des variances entre les deux groupes:
var.test(CA_HT_CLIENT ~ PLUSouMOINS40, data = merge_echST_CAHT_IDCLIENT, var.equal = T)  # => Fisher

#pvalue = 0.07591 
#p-value > 0.05 donc H0 validée. On peut appliquer le Test de Student:

## On peut donc tester l'égalité des moyennes grace au test de Student (car l'hypothèse de l'égalité des variances est vérifiée )
t.test(CA_HT_CLIENT ~ PLUSouMOINS40, data = merge_echST_CAHT_IDCLIENT, var.equal = T)
#pvalue = 0.302 
# pvalue > 0.05 donc on ne rejette pas H0 de l'égalité des moyennes. Les plus de 40 ans dépensent pareil que les moins de 40 ans.





# 2 : Comparaison de deux proportions:
# Le % de femmes VIP est > au % d’hommes VIP (unilateral droite)


# Calcul de la proportion de VIP par sexe (0 = pas VIP, 1 = VIP) :
prop.table(table(echST$CIVILITE, echST$VIP))

table(echST$CIVILITE, echST$VIP)


# Quel test statistique? Test de comparaison de 2 proportions 

# Quelles sont les hypothèses? Hypothèse unilatérale à droite
# H0: p1-p2 <= 0
# HA: p1 - p2 > 0

# p1 le % des femmes VIP 
# p2 le % des hommes VIP

# H0 : Le pourcentage de femmes VIP est supérieur au % d’hommes VIP 

prop.test(32, 161, p = 0.06, alternative = "greater", conf.level = 0.95, correct = TRUE)

# pvalue (2.212) > α => on ne rejette pas H0 : 
# Le pourcentage de femmes VIP est supérieur à celui des hommes.



# Le % de femmes de plus de 40 ans est < % hommes de plus de 40 ans (unilatéral gauche) - CLARA
## Calcul de la proportion de +40 ans par sexe:

# Création colonne "PLUS DE 40 ANS" et "MOINS DE 40 ANS"
echST <- mutate(echST, PLUSouMOINS40 = case_when(AGE >= 40 ~"Plus de 40 ans",
                                                 AGE < 40 ~"Moins de 40 ans"))
# Calcul de la proportion de +40 ans par sexe:
prop.table(table(echST$CIVILITE, echST$PLUSouMOINS40))
table(echST$CIVILITE, echST$PLUSouMOINS40)

# Quel test statistique? Test de comparaison de 2 proportions 

# Quelles sont les hypothèses? Hypothèse unilatérale à droite
# H0: p1-p2 >= 0
# HA: p1 - p2 < 0

# p1 le % des femmes +40ans 
# p2 le % des hommes +40ans

# H0 : Le pourcentage de femmes +40ans est supérieur au % d’hommes +40ans



# 3 : Comparaison de plus de deux échantillons
# Moyenne de CA par tranche d'âge DAVID 




  
save.image("version1")
load("version1")


           
           
           
           
           
           