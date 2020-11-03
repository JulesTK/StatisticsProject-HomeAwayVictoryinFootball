#Test Student sur les 4 grands championnats européens
#English Premier League

install.packages("readr")
library(readr)

season1819_PL_csv <- read_csv("https://pkgstore.datahub.io/sports-data/english-premier-league/season-1819_csv/data/916634f7ec37dd45c86159bc723eb340/season-1819_csv.csv")
View(season1819_PL_csv)
dataPL_1819=season1819_PL_csv[,-c(1:2)]
View(dataPL_1819)
dataPL_1819=dataPL_1819[, -c(6:60)]
View(dataPL_1819)

season1718_PL_csv <- read_csv("https://pkgstore.datahub.io/sports-data/english-premier-league/season-1718_csv/data/3eb1292e4a07027e7047e46b1dba5a2d/season-1718_csv.csv")
dataPL_1718=season1718_PL_csv[,-c(1:2)]
dataPL_1718=dataPL_1718[, -c(6:63)]
View(dataPL_1718)

season1617_PL_csv <- read_csv("https://pkgstore.datahub.io/sports-data/english-premier-league/season-1617_csv/data/d6b7551d3e130b6e59240d7018524498/season-1617_csv.csv")
dataPL_1617=season1617_PL_csv[,-c(1:2)]
dataPL_1617=dataPL_1617[, -c(6:63)]
View(dataPL_1617)

season1516_PL_csv <- read_csv("https://pkgstore.datahub.io/sports-data/english-premier-league/season-1516_csv/data/09fbe4a898ce2d7d8909ed55c913f2e7/season-1516_csv.csv")
dataPL_1516=season1516_PL_csv[,-c(1:2)]
dataPL_1516=dataPL_1516[, -c(6:63)]
View(dataPL_1516)

season1415_PL_csv <- read_csv("https://pkgstore.datahub.io/sports-data/english-premier-league/season-1415_csv/data/4e5693f4c43c835a61a9cce3d1a2c4f1/season-1415_csv.csv")
dataPL_1415=season1415_PL_csv[,-c(1:2)]
dataPL_1415=dataPL_1415[, -c(6:66)]
View(dataPL_1415)

season1314_PL_csv <- read_csv("https://pkgstore.datahub.io/sports-data/english-premier-league/season-1314_csv/data/2e1cb721e6d34499bda05975afcf11a9/season-1314_csv.csv")
dataPL_1314=season1314_PL_csv[,-c(1:2)]
dataPL_1314=dataPL_1314[, -c(6:66)]
View(dataPL_1314)

dataPL=rbind(dataPL_1819,dataPL_1718,dataPL_1617,dataPL_1516,dataPL_1415,dataPL_1314)
dataPL$FTR2=ifelse(dataPL$FTR=='H',1,0)
dataPL$FTR3=ifelse(dataPL$FTR=='A',1,0)
View(dataPL)

domPL=as.matrix(dataPL$FTR2)
extPL=as.matrix(dataPL$FTR3)

SdomPL=colSums(domPL)
SextPL=colSums(extPL)
SdomPL
SextPL

mudomPL=mean(dataPL$FTR2[1:2280])
muextPL=mean(dataPL$FTR3[1:2280])
mudomPL
muextPL

#Test de Student sur les moyennes PL
testPL=t.test(dataPL$FTR2[1:2280],dataPL$FTR3[1:2280],alternative = 'greater')
P=testPL$p.value
testPL
P# Affichage de la p-value
P<0.05 #return TRUE donc on rejette HO 
testPL$statistic #stat de test


#Bundesliga Allemande
season1819_BUN_csv <- read_csv("https://pkgstore.datahub.io/sports-data/german-bundesliga/season-1819_csv/data/b5d1d632500bf4f26791f91a2990ee2a/season-1819_csv.csv")
View(season1819_BUN_csv)
dataBUN_1819=season1819_BUN_csv[,-c(1:2)]
View(dataBUN_1819)
dataBUN_1819=dataBUN_1819[,-c(6:59)]
View(dataBUN_1819)

season1718_BUN_csv <- read_csv("https://pkgstore.datahub.io/sports-data/german-bundesliga/season-1718_csv/data/26fe5a3646f607a523e0257c7ee70cce/season-1718_csv.csv")
dataBUN_1718=season1718_BUN_csv[,-c(1:2)]
dataBUN_1718=dataBUN_1718[, -c(6:62)]
View(dataBUN_1718)

season1617_BUN_csv <- read_csv("https://pkgstore.datahub.io/sports-data/german-bundesliga/season-1617_csv/data/8f0d10c346a02caa5ae3cd70ec0c104a/season-1617_csv.csv")
dataBUN_1617=season1617_BUN_csv[,-c(1:2)]
dataBUN_1617=dataBUN_1617[, -c(6:62)]
View(dataBUN_1617)

season1516_BUN_csv <- read_csv("https://pkgstore.datahub.io/sports-data/german-bundesliga/season-1516_csv/data/e80cd71251723a74416cfe0acea8af39/season-1516_csv.csv")
dataBUN_1516=season1516_BUN_csv[,-c(1:2)]
dataBUN_1516=dataBUN_1516[, -c(6:62)]
View(dataBUN_1516)

season1415_BUN_csv <- read_csv("https://pkgstore.datahub.io/sports-data/german-bundesliga/season-1415_csv/data/4ca47cf8526001ab7a9098824c5cd88d/season-1415_csv.csv")
dataBUN_1415=season1415_BUN_csv[,-c(1:2)]
dataBUN_1415=dataBUN_1415[, -c(6:65)]
View(dataBUN_1415)

season1314_BUN_csv <- read_csv("https://pkgstore.datahub.io/sports-data/german-bundesliga/season-1314_csv/data/00c4a3143f03d9ca96ca39d85a46713a/season-1314_csv.csv")
dataBUN_1314=season1314_BUN_csv[,-c(1:2)]
dataBUN_1314=dataBUN_1314[, -c(6:65)]
View(dataBUN_1314)

dataBUN=rbind(dataBUN_1819,dataBUN_1718,dataBUN_1617,dataBUN_1516,dataBUN_1415,dataBUN_1314)
dataBUN$FTR2=ifelse(dataBUN$FTR=='H',1,0)
dataBUN$FTR3=ifelse(dataBUN$FTR=='A',1,0)
View(dataBUN)

domBUN=as.matrix(dataBUN$FTR2)
extBUN=as.matrix(dataBUN$FTR3)

SdomBUN=colSums(domBUN)
SextBUN=colSums(extBUN)

mudomBUN=mean(dataBUN$FTR2[1:1836])
muextBUN=mean(dataBUN$FTR3[1:1836])
mudomBUN
muextBUN

#Test de Student sur les moyennes BUN
testBUN=t.test(dataBUN$FTR2[1:1836],dataBUN$FTR3[1:1836],alternative = 'greater')
P=testBUN$p.value
testBUN
P# Affichage de la p-value
P<0.05 #return TRUE donc on rejette HO et on peut dire que les moyennes sont differntes
testBUN$statistic #stat de test

#Serie A Italienne

season1819_IT_csv <- read_csv("https://pkgstore.datahub.io/sports-data/italian-serie-a/season-1819_csv/data/5a5b42768c67998329baa8d5685b100b/season-1819_csv.csv")
View(season1819_IT_csv)
dataIT_1819=season1819_IT_csv[,-c(1:2)]
View(dataIT_1819)
dataIT_1819=dataIT_1819[, -c(6:59)]
View(dataIT_1819)

season1718_IT_csv <- read_csv("https://pkgstore.datahub.io/sports-data/italian-serie-a/season-1718_csv/data/efe601bb3cbc697cd2df299de155029d/season-1718_csv.csv")
dataIT_1718=season1718_IT_csv[,-c(1:2)]
dataIT_1718=dataIT_1718[, -c(6:62)]
View(dataIT_1718)

season1617_IT_csv <- read_csv("https://pkgstore.datahub.io/sports-data/italian-serie-a/season-1617_csv/data/2f7cd2011c6604d0d40c35886099cf82/season-1617_csv.csv")
dataIT_1617=season1617_IT_csv[,-c(1:2)]
dataIT_1617=dataIT_1617[, -c(6:62)]
View(dataIT_1617)

season1516_IT_csv <- read_csv("https://pkgstore.datahub.io/sports-data/italian-serie-a/season-1516_csv/data/08a07a4a4410298ccd7aa5dd8910ce78/season-1516_csv.csv")
dataIT_1516=season1516_IT_csv[,-c(1:2)]
dataIT_1516=dataIT_1516[, -c(6:62)]
View(dataIT_1516)

season1415_IT_csv <- read_csv("https://pkgstore.datahub.io/sports-data/italian-serie-a/season-1415_csv/data/ffebcde71c37256b20fb036bdad4f198/season-1415_csv.csv")
dataIT_1415=season1415_IT_csv[,-c(1:2)]
dataIT_1415=dataIT_1415[, -c(6:65)]
View(dataIT_1415)

season1314_IT_csv <- read_csv("https://pkgstore.datahub.io/sports-data/italian-serie-a/season-1314_csv/data/eda0c875c805da8364c04549204630a0/season-1314_csv.csv")
dataIT_1314=season1314_IT_csv[,-c(1:2)]
dataIT_1314=dataIT_1314[, -c(6:65)]
View(dataIT_1314)

dataIT=rbind(dataIT_1819,dataIT_1718,dataIT_1617,dataIT_1516,dataIT_1415,dataIT_1314)
dataIT$FTR2=ifelse(dataIT$FTR=='H',1,0)
dataIT$FTR3=ifelse(dataIT$FTR=='A',1,0)
View(dataIT)

domIT=as.matrix(dataIT$FTR2)
extIT=as.matrix(dataIT$FTR3)

SdomIT=colSums(domIT)
SextIT=colSums(extIT)

mudomIT=mean(dataIT$FTR2[1:2280])
muextIT=mean(dataIT$FTR3[1:2280])
mudomIT
muextIT

#Test de Student sur les moyennes IT
testIT=t.test(dataIT$FTR2[1:2280],dataIT$FTR3[1:2280],alternative = 'greater')
P=testIT$p.value
testIT
P# Affichage de la p-value
P<0.05 #return TRUE donc on rejette HO et on peut dire que les moyennes sont differntes
testIT$statistic #stat de test

#Liga Espagnol

season1819_ES_csv <- read_csv("https://pkgstore.datahub.io/sports-data/spanish-la-liga/season-1819_csv/data/b2766fcdc32a933291a031fdba39243c/season-1819_csv.csv")
View(season1819_ES_csv)
dataES_1819=season1819_ES_csv[,-c(1:2)]
View(dataES_1819)
dataES_1819=dataES_1819[, -c(6:59)]
View(dataES_1819)

season1718_ES_csv <- read_csv("https://pkgstore.datahub.io/sports-data/spanish-la-liga/season-1718_csv/data/7e59aa77a91881dcd508d7e11be3fcf4/season-1718_csv.csv")
dataES_1718=season1718_ES_csv[,-c(1:2)]
dataES_1718=dataES_1718[, -c(6:62)]
View(dataES_1718)

season1617_ES_csv <- read_csv("https://pkgstore.datahub.io/sports-data/spanish-la-liga/season-1617_csv/data/f92c62e242adde9623462e0c2021b31e/season-1617_csv.csv")
dataES_1617=season1617_ES_csv[,-c(1:2)]
dataES_1617=dataES_1617[, -c(6:62)]
View(dataES_1617)

season1516_ES_csv <- read_csv("https://pkgstore.datahub.io/sports-data/spanish-la-liga/season-1516_csv/data/7ca084dfa535d8677bf0338419ae96a3/season-1516_csv.csv")
dataES_1516=season1516_ES_csv[,-c(1:2)]
dataES_1516=dataES_1516[, -c(6:62)]
View(dataES_1516)

season1415_ES_csv <- read_csv("https://pkgstore.datahub.io/sports-data/spanish-la-liga/season-1415_csv/data/7fe781293daa7725cf1078fe93993229/season-1415_csv.csv")
dataES_1415=season1415_ES_csv[,-c(1:2)]
dataES_1415=dataES_1415[, -c(6:65)]
View(dataES_1415)

season1314_ES_csv <- read_csv("https://pkgstore.datahub.io/sports-data/spanish-la-liga/season-1314_csv/data/1a68408ec94a630e0173e28388591b12/season-1314_csv.csv")
dataES_1314=season1314_ES_csv[,-c(1:2)]
dataES_1314=dataES_1314[, -c(6:65)]
View(dataES_1314)

dataES=rbind(dataES_1819,dataES_1718,dataES_1617,dataES_1516,dataES_1415,dataES_1314)
dataES$FTR2=ifelse(dataES$FTR=='H',1,0)
dataES$FTR3=ifelse(dataES$FTR=='A',1,0)
View(dataES)

domES=as.matrix(dataES$FTR2)
extES=as.matrix(dataES$FTR3)

SdomES=colSums(domES)
SextES=colSums(extES)

mudomES=mean(dataES$FTR2[1:2280])
muextES=mean(dataES$FTR3[1:2280])
mudomES
muextES

#Test de Student sur les moyennes ES
testES=t.test(dataES$FTR2[1:2280],dataES$FTR3[1:2280],alternative = 'greater')
P=testES$p.value
testES
P# Affichage de la p-value
P<0.05 #return TRUE donc on rejette HO et on peut dire que les moyennes sont differntes
testES$statistic #stat de test

#Test du Chi2 sur les 5 grand championnat Européen

fr=c(1032,644)
SdomPL
SextPL
SnulPL=2280-(SdomPL+SextPL)
SnulPL
ang=c(SdomPL,SextPL)
SdomBUN
SextBUN
SnulBUN=1836-(SdomBUN+SextBUN)
SnulBUN
all=c(SdomBUN,SextBUN)
SdomIT
SextIT
SnulIT=2280-(SdomIT+SextIT)
SnulIT
it=c(SdomIT,SextIT)
SdomES
SextES
SnulES=2280-(SdomES+SextES)
SnulES
es=c(SdomES,SextES)

tableau=matrix(c(fr,ang,all,it,es),5,2,byrow=T)  
tableau
#H0 : il n'y a pas de différence significative entre match à domicile et match à l'extérieur entre les 5 grands championnats
#H1 : il y a une différence entre les 5 championnats

khitest=chisq.test(tableau)  
khitest

p=khitest$p.value
p 
p<0.05   
