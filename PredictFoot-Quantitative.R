setwd("C:/RStudio/Jeux de donnees/France")

match1 <- read.csv(file = "F1.csv", header = TRUE, sep = ",", dec = ",")
match2 <- read.csv(file = "F2.csv", header = TRUE, sep = ",", dec = ",")
match3 <- read.csv(file = "F3.csv", header = TRUE, sep = ",", dec = ",")
match4 <- read.csv(file = "F4.csv", header = TRUE, sep = ",", dec = ",")

library(dplyr)
match<-bind_rows(match1,match2,match3,match4)
#print(match)
#dim(match)

match[64:66]<-NULL
#dim(match)

coul=rainbow(12)

#Analyse équipe Monaco, nb victoire,egalite et defaite a domicile

AnalyseEquipe<- function(equipe) {
  #nombre total de match de monaco à domicile
  nbTotalMatchMonacoDomicile <- table(match$HomeTeam==equipe)[2]
  
  nbMatchMonacoH <- table(match$HomeTeam == equipe & match$FTR=="H")[2]
  #print (paste("nombre de victoire à domicile de", equipe, "= ", nbMatchMonacoH))
  
  nbMatchMonacoD <- table(match$HomeTeam == equipe & match$FTR=="D")[2]
  #print (paste("nombre d'égalité à domicile de", equipe, "= ",nbMatchMonacoD))
  
  nbMatchMonacoA <- table(match$HomeTeam == equipe & match$FTR=="A")[2]
  #print (paste("nombre de défaite à domicile de", equipe, "= ",nbMatchMonacoA))

  #nombre total de buts marqués par equipe à domicile
  m<-match
  t<-match[5]  #Ce que tu veux réccuperer 
  b<-which(match[3]==equipe) #Condition
  
  r<-t[c(b),] #lien entre les 2
  #print (r) #affiche tout les buts
  o<-sum(r) #fait la somme de tout les buts
  
  #nombre total de buts encaissés par equipe à domicile
  m<-match
  t<-match[6]  #Ce que tu veux réccuperer 
  b<-which(match[3]==equipe) #Condition
  
  r<-t[c(b),] #lien entre les 2
  #print (r) #affiche tous les buts
  o1<-sum(r) #fait la somme de tous les buts
  
  #nombre total de shoot cadres par monaco à domicile 
  #HS
  t<-match[13]  #Ce que tu veux réccuperer 
  b<-which(match[3]==equipe) #Condition
  
  r<-t[c(b),] #lien entre les 2
  #print (r) #affiche tous les shoot de monaco à domicile
  o2<-sum(r) #fait la somme de tous les shoot
  
  
  print(paste("Nombre de match jouer par",equipe,"à domicile","=",nbTotalMatchMonacoDomicile,":"))
  print(paste(nbMatchMonacoH,"Victoires",nbMatchMonacoA,"Défaites","et",nbMatchMonacoD,"Egalités"))
  print(paste("Nombre de buts: marquer=",o,", encaisser=",o1,"avec",o2,"tir cadrés"))
  
  #nombre total de match de monaco à l'exterieur
  nbTotalMatchMonacoExterieur <- table(match$AwayTeam==equipe)[2]
  #print(paste("nombre de match de",equipe,"à l'exterieur","=",nbTotalMatchMonacoExterieur))
  #Analyse équipe Monaco, nb victoire,egalite et defaite a l'exterieur
  
  nbMatchMonacoVA <- table(match$AwayTeam == equipe & match$FTR=="A")[2]
  #print (paste("nombre de victoire à l'extérieur de", equipe, "= ",nbMatchMonacoVA))
  
  nbMatchMonacoDA <- table(match$AwayTeam == equipe & match$FTR=="D")[2]
  #print (paste("nombre d'égalité à l'extérieur de", equipe, "= ",nbMatchMonacoDA))
  
  nbMatchMonacoHA <- table(match$AwayTeam == equipe & match$FTR=="H")[2]
  #print (paste("nombre de défaite à l'extérieur de", equipe, "= ",nbMatchMonacoHA))
  print("")
  
  #nombre total de buts marqués par monaco à l'exterieur
  t<-match[6]  #Ce que tu veux réccuperer 
  b<-which(match[4]==equipe) #Condition
  
  r<-t[c(b),] #lien entre les 2
  #print (r) #affiche tous les buts
  o<-sum(r) #fait la somme de tous les buts
  
  #nombre total de buts encaissés par monaco à l'exterieur
  t<-match[5]  #Ce que tu veux réccuperer 
  b<-which(match[4]==equipe) #Condition
  
  r<-t[c(b),] #lien entre les 2
  #print (r) #affiche tous les buts
  o1<-sum(r) #fait la somme de tous les buts
  
  #nombre total de shoot cadres par monaco à l'exterieur 
  #AS
  t<-match[14]  #Ce que tu veux réccuperer 
  b<-which(match[4]==equipe) #Condition
  
  r<-t[c(b),] #lien entre les 2
  #print (r) #affiche tous les shoot à l'extérieur à la mi-temps
  o2<-sum(r) #fait la somme de tous les shoot à l'ext?rieur
  
  print(paste("Nombre de match jouer par",equipe,"à l'extérieur","=",nbTotalMatchMonacoExterieur,":"))
  print(paste(nbMatchMonacoVA,"Victoires",nbMatchMonacoHA,"Défaites","et",nbMatchMonacoDA,"Egalités"))
  print(paste("Nombre de buts: marquer=",o,", encaisser=",o1,"avec",o2,"tir cadrés"))
  
  #Domicile
  nbVicDomMonaco <- (nbMatchMonacoH/nbTotalMatchMonacoDomicile)
  #print(nbVicDomMonaco)
  
  nbEgalDomMonaco <- (nbMatchMonacoD/nbTotalMatchMonacoDomicile)
  #print(nbEgalDomMonaco)
  
  nbDefDomMonaco <- (nbMatchMonacoA/nbTotalMatchMonacoDomicile)
  #print(nbDefDomMonaco)
  
  
  #nbDefDomMonaco+nbEgalDomMonaco+nbVicDomMonaco
  #
  n2 <- c(nbVicDomMonaco,nbDefDomMonaco,nbEgalDomMonaco)
  barplot(n2,main = "Domicile",xlab="", ylab="Pourcentage Victoire", ylim=c(0,0.7), names.arg=c("Victoir", "D?faite", "Egalite"),col=c("darkblue","red","white"))
  
  #Match Away
  nbVicAwMonaco <- (nbMatchMonacoVA/nbTotalMatchMonacoExterieur)
  #print(nbVicAwMonaco)
  
  nbEgalAwMonaco <- (nbMatchMonacoDA/nbTotalMatchMonacoExterieur)
  #print(nbEgalAwMonaco)
  
  nbDefAwMonaco <- (nbMatchMonacoHA/nbTotalMatchMonacoExterieur)
  #print(nbDefAwMonaco)
  
  
  #nbDefDomMonaco+nbEgalDomMonaco+nbVicDomMonaco
  #
  
  n3 <- c(nbVicDomMonaco,nbVicAwMonaco,nbDefDomMonaco,nbDefAwMonaco,nbEgalDomMonaco,nbEgalAwMonaco)
  barplot(n3, main = "Domicile/Exterieur",xlab="", ylab="Pourcentage V/D/E", ylim=c(0,max(nbVicDomMonaco,nbVicAwMonaco,nbDefDomMonaco,nbDefAwMonaco,nbEgalDomMonaco,nbEgalAwMonaco)+0.1) ,names.arg=c("V-H", "V-A", "D-H","D-A","E-H","E-A"),col=c("darkblue","darkblue","red","red","white","white"))
}
AnalyseEquipe("Lyon")


#Fonction nombre de tir marquer en fonction du nombre de tir cadres
#Domicile
fonctHomeTeamTirCad <- function(equipe) {
  mm<-match[c(1:14)]
  t<-mm[13]
  tt<-mm[5]
  
  b<-which(mm[3]==equipe)
  #print(b)
  #Nombre de tir cadres par match
  r<-t[c(b),]
  #print(r)
  #Nombre de buts marquer
  r1<-tt[c(b),]
  #print (r1)
  #Graphe nombre de buts en fonction du nombre de tir cadres
  b<-boxplot(r1~r,xlab="Nombre de tirs cadres", main=paste("Equipe",equipe,"Domicile"),ylab="Nombre de buts marquer",col=coul)
  #print(mean(r))
  #print (b)
  f<-b$stats[,round(mean(r),0)]
  #print (f)
  return (mean(f))
}
fonctHomeTeamTirCad("Paris SG")

#Exterieur
fonctAwayTeamTirCad <- function(equipe) {
  mm<-match[c(1:14)]
  t<-mm[14]
  tt<-mm[6]
  
  b<-which(mm[4]==equipe)
  #print(b)
  r<-t[c(b),]
  #print(r)
  r1<-tt[c(b),]
  
  b<-boxplot(r1~r,xlab="Nombre de tirs cadres", main=paste("Equipe",equipe,"Exterieur"),ylab="Nombre de buts marquer",col=coul)
  #print(mean(r))
  f<-b$stats[,round(mean(r,na.rm = TRUE),0)]
  
  #print (f)
  return (mean(f))
}
fonctAwayTeamTirCad("Paris SG")

#Fonction nombre de but encaisser par rapport au nb de faute commise
#Domicile
fonctHomeTeamFautes <- function(equipe) {
  mm<-match[c(1:16)]
  t<-mm[15]
  tt<-mm[6]
  
  b<-which(mm[3]==equipe)
  #print(b)
  #Nombre de fautes commise par match
  r<-t[c(b),]
  #print(r)
  #Nombre de buts encaisser
  r1<-tt[c(b),]
  #print (r1)
  #Graphe nombre de buts en fonction de fautes commise
  b<-boxplot(r1~r,xlab="Nombre de Fautes", main=paste("Equipe",equipe,"Domicile"),ylab="Nombre de buts encaisser",col=coul)
  #print(mean(r))
  #print (b)
  f<-b$stats[,round(mean(r),0)]
  #print (f)
  return (mean(f))
}
fonctHomeTeamFautes("Paris SG")

#Exterieur
fonctAwayTeamFautes <- function(equipe) {
  mm<-match[c(1:16)]
  t<-mm[16]
  tt<-mm[5]
  
  b<-which(mm[4]==equipe)
  #print(b)
  r<-t[c(b),]
  #print(r)
  r1<-tt[c(b),]
  
  b<-boxplot(r1~r,xlab="Nombre de Fautes", main=paste("Equipe",equipe,"Exterieur"),ylab="Nombre de buts encaisser",col=coul)
  #print(mean(r))
  f<-b$stats[,round(mean(r,na.rm = TRUE),0)]
  
  #print (f)
  return (mean(f))
}
fonctAwayTeamFautes("Paris SG")



#Fonction nombre de but encaisser par rapport au nb carton jaune
#Domicile
fonctHomeTeamCartJaune <- function(equipe) {
  mm<-match[c(1:20)]
  t<-mm[19]
  tt<-mm[6]
  
  b<-which(mm[3]==equipe)
  #print(b)
  #Nombre de fautes commise par match
  r<-t[c(b),]
  #print(r)
  #Nombre de buts encaisser
  r1<-tt[c(b),]
  #print (r1)
  #Graphe nombre de buts en fonction de fautes commise
  b<-boxplot(r1~r,xlab="Nombre de carton Jaune", main=paste("Equipe",equipe,"Domicile"),ylab="Nombre de buts encaisser",col=coul)
  #print(mean(r))
  #print (b)
  f<-b$stats[,round(mean(r),0)]
  #print (f)
  return (mean(f))
}
fonctHomeTeamCartJaune("Paris SG")

#Exterieur
fonctAwayTeamCartJaune <- function(equipe) {
  mm<-match[c(1:20)]
  t<-mm[20]
  tt<-mm[5]
  
  b<-which(mm[4]==equipe)
  #print(b)
  r<-t[c(b),]
  #print(r)
  r1<-tt[c(b),]
  
  b<-boxplot(r1~r,xlab="Nombre de carton Jaune", main=paste("Equipe",equipe,"Exterieur"),ylab="Nombre de buts encaisser",col=coul)
  #print(mean(r))
  f<-b$stats[,round(mean(r,na.rm = TRUE),0)]
  
  #print (f)
  return (mean(f))
}
fonctAwayTeamCartJaune("Paris SG")


#Fonction nombre de but encaisser par rapport au nb carton rouge
#Domicile
fonctHomeTeamCartRouge <- function(equipe) {
  mm<-match[c(1:22)]
  t<-mm[21]
  tt<-mm[6]
  
  b<-which(mm[3]==equipe)
  #print(b)
  #Nombre de fautes commise par match
  r<-t[c(b),]
  #print(r)
  #Nombre de buts encaisser
  r1<-tt[c(b),]
  #print (r1)
  #Graphe nombre de buts en fonction de fautes commise
  b<-boxplot(r1~r,xlab="Nombre de carton Rouge", main=paste("Equipe",equipe,"Domicile"),ylab="Nombre de buts encaisser",col=coul)
  #print(mean(r))
  #print (b)
  f<-b$stats[,round(mean(r),0)]
  #print (f)
  return (mean(f))
}
fonctHomeTeamCartRouge("Paris SG")

#Exterieur
fonctAwayTeamCartRouge <- function(equipe) {
  mm<-match[c(1:22)]
  t<-mm[22]
  tt<-mm[5]
  
  b<-which(mm[4]==equipe)
  #print(b)
  r<-t[c(b),]
  #print(r)
  r1<-tt[c(b),]
  
  b<-boxplot(r1~r,xlab="Nombre de carton Rouge", main=paste("Equipe",equipe,"Exterieur"),ylab="Nombre de buts encaisser",col=coul)
  #print(mean(r))
  f<-b$stats[,round(mean(r,na.rm = TRUE),0)]
  
  #print (f)
  return (mean(f))
}
fonctAwayTeamCartRouge("Paris SG")


#Fonction nombre de but marquer par rapport au nb de corner
#Domicile
fonctHomeTeamCorner <- function(equipe) {
  mm<-match[c(1:24)]
  t<-mm[17]
  tt<-mm[5]
  
  b<-which(mm[3]==equipe)
  #print(b)
  #Nombre de corner obtenu par match
  r<-t[c(b),]
  print(r)
  #Nombre de buts marquer
  r1<-tt[c(b),]
  print (r1)
  #Graphe nombre de buts en fonction de fautes commise
  b<-boxplot(r1~r,xlab="Nombre de corner", main=paste("Equipe",equipe,"Domicile"),ylab="Nombre de buts encaisser",col=coul)
  print(mean(r))
  #print (b)
  f<-b$stats[,round(mean(r),0)]
  #print (f)
  return (mean(f))
}
fonctHomeTeamCorner("Paris SG")

#Exterieur
fonctAwayTeamCorner <- function(equipe) {
  mm<-match[c(1:24)]
  t<-mm[18]
  tt<-mm[6]
  
  b<-which(mm[4]==equipe)
  #print(b)
  r<-t[c(b),]
  #print(r)
  r1<-tt[c(b),]
  
  b<-boxplot(r1~r,xlab="Nombre de corner", main=paste("Equipe",equipe,"Exterieur"),ylab="Nombre de buts encaisser",col=coul)
  #print(mean(r))
  f<-b$stats[,round(mean(r,na.rm = TRUE),0)]
  
  #print (f)
  return (mean(f))
}
fonctAwayTeamCorner("Paris SG")










#match$HomeTeam

#match[40:60] <- NULL

#table(match$AwayTeam)

#nbMatch<-table(match)

#table<-sqlQuery("select * from match")

nbTotalMatch <- dim(match)[1]
print (nbTotalMatch)

scoreMatchHome <- match$FTHG
print(scoreMatchHome)


scoreMatchAway <- match$FTAG
print (scoreMatchAway)


nb <- (match$FTHG >= match$FTAG)
print(nb)

nbVictDom <- table(nb)[2]
print(nbVictDom)

nbVictExt <- table(nb)[1]
print(nbVictExt)

nbEgalite <- (match$FTHG == match$FTAG)
nbEgalite <- table(nbEgalite)[2]
print (nbEgalite)

nbVictDom <- (nbVictDom - nbEgalite)
print(nbVictDom)


nb1<-(nbVictDom/nbTotalMatch)
nb2<-(nbVictExt/nbTotalMatch)
nb3<-(nbEgalite/nbTotalMatch)

nb1+nb2+nb3

#match$FTR
n <- c(nb1,nb2,nb3)
barplot(n,xlab="",main = "Saisons 2014 à 2018", ylab="Pourcentage Victoire", ylim=c(0,0.5), names.arg=c("Domicile", "Exterieur", "Egalite"),col=c("darkblue","red","white"))

#h=hist(match$FTHG,probability = TRUE)


#nombre total de fautes commises par monaco à domicile 
#HF
m<-match
t<-match[15]  #Ce que tu veux réccuperer 
b<-which(match[3]=="Monaco") #Condition

r<-t[c(b),] #lien entre les 2
print (r) #affiche toutes les fautes commises
sum(r) #fait la somme de toutes les fautes commises par monaco à domicile


#nombre total de fautes commises par monaco à l'extérieur 
#AF
m<-match
t<-match[16]  #Ce que tu veux réccuperer 
b<-which(match[4]=="Monaco") #Condition

r<-t[c(b),] #lien entre les 2
print (r) #affiche toutes les fautes commises
sum(r) #fait la somme de toutes les fautes commises par monaco à l'extérieur

print(sum(r))
nbTotalMatchMonacoExterieur <- table(match$AwayTeam=="Monaco")[2]
calcul2<-sum(r)  
n<-c(calcul,calcul2)
barplot(n,xlab="",main = "Saisons 2014 à 2018 ", ylab="Nombre de fautes ", ylim=c(0,1080), names.arg = c("Domicile", "Exterieur"),col=c("green","green"))


#nombre total de corners obtenus par monaco à domicile 
#HC
m<-match
t<-match[17]  #Ce que tu veux reccuperer 
b<-which(match[3]=="Monaco") #Condition

r<-t[c(b),] #lien entre les 2
print (r) #affiche tous les corners obtenus
sum(r) #fait la somme de tous les corners obtenus par monaco à domicile


#nombre total de corners obtenus par monaco à l'extérieur 
#AC
m<-match
t<-match[18]  #Ce que tu veux reccuperer 
b<-which(match[4]=="Monaco") #Condition

r<-t[c(b),] #lien entre les 2
print (r) #affiche tous les corners obtenus
sum(r) #fait la somme de tous les corners obtenus par monaco à l'extérieur

print(sum(r))
nbTotalMatchMonacoExterieur <- table(match$AwayTeam=="Monaco")[2]
calcul2<-sum(r)  
n<-c(calcul,calcul2)
barplot(n,xlab="",main = "Saisons 2014 à 2018 ", ylab="Nombre de corners", ylim=c(0,400), names.arg = c("Domicile", "Exterieur"),col=c("Black","Black"))



#nombre total de cartons jaunes reçus par monaco à domicile
#HY
m<-match
t<-match[19]  #Ce que tu veux reccuperer 
b<-which(match[3]=="Monaco") #Condition

r<-t[c(b),] #lien entre les 2
print (r) #affiche tous les cartons jaunes
sum(r) #fait la somme de tous les cartons jaunes reçus par monaco à domicile

nbTotalMatchMonacoExterieur <- table(match$HomeTeam==equipe)[2]
calcul<-sum(r)  
n<-c(calcul)


#nombre total de cartons jaunes reçus par monaco à l'extérieur
#AY
m<-match
t<-match[20]  #Ce que tu veux reccuperer 
b<-which(match[4]=="Monaco") #Condition

r<-t[c(b),] #lien entre les 2
print (r) #affiche tous les cartons jaunes
sum(r) #fait la somme de tous les cartons jaunes reçus par monaco à l'extérieur
print(sum(r))
nbTotalMatchMonacoExterieur <- table(match$AwayTeam=="Monaco")[2]
calcul2<-sum(r)  
n<-c(calcul,calcul2)
barplot(n,xlab="",main = "Saisons 2014 à 2018 ", ylab="Nombre de cartons jaunes", ylim=c(0,160), names.arg = c("Domicile", "Exterieur"),col=c("Yellow","Yellow"))


#nombre total de cartons rouges reçus par monaco à domicile
#HR

NbCartRDom <- function(equipe){
  m<-match
  t<-match[21]  #Ce que tu veux réccuperer 
  b<-which(match[3]==equipe) #Condition
  
  r<-t[c(b),] #lien entre les 2
  print (r) #affiche tous les cartons rouges
  sum(r) #fait la somme de tous les cartons rouges reçus par monaco à domicile
  
  nbTotalMatchMonacoExterieur <- table(match$AwayTeam==equipe)[2]
  calcul<-sum(r)  
  n<-c(calcul)
  
  
  #nombre total de cartons rouges reçus par monaco à l'extérieur
  #AR
  m<-match
  t<-match[22]  #Ce que tu veux reccuperer 
  b<-which(match[4]==equipe) #Condition
  
  r<-t[c(b),] #lien entre les 2
  print (r) #affiche tous les cartons rouges
  sum(r) #fait la somme de tous les cartons rouges reçus par monaco à l'extérieur
  print(sum(r))
  nbTotalNombreDeMatchsMonacoExterieur <- table(match$AwayTeam==equipe)[2]
  calcul2<-sum(r)  
  n<-c(calcul,calcul2)
  barplot(n,xlab="",main = "Saisons 2014 à 2018 ", ylab="Nombre de cartons rouges", ylim=c(0,max(calcul,150)), names.arg = c("Domicile", "Exterieur"),col=c("Red","Red"))
  
}
NbCartRDom("Monaco")
