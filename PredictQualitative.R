setwd("C:/RStudio/Jeux de donnees")

donnees <- read.csv(file = "FIFA19-U.csv", header = TRUE, sep = ",", dec = ",")

#Supprime les doublons
doublons <- which(duplicated(donnees$player_extended_name))
donneesQ<-donnees[-doublons,]

#Supprime les icons
donneesQ[donneesQ$club=="Icons",]<-NA
donneesQ<-na.omit(donneesQ)

#Rennomage gold-Rare en gold
donneesQ$quality[donneesQ$quality == "Gold - Rare"]<- "Gold"
donneesQ$quality[donneesQ$quality == "Silver - Rare"]<- "Silver"
donneesQ$quality[donneesQ$quality == "Bronze - Rare"]<- "Bronze"

#Shooting note
donneesQ$shooting
donneesQ$shooting[donneesQ$shooting == "N/A"] <- NA
print(min(as.numeric(as.character(donneesQ$shooting)), na.rm=TRUE))
print(max(as.numeric(as.character(donneesQ$shooting)), na.rm=TRUE))
print(mean(as.numeric(as.character(donneesQ$shooting)), na.rm=TRUE))

donneesQ["Shoot"] <- as.numeric(as.character(donneesQ$shooting))
donneesQ$Shoot[donneesQ$Shoot >= 74] <- "Très bon"
donneesQ$Shoot[donneesQ$Shoot <74 & donneesQ$Shoot > 64] <- "Moyen"
donneesQ$Shoot[donneesQ$Shoot <= 64] <- "Faible"

#Dribbling note
donneesQ$dribbling[donneesQ$dribbling == "N/A"] <- NA
donneesQ["dribble"]<- as.numeric(as.character(donneesQ$dribbling))

print(min(as.numeric(as.character(donneesQ$dribbling)), na.rm=TRUE))
print(max(as.numeric(as.character(donneesQ$dribbling)), na.rm=TRUE))
print(mean(as.numeric(as.character(donneesQ$dribbling)), na.rm=TRUE))

donneesQ$dribble[donneesQ$dribble>=74] <- "Excellent"
donneesQ$dribble[donneesQ$dribble <74 & donneesQ$dribble >64]<- "Bon"
donneesQ$dribble[donneesQ$dribble<=64]<- "Faible"

#Passing
donneesQ$passing[donneesQ$passing == "N/A"] <- NA
min(as.numeric(as.character(donneesQ$passing)), na.rm=TRUE)
print(max(as.numeric(as.character(donneesQ$passing)), na.rm=TRUE))
print(mean(as.numeric(as.character(donneesQ$passing)), na.rm=TRUE))

donneesQ["Passe"]<- as.numeric(as.character(donneesQ$passing))
donneesQ$Passe[donneesQ$Passe >=74]<- "Precision +++"
donneesQ$Passe[donneesQ$Passe <74 & donneesQ$Passe >64]<- "Precision ++"
donneesQ$Passe[donneesQ$Passe <=64]<- "Precision +"

#physical note
donneesQ$physicality[donneesQ$physicality == "N/A"] <- NA
min(as.numeric(as.character(donneesQ$physicality)), na.rm=TRUE)
print(max(as.numeric(as.character(donneesQ$physicality)), na.rm=TRUE))
print(mean(as.numeric(as.character(donneesQ$physicality)), na.rm=TRUE))

donneesQ["Physique"]<- as.numeric(as.character(donneesQ$physicality))
donneesQ$Physique[donneesQ$Physique >=74]<- "Puissant"
donneesQ$Physique[donneesQ$Physique <74 & donneesQ$Physique >64]<- "Moyen"
donneesQ$Physique[donneesQ$Physique <=64]<- "Faible"

#defending note
donneesQ$defending[donneesQ$defending == "N/A"] <- NA
min(as.numeric(as.character(donneesQ$defending)), na.rm=TRUE)
print(max(as.numeric(as.character(donneesQ$defending)), na.rm=TRUE))
print(mean(as.numeric(as.character(donneesQ$defending)), na.rm=TRUE))

donneesQ["Defence"]<- as.numeric(as.character(donneesQ$defending))
donneesQ$Defence[donneesQ$Defence >=74]<- "Tres bon"
donneesQ$Defence[donneesQ$Defence <74 & donneesQ$Defence >64]<- "Bon"
donneesQ$Defence[donneesQ$Defence <=64]<- "Faible"

#Renommer attaquant
donneesQ["PostionJoueur"]<- as.character(donneesQ$position)
donneesQ$PostionJoueur[donneesQ$PostionJoueur=="ST" | donneesQ$PostionJoueur=="CF" | donneesQ$PostionJoueur=="RW"  | donneesQ$PostionJoueur=="LW"]<-"Attaquant"

#Renommer defense
donneesQ$PostionJoueur[donneesQ$PostionJoueur=="CB" | donneesQ$PostionJoueur=="RB" | donneesQ$PostionJoueur=="LB" | donneesQ$PostionJoueur=="LWB"]<-"Defenseur"

#Renommer milieux
donneesQ$PostionJoueur[donneesQ$PostionJoueur=="CM" | donneesQ$PostionJoueur=="CAM" | donneesQ$PostionJoueur=="CDM" | donneesQ$PostionJoueur=="LM" | donneesQ$PostionJoueur=="RM"]<-"Milieux"

#Renommer gardiens
donneesQ$PostionJoueur[donneesQ$PostionJoueur=="GK"]<-"Gardien"


#Fonction Moyenne générale club
fonctMoyenneGeneral <- function(club) {
  m<-donneesQ
  #print (m)
  t<-m[4]  #Ce que je veux réccuperer
  #print(t)
  b<-which(m$club==club)  #Ma condition
  #dim(b)
  #print(b)
  r<-t[c(b),]  #lien entre t et b
  #print(r)
  #dim(r)
  
  total<-sum(r=="Gold" | r=="Silver" | r=="Bronze")
  gold<-round(sum(r=="Gold")/total,2)
  silver<-round(sum(r=="Silver")/total,2)
  bronze<-round(sum(r=="Bronze")/total,2)
  maxi<-max(gold,silver,bronze)+0.1
  
  pr<-paste(club ,"moyenne gold:",gold)
  print(pr)
  
  pr1<-paste(club,"moyenne silver:",silver)
  print(pr1)
  
  pr2<-paste(club, "moyenne bronze:",bronze)
  print(pr2)
  
  n <- c(gold,silver,bronze)
  barplot(n,xlab="",main = paste(club,"Joueurs:"), ylab="Pourcentage", ylim=c(0,maxi), names.arg=c("Gold", "Silver", "Bronze"),col=c("gold","grey","brown"))
}
fonctMoyenneGeneral("Paris Saint-Germain")


##Club DEFENSEUR: Moyenne gold, silver, bronze + graphe
fonctMoyenneDef<- function(club) {
  m<-donneesQ
  #print (m)
  t<-m[4]
  #print(t)
  b<-which(m$club==club & (m$position=="CB" | m$position=="LB" | m$position=="RB"))
  #dim(b)
  #print(b)
  r<-t[c(b),]
  #print(r)
  #dim(r)
  
  total<-sum(r=="Gold" | r=="Silver" | r=="Bronze")
  gold<-round(sum(r=="Gold")/total,2)
  silver<-round(sum(r=="Silver")/total,2)
  bronze<-round(sum(r=="Bronze")/total,2)
  maxi<-max(gold,silver,bronze)+0.1
  
  pr<-paste(club,"moyenne gold:",gold)
  print(pr)
  
  pr1<-paste(club,"moyenne silver:",silver)
  print(pr1)
  
  pr2<-paste(club,"moyenne bronze:",bronze)
  print(pr2)
  
  n <- c(gold,silver,bronze)
  barplot(n,xlab="",main = paste(club,"Défenseur:"), ylab="Pourcentage", ylim=c(0,maxi), names.arg=c("Gold", "Silver", "Bronze"),col=c("gold","grey","brown"))
}
fonctMoyenneDef("Chelsea")

fonctMoyenneMilieux<- function(club) {
  m<-donneesQ
  #print (m)
  t<-m[4]
  #print(t)
  b<-which(m$club==club & (m$position=="CM" | m$position=="CAM" | m$position=="CDM" | m$position=="LM" | m$position=="RM"))
  #dim(b)
  #print(b)
  r<-t[c(b),]
  #print(r)
  #dim(r)
  
  total<-sum(r=="Gold" | r=="Silver" | r=="Bronze")
  gold<-round(sum(r=="Gold")/total,2)
  silver<-round(sum(r=="Silver")/total,2)
  bronze<-round(sum(r=="Bronze")/total,2)
  
  maxi<-max(gold,silver,bronze)+0.1
  
  pr<-paste(club,"moyenne milieux gold:",gold)
  print(pr)
  
  pr1<-paste(club, "moyenne milieux silver:",silver)
  print(pr1)
  
  pr2<-paste(club,"moyenne milieux bronze:",bronze)
  print(pr2)
  
  n <- c(gold,silver,bronze)
  barplot(n,xlab="",main = paste(club,"Milieux:"), ylab="Pourcentage", ylim=c(0,maxi), names.arg=c("Gold", "Silver", "Bronze"),col=c("gold","grey","brown"))
  
}
fonctMoyenneMilieux("Chelsea")

###Club ATTAQUANTS: Moyenne gold, silver, bronze+ graphe
fonctMoyenneAtt <- function(club) {
  m<-donneesQ
  #print (m)
  t<-m[4]
  #print(t)
  b<-which(m$club==club & (m$position=="ST" | m$position=="CF" | m$position=="RW" | m$position=="LW"))
  #dim(b)
  #print(b)
  r<-t[c(b),]
  #print(r)
  #dim(r)
  
  total<-sum(r=="Gold" | r=="Silver" | r=="Bronze")
  gold<-round(sum(r=="Gold")/total,2)
  silver<-round(sum(r=="Silver")/total,2)
  bronze<-round(sum(r=="Bronze")/total,2)
  maxi<-max(gold,silver,bronze)+0.1
  
  pr<-paste(club, "moyenne attaquants gold:",gold)
  print(pr)
  
  pr1<-paste(club,"moyenne attaquants silver:",silver)
  print(pr1)
  
  pr2<-paste(club, "moyenne attaquants bronze:",bronze)
  print(pr2)
  
  n <- c(gold,silver,bronze)
  barplot(n,xlab="",main = paste(club,"Attaquants:"), ylab="Pourcentage", ylim=c(0,maxi), names.arg=c("Gold", "Silver", "Bronze"),col=c("gold","grey","brown"))
}
fonctMoyenneAtt("Manchester City")

###FonctMoyenneMilieux a rajouter

###Club Gardien: Moyenne gold, silver, bronze + graphe
fonctMoyenneGar <- function(club) {
  m<-donneesQ
  #print (m)
  t<-m[4]
  #print(t)
  b<-which(m$club==club & (m$position=="GK"))
  #dim(b)
  #print(b)
  r<-t[c(b),]
  #print(r)
  #dim(r)
  
  total<-sum(r=="Gold" | r=="Silver" | r=="Bronze")
  gold<-round(sum(r=="Gold")/total,2)
  silver<-round(sum(r=="Silver")/total,2)
  bronze<-round(sum(r=="Bronze")/total,2)
  maxi<-max(gold,silver,bronze)+0.1
  
  pr<-paste(club, "moyenne gardiens gold:",gold)
  print(pr)
  
  pr1<-paste(club,"moyenne gardiens silver:",silver)
  print(pr1)
  
  pr2<-paste(club, "moyenne gardiens bronze:",bronze)
  print(pr2)
  
  n <- c(gold,silver,bronze)
  barplot(n,xlab="",main = paste(club,"Gardiens:"), ylab="Pourcentage", ylim=c(0,maxi), names.arg=c("Gold", "Silver", "Bronze"),col=c("gold","grey","brown"))
}
fonctMoyenneGar("Paris Saint-Germain")


#Fonction classifie joueur, Prend en parametre player_extended_name
fonctionJoueur <- function(joueur){
  m<-donneesQ
  t<-m[90]  #Ce que tu veux reccuperer 
  b<-which(m[3]==joueur) #Condition
  p<-t[c(b),] #lien entre les 2
  #print(p)
  nbGold<-0
  nbSilver<-0
  nbBronze<-0
  
  t<-m[85]   #Shoot
  t2<-m[87]  #passe
  t3<-m[86]  #driblle
  t4<-m[88]  #physique
  t5<-m[89]  #Defence
  
  r<-t[c(b),]
  r2<-t2[c(b),]
  r3<-t3[c(b),]
  r4<-t4[c(b),]
  r5<-t5[c(b),]
  
  if (p=="Attaquant"){
    nbGold<-sum(r=="Très bon")+sum(r2=="Precision +++")+sum(r3=="Excellent")+sum(r4=="Puissant")
    nbSilver<-sum(r=="Moyen")+sum(r2=="Precision ++")+sum(r3=="Bon")+sum(r4=="Moyen")
    nbBronze<-sum(r=="Faible")+sum(r2=="Precision +")+sum(r3=="Faible")+sum(r4=="Faible")
    #print(paste(nbGold,nbSilver,nbBronze))
  }else if(p=="Defenseur"){
    nbGold<-sum(r5=="Tres bon")+sum(r2=="Precision +++")+sum(r4=="Puissant")
    nbSilver<-sum(r5=="Bon")+sum(r2=="Precision ++")+sum(r4=="Moyen")
    nbBronze<-sum(r5=="Faible")+sum(r2=="Precision +")+sum(r4=="Faible")
    #print(paste(nbGold,nbSilver,nbBronze))
  }else if(p=="Milieux"){
    nbGold<-sum(r5=="Tres bon")+sum(r=="Très bon")+sum(r2=="Precision +++")+sum(r3=="Excellent")+sum(r4=="Puissant")
    nbSilver<-sum(r5=="Bon")+sum(r=="Moyen")+sum(r2=="Precision ++")+sum(r3=="Bon")+sum(r4=="Moyen")
    nbBronze<-sum(r5=="Faible")+sum(r=="Faible")+sum(r2=="Precision +")+sum(r3=="Faible")+sum(r4=="Faible")
    #print(paste(nbGold,nbSilver,nbBronze))
  }

  if(nbGold>=nbSilver & nbGold>nbBronze){
    #print("Gold")
    return("Gold")
  }else if(nbSilver>=nbGold & nbSilver>=nbBronze){
    #print("Silver")
    return ("Silver")
  }else{
    #print("Bronze")
    return ("Bronze")
  }
  
}


###Club Moyenne general d'apres nous
fonctMoyenneClubDetails <- function(club) {
  m<-donneesQ
  #print (m)
  t<-m[3]
  b<-which(m$club ==club)
  r<-t[c(b),]
  
  gold<-0
  silver<-0
  bronze<-0
  total<-0

  for(i in r){
    total<-total+1
    if(fonctionJoueur(as.character(i))=="Gold"){
      gold<-gold+1
    }else if(fonctionJoueur(as.character(i))=="Silver"){
      silver<-silver+1
    }else if(fonctionJoueur(as.character(i))=="Bronze"){
      bronze<-bronze+1
    }else{
        print("")
      }
  }
  #print (total)
  #print (paste("Gold:",gold))
  #print(paste("Silver:",silver))
  #print (paste("Bronze:",bronze))
  
  goldFin<-(gold)/total
  silverFin<-(silver)/total
  bronzeFin<-(bronze)/total
    
  maxi<-max(goldFin,silverFin,bronzeFin)+0.1
  
  pr<-paste(club, "moyenne Gold:",round(goldFin,2))
  print(pr)
  
  pr1<-paste(club,"moyenne Silver:",round(silverFin,2))
  print(pr1)
  
  pr2<-paste(club, "moyenne Bronze:",round(bronzeFin,2))
  print(pr2)
  
  n <- c(goldFin,silverFin,bronzeFin)
  barplot(n,xlab="",main = paste(club,"Moyenne General:"), ylab="Pourcentage", ylim=c(0,maxi), names.arg=c("Gold", "Silver", "Bronze"),col=c("gold","grey","brown"))
}

fonctMoyenneClubDetails("Manchester City")
fonctMoyenneGeneral("Manchester City")

###Club Moyenne attaquant d'apres nous
fonctMoyenneAttDetails <- function(club) {
  m<-donneesQ
  #print (m)
  t<-m[3]
  b<-which(m$club == club & m[90]=="Attaquant")
  r<-t[c(b),]
  
  gold<-0
  silver<-0
  bronze<-0
  total<-0
  
  for(i in r){
    total<-total+1
    if(fonctionJoueur(as.character(i))=="Gold"){
      gold<-gold+1
    }else if(fonctionJoueur(as.character(i))=="Silver"){
      silver<-silver+1
    }else if(fonctionJoueur(as.character(i))=="Bronze"){
      bronze<-bronze+1
    }
  }
  #print (total)
  #print (paste("Gold:",gold))
  #print(paste("Silver:",silver))
  #print (paste("Bronze:",bronze))
  
  goldFin<-(gold)/total
  silverFin<-(silver)/total
  bronzeFin<-(bronze)/total
  
  maxi<-max(goldFin,silverFin,bronzeFin)+0.1
  
  print ("D'apres nous:")
  
  pr<-paste(club, "moyenne Attaquant Gold:",round(goldFin,2))
  print(pr)
  
  pr1<-paste(club,"moyenne Attaquant Silver:",round(silverFin,2))
  print(pr1)
  
  pr2<-paste(club, "moyenne Attaquant Bronze:",round(bronzeFin,2))
  print(pr2)
  
  n <- c(goldFin,silverFin,bronzeFin)
  barplot(n,xlab="",main = paste(club,"Moyenne Attaquants:"), ylab="Pourcentage", ylim=c(0,maxi), names.arg=c("Gold", "Silver", "Bronze"),col=c("gold","grey","brown"))
}


fonctMoyenneAttDetails("Manchester City")
fonctMoyenneAtt("Manchester City")


###Club Deffenseur Moyenne d'apres nous
fonctMoyenneDefDetails <- function(club) {
  m<-donneesQ
  #print (m)
  t<-m[3]
  b<-which(m$club == club & m[90]=="Defenseur")
  r<-t[c(b),]
  
  gold<-0
  silver<-0
  bronze<-0
  total<-0
  
  for(i in r){
    total<-total+1
    if(fonctionJoueur(as.character(i))=="Gold"){
      gold<-gold+1
    }else if(fonctionJoueur(as.character(i))=="Silver"){
      silver<-silver+1
    }else if(fonctionJoueur(as.character(i))=="Bronze"){
      bronze<-bronze+1
    }else{
      print("")
    }
  }
  #print (total)
  #print (paste("Gold:",gold))
  #print(paste("Silver:",silver))
  #print (paste("Bronze:",bronze))
  
  goldFin<-(gold)/total
  silverFin<-(silver)/total
  bronzeFin<-(bronze)/total
  
  maxi<-max(goldFin,silverFin,bronzeFin)+0.1
  
  print ("D'apres nous:")
  
  pr<-paste(club, "moyenne defenseur Gold:",round(goldFin,2))
  print(pr)
  
  pr1<-paste(club,"moyenne defenseur Silver:",round(silverFin,2))
  print(pr1)
  
  pr2<-paste(club, "moyenne defenseur Bronze:",round(bronzeFin,2))
  print(pr2)
  
  n <- c(goldFin,silverFin,bronzeFin)
  barplot(n,xlab="",main = paste(club,"Moyenne Defenseur:"), ylab="Pourcentage", ylim=c(0,maxi), names.arg=c("Gold", "Silver", "Bronze"),col=c("gold","grey","brown"))
}

fonctMoyenneDefDetails("Manchester City")
fonctMoyenneDef("Manchester City")


###Club Milieux Moyenne d'apres nous
fonctMoyenneMilieuxDetails <- function(club) {
  m<-donneesQ
  #print (m)
  t<-m[3]
  b<-which(m$club == club & m[90]=="Milieux")
  r<-t[c(b),]
  
  gold<-0
  silver<-0
  bronze<-0
  total<-0
  
  for(i in r){
    total<-total+1
    if(fonctionJoueur(as.character(i))=="Gold"){
      gold<-gold+1
    }else if(fonctionJoueur(as.character(i))=="Silver"){
      silver<-silver+1
    }else if(fonctionJoueur(as.character(i))=="Bronze"){
      bronze<-bronze+1
    }else{
      print("")
    }
  }
  #print (total)
  #print (paste("Gold:",gold))
  #print(paste("Silver:",silver))
  #print (paste("Bronze:",bronze))
  
  goldFin<-(gold)/total
  silverFin<-(silver)/total
  bronzeFin<-(bronze)/total
  
  maxi<-max(goldFin,silverFin,bronzeFin)+0.1
  
  print ("D'apres nous:")
  pr<-paste(club, "moyenne Milieux Gold:",round(goldFin,2))
  print(pr)
  
  pr1<-paste(club,"moyenne Milieux Silver:",round(silverFin,2))
  print(pr1)
  
  pr2<-paste(club, "moyenne Milieux Bronze:",round(bronzeFin,2))
  print(pr2)
  
  n <- c(goldFin,silverFin,bronzeFin)
  barplot(n,xlab="",main = paste(club,"Moyenne Milieux:"), ylab="Pourcentage", ylim=c(0,maxi), names.arg=c("Gold", "Silver", "Bronze"),col=c("gold","grey","brown"))
}
fonctMoyenneMilieuxDetails("Manchester United")
fonctMoyenneMilieux("Manchester United")


fonctMoyenneMilieuxDetails("Juventus")
fonctMoyenneMilieux("Juventus")
fonctMoyenneDefDetails("Juventus")
fonctMoyenneDef("Juventus")
fonctMoyenneAttDetails("Juventus")
fonctMoyenneAtt("Juventus")
