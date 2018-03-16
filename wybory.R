#podzial mandatow metoda D'Hondta
dhondt <- function(votes, mandates){
  parties<-length(votes)
  given<-rep(0,parties)
  dhondtlist<-rep(votes)

  for(i in 1:mandates){
    max<-1
    for(j in 1:parties){
      if(dhondtlist[j]>max){
        max<-dhondtlist[j]
        index<-j
      }
    }
    given[index]<-(given[index]+1)
    dhondtlist[index]<-votes[index]/(given[index]+1)
  }
  return(given)
}

#podzial mandatow metoda Sainte-Lague
saintlague <- function(votes, mandates){
  parties<-length(votes)
  given<-rep(0,parties)
  dhondtlist<-rep(votes)
  
  for(i in 1:mandates){
    max<-1
    for(j in 1:parties){
      if(dhondtlist[j]>max){
        max<-dhondtlist[j]
        index<-j
      }
    }
    given[index]<-(given[index]+1)
    dhondtlist[index]<-votes[index]/(2*given[index]+1)
  }
  return(given)
}

#sprawdzenie czy partie przekroczyly staly prog wyborczy
elect.thres <- function(votes, threshold){
  list <- c()
  count <- length(votes)
  for (i in 1:count){
    if (votes[i]>=threshold)
      list <- c(list, i)
  }
  return(list)
}

#nazywanie powtarzajacych sie danych
namess <- function(text, n){
  t<-c()
  for(i in 1:n){
    t<-c(t,paste(text, i))
  }
  return(t)
}

#symulacja wyborow, dane wejsciowe to odpowiednio sformatowane dane o okregach
#wektor z wynikami sondazu oraz frekwencja podana jako liczba z przedzialu <0,1>
#domyslnie 60% procent glosow niewaznych domyslnie 5% oraz 
#odchylenie standardowe bledu rozkladu sondazu
elect.sym <- function(datacons, survey, freq=.6, blankp=.05, res=1.5, method=dhondt){
  
  parties<-length(survey)
  constituencies<-length(datacons$Nr.okregu)
  
  allvotes<-round(abs(datacons$Liczba.wyborcow * rnorm(constituencies, freq, res/50)), digits=0)
  blanks<-round(abs(allvotes * rnorm(41, blankp, res/400)), digits=0)
  
  votes <- backp <- matrix(nrow = constituencies, ncol = parties)
  colnames(backp) <- namess("Poparcie partii", parties)
  colnames(votes) <- namess("Glosy partii", parties)
  
  for(i in 1:constituencies){
    temp1 <- abs(rnorm(parties, survey, res))
    temp1 <- temp1/sum(temp1)
    backp[i,] <- temp1
    votes[i,] <- round((temp1*(allvotes[i]-blanks[i])), digits=0)
  }
  
  blanks<-allvotes-rowSums(votes)
  backcountry<-round(colSums(votes)/sum(votes), digits = 4)
  threshold<-elect.thres(backcountry, .05)
  
  mandates<-matrix(rep(0,parties*constituencies), nrow = constituencies)
  colnames(mandates) <- namess("Mandaty dla partii", parties)
  for(i in 1:constituencies){
    mandates[i, threshold] <- method(votes[i, threshold], datacons$Liczba.mandatow[i])
  }
  
  mandates <- rbind(mandates,colSums(mandates))
  votes <- rbind(votes, colSums(votes))
  backp <- rbind(backp,backcountry)
  allvotes <- c(allvotes, sum(allvotes))
  blanks <- c(blanks, sum(blanks))
  
  datacons$Siedziba<-as.character(datacons$Siedziba)
  datacons[42,]<-list(0,"Podsumowanie",sum(datacons[3]), sum(datacons[4]))
  
  datacons["Glosy oddane"]<-allvotes
  datacons<-cbind(datacons,votes)
  datacons["Glosy niewazne"]<-blanks
  datacons<-cbind(datacons, mandates)
  
  return(datacons)
}

#Dane o okregach wyborczych
data_cons <- read.csv(file = "https://raw.githubusercontent.com/hubertmis1/wybory/master/OkregiWyborcze.csv", sep=";")


