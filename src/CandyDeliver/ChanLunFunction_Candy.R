StarFunction <- function (Pricedata){  #this function creates the star pattern/fengxin
  i<-1
  j<-2
  ConnectPoint <- matrix(0, nrow=4, ncol=length(Pricedata[,1]))
  
  
  Pattern <- function (Pricedata, index) {
    if((Pricedata[index,3]<Pricedata[index+1,3])&(Pricedata[index+1,3]>Pricedata[index+2,3])& 
       (Pricedata[index,4]<Pricedata[index+1,4])&(Pricedata[index+1,4]>Pricedata[index+2,4])){
      #this is shooting star
      return(1)
    }else if((Pricedata[index,3]>Pricedata[index+1,3])&(Pricedata[index+1,3]<Pricedata[index+2,3])&
             (Pricedata[index,4]>Pricedata[index+1,4])&(Pricedata[index+1,4]<Pricedata[index+2,4])){
      #this is morning star
      return(2)
    }else{
      return(0)
    }
  }
  
  
  while (i<=length(Pricedata[,1])-2) {
    if(Pattern(Pricedata,i)==1){
      ConnectPoint[1,j]<-1 #star pattern
      ConnectPoint[2,j]<-Pricedata[i+1,3] # middle star high
      ConnectPoint[3,j]<-min(Pricedata[i,4],Pricedata[i+1,4],Pricedata[i+2,4]) #min of the low of the three candlesticks
      ConnectPoint[4,j]<-max(Pricedata[i,3],Pricedata[i+1,3],Pricedata[i+2,3]) #max of the high of the three candlesticks
      i=i+1
      j=j+1
    }else if(Pattern(Pricedata,i)==2){
      ConnectPoint[1,j]<-2
      ConnectPoint[2,j]<-Pricedata[i+1,4] # middle star low 
      ConnectPoint[3,j]<-min(Pricedata[i,4],Pricedata[i+1,4],Pricedata[i+2,4]) #min of the low of the three candlesticks
      ConnectPoint[4,j]<-max(Pricedata[i,3],Pricedata[i+1,3],Pricedata[i+2,3]) #max of the high of the three candlesticks
      i=i+1
      j=j+1
    }else{
      i=i+1
      j=j+1
    }
  }
  
  k<-which(ConnectPoint[1,]!=0)
  PatternCompare <- function (ConnectPoint, index){
    if(ConnectPoint[1,k[index+1]]==1 & ConnectPoint[1,k[index]]==1 & ConnectPoint[4,k[index+1]]>ConnectPoint[4,k[index]]){
      k<-k[-index]
    }else if(ConnectPoint[1,k[index+1]]==1 & ConnectPoint[1,k[index]]==1 & ConnectPoint[4,k[index+1]]<=ConnectPoint[4,k[index]]){
      k<-k[-(index+1)]
    }else if(ConnectPoint[1,k[index+1]]==2 & ConnectPoint[1,k[index]]==2 & ConnectPoint[3,k[index+1]]<ConnectPoint[3,k[index]]){
      k<-k[-index]
    }else if(ConnectPoint[1,k[index+1]]==2 & ConnectPoint[1,k[index]]==2 & ConnectPoint[3,k[index+1]]>=ConnectPoint[3,k[index]]){
      k<-k[-(index+1)]
    }else if(ConnectPoint[1,k[index+1]]!=ConnectPoint[1,k[index]] & k[index+1]-k[index]<4){
      k<-k[-(index+1)]
    }else if(ConnectPoint[1,k[index+1]]!=ConnectPoint[1,k[index]] & k[index+1]-k[index]>=4){
      if((ConnectPoint[1,k[index+1]]==1 & ConnectPoint[1,k[index]]==2 & ConnectPoint[4,k[index+1]]<=ConnectPoint[4,k[index]]) |+
         (ConnectPoint[1,k[index+1]]==2 & ConnectPoint[1,k[index]]==1 & ConnectPoint[3,k[index+1]]>=ConnectPoint[3,k[index]])){
        k<-k[-(index+1)]
      }
    }else{return(k)}
    
    return(k)
  }
  PatternCheck <- function(ConnectPoint, k, index){
    if(ConnectPoint[1,k[index+1]]!=ConnectPoint[1,k[index]] & k[index+1]-k[index]>=4){
      if((ConnectPoint[1,k[index+1]]==1 & ConnectPoint[1,k[index]]==2 & ConnectPoint[4,k[index+1]]>ConnectPoint[4,k[index]]) |+
         (ConnectPoint[1,k[index+1]]==2 & ConnectPoint[1,k[index]]==1 & ConnectPoint[3,k[index+1]]<ConnectPoint[3,k[index]])){
        return(TRUE)
      }
      else{return(FALSE)
      }
    }else{return(FALSE)}
  }
  
  u<-1
  while (u<length(k)){
    if(PatternCheck(ConnectPoint,k,u)==FALSE){
      k<-PatternCompare(ConnectPoint,u)    
    }else{u=u+1}
  }
  
  StarData <-data.frame(Pricedata[k,1], ConnectPoint[,k][2,], ConnectPoint[,k][1,])
  colnames(StarData) <- c("Date", "Price", "StarType")
  
  Lastpoint <-cbind(tail(Pricedata,1)[c("Date","Close")], data.frame(StarType=0))#this is the close price of the last candlestick 
  colnames(Lastpoint) <- c("Date", "Price", "StarType")
  
  if ((tail(StarData,1)$StarType==1 & Lastpoint$Price>tail(StarData,1)$Price) | #this step just extend the Bi if necessary
      (tail(StarData,1)$StarType==2 & Lastpoint$Price<tail(StarData,1)$Price)) {
    StarData[nrow(StarData),] <- Lastpoint
  }else{
    StarData <- rbind(StarData,Lastpoint)
  }
  
  
  return(StarData)
}



BiFunction<-function(StarData){
  Bi<- as.data.frame(matrix(0, nrow = nrow(StarData)-1, ncol = 5))
  colnames(Bi) <- c("SLOPE", "MIN", "MAX", "BiStartD", "BiEndD")
  for(i in 1:(nrow(StarData)-1)){   #get the bi slope, min and max
    if (StarData$Price[i+1]>StarData$Price[i]){
      Bi[i,1]<-1
      Bi[i,2]<-StarData$Price[i]
      Bi[i,3]<-StarData$Price[i+1]  
      Bi[i,4]<-StarData$Date[i] 
      Bi[i,5]<-StarData$Date[i+1] 
    }
    else{
      Bi[i,1]<- -1
      Bi[i,2]<-StarData$Price[i+1]
      Bi[i,3]<-StarData$Price[i] 
      Bi[i,4]<-StarData$Date[i] 
      Bi[i,5]<-StarData$Date[i+1] 
    }
  }
  
  return(Bi)
}


planet <- function(Bi, line1, line2){   #get the planet high and low
  planet_range<- as.data.frame(matrix(0, nrow =1, ncol = 4))
  
  planet_range[1,1]<-min(Bi[line1,"MAX"], Bi[line2,"MAX"])
  planet_range[1,2]<-max(Bi[line1,"MIN"],Bi[line2,"MIN"])
  planet_range[1,3]<-Bi[line1,"BiStartD"]
  planet_range[1,4]<-Bi[line2,"BiEndD"]
  colnames(planet_range) <- c("PlanetHigh", "PlanetLow", "PlanetStartD", "PlanetEndD")
  return(planet_range)
}


NewHigh<-function(Bi, newhighline, beginl){
  j<-newhighline
  for(j in newhighline:nrow(Bi)){ #check which line gives new high
    if(Bi[j,"MAX"]>max(Bi[beginl+1,"MAX"], Bi[beginl+3,"MAX"])){break}else{j<-j+1}
  }
  return(j)
}


NewLow<-function(Bi, newlowline, beginl){
  j<-newlowline
  for(j in newlowline:nrow(Bi)){ #check which line gives new low
    if(Bi[j,"MIN"]<min(Bi[beginl+1,"MIN"], Bi[beginl+3,"MIN"])){break}else{j<-j+1}
  }
  return(j)
}


ThirdBuy<-function(Bi,planet_range,thirdbuyline,beginl){
  k<-thirdbuyline
  if(k<=nrow(Bi)-2){ #check which line gives third buy
    while(k<=(nrow(Bi)-2)){
      if(Bi[k,"MIN"]>=planet_range[1,"PlanetHigh"] 
         #& Bi[k+2,"MIN"]>=planet_range[1,"PlanetHigh"]
      ){#if condition not met, then move the k to the next thirdbuyline and reconsider 
        if ((k+2)<NewLow(Bi,thirdbuyline,beginl)){k<-k+2;break}else{k<-NewLow(Bi,thirdbuyline,beginl)+1;break}
      }
      else{k<-k+2}
    }
  }
  else{k<-thirdbuyline+1}
  return(k)
}


ThirdSale<-function(Bi,planet_range,thirdsaleline,beginl){
  k<-thirdsaleline
  if(k<=(nrow(Bi)-2)){#check which line gives third sale
    while(k<=(nrow(Bi)-2)){
      if(Bi[k,"MAX"]<=planet_range[1,"PlanetLow"] 
         #& Bi[k+2,"MAX"]<=planet_range[1,"PlanetLow"]
      ){#if condition not met, then move the k to the next thirdsaleline and reconsider
        if ((k+2)<NewHigh(Bi,thirdsaleline,beginl)){k<-k+2;break}else{k<-NewHigh(Bi,thirdsaleline,beginl)+1;break}
      }
      else{k<-k+2}    
    }
  }
  else{k<-thirdsaleline+1}
  return(k)
}


CountThirdBuy<-function(Bi,planet_range,beginl, endl){
  count<-0
  while(beginl<=min(endl,nrow(Bi))){
    if(Bi[beginl,"MIN"]>planet_range[1,"PlanetHigh"]){count<-count+1}
    beginl<-beginl+2
  }
  return(count)
}


CountThirdSale<-function(Bi,planet_range,beginl, endl){
  count<-0
  while(beginl<=min(endl,nrow(Bi))){
    if(Bi[beginl,"MAX"]<planet_range[1,"PlanetLow"]){count<-count+1}
    beginl<-beginl+2
  }
  return(count)
}


DownPlanetBreaker <-function(Bi, newlowline, beginl){  # this checks if there's any bi exceeding the top of the incoming bi of the downward planet.
  count<-0
  for(j in (beginl+1):(newlowline-1)){ #check which line gives new high
    if(Bi[j,"MAX"]>Bi[beginl,"MAX"]){count<-count+1}else{next}
  }
  return(count)
}


UpPlanetBreaker <- function(Bi, newhighline, beginl){# this checks if there's any bi exceeding the bottom of the incoming bi of the upward planet.
  count<-0
  for(j in (beginl+1):(newhighline-1)){ #check which line gives new low
    if(Bi[j,"MIN"]<Bi[beginl,"MIN"]){count<-count+1}else{next}
  }
  return(count)
}


PlanetFunction <- function(Bi){   #this function creates the Bi and planet/zhongshu
  TotalBi<-nrow(Bi)
  beginl<-1
  Finalplanet <-as.data.frame(matrix(0, nrow=TotalBi, ncol=4))
  colnames(Finalplanet) <- c("PlanetHigh", "PlanetLow", "PlanetStartD", "PlanetEndD")
  
  while(beginl<=(TotalBi-4) ){
    if (Bi[beginl,"SLOPE"]==1) {
      if(Bi[beginl+1, "MIN"]>Bi[beginl, "MIN"]){
        if(Bi[beginl+3, "MIN"]<Bi[beginl+1, "MAX"]){ #if line2 and line4 has intersect
          newhighline<- beginl+4
          thirdsaleline <-beginl+4
          planet_range<-planet(Bi,beginl+1, beginl+3)
          TrueNewHighLine<-NewHigh(Bi,newhighline,beginl)
          
          if(TrueNewHighLine<=TotalBi & #must have new high to perfect the planet
             CountThirdBuy(Bi=Bi, planet_range=planet_range, beginl=newhighline, endl=TrueNewHighLine-2)<2 &  #must not have more than 1 thirdbuy inside the planet
             TrueNewHighLine<=ThirdSale(Bi,planet_range,thirdsaleline,beginl) &
             UpPlanetBreaker(Bi,TrueNewHighLine,beginl)==0){# planet is born
            Finalplanet[beginl+1,1] <-planet_range[1,1] #plaenet high
            Finalplanet[beginl+1,2] <-planet_range[1,2] #planet low
            Finalplanet[beginl+1,3] <-planet_range[1,3] #planet start date
            Finalplanet[beginl+1,4] <-Bi[TrueNewHighLine, "BiStartD"] ##planet end date
            LastHigh<-TrueNewHighLine
            
            if(LastHigh<=(TotalBi-2)){#check after the newhigh we have at least 2 bi to compare before the last bi.
              j<-LastHigh+2
              NumKuoZhan<-0
              
              while(j<=TotalBi){
                if(Bi[j,"MAX"]>planet_range$PlanetLow){condition1<-TRUE} #no new down planet formed
                #else if((j<=TotalBi-2) & Bi[j,"MAX"]<=planet_range$PlanetLow & Bi[j+2,"MAX"]>planet_range$PlanetLow){condition1<-TRUE}
                else{condition1<-FALSE}
                
                if(Bi[j,"MIN"]<planet_range$PlanetHigh){condition2<-TRUE}#no new up planet formed or break the current planet
                else if(j<=TotalBi-2 
                        & Bi[j,"MIN"]>=planet_range$PlanetHigh #thirdbuy is formed to confirm planet
                        & Bi[j+2,"MIN"]<planet_range$PlanetHigh
                        & Bi[j+2,"MIN"]>Bi[beginl, "MIN"]
                        & Bi[j+2,"MAX"]>Bi[j, "MAX"]){condition2<-TRUE;NumKuoZhan<-NumKuoZhan+1}else{condition2<-FALSE}
                
                if(condition1==TRUE & (condition2==TRUE & NumKuoZhan<=1) & Bi[j,"MIN"]>Bi[beginl, "MIN"]){ #of LastHigh min doesn't break the up planet
                  if(Bi[j,"MAX"]>=Bi[LastHigh,"MAX"]){LastHigh<-j} #if conditions are all satisfied, update LastHigh, else break the whole loop
                }else{break}
                j<-j+2
              }
              Finalplanet[beginl+1,4]<-Bi[LastHigh, "BiStartD"]
            }
            
            beginl=LastHigh
          }
          else{beginl=beginl+1} #planet wasn't born, need to reconsider
        }
        else{beginl<-beginl+2}
      }
      else{beginl<-beginl+1}
    }
    else if(Bi[beginl,"SLOPE"]== -1){
      if(Bi[beginl+1, "MAX"]<=Bi[beginl, "MAX"]){
        if(Bi[beginl+3, "MAX"]>Bi[beginl+1, "MIN"]){ #if line2 and line4 has intersect
          newlowline<-beginl+4
          thirdbuyline<-beginl+4
          planet_range<-planet(Bi,beginl+1, beginl+3)
          TrueNewLowLine<-NewLow(Bi,newlowline,beginl)
          
          if(TrueNewLowLine<=TotalBi & #if new low occurs on or before the last bi 
             CountThirdSale(Bi=Bi, planet_range=planet_range, beginl = newlowline, endl = TrueNewLowLine-2)<2 &  #must not have more than 1 thirdsale inside the planet
             TrueNewLowLine<=ThirdBuy(Bi,planet_range,thirdbuyline,beginl) &  #if new low occurs before a new planet forms
             DownPlanetBreaker(Bi,TrueNewLowLine,beginl)==0){# doesn't allow the max of the newlow exceed the planet's incoming bi
            Finalplanet[beginl+1,1] <-planet_range[1,1]
            Finalplanet[beginl+1,2] <-planet_range[1,2]
            Finalplanet[beginl+1,3] <-planet_range[1,3]
            Finalplanet[beginl+1,4] <-Bi[TrueNewLowLine, "BiStartD"]
            LastLow<-TrueNewLowLine
            
            if(LastLow<=(TotalBi-2)){#check after the newlow we have at least 2 bi to compare before the last bi.
              j<-LastLow+2
              NumKuoZhan<-0
              while(j<=TotalBi){
                if(Bi[j,"MIN"]<planet_range$PlanetHigh){condition3<-TRUE }#no new up planet formed 
                #else if((j<=TotalBi-2) & Bi[j,"MIN"]>=planet_range$PlanetHigh & Bi[j+2,"MIN"]<planet_range$PlanetHigh){condition3<-TRUE}
                else{condition3<-FALSE}
                
                if(Bi[j,"MAX"]>planet_range$PlanetLow){condition4<-TRUE}#no new down planet formed or break the current planet 
                else if(j<=TotalBi-2 
                        & Bi[j,"MAX"]<=planet_range$PlanetLow 
                        & Bi[j+2,"MAX"]>planet_range$PlanetLow 
                        & Bi[j+2,"MAX"]<Bi[beginl, "MAX"]
                        & Bi[j+2,"MIN"]<Bi[j, "MIN"]){condition4<-TRUE;NumKuoZhan<-NumKuoZhan+1}else{condition4<-FALSE}
                
                if(condition3==TRUE & (condition4==TRUE & NumKuoZhan<=1) & Bi[j,"MAX"]<Bi[beginl, "MAX"]){ #of LastLow max doesn't break the down planet
                  if(Bi[j,"MIN"]<=Bi[LastLow,"MIN"]){LastLow<-j} #if conditions are all satisfied, update LastLow, else break the whole loop
                }else{break}
                j<-j+2
              }
              Finalplanet[beginl+1,4]<-Bi[LastLow, "BiStartD"]
            }
            
            beginl=LastLow
          }
          else{beginl=beginl+1} #planet wasn't born, need to reconsider
        }
        else{beginl<-beginl+2}
      }
      else{beginl<-beginl+1}
    }
  }
  return(Finalplanet)
}
