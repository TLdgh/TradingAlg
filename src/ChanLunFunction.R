StarFunction <- function (Pricedata){  #this function creates the star pattern/fengxin
  StarData<-Pricedata%>%mutate(Direction=c(0,ifelse(diff(High)<0,-1,1)))%>%
    mutate(Step1=c(0,diff(Direction)))%>%
    mutate(Step2=c(diff(Direction),0))%>%
    mutate(Star=1*(Step2!=0)*Direction,Index=index(.),Step1=NULL,Step2=NULL)
  
  OnlyStar<-StarData%>%filter(Star!=0)
  initiali<-1
  i<-2
  while(i<=nrow(OnlyStar)){
    j<-OnlyStar[initiali,"Index"]
    initialRange<-range(StarData[(j-1):(j+1),c("High","Low")])
    
    if((OnlyStar[i,"Star"]==1 & OnlyStar[i,"High"]>max(initialRange)) | 
       (OnlyStar[i,"Star"]==-1 & OnlyStar[i,"Low"]<min(initialRange))
    ){
      if(sum(OnlyStar[c(initiali, i),"Star"])==0 & (OnlyStar[i,"Index"]-OnlyStar[initiali,"Index"]-1)>=3){
        initiali<-i
        i<-i+1}
      else if(sum(OnlyStar[c(initiali, i),"Star"])==0 & (OnlyStar[i,"Index"]-OnlyStar[initiali,"Index"]-1)<3){
        OnlyStar[i,"Star"]<-0
        i<-i+1}
      else if(sum(OnlyStar[c(initiali, i),"Star"])!=0){
        OnlyStar[initiali,"Star"]<-0
        initiali<-i
        i<-i+1}
    }else{
      OnlyStar[i,"Star"]<-0
      i<-i+1
    }
  }
  
  OnlyStar[which(OnlyStar$Star==-1),"Star"]<-2 #the morning star (底分型), 1 is shooting star (顶分型)
  
  P1<-OnlyStar%>%filter(Star==2)%>%select(Date,Price=Low,StarType=Star, Index)
  P2<-OnlyStar%>%filter(Star==1)%>%select(Date,Price=High,StarType=Star,Index)
  P3<-tail(StarData,1)%>%select(Date,Price=Close,StarType=Star,Index)
  
  StarData<-arrange(rbind(P1,P2,P3),Index)
  
  return(StarData)
}



BiFunction<-function(StarData){
  Bi<- as.data.frame(matrix(0, nrow = nrow(StarData)-2, ncol = 5))
  colnames(Bi) <- c("SLOPE", "MIN", "MAX", "BiStartD", "BiEndD")
  for(i in 1:(nrow(StarData)-2)){   #get the bi slope, min and max
    if (StarData$Price[i+1]>StarData$Price[i]){
      Bi[i,1]<-1
      Bi[i,2]<-StarData$Price[i] #the MIN
      Bi[i,3]<-StarData$Price[i+1]  #the MAX
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
  
  if(Bi[i,1]==1 & diff(tail(StarData$Price,2))>0){
    Bi[i,"MAX"]<-StarData$Price[i+2]
    Bi[i,"BiEndD"]<-StarData$Date[i+2]
  }else if(Bi[i,1]==-1 & diff(tail(StarData$Price,2))<0){
    Bi[i,"MIN"]<-StarData$Price[i+2]
    Bi[i,"BiEndD"]<-StarData$Date[i+2]
  }else if(Bi[i,1]==1 & diff(tail(StarData$Price,2))<0){
    Bi<-rbind(Bi,data.frame(SLOPE=-1,MIN=StarData$Price[i+2],MAX=StarData$Price[i+1],BiStartD=StarData$Date[i+1],BiEndD=StarData$Date[i+2]))
  }else if(Bi[i,1]==-1 & diff(tail(StarData$Price,2))>0){
    Bi<-rbind(Bi,data.frame(SLOPE=1,MIN=StarData$Price[i+1],MAX=StarData$Price[i+2],BiStartD=StarData$Date[i+1],BiEndD=StarData$Date[i+2]))
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


SimpThirdBuy<-function(Bi,planet_range,beginl){ #used by ScheduleAlert
  count<-0
  while(count<1 & beginl<=nrow(Bi)-2){
    if(Bi[beginl+2,"MIN"]>=planet_range[1,"PlanetHigh"]){count<-1}else{beginl<-beginl+2}    
  }
  return(count)
}

SimpThirdSale<-function(Bi,planet_range,beginl){ #used by ScheduleAlert
  count<-0
  while(count<1 & beginl<=nrow(Bi)-2){
    if(Bi[beginl+2,"MAX"]<=planet_range[1,"PlanetLow"]){count<-1}else{beginl<-beginl+2}    
  }
  return(count)
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


LineSegment<-function(Bi){   #this function creates the Line Segment and planet/zhongshu
  TotalBi<-nrow(Bi)
  beginl<-1
  
  while(beginl<=(TotalBi-4) ){
    if(Bi[beginl,"SLOPE"]==1){
      if(Bi[beginl+1, "MIN"]>Bi[beginl, "MIN"]){
        if((Bi[beginl+3, "MIN"]>=Bi[beginl+1, "MIN"]) & (Bi[beginl+3, "MAX"]<=Bi[beginl+1, "MAX"]) ){
          Bi[beginl+1,"MIN"]<-Bi[beginl+3, "MIN"]
          Bi[beginl+1,"BiEndD"]<-Bi[beginl+3, "BiEndD"]
          Bi<-Bi[-c(beginl+2,beginl+3),]
          TotalBi<-nrow(Bi)
          beginl<-1
        }else if((Bi[beginl+3, "MIN"]<=Bi[beginl+1, "MIN"]) & (Bi[beginl+3, "MAX"]>=Bi[beginl+1, "MAX"])){
          beginl<-beginl+3
        }
        else{beginl<-beginl+2}
      }
      else{beginl<-beginl+1}
    }
    else if(Bi[beginl,"SLOPE"]== -1){
      if(Bi[beginl+1, "MAX"]<=Bi[beginl, "MAX"]){
        if((Bi[beginl+3, "MAX"]<=Bi[beginl+1, "MAX"]) & (Bi[beginl+3, "MIN"]>=Bi[beginl+1, "MIN"])){
          Bi[beginl+1,"MAX"]<-Bi[beginl+3, "MAX"]
          Bi[beginl+1,"BiEndD"]<-Bi[beginl+3, "BiEndD"]
          Bi<-Bi[-c(beginl+2,beginl+3),]
          TotalBi<-nrow(Bi)
          beginl<-1
        }else if((Bi[beginl+3, "MAX"]>=Bi[beginl+1, "MAX"]) & (Bi[beginl+3, "MIN"]<=Bi[beginl+1, "MIN"])){
          beginl<-beginl+3
        }
        else{beginl<-beginl+2}
      }
      else{beginl<-beginl+1}
    }
  }
  P1<-Bi%>%mutate(Ind=index(Bi))%>%filter(SLOPE==1)%>%select(Date=BiStartD,Price=MIN,Ind)
  P2<-Bi%>%mutate(Ind=index(Bi))%>%filter(SLOPE==-1)%>%select(Date=BiStartD,Price=MAX,Ind)
  Bi<-arrange(rbind(P1,P2), Ind)
  return(Bi)
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
            
            beginl<-LastHigh
          }
          else{beginl<-beginl+1} #planet wasn't born, need to reconsider
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
            
            beginl<-LastLow
          }
          else{beginl<-beginl+1} #planet wasn't born, need to reconsider
        }
        else{beginl<-beginl+2}
      }
      else{beginl<-beginl+1}
    }
  }
  return(Finalplanet)
}


ChanLunStr<-function(Pricedata){
  StarData <- StarFunction(Pricedata)
  Bi<- BiFunction(StarData)%>%mutate(BiIndex=index(.),.after=SLOPE)
  Finalplanet<-subset(as.data.frame(PlanetFunction(Bi)), PlanetHigh!=0)%>%mutate(PlanetIndex=index(.), .before=PlanetHigh)
  PlanetDates<-Finalplanet$PlanetEndD
  
  BiPlanetStr<-inner_join(x=Finalplanet, y=Bi, by=c("PlanetStartD"="BiEndD"))%>%inner_join(x=., y=Bi, by=c("PlanetEndD"="BiStartD"))%>%select(-SLOPE.y)
  BiPlanetStr<-rename(BiPlanetStr, c("SLOPE"="SLOPE.x", "InMIN"="MIN.x", "InMAX"="MAX.x", "InIndex"="BiIndex.x",
                                     "OutMIN"="MIN.y", "OutMAX"="MAX.y", "OutIndex"="BiIndex.y"))
  
  Rev1<-function(OutIndex,SLOPE,OutMIN,InMAX,OutMAX,InMIN){ #this checks if the planet is truely reversed
    i<-OutIndex+2
    j<-OutIndex+4
    
    Reversal<-0
    if(SLOPE==-1){
      while(j<=nrow(Bi) & (Bi[i, "MIN"]>OutMIN | Bi[j, "MIN"]>OutMIN) ){
        if(Bi[i, "MAX"]>InMAX | Bi[j, "MAX"]>InMAX ){Reversal<-1;break}else{i<-i+2; j<-j+2}
      }
    }else{
      while(j<=nrow(Bi) & (Bi[i, "MAX"]<OutMAX | Bi[j, "MAX"]<OutMAX) ){
        if(Bi[i, "MIN"]<InMIN | Bi[j, "MIN"]<InMIN ){Reversal<-1;break}else{i<-i+2; j<-j+2}
      }
    }
    return(Reversal)
  }
  
  BiPlanetStr<-BiPlanetStr%>%rowwise()%>%mutate(Reversal1=Rev1(OutIndex,SLOPE,OutMIN,InMAX,OutMAX,InMIN))
  Reversal2<-c(1*(diff(BiPlanetStr$SLOPE)!=0),0) #check if the next planet has an opposite direction than the current planet. The last planet is always assumed to be 0
  BiPlanetStr<-BiPlanetStr%>%add_column(Reversal2=Reversal2)
  return(list(StarData=StarData, Bi=Bi, BiPlanetStr=BiPlanetStr))
}
