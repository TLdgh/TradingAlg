MakeName<-function(DataInfo){
if (DataInfo["SecurityType"]=="FUT") {   #give the name of the security and its corresponding file names
  nam <- paste(DataInfo["Symb"],DataInfo["intv"], sep = "")
}else if(DataInfo["SecurityType"] == "STK"){
  if(DataInfo["intv"]=="daily" | DataInfo["intv"]=="weekly" | DataInfo["intv"]=="monthly"){
    if(DataInfo["GlobalMarket"]=="China"){
      nam <- paste(DataInfo["A_STOK"],"_",DataInfo["intv"], sep = "")
    }else{
      nam <- paste(toupper(DataInfo["Symb"]),"_",DataInfo["intv"], sep = "")
    }
  }
  else{nam <- paste(toupper(DataInfo["Symb"]),DataInfo["intv"], sep = "")
  }
}else if(DataInfo["SecurityType"] == "FOREX" ){
  nam <- paste(toupper(gsub("/","",DataInfo["Symb"])),"_",DataInfo["intv"], sep = "")
}
return(nam)
}