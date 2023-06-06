ModeVal1 = function(v){
  a= data.frame(table(v))
  b= a$v[a$Freq == max(a$Freq)]
  
  c= as.character(b)
  n= as.numeric(c)
  
  type = ""
  len = length(n)
  
  if(len ==1){
    type = "(unimodal)"
  }
  else if(len == 2){
    type = "(bimodal)"
  }
  else if(len == 3){
    type = "(trimodal)"
  }
  
  
  if(max(a$Freq) == 1){
    if(length(a$v)==1){
      return(paste(c,type))
    }
    else{ 
      return("no mode")
    }
  }
  else if(length(unique(a[['Freq']])) == 1){
    return("no mode")

  }
  else{
    return(paste(c,type))
  }
}
