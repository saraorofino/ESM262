#' Compute seasonal mean flows
#'
#' This function computes winter and summer flows from a record
#’ @param str data frame with columns month and streamflow 
compute_season_flow = function(str, kind="mean") {
  
  str$season = ifelse( str$month %in% c(1,2,3,10,11,12),"winter","summer")
  
  
  tmp = subset(str, str$season=="winter")
  if(kind=="mean") winter= mean(tmp$streamflow)
  if(kind=="max") winter= max(tmp$streamflow)
  if(kind=="min") winter= min(tmp$streamflow)
  
  tmp = subset(str, str$season=="summer")
  if(kind=="mean") summer= mean(tmp$streamflow)
  if(kind=="max") summer= max(tmp$streamflow)
  if(kind=="min") summer = min(tmp$streamflow)
  
  return(list(summer=summer, winter=winter))
}