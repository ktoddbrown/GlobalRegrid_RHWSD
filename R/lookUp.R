lookUp <- function(soilID, refTable=dataTable){
  #cat(soilID, ': ')
  if(is.na(soilID) || soilID == 0) {
    #cat('NA\n')
    return(rep(NA, times=4))
  }
  
  ##look up the soil ID for the grid cell trim the reference table to only icnlude the soil ID of interest
  refTable <- refTable[refTable$MU_GLOBAL == soilID,]
  ##Pull the weights and convert them to percentages
  weights <- refTable$SHARE/100 ##area fraction
  weights[refTable$ISSOIL == 0] <- NA  ##If it's not a soil then remove it
  
  ##Pull the size of the sub-soil
  s_depth <- refTable$REF_DEPTH-30
  s_depth[s_depth < 0] <- NA
  s_depth <- s_depth/100 #convert from cm to m
  s_depth_sd <- s_depth*refTable$REF_DEPTH_SD/refTable$REF_DEPTH
  
  ##Pull the size of the top-soil
  t_depth <- refTable$REF_DEPTH
  t_depth[t_depth > 30] <- 30
  t_depth <- t_depth/100
  t_depth_sd <- t_depth*refTable$REF_DEPTH_SD/refTable$REF_DEPTH
  
  ##Pull the weighted average of the depth
  #depth <- sum(refTable$REF_DEPTH*weights, na.rm=TRUE)/sum(weights, na.rm=TRUE)
  
  subBD <- refTable$S_BULK_DENSITY*1e3#convert from g/cm^3 to kg/m^3
  topBD <- refTable$T_BULK_DENSITY*1e3
  subOC <- refTable$S_OC/100
  topOC <- refTable$T_OC/100
  
  ##values from weighted averages
  bulk <- sum(c(subBD*s_depth*weights,
                topBD*t_depth*weights),
              na.rm=TRUE)
  
  ##Calculate SOC
  soc <- sum(c(subOC*subBD*s_depth*weights,
               topOC*topBD*t_depth*weights),
             na.rm=TRUE)
  
  ##Calculate OC is wrapped up int he first two so don't bother
  ##oc <- soc/bulk
  ##oc_sd <- sqrt(oc^2*(bulk_sd^2/bulk^2+soc_sd^2/soc^2))
  ##cat('[', soilID,']: ', c(bulk, bulk_sd, soc, soc_sd), '\n')
  return(c(bulk, soc))
}
