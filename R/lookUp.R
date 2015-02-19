makeSD <- function(OC, BD=NA){
    #returns the relative organic carbon and bulk density errors
    #recall that the sd of two products is the sqrt of the sum of squares
    sRange <- c(0.06,  0.50) #relative range

    if(FALSE){
      dRange <- range(as.vector(OC), na.rm=TRUE)
      
      oc_sd <- ((sRange[2]-sRange[1])*(exp(dRange[2])-exp(dRange[1]))^-1*exp(OC)+
                  sRange[1]-(sRange[2]-sRange[1])*exp(dRange[1])*(exp(dRange[2])-exp(dRange[1]))^-1) #fit an exp curve to the relative range at the min/max of the data
    }else{
      dRange <- range(0,40)
      
      a <- diff(sRange)/diff(dRange)
      b <- sRange[1]-a*dRange[1]

      oc_sd <- (a*OC+b)
      
      oc_sd[is.finite(OC) && OC > dRange[2]] <-
        dRange[2]*OC[is.finite(OC) && OC > dRange[2]]
    }
    return(oc_sd)
}

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
    subOCsd <- refTable$S_OC_SD/100
    topOCsd <- refTable$T_OC_SD/100
    subBDsd <- refTable$S_BULK_DENSITY_SD*1e3
    topBDsd <- refTable$T_BULK_DENSITY_SD*1e3

    ##values from weighted averages
    bulk <- sum(c(subBD*s_depth*weights,
                  topBD*t_depth*weights),
                na.rm=TRUE)
    bulk_sd <- sqrt(sum(c((subBD*s_depth*weights)^2*
                          ((subBDsd/subBD)^2 + (s_depth_sd/s_depth)^2),
                          (topBD*t_depth*weights)^2*
                          ((topBDsd/topBD)^2 + (t_depth_sd/t_depth)^2)),
                        na.rm=TRUE))

    ##Calculate SOC
    soc <- sum(c(subOC*subBD*s_depth*weights,
                 topOC*topBD*t_depth*weights),
                   na.rm=TRUE)

    soc_sd <- sqrt(sum(c((subOC*subBD*s_depth*weights)^2*
               ((subOCsd/subOC)^2 + (subBDsd/subBD)^2 + (s_depth_sd/s_depth)^2),
                         (topOC*topBD*t_depth*weights)^2*
               ((topOCsd/topOC)^2 + (topBDsd/topBD)^2 + (t_depth_sd/t_depth)^2)),
                       na.rm=TRUE))

    ##Calculate OC is wrapped up int he first two so don't bother
    ##oc <- soc/bulk
    ##oc_sd <- sqrt(oc^2*(bulk_sd^2/bulk^2+soc_sd^2/soc^2))
    ##cat('[', soilID,']: ', c(bulk, bulk_sd, soc, soc_sd), '\n')
        return(c(bulk, bulk_sd, soc, soc_sd))
}
