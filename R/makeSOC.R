makeSOC <- function(hwsd_raster, hwsd_data, verbose=FALSE){

  if(verbose){message('Pulling SOC calculations from bulk density, OC and soil fraction. Cross with raster levels.')}
  datakey <- hwsd_data$HWSD_DATA %>%
    dplyr::select(ID, MU.GLOBAL, SHARE, ISSOIL, T.GRAVEL, T.BULK.DENSITY,T.OC, S.GRAVEL, S.BULK.DENSITY, S.OC, REF.DEPTH) %>%
    dplyr::mutate(S.THICKNESS = if_else(REF.DEPTH > 30, as.numeric(REF.DEPTH - 30), as.numeric(0))/100,
                  T.THICKNESS = pmin(as.numeric(REF.DEPTH), 30)/100,
                  SoilFrac = as.numeric(ISSOIL) * as.numeric(SHARE)/100,
                  T.BD = (as.numeric(T.BULK.DENSITY) * 1e3), #convert kg/dm3 to kg/m3
                  S.BD = (as.numeric(S.BULK.DENSITY) * 1e3),
                  T.OC = as.numeric(T.OC)/100, #convert from percent to fraction
                  S.OC = as.numeric(S.OC)/100,
                  T.GRAVEL = as.numeric(T.GRAVEL)/100,
                  S.GRAVEL = as.numeric(S.GRAVEL)/100) %>% 
    dplyr::group_by(MU.GLOBAL) %>%
    dplyr::summarise(SOC_top.kg_per_m2 = sum(SoilFrac * T.THICKNESS * T.BD * T.OC * (1 - T.GRAVEL), na.rm=TRUE), 
      SOC_sub.kg_per_m2 = sum(SoilFrac * S.THICKNESS * S.BD * S.OC * (1 - S.GRAVEL), na.rm=TRUE),
      SOC_top.kg_per_m2_BDdepthOC = mean(T.THICKNESS * T.BD * T.OC , na.rm=TRUE), 
      SOC_sub.kg_per_m2_BDdepthOC = mean(S.THICKNESS * S.BD * S.OC , na.rm=TRUE),
      SoilFrac = sum(SoilFrac)) %>%
    dplyr::rename(ID = MU.GLOBAL) %>% 
    dplyr::mutate(ID = as.numeric(as.character(ID)))
  
  if(verbose){message('ratifying the raster')}
  soc_map <- ratify(hwsd_raster)
  
  if(verbose){message('setting the levels')}
  temp <- levels(soc_map)[[1]]
  temp2 <- dplyr::full_join(temp, datakey, by = 'ID')
  levels(soc_map) <- temp2
  
  return(soc_map)
}