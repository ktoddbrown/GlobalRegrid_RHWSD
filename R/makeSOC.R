#' Calculate soil organic carbon stocks
#' 
#' This function calculates the organic soil carbon stocks from the data structure and returns it as a ratified raster.
#' Note that there are several options on how to calculate this soil carbon stock and we return both the coarse fraction corrected and uncorrected estimates using the `BULK.DENSITY` values.
#' Note that we did NOT use the `REF.BULK.DENSITY` calculations here and such a choice is debatable.
#' Use your best judgement after consulting the project documentation.
#'
#' @param hwsd_raster a raster object we will ratify and return with annotated values. Be careful with the size here!
#' @param hwsd_data a list containing the dataframe `HWSD_DATA` that has the reference values
#' @param verbose a boolean that dumps messages on function status
#'
#' @return a ratified raster object with top and sub soil organic carbon stocks
#' @export
#'
#' @importFrom dplyr select mutate group_by summarise rename left_join if_else
#' @importFrom tidyr replace_na
#' @importFrom raster raster
#' @import magrittr
makeSOC <- function(hwsd_raster, hwsd_data, verbose=FALSE){

  if(verbose){message('Pulling SOC calculations from bulk density, OC and soil fraction. Cross with raster levels.')}
  datakey <- hwsd_data$HWSD_DATA %>%
    dplyr::select(ID, MU.GLOBAL, SHARE, ISSOIL,  REF.DEPTH, 
                  T.GRAVEL, T.REF.BULK.DENSITY, T.BULK.DENSITY, T.OC, 
                  S.GRAVEL, S.REF.BULK.DENSITY, S.BULK.DENSITY, S.OC) %>%
    tidyr::replace_na(list(SHARE = 0, ISSOIL = 0, T.GRAVEL = 0, T.BULK.DENSITY = 0,T.OC = 0, S.GRAVEL = 0, S.BULK.DENSITY = 0, S.OC = 0)) %>%
    dplyr::mutate(S.THICKNESS = if_else(REF.DEPTH > 30, as.numeric(REF.DEPTH - 30), as.numeric(0))/100,
                  T.THICKNESS = pmin(as.numeric(REF.DEPTH), 30)/100,
                  SoilFrac = as.numeric(ISSOIL) * as.numeric(SHARE)/100,
                  T.REF.BULK.DENSITY = (as.numeric(T.REF.BULK.DENSITY) * 1e3), #convert kg/dm3 to kg/m3
                  S.REF.BULK.DENSITY = (as.numeric(S.REF.BULK.DENSITY) * 1e3),
                  T.BULK.DENSITY = (as.numeric(T.BULK.DENSITY) * 1e3), #convert kg/dm3 to kg/m3
                  S.BULK.DENSITY = (as.numeric(S.BULK.DENSITY) * 1e3),
                  T.OC = as.numeric(T.OC)/100, #convert from percent to fraction
                  S.OC = as.numeric(S.OC)/100,
                  T.GRAVEL = as.numeric(T.GRAVEL)/100,
                  S.GRAVEL = as.numeric(S.GRAVEL)/100
                  ) %>% 
    dplyr::group_by(MU.GLOBAL) %>%
    dplyr::summarise(SOC_top.kg_per_m2_coarse = sum(SoilFrac * T.THICKNESS * T.BULK.DENSITY * T.OC * (1 - T.GRAVEL), na.rm=TRUE), 
                     SOC_sub.kg_per_m2_coarse = sum(SoilFrac * S.THICKNESS * S.BULK.DENSITY * S.OC * (1 - S.GRAVEL), na.rm=TRUE),
                     SOC_top.kg_per_m2_noCoarse = sum(SoilFrac * T.THICKNESS * T.BULK.DENSITY * T.OC, na.rm=TRUE), 
                     SOC_sub.kg_per_m2_noCoarse = sum(SoilFrac * S.THICKNESS * S.BULK.DENSITY * S.OC, na.rm=TRUE),
                     SOC_top.kg_per_m2_refBD_coarse = sum(SoilFrac * T.THICKNESS * T.REF.BULK.DENSITY * (1 - T.GRAVEL) * T.OC, na.rm=TRUE), 
                     SOC_sub.kg_per_m2_refBD_coarse = sum(SoilFrac * S.THICKNESS * S.REF.BULK.DENSITY * (1 - S.GRAVEL) * S.OC, na.rm=TRUE),
                     SOC_top.kg_per_m2_refBD_noCoarse = sum(SoilFrac * T.THICKNESS * T.REF.BULK.DENSITY * T.OC, na.rm=TRUE), 
                     SOC_sub.kg_per_m2_refBD_noCoarse = sum(SoilFrac * S.THICKNESS * S.REF.BULK.DENSITY * S.OC, na.rm=TRUE),
      SoilFrac = sum(SoilFrac)) %>%
    dplyr::rename(ID = MU.GLOBAL) %>% 
    dplyr::mutate(ID = as.numeric(as.character(ID)))
  
  if(verbose){message('ratifying the raster')}
  soc_map <- raster::ratify(hwsd_raster)
  
  if(verbose){message('setting the levels')}
  temp <- levels(soc_map)[[1]]
  temp2 <- dplyr::left_join(temp, datakey, by = 'ID')
  levels(soc_map) <- temp2
  
  return(soc_map)
}