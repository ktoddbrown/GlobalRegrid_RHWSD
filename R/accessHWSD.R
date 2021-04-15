#' Title
#'
#' You will need to install mdbtools on your system to use Hmisc::mdb.get
#'
#' @param downloadFolder 
#'
#' @return
#' @export
#'
#' @importFrom Hmisc mdb.get
#' @importFrom raster raster
#' @importFrom dplyr select mutate group_by summarise right_join
#'
#' @examples
accessHWSD <- function(downloadFolder, HWSD_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"){
  #downloadFolder <- 'temp'
  if(!dir.exists(downloadFolder)){
    stop('Need target for download files.')
  }
  
  if(!file.exists(file.path(downloadFolder, 'HWSD.zip')) |
     !file.exists(file.path(downloadFolder, 'HWSD_RASTER.zip'))){
    #downloads
    download.file(url = 'http://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/HWSD.zip', destfile = file.path(downloadFolder, 'HWSD.zip')) #mdb
    download.file(url = 'http://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/HWSD_RASTER.zip', destfile = file.path(downloadFolder, 'HWSD_RASTER.zip')) #Raster
  }
  
  #unzip
  unzip(file.path(downloadFolder, 'HWSD.zip'), exdir = downloadFolder)
  unzip(file.path(downloadFolder, 'HWSD_RASTER.zip'), exdir = file.path(downloadFolder, 'HWSD_RASTER'))
  
  HWSD_raster <- raster::raster(file.path(downloadFolder, 'HWSD_RASTER', 'hwsd.bil'))
  
  proj4string(HWSD_raster) <- HWSD_proj
  
  HWSD_mdb <- Hmisc::mdb.get(file.path(downloadFolder, 'HWSD.mdb'))
  
  return(list(raster = HWSD_raster, db = HWSD_mdb))
  
}