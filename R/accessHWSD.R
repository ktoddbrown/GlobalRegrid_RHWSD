#' Download and load HWSD
#'
#' This function downloads the HWSD to a target folder from the project webpage, and unzips it.
#' It then loads the map of the mapping units as a raster object and the MS Access Database as a list of tables.
#' You will need to install mdbtools on your system to use Hmisc::mdb.get
#' 
#'
#' @param downloadFolder locatin to download the files to
#' @param HWSD_proj string defining the crs projection
#'
#' @return a list contining the raster of the mapping units and a list of the data tables supporting this raster
#' @export
#'
#' @importFrom Hmisc mdb.get
#' @importFrom raster raster crs
#' @importFrom utils download.file unzip
#'
accessHWSD <- function(downloadFolder, HWSD_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"){
  #downloadFolder <- 'temp'
  if(!dir.exists(downloadFolder)){
    stop('Need target for download files.')
  }
  
  if(!file.exists(file.path(downloadFolder, 'HWSD.zip')) |
     !file.exists(file.path(downloadFolder, 'HWSD_RASTER.zip'))){
    #downloads
    download.file(url = 'http://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/HWSD.zip', 
                  destfile = file.path(downloadFolder, 'HWSD.zip')) #mdb
    download.file(url = 'http://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/HWSD_RASTER.zip', 
                  destfile = file.path(downloadFolder, 'HWSD_RASTER.zip')) #Raster
  }
  
  #unzip
  unzip(file.path(downloadFolder, 'HWSD.zip'), exdir = downloadFolder)
  unzip(file.path(downloadFolder, 'HWSD_RASTER.zip'), exdir = file.path(downloadFolder, 'HWSD_RASTER'))
  
  #readin the raster
  HWSD_raster <- raster::raster(file.path(downloadFolder, 'HWSD_RASTER', 'hwsd.bil'))
  #manually set the projection
  raster::crs(HWSD_raster) <- HWSD_proj
  
  #read in the MS Access database
  HWSD_mdb <- Hmisc::mdb.get(file.path(downloadFolder, 'HWSD.mdb'))
  
  return(list(raster = HWSD_raster, db = HWSD_mdb))
  
}