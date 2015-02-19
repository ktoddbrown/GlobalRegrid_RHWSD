##Programer: Kathe Todd-Brown ktoddbrown@gmail.com
##Date: Revised for GitHub: 2015
#       Major revisions: 4 Febuary 2013
#       Revised and commented, 23 April 2013
#       Expanded to full resolution and error analysis added
##Purpose: Load HWSD soil carbon density to R format
##Prep: Download HWSD_RASTER.zip (20.7 MB) and HWSD.mdb (46.8 MB) from HWSD data webpage
#       Convert HWSD.mdb file to HWSD.sqlite (12.6 MB) using MDB Explorer
library(compiler)
library(raster) ##Deal with spatial maps nicely
library(RSQLite) ##Load the atribute database
source('lookUp.R')

closeAllConnections()
##Load the raster
hwsd <- raster("./HWSD_RASTER/hwsd.bil") #1.87 GB or HWSD_RASTER.zip MB
(proj4string(hwsd) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#hwsd.regrid <- projectRaster(hwsd, crs=newproj,
#					res=c(1,1), filename='temp.bil', method='ngb', overwrite=TRUE)

if(!('shortDataTable' %in% ls())){
	##Load the atribute database
	m <- dbDriver("SQLite")
	##HWSD.sqlite created from .mdb using MDB Explorer
	##...load into database
	con <- dbConnect(m, dbname = "HWSD.sqlite") #12.6 MB


	##Inform the user of what we have
	cat('List of available tables:\n')
	print(dbListTables(con)) ##list the tables
	cat('HWSD_DATA table structure\n')
	print(dbGetQuery(con, "pragma table_info(HWSD_DATA)")$name)


	##What entries we are interested in from the HWSD_DATA
	##...pull for the density and OC info
	colNames.data <- c('ID', 'MU_GLOBAL', 'ISSOIL',  'REF_DEPTH',
					   'T_REF_BULK_DENSITY', 'S_REF_BULK_DENSITY',
					   'T_BULK_DENSITY', 'S_BULK_DENSITY',
					   'T_OC', 'S_OC', 'SHARE')
	##What entries we are interested in from the HWSD_SMU
	##...pull so we can see what type of soil are have
	colNames.smu <- c('ID', 'MU_GLOBAL', 'SU_SYMBOL', 'SU_CODE')

	##Debugging comments, peak at the start of the tables
	#tmp1 <- dbGetQuery(con, paste("select", paste(colNames.data, collapse = ", "),"from HWSD_DATA limit 10"))
	#tmp2 <- dbGetQuery(con, paste("select", paste(colNames.smu, collapse = ", "), "from HWSD_SMU limit 10"))

	##Pull the tables we are interested in
	dataTable <- dbGetQuery(con, paste("select", paste(colNames.data, collapse = ", "), "from HWSD_DATA"))
	smu.table <- dbGetQuery(con, paste("select", paste(colNames.smu, collapse = ", "), "from HWSD_SMU"))

	cat('making sd...')
	dataTable$T_REF_BULK_DENSITY_SD <- dataTable$T_REF_BULK_DENSITY*0.15
	dataTable$S_REF_BULK_DENSITY_SD <- dataTable$S_REF_BULK_DENSITY*0.15
	dataTable$T_BULK_DENSITY_SD <- dataTable$T_BULK_DENSITY*0.15
	dataTable$S_BULK_DENSITY_SD <- dataTable$S_BULK_DENSITY*0.15
	dataTable$T_OC_SD <- dataTable$T_OC*makeSD(OC=dataTable$T_OC)
	dataTable$S_OC_SD <- dataTable$S_OC*makeSD(OC=dataTable$S_OC)
	cat('done\n')

	cat('process dataTable:', format(Sys.time(), '%H:%M:%OS3'), '...')
	vapplyWrapper <- cmpfun(function (soilID){
	  return(vapply(soilID, FUN=function(x){c(x, lookUp(x))}, 
                  FUN.VALUE=rep(0, times=5), USE.NAMES=FALSE))}) ##streight up complied vapply, this is the one used

	shortDataTable <- vapplyWrapper(unique(dataTable$MU_GLOBAL))
#	shortDataTable <- vapply(dataTable$MU_GLOBAL, FUN=lookUp, FUN.VALUE=rep(0, length=5), USE.NAMES=FALSE, refTable=dataTable) #precompiling thsi would be faster
	shortDataTable  <- data.frame(t(shortDataTable))
	names(shortDataTable) <- c('MU', 'bulk', 'bulk_sd', 'soc', 'soc_sd')
	shortDataTable <- rbind(c(0, rep(NA, length=4)), shortDataTable)
	shortDataTable <- rbind(rep(NA, length=5), shortDataTable)
	cat(format(Sys.time(), '%H:%M:%OS3'), 'done\n')
}else{
	cat('shortDataTable already created... moving on.\n')
}

bd_file <- 'fullBulkDensity.grd'
socSd_file <- 'fullSOCSd.grd'
soc_file <- 'fullSOC.grd'
bdSd_file <- 'fullBulkSDDensity.grd'

if(FALSE){
    bulk <- raster(hwsd)
    bulk_sd <- raster(hwsd)
    soc <- raster(hwsd)
    soc_sd <- raster(hwsd)

    bulk <- writeStart(bulk, file=bd_file, overwrite=TRUE)
    bulk_sd <- writeStart(bulk_sd, file=bdSd_file, overwrite=TRUE)
    soc <- writeStart(soc, file=soc_file, overwrite=TRUE)
    soc_sd <- writeStart(soc_sd, file=socSd_file, overwrite=TRUE)

    bs <- blockSize(hwsd)
    cat('going through', bs$n, 'blocks\n')
    for(ii in 1:bs$n){
        if(ii %% 10 == 0){
            cat(ii, '\n')
        }else{
            cat('.')
        }
        soilIndex <- getValues(hwsd, row=bs$row[ii], nrows=bs$nrows[ii])
        soilInfo <- shortDataTable[match(as.vector(soilIndex), shortDataTable$MU),]

        bulk <- writeValues(bulk, soilInfo$bulk, bs$row[ii])
        bulk_sd <- writeValues(bulk_sd, soilInfo$bulk_sd, bs$row[ii])
        soc <- writeValues(soc, soilInfo$soc, bs$row[ii])
        soc_sd <- writeValues(soc_sd, soilInfo$soc_sd, bs$row[ii])
    }

    writeStop(bulk)
    writeStop(bulk_sd)
    writeStop(soc)
    writeStop(soc_sd)
}

if(FALSE){
bulk <- raster(bd_file)
bulk_sd <- raster(bdSd_file)
soc <- raster(soc_file)
soc_sd <- raster(socSd_file)

bulk.regrid <- aggregate(bulk, fact=120, fun=mean, filename='bulk_area1deg.grd')
bulk_sd.regrid <- aggregate(bulk_sd, fact=120, fun=mean, filename='bulkSd_area1deg.grd')
soc.regrid <- aggregate(soc, fact=120, fun=mean, filename='soc_area1deg.grd')
soc_sd.regrid <- aggregate(soc_sd, fact=120, fun=mean, filename='socSd_area1deg.grd')

writeRaster(soc.regrid, filename='HWSD_1deg.nc', varname='SOC', varunit='kg m^-2', longname='Area weighted soil organic carbon from HWSD')
}

cat('done\n')
