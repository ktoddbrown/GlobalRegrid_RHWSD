# Accessing, processing and regridding the Harmonized World Soil Database map

This repository is intended to demonstrate how to use R to access, process and regrid the Harmonized World Soil Database v 1.2 map product.
I am not affilated with the group who produced this data product and any usage of this code should acknowledge the orginal product located http://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v12/en/ including download links and documentation.


## Usage and introduction

These `accessHWSD` will download and ingest the HWSD main database and raster of the mapping units.
If you are interested in the soil carbon stock calculations `makeSOC` will calculate the soil organic carbon stocks for each mapping unit.

These functions rely on the `Hmisc` package for it's MS Access database interface and that may require additional operating system level installs for it to run on your computer.

```{r}

library(tidyverse)
library(Hmisc) #check that mdb.get tools are installed on your operating systems to connect w/ access db
library(raster)

source('R/accessHWSD.R')
source('R/makeSOC.R')

```

## Working with HWSD maps

```{r}
#This code delivers a boatload of warnings but works!
HWSD <- accessHWSD(downloadFolder = 'temp') 

#regrid using the most common mapping unit
#an aggrigation factor of 120 is a single degree sized grid
map_lowres <- raster::aggregate(HWSD$raster, fact = 60, fun = modal) 

#append the soil carbon stock calculations to the regridded product
soc_map <- makeSOC(map_lowres, HWSD$db, verbose=TRUE) 

##While this would be ideal, it broke my memory requirements.
##...If you have a work around I would love to hear it!
##...There is a way to process by 'block' to calculate global totals form the orginal raster below.
#soc_map <- makeSOC(HWSD$raster, HWSD$db, verbose=TRUE)

```

```{r}
regrided.df <- as.data.frame(soc_map, xy=TRUE) %>%
                full_join(as.data.frame(area(soc_map), xy = TRUE) %>% 
                          rename('grid_area_km2' = 'layer'), 
                        by = c('x', 'y'))

ggplot(regrided.df) +
  geom_raster(aes(x=x, y=y, fill=hwsd_SOC_top.kg_per_m2_coarse)) +
  guides(fill = guide_legend(title = "SOC,  0- 30cm [kg m-2]")) +
  scale_fill_distiller(palette = 'YlOrBr', direction = 1, na.value = 'lightblue', values = c(0, .1, .2, .5, 1))+
  theme_void()

ggplot(regrided.df) +
  geom_raster(aes(x=x, y=y, fill=hwsd_SOC_sub.kg_per_m2_coarse)) +
  guides(fill = guide_legend(title = "SOC, 30-100cm [kg m-2]")) +
  scale_fill_distiller(palette = 'YlOrBr', direction = 1, na.value = 'lightblue',  values = c(0, .1, .2, .5, 1))+
  theme_void()

ggplot(regrided.df) +
  geom_raster(aes(x=x, y=y, fill=hwsd_SOC_top.kg_per_m2_coarse + hwsd_SOC_sub.kg_per_m2_coarse)) +
  guides(fill = guide_legend(title = "SOC,  0-100cm [kg m-2]")) +
  scale_fill_distiller(palette = 'YlOrBr', direction = 1, na.value = 'lightblue', values = c(0, .1, .2, .5, 1)) +
  theme_void()
```


## Calculate global totals

HWSD has many other soil properties but I was interested in organic soil carbon stocks.
Here is a calculation of those that compares the global totals between the original grid and the most-common mapping unit regrided using two different SOC calculations methods.


```{r}


#split the big raster into smaller sizes
HWSDblocks <- blockSize(HWSD$raster)

#tic <- system.time(

HWSDCounts <- plyr::adply(1:HWSDblocks$n, c(1), function(xx){
  #temp <- table(raster::getValues(HWSD$raster, row = ii, nrows = 1))
  return(tibble(MU = raster::getValues(HWSD$raster, row = HWSDblocks$row[xx], nrows = HWSDblocks$nrows[xx]),
                rowIndex = rep(HWSDblocks$row[xx]+ 0:(HWSDblocks$nrows[xx]-1), each = ncol(HWSD$raster))) %>%
           group_by_all() %>% tally())
}, .id = 'rowIndex')

#) #took about 95 seconds on my machine so grab a coffee

HWSDArea <- tibble(area_km2 = getValues(area(crop(HWSD$raster, extent(HWSD$raster, 1, nrow(HWSD$raster), 1, 1)))),
                   rowIndex = 1:nrow(HWSD$raster))

#this is basically what is in makeSOC
HWSD_MU_SOC <- HWSD$db$HWSD_DATA %>%
    dplyr::select(ID, #sample id
                  MU.GLOBAL, #mapping unit
                  SHARE, #share of maping unit
                  ISSOIL, #is soil
                  T.GRAVEL, #total gravel by fraction, top soil
                  T.BULK.DENSITY, #bulk deinsity in kg/m3, top soil
                  T.REF.BULK.DENSITY, #bulk deinsity in kg/m3, top soil
                  T.OC, #organic carbon fraction, top soil
                  S.GRAVEL,  #total gravel by fraction, sub soil
                  S.REF.BULK.DENSITY, S.BULK.DENSITY, S.OC, 
                  REF.DEPTH #reference depth
                  ) %>%
    tidyr::replace_na(list(SHARE = 0, ISSOIL = 0, 
    T.GRAVEL = 0, T.BULK.DENSITY = 0, T.REF.BULK.DENSITY = 0, T.OC = 0, 
    S.GRAVEL = 0, S.BULK.DENSITY = 0, S.REF.BULK.DENSITY = 0, S.OC = 0)) %>%
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
                  S.GRAVEL = as.numeric(S.GRAVEL)/100) %>% 
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
    dplyr::mutate(MU = as.numeric(as.character(MU.GLOBAL))) %>%
  dplyr::filter(SoilFrac > 0)

#pull it all together
HWSD_stocks <- HWSDCounts %>%
  group_by(MU) %>%
  full_join(HWSD_MU_SOC, by= 'MU') %>%
  group_by(rowIndex) %>%
  full_join(HWSDArea, by = 'rowIndex')

```

## Orginal grid totals

Organic carbon stocks are constructed from: the mass per volume ratio of the soil, times the soil layer thickness, times the organic carbon mass fraction, to produce the soil organic carbon stock per area.
This relatively straight forward calculation causes no end of headaches.

The first thing to address is the depth of the soil layer.
HWSD reports two layers 0-30 cm and 30-100cm as the top and subsurface soils, except where the reference depth is less then the layer thickness.
Somewhat annoying but we catch this with an `if_else` clause and `pmin` fairly cleanly and move on.

```
#code does not run
S.THICKNESS = if_else(REF.DEPTH > 30, as.numeric(REF.DEPTH - 30), as.numeric(0))/100,
T.THICKNESS = pmin(as.numeric(REF.DEPTH), 30)/100
```

Organic carbon percent is reported in HWSD and we want organic fraction.
A simple `/100` conversion takes care of this.
This value in the US is typically measured using a loss on ignition combined with a calibrated conversion mass to carbon ratio.

Bulk density is the sticky point in this calculation.
HWSD reports two bulk density `T.BULK.DENSITY` and `T.REF.BULK.DENSITY`, of which only `T.REF.BULK.DENSITY` is documented in the reference that contains the following note:

> $^{10}$ Bulk density, as a soil characteristic, is a function rather than a single value (USDA-NRCS, 2004 #3078, p. 73) as it is
> highly dependent on soil conditions at the time of sampling: changes in (field) water content will alter bulk density. The
> SOTWIS database provides estimates of bulk density values derived from available analyzed data, and thus consider
> differences in soil texture, organic matter content and porosity. Careful review of these values also by comparison with
> calculated reference bulk densities has revealed substantial differences. For reasons of data quality and consistency of the
> HWSD, reference bulk density values – calculated using equations developed by Saxton et al. (1986), have been used here:
> these equations represent a statistical estimate and reflect only the textural influence. 

```{r}
ggplot(HWSD$db$HWSD_DATA) + 
  geom_point(aes(x=T.BULK.DENSITY, y = T.REF.BULK.DENSITY)) +
  geom_point(aes(x=S.BULK.DENSITY, y = S.REF.BULK.DENSITY)) +
  labs(x='BULK.DENSITY', y='REF.BULK.DENSITY')
```


Unfortunately this is further complicated by the fact that soils are almost always sieved before organic carbon is measured.
Thus we typically need to correct for the coarse fraction (generally gravel above a certain size that does not contain organic carbon).
But the given citation seems to be for water potential and not bulk density here and it's not clear if this ref bulk density is corrected or not for the coarse fraction.
So we are left with two very different bulk density estimates that may or may not need a coarse fraction correction.

Lacking clearer guidance (and feel free to weigh in in the issues to discuss this) I choose to fall back on what quartiles of gridded soil organic carbon stocks and total soil carbon stocks are generated by which options.


 SOC quantiles and total |`BULK.DENSITY` | `REF.BULK.DENSITY`
---|---------------|------------------
`(1 - GRAVEL)` | [0.16, 5.02, 7.20, 11.04,  118.73] kg-C/m2; 1121 Pg-C | [0.00, 5.04, 7.11, 11.35, 516.70] kg-C/m2; 2461 Pg-C
`1` | [0.20, 5.59,  7.93, 11.80, 118.73] kg-C/m2; 1214 Pg-C | [0.00, 5.62, 7.83, 12.45,  527.55 ] kg-C/m2; 2587 Pg-C


```{r}
#Example: global soil carbon totals in Pg-C below for `BULK.DENSITY` with `(1-GRAVEL)`
quantile(HWSD_stocks$SOC_top.kg_per_m2_coarse + HWSD_stocks$SOC_sub.kg_per_m2_coarse, na.rm=TRUE)
sum(HWSD_stocks$area_km2*HWSD_stocks$n*(HWSD_stocks$SOC_top.kg_per_m2_coarse + HWSD_stocks$SOC_sub.kg_per_m2_coarse)*(1e6/1e12), na.rm=TRUE)
```

Batjes 2016 estimated total soil carbon stocks at 1408 +- 154 Pg-C (mean + sd from n=400 bootstrap) with a related but different data product (WOSIS-30sec), which suggests use of `BULK.DENSITY` without a course fraction correction, counter to the guidance given in the HWSD documentation.
In hindsight, I find this reassuring because I used the uncorrected `BULK.DENSITY` total in my 2013 work (Todd-Brown, etal 2013) and less formally ball parked a similar uncertainty.

So there you go.
If you are going to use the HWSD in research, state your assumptions in the analysis and maybe consider getting in touch with ISRIC for guidance.
Soils are complicated. Good luck.

### Top soil carbon estimates


Often reported are the soil carbon totals for just the top layer (0-30cm)

 SOC (0-30cm)   | `BULK.DENSITY` | `REF.BULK.DENSITY` 
----|---------------|------------------ 
 `(1 - S.GRAVEL)` | 575 Pg-C | 964 Pg-C 
 `1` | 622 Pg-C | 1025 Pg-C 


## Other useful resources

This was very helpful in thinking about blocking rasters
https://strimas.com/post/processing-large-rasters-in-r/

An additional regridded version of HWSD can be found http://daac.ornl.gov/SOILS/guides/HWSD.html (unrelated to this repo) and there is a R package that can be found here https://github.com/dlebauer/rhwsd (also unrelated to this repo).

This work was based off of Rossiter, D. G. (2012). Processing the Harmonized World Soil Database (Version 1.2) in R. Institute of Soil Science, Chinese Academy of Sciences.

The work below was cited in the bulk density discussion:

  - Batjes, N.H. Harmonized soil property values for broad-scale modelling (WISE30sec) with estimates of global soil carbon stocks, Geoderma (269), https://doi.org/10.1016/j.geoderma.2016.01.034. 2016.
  - Saxton, K E, W J Rowls, J S Romberger, and R I Panendick. Estimating generalized soil-water characteristics from texture. Soil Science Society of America Journal. https://pubag.nal.usda.gov/download/35/PDF 1986.

This work was motivated by the soil carbon stock CMIP5 analysis documented here:

  - Todd-Brown, K. E. O., Randerson, J. T., Post, W. M., Hoffman, F. M., Tarnocai, C., Schuur, E. A. G., and Allison, S. D.: Causes of variation in soil carbon simulations from CMIP5 Earth system models and comparison with observations, Biogeosciences, 10, 1717–1736, https://doi.org/10.5194/bg-10-1717-2013, 2013.
  - Todd-Brown, K. E. O., Randerson, J. T., Hopkins, F., Arora, V., Hajima, T., Jones, C., Shevliakova, E., Tjiputra, J., Volodin, E., Wu, T., Zhang, Q., and Allison, S. D.: Changes in soil organic carbon storage predicted by Earth system models during the 21st century, Biogeosciences, 11, 2341–2356, https://doi.org/10.5194/bg-11-2341-2014, 2014.
