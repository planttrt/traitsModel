library(raster)
library(data.table)


rasterFolder <- '~/../Downloads/SoilGrids/'

taxUSDA <- raster(paste0(rasterFolder,'TAXOUSDA_250m_ll.tif'))
taxNWRB <- raster(paste0(rasterFolder,'TAXNWRB_250m_ll.tif'))
sand <- raster(paste0(rasterFolder,'SNDPPT_M_sl1_250m_ll.tif'))
silt <- raster(paste0(rasterFolder,'SLTPPT_M_sl1_250m_ll.tif'))
clay <- raster(paste0(rasterFolder,'CLYPPT_M_sl1_250m_ll.tif'))
ph <- raster(paste0(rasterFolder,'PHIHOX_M_sl1_250m_ll.tif'))
orgCct <- raster(paste0(rasterFolder,'ORCDRC_M_sl1_250m_ll.tif'))
orgCst<- raster(paste0(rasterFolder,'OCSTHA_M_sd1_250m_ll.tif'))

spCoords <- read.csv('data/pre/lonLatAll.csv')
ww <- read.csv('data/pre/wwAll.csv')$w
so <- read.csv('data/pre/soilAll.csv', header = F)

exTaxUSDA <- extract(taxUSDA, spCoords)
exSand <- extract(sand, spCoords)
exSilt <- extract(silt, spCoords)
exClay <- extract(clay, spCoords)
exPh <- extract(ph, spCoords)
exOrgCct <- extract(orgCct, spCoords)
exOrgCst <- extract(orgCst, spCoords)

soilGrids <- data.table(tax= exTaxUSDA, 
                        sand= exSand,
                        silt=exSilt,
                        clay=exClay,
                        ph= exPh,
                        orgCst=exOrgCst,
                        orgCct= exOrgCct)

# rownames(soilGrids) <- rownames(spCoords)
# soilGrids[,ID:= tax]

 
# taxCode <- read.csv('data/post/soilTaxUSDA.csv')
# taxCode <- as.data.table(taxCode)
# 
# soilGrids.ext <- merge(soilGrids, taxCode, by='ID', all.x=T)
# soilGrids.ext
# soilGrids.ext$ID <- NULL
# soilGrids.ext$Color <- NULL


write.csv(soilGrids, 'data/post/soilGrids.csv')

soilGrids.ext <- soilGrids[ww,]
soilGrids.ext$tax <- NULL
write.csv(soilGrids.ext, 'data/post/soilGrids.ext.csv', row.names = F)
