
whichPolygon <- function(coords, shape){
  shape@data<- cbind(1:nrow(shape@data), shape@data)
  crs.shape<- projection(shape)
  coords2<- SpatialPoints(coords, proj4string = CRS(crs.shape))
  pol.n<- over(coords2, shape,fn=NULL)
  matrix(pol.n[,1],ncol=1)
}

cl <- read.csv('data/pre/climate.csv')
so <- read.csv('data/pre/soilAll.csv', header = F)
ll <- read.csv('data/pre/lonLatAll.csv')

traitMeans <- as.matrix(c(-1.2073003,   0.6065312, 24.0440945,   1.8845038,   0.1534890, 141.4800000))
traitSds <- as.matrix(c( 1.28865714,  0.12721717,  7.62135557,  0.51061987,  0.05969728, 68.83460771))
rownames(traitMeans) <- rownames(traitSds) <- c('gmPerSeed','gmPerCm', 'maxHt', 'leafN', 'leafP', 'SLA')

bm <- read.csv('data/pre/traitWt.csv')
tw <- bm/rowSums(bm)
  
ww <- read.csv('data/pre/wwAll.csv')$w


plotByX <- cbind(ll, cl)[ww,]
plotByX$soil <- so[ww,]

plotByW <- tw[ww,]
plotByY <- bm[ww,]

dem <- read.csv('data/pre/demAll.csv')

elev <- dem$elevation
plotByX <- cbind(plotByX, elev)

load('data/forestTraits.Rdata')
speciesByTraits <- forestTraits$specByTrait[c(1,6,2,3:5,10, 7:9,11:12)]
traitTypes <- forestTraits$traitTypes[c(1,6,2,3:5,10, 7:9,11:12)]
colnames(speciesByTraits)[1:6] <- c('SM','WD','MH','N','P','SLA')
colnames(speciesByTraits)

# speciesByTraits <- as.data.frame(t(traitBySpecies))


# leaf <- colnames(speciesByTraits)[7:9][rowSums(speciesByTraits[,7:9]*matrix(1:3, nrow = 65, ncol = 3, byrow = T))]
# speciesByTraits <- cbind(speciesByTraits[,1:6],leaf )

# traitMuAll <- read.csv('data/pre/traitMuAll.csv')
# traitSdAll <- read.csv('data/pre/traitSdAll.csv')
# 

#denormalizing the speciesByTraits matrix
for(i in 1:6) 
  speciesByTraits[,i] <- speciesByTraits[,i]*traitSds[i] + traitMeans[i]  

speciesByTraits$N <- speciesByTraits$N*10 #unit conversion
speciesByTraits$P <- speciesByTraits$P*10 #unit conversion


source('/Volumes/5TB-STORE/MacBook/Projects/procVisData/geoSpatial.R')
library(raster)
library(data.table)
library(tools)

### readin slope aspect
allPlotDEM <- read.csv('data/pre/allPlotDEMdata.csv')

allPlotDEM[which(is.na(allPlotDEM$slope)), c("slope","aspect")] <- 0

plotNames <- matrix(unlist(strsplit(rownames(plotByX), split = '_ereg_')), ncol = 2, byrow = T)[,1]
allPlotDEM$X <- as.character(allPlotDEM$X)
w <-match(plotNames, allPlotDEM$X)
plotByX <- cbind(plotByX, allPlotDEM[w,4:6])
plotByX <- as.data.table(plotByX)

physioShape <- shapefile('data/maps/physioProvinceLatLon/physioProvinceLatLon.shp')
wPoly <- whichPolygon(plotByX[,.(plotLon, plotLat)], physioShape)
physioRegion <- as.factor(toTitleCase(tolower(physioShape@data$PROVINCE[wPoly])))
table(physioRegion)

ecoShape <- shapefile('data/maps/ecoregions/eco_us_latlon.shp')
wEco <- whichPolygon(plotByX[,.(plotLon, plotLat)], ecoShape)
ecoRegion <- as.factor(toTitleCase(tolower(ecoShape@data$PROVINCE[wEco])))
table(ecoRegion)

plotByX$ecoRegion <- ecoRegion
plotByX$physioRegion <- physioRegion
exposure <- slopeAspectToExposure(plotByX$slope, plotByX$aspect, degree = T)

plotByX <- cbind(plotByX, exposure)

# traitMuAll <- read.csv('data/pre/traitMuAll.csv')
# traitSdAll <- read.csv('data/pre/traitSdAll.csv')
# plotNames2 <- matrix(unlist(strsplit(rownames(traitMuAll), split = '_ereg_')), ncol = 2, byrow = T)[,1]
# 
# w <-match(plotNames, plotNames2)
# traitMuAll <- traitMuAll[w,]
# traitSdAll <- traitSdAll[w,]


colnames(plotByX)[which(colnames(plotByX)=='therm')] <- 'surplus'



spp <- colnames(plotByW)
spp <- spp[spp %in%rownames(speciesByTraits)]

write.table(plotByX, 'data/post/plotByX.csv', sep = ',')
write.table(plotByW[,spp], 'data/post/plotByW.csv', sep = ',')
write.table(plotByY[,spp], 'data/post/plotByY.csv', sep = ',')
write.table(speciesByTraits[spp,], 'data/post/speciesByTraits.csv', sep = ',')
write.table(traitTypes, 'data/post/traitTypes.csv', sep = ',')
# write.table(traitMuAll, 'data/post/traitMuAll.csv', sep = ',')
# write.table(traitSdAll, 'data/post/traitSdAll.csv', sep = ',')


