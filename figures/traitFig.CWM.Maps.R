library(raster)

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitFunctions.R')
source('figures/traitColorSet.R')

cwmNames <- c('Leaf N (mg/g)', 'Leaf P (mg/g)', 'SLA (cm²/g)', 'Wood density (kg/m³)')
png('figures/traitFig.CWM.Maps.png', units='in',res=300, height  = 10, width=11)
par(mfrow=c(2,2), oma=c(0.0,0,1,0), mar=c(2,3,1,1))
for(j in 1:4){
  
  ssj <- traitData$plotByCWM[,c(4:6,2)[j]]
  
  par(xaxt='n', yaxt='n')
  mapColorData(x = plotByX$plotLon, 
               y = plotByX$plotLat, 
               data = ssj,
               valRange = quantile(ssj, probs = seq(0.05,.95, by = .1), na.rm = T), 
               xlim = range(plotByX$plotLon), 
               ylim = range(plotByX$plotLat),
               statesborder = F,
               cex.all = 2, 
               colList = rev(colList.SurfAndTurf),
               symSize=1 )
  plot(mapRegion, add = T)
  
  mtext(cwmNames[j],  line = -2, cex=1.89, adj = .95, font=2)
  #mapOutlines(glacialLine, mapRegion, lwd2 = 8)
  
  par(xaxt='s', yaxt='s')
  
  axis(1, cex.axis=1.7)
  axis(2, cex.axis=1.7)
}
dev.off()

