source('~/Projects/procVisData/geoSpatial.R')

require(data.table)

# betachains <- output$chains$agibbs
# burnin <- floor(nrow(betachains)/2)
# includeInteractions <- F
# traitsToPlot <- c('N','P','SLA') 
# predictorsToPlot <- NULL
# onlySignificant <- T

plotSensitivity <- function(pngName, txtTitle, paramSensList, traitNames, 
                            colList=rev(colList.SurfAndTurf),
                            subFigIndex = c('(a)','(b)','(c)') ){
  
  png(pngName, units='in',res=300, height  = 5, width=13)
  par(mfrow=c(1,3), oma=c(1,0,7,0), mar=c(2,3,1,1))
  #  par(mfrow=c(1,3), oma=c(0.0,0,5,0), mar=c(3,3,3,0))
  for(j in 4:6){
    
    ssj <- paramSensList[[j-3]]$mean
    
    ww <- paramSensList[[j-3]]$sign!=0
    
    par(xaxt='n', yaxt='n')
    valRange <- quantile(ssj[ww], probs = seq(0.05,.95, by = .1), na.rm = T)
    if(prod(range(valRange))<0){
      valRange1 <- c(-valRange[valRange>0],0,valRange[valRange>0])
      valRange2 <- c(valRange[valRange<0],0,-rev(valRange[valRange<0]))
      valRange <- switch((length(valRange1)>length(valRange2))+1, valRange2, valRange1)
    }
    mapColorData(x = plotByX$plotLon[ww], 
                 y = plotByX$plotLat[ww],
                 data = ssj[ww],
                 statesborder = F,
                 xlim = range(plotByX$plotLon), 
                 ylim = range(plotByX$plotLat), 
                 valRange = valRange, 
                 cex.all = 2, colList = colList, symSize=1.2, alpha = .5)
    
    mtext(text = traitNames[j], side = 3, line = .7, cex=2)
    mtext(text =  subFigIndex[j-3], side = 3, line = 1.5,at = -100, cex=2)
    plot(mapRegion, add=T)
    par(xaxt='s', yaxt='s')
    axis(1, cex.axis=1.7)
    axis(2, cex.axis=1.7)
  }
  mtext(outer = T, side = 3, text = txtTitle, cex = 1.6, line = 4)
  dev.off()
  
}
plotSensitivitySign <- function(pngName, txtTitle, paramSensList, traitNames, 
                                colList=c( "#5e3c99", "#f7f7f7", "#e66101"),
                                subFigIndex = c('(a)','(b)','(c)') ){
  
  png(pngName, units='in',res=300, height  = 5, width=15)
  par(mfrow=c(1,3), oma=c(0.0,0,2,0))
  for(j in 4:6){
    
    ssj <- paramSensList[[j-3]]$sign
    ww <- ssj!=0
    par(xaxt='n', yaxt='n')
    mapColorData(x = plotByX$plotLon[ww], y = plotByX$plotLat[ww], data = ssj[ww],
                 #valRange = quantile(ssj, probs = seq(0.05,.95, by = .1), na.rm = T), 
                 valRange = c(-1,0,1),
                 cex.all = 2, colList = colList, symSize=.7)
    
    mtext(text = traitNames[j], side = 3, line = .7, cex=2)
    mtext(text =  subFigIndex[j-3], side = 3, line = 1.5,at = -100, cex=2)
    
    par(xaxt='s', yaxt='s')
    axis(1, cex.axis=1.7)
    axis(2, cex.axis=1.7)
  }
  mtext(outer = T, side = 3, text = txtTitle, cex = 1.6)
  dev.off()
  
}
postGibbsChains <- function(betachains,
                            burnin=1, 
                            traitsToPlot=NULL,
                            predictorsToPlot=NULL, 
                            normalized = T,
                            standardizedT = F, 
                            includeInteractions=T,
                            includeMainEffects =T,
                            excludeIntercept = T,
                            onlySignificant=T, 
                            sdTraits=NULL, 
                            exactPredictors=F){
  
  wFactors <- which(apply(output$inputs$x, 2, function(x)all(x%in%c(0,1))))
  sdCols <- apply(output$inputs$x, 2, sd)
  sdCols[wFactors] <- 1
  
  
  chains <- betachains[-(1:burnin),]
  
  ng <- nrow(chains)
  nbeta <- ncol(chains)
  fullMatrix <- matrix(unlist(
    strsplit(colnames(chains), split = '_')), 
    ncol=2, byrow = T)
  colnames(fullMatrix) <- c('trait', 'predictor')
  fullMatrix <- as.data.table(fullMatrix)
  id <- 1:nbeta
  fullMatrix <- cbind(id, fullMatrix)
  fullMatrix$interaction <- grepl(':',fullMatrix$predictor)
  fullMatrix
  fullMatrix[, pred:=strsplit(predictor,':')]
  tmp <- function(x){
    if(length(x)!=1) {
      return(unlist(x))
    }else{
      return(c(unlist(x),NA))
    }
  }
  fullMatrix$pred1 <- t(sapply(fullMatrix$pred, tmp))[,1]
  fullMatrix$pred2 <- t(sapply(fullMatrix$pred, tmp))[,2]
  
  
  sdCols <- sdCols[match(fullMatrix$predictor, names(sdCols))]
  sdTraits <- sdTraits[match(fullMatrix$trait, names(sdTraits))]
  
  if(normalized) chains <- t(t(chains)*sdCols)
  if(standardizedT) chains <- t(t(chains)/sdTraits)
  
  summChains <- t(apply(chains, 2, quantile, probs=c(.5,.025,.975)))
  colnames(summChains) <- c('median','low','high')
  fullMatrix <- cbind(fullMatrix, summChains)
  fullMatrix[, signifcant:=sign(high*low)==1]
  fullMatrix
  if(is.null(traitsToPlot)) traitsToPlot <- unique(fullMatrix$trait)
  if(is.null(predictorsToPlot)) predictorsToPlot <- unique(fullMatrix$predictor)
  
  predictorFilter = 1:nrow(fullMatrix)%in%unique(unlist(apply(as.matrix(predictorsToPlot), 1, grep, fullMatrix$predictor)))
  interactionFilter<- c(includeInteractions, !includeMainEffects)
  if(!(includeInteractions|includeMainEffects)) interactionFilter <- c()
  
  if(!exactPredictors){
    nameMatrix <- fullMatrix[
      trait%in%traitsToPlot&
        (predictor%in%predictorsToPlot|
           pred1%in%predictorsToPlot|
           pred2%in%predictorsToPlot)&
        #predictorFilter&
        (signifcant|!onlySignificant)&
        interaction%in%interactionFilter&
        ((predictor!='intercept')|!excludeIntercept)
      , ]
  }else{
    nameMatrix <- fullMatrix[
      trait%in%traitsToPlot&
        predictor%in%predictorsToPlot&
        (signifcant|!onlySignificant)&
        ((predictor!='intercept')|!excludeIntercept)
      , ]
  }
  
  list(    chains = chains[, nameMatrix$id],
           nameMatrix = nameMatrix,
           fullchain =chains,
           fullMatrix = fullMatrix
  )
}



mapMultiSpecies <- function(group, nameFile, txtTitle, plotByW, plotByX){
  png(nameFile, units='in',res=300, height  = 10, width=10)
  
  layout(matrix(c(1,1,2,1,1,3,4,5,6), 3,3, byrow = T))
  par(oma=c(1,1,2,0), mar=c(2,2,2,2))
  
  bmTot <- rowSums(plotByW[,group])
  
  par(xaxt='n', yaxt='n')
  w <- rowSums(plotByW[,group])>=.05
  mapColorData(plotByX$plotLon[w], plotByX$plotLat[w], bmTot[w],
               symSize = 1.5, colList = c('#999999','black'), 
               legendShow = T,valRange = c(.05,.95),cex.all = 2,
               xlim = range(plotByX$plotLon), statesborder = F,
               ylim = range(plotByX$plotLat), 
               alpha = .7)
  
  mtext(text = txtTitle, font=2,side = 3, cex=2, line = 1)
  plot(mapRegion, add = T)
  axis(1, xaxt='s', cex.axis=2)
  axis(2, yaxt='s', cex.axis=2)
  
  for(sp in group){
    abund <- plotByY[,sp]
    w <- abund>0
    mapColorData(plotByX$plotLon[w], plotByX$plotLat[w], 
                 abund[w], symSize = .9, cex.all = 2,
                 valRange = quantile(abund[w], probs=seq(0.025,.975, length.out = 10)),
                 colList = colList.brownGreen, 
                 xlim = range(plotByX$plotLon), 
                 ylim = range(plotByX$plotLat),
                 legendShow = F, 
                 statesborder = F,
                 alpha = .5)
    mtext(text = sp, side = 3, cex=1.5)
    plot(mapRegion, add = T)
  }
  
  dev.off()
}

posteriorPlots <- function(post, pngName, statsParam=c(.025,.25,.50,.75,.975)){
  
  png(paste0(pngName, '-1', '.png'), width = 4, height = 6, res =150, units='in')
  par(mfrow=c(3,1), bty='n', xaxt='s', yaxt='n', mar=c(1,1,1,1), oma=c(1,1,1,1))
  for(t in c('N','P','SLA')){
    chains <- post$chains[,which(post$nameMatrix[,trait]==t)]
    plotGibbsBoxplots(chains, statsParam= statsParam, textAdj = 0,cex.label = 1.8,
                      labels = post$nameMatrix$predictor, sigPlot = F, sort = F)
    # plotGibbsChains(chains, labels = post$nameMatrix$predictor)
    mtext(text = t, side = 2, line = 0, cex=2, font=2)
  }
  dev.off()
  
  # png(paste0(pngName, '-2', '.png'), width = 4, height = 6, res =150, units='in')
  # par(mfrow=c(3,1), bty='n', xaxt='s', yaxt='n', mar=c(1,1,1,1), oma=c(1,1,1,1))
  # for(t in c('N','P','SLA')){
  #   chains <- post$chains[,which(post$nameMatrix[,trait]==t)]
  #   plotGibbsDensity(chains, labels = post$nameMatrix$predictor, txtAdj = 0, title.text = '', xlab = '', ylab = '')
  #   mtext(text = t, side = 2, line = 0, cex=2, font=2)
  # }
  # dev.off()
  
}

