library(data.table)
source('~/Projects/procVisData/bayesianFunctions.R')

getSensitivity <- function(param, output, traitNames, traitData, normalized = T) {
  
  traitSd <- apply(traitData$plotByCWM, 2, sd)
  
  paramSensList <- list()
  
  wFactors <- which(apply(output$inputs$x, 2, function(x)all(x%in%c(0,1))))
  sdCols <- apply(output$inputs$x, 2, sd)
  sdCols[wFactors] <- 1
  sdParam <- sdCols[match(param, names(sdCols))]
  if(!normalized) sdParam <- 1
  
  for(j in 4:6){
    postParam <- postGibbsChains(betachains = output$chains$agibbs, 
                                 burnin = output$modelList$burnin,
                                 traitsToPlot = traitNames[j] ,
                                 predictorsToPlot = param, 
                                 onlySignificant = F, 
                                 normalized = F, 
                                 includeInteractions = T, 
                                 includeMainEffects = T)
    
    tmp <- as.data.table(postParam$nameMatrix)[, .(interaction,pred1, pred2)]
    tmp[,inter:=pred1]
    tmp[,fact:=1]
    tmp[inter==param[1],inter:=pred2]
    tmp[interaction==F, inter:=param[1]]   
    
    tmp[grep('\\^2',tmp$pred1),interaction:=T]
    tmp[grep('\\^2',tmp$pred1),fact:=2]
    
    sensVectors <- output$inputs$x[,tmp$inter]*tmp$fact
    sensVectors[,!tmp$interaction] <- 1
    # sensVectors <- cbind(1, output$x[,interactionsList])
    
    paramSens <- sensVectors%*%t(postParam$chains)/traitSd[j]*sdParam
    
    paramSens.Summ <- as.data.frame(t(apply(paramSens, 1, quantile, probs=c(.5,.025,.975))))
    paramSens.Summ$mean <- rowMeans(paramSens)
    paramSens.Summ$signif <- (sign(paramSens.Summ[,"2.5%"]*paramSens.Summ[,"97.5%"])>0)
    paramSens.Summ$sign <- paramSens.Summ$signif*sign(paramSens.Summ[,"2.5%"])
    
    paramSensList[[length(paramSensList)+1]] <- paramSens.Summ
    
  }
  names(paramSensList) <- traitNames[4:6]
  
  paramSensList
}

getCWT.FilterOnLeaf <- function(output, speciesByTraits, plotByW){
  species <- rownames(speciesByTraits)
  sp <- 1:length(species)
  speciesByTraitsDT <- as.data.table(cbind(sp,species, speciesByTraits))
  
  evergreenLeaf <- speciesByTraitsDT[leaf=='NLEver', .(sp, species)]
  deciduousLeaf <- speciesByTraitsDT[leaf=='Deciduous', .(sp, species)]
  
  CWT.evergreen <- (plotByW[,evergreenLeaf$species])%*%as.matrix(
    speciesByTraits[evergreenLeaf$species,c("N","P","SLA")])
  
  CWT.deciduous <- (plotByW[,deciduousLeaf$species])%*%as.matrix(
    speciesByTraits[deciduousLeaf$species,c("N","P","SLA")])
  
  betaTraitMu.Ever <- output$parameters$betaMu[,evergreenLeaf$sp]%*%
    as.matrix(traitData$specByTrait[evergreenLeaf$sp,])
  
  betaTraitMu.Decid <- output$parameters$betaMu[,deciduousLeaf$sp]%*%
    as.matrix(traitData$specByTrait[deciduousLeaf$sp,])
  
  
  
  spCols <- matrix(unlist(strsplit(colnames(output$chains$bgibbs),split = '_')), ncol=2, byrow=T)[,1]
  
  decidCols <- spCols%in%deciduousLeaf$species
  everCols <- spCols%in%evergreenLeaf$species
  
  dim(output$chains$bgibbs[,decidCols])
  colnames(output$chains$bgibbs[,everCols])
  
  list(evergreen = CWT.evergreen, 
       deciduous = CWT.deciduous,
       betaTraitMu.Ever=betaTraitMu.Ever[,c("N","P","SLA")],
       betaTraitMu.Decid=betaTraitMu.Decid[,c("N","P","SLA")]
  )
}

getCWT.CondOnLeaf <- function(output){
  tMu <- output$prediction$tMu
  deciTrait <- everTrait <- tMu
  
  deciTrait[,10:12] <- everTrait[,10:12] <- 0
  everTrait[,'leafneedleevergreen'] <- 1
  everTrait[,'leafother'] <- 1
  deciTrait[,'leafbroaddeciduous'] <- 1
  
  oMu <- output$parameters$sigmaTraitMu
  
  condDecid <- conditionalMVNVec(deciTrait[,-10],tMu[,-10],oMu[-10,-10],cdex=c(1:9, 13:14))
  condEver <- conditionalMVNVec(everTrait[,-10],tMu[,-10],oMu[-10,-10],cdex=c(1:9, 13:14))
  
  # condDecid <- conditionalMVNVec(deciTrait,tMu,oMu,cdex=c(1:6))
  # condEver <- conditionalMVNVec(everTrait,tMu,oMu,cdex=c(1:6))
  
  # sgibbs <- out1$chains$sgibbs
  # sMu <- matrix( colMeans(sgibbs),S,S )
  # colnames(condDecid$mu) <- colnames(sMu)[1:6]
  # colnames(condEver$mu) <- colnames(sMu)[1:6]
  
  list(condDecid=condDecid$mu, condEver=condEver$mu)
}

getCWT.Mass.Area <- function(output, speciesByTraits, plotByW){
  CWT.Mass <- (plotByW)%*%as.matrix(speciesByTraits[,c("N","P","SLA")])
  speciesByTraits.Area <- speciesByTraits[,c("N","P")]/speciesByTraits[,"SLA"]
  speciesByTraits.Area['other',] <- 0
  CWT.Area <- (plotByW)%*%as.matrix(speciesByTraits.Area[,c("N","P")])
  
  CWT.pred <- output$prediction$tMu[,1:6]
  CWT.cond <- getCWT.CondOnLeaf(output)
  
  list(perMass = CWT.Mass, perArea = CWT.Area, pred = CWT.pred, cond = CWT.cond)
}

mapOutlines <- function(glacialLine, mapRegion, lwd1 =2, lwd2=6, col1='grey',col2='white'){
  lines(glacialLine[-nrow(glacialLine),1], glacialLine[-nrow(glacialLine),2], lwd=lwd2, col=col2)
  lines(glacialLine[-nrow(glacialLine),1], glacialLine[-nrow(glacialLine),2], lwd=lwd1, col=col1)
  plot(mapRegion, add=T)
}