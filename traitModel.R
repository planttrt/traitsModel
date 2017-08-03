library(gjam)
library(data.table)

set.seed(2016)
source('traitPreModel.R')

traitData  <- gjamSpec2Trait(pbys = plotByW, 
                             sbyt = speciesByTraits, 
                             tTypes = traitTypes)

traitList <- list(plotByTrait = traitData$plotByCWM, 
                  traitTypes = traitData$traitTypes, 
                  specByTrait = traitData$specByTrait)

reductList <- list(r = 3, N = 20)

modelList <- list(ng=4000, 
                  burnin=2000,
                  typeNames = 'FC', 
                  holdoutN = 20,
                  reductList = reductList,
                  traitList = traitList)

set.seed(2016)


output  <- gjam(~ temp + moisture + deficit + soil +u1 + u2 + u3 + 
                       I(moisture^2) +
                       moisture*deficit + 
                       moisture*soil + 
                       temp*soil + 
                       deficit*soil,

                     xdata = plotByX,
                     ydata = plotByW,
                     modelList = modelList)

save.image(paste('output',
                 modelList$ng/1000, modelList$burnin/1000,
                 substring(make.names(Sys.time()),2),
                 '.RData', sep = '-'))

gjamPlot(output, plotPars = list(GRIDPLOTS = T, SMALLPLOTS = F))


muTable <- output$parameterTables$betaTraitMu[,4:6]*sdCols/matrix(sdTraits[4:6],nrow = ncol(output$x), 3, byrow = T)
seTable <- output$parameterTables$betaTraitSe[,4:6]*sdCols/matrix(sdTraits[4:6],nrow = ncol(output$x), 3, byrow = T)

write.csv(muTable, file = 'betaTraitMu.csv')
write.csv(seTable, file = 'betaTraitSe.csv')

