# this script file is meant to use for analyzing model output
source('figures/traitPostAux.R')
source('~/Projects/procVisData/dataViz.R')

post <- postGibbsChains(betachains = output$chains$agibbs, 
                        burnin = output$modelList$burnin,
                        traitsToPlot = c('N','P','SLA') ,
                        predictorsToPlot = c('temp', 'moisture','deficit','I(moisture^2)', 'moisture:deficit'), 
                        onlySignificant = F, 
                        normalized = T, 
                        standardizedT = T,
                        sdTraits = sdTraits,
                        excludeIntercept  =F,
                        includeInteractions = F,
                        includeMainEffects = T,
                        exactPredictors = T)

posteriorPlots(post, 'figures/traitFig.PostProc.Climate', statsParam=statsParam)
