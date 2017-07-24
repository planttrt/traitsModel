library(data.table)
source('figures/traitPostAux.R')
source('~/Projects/procVisData/dataViz.R')

subBeta <- as.data.table(output$chains$agibbs[-(1:output$burnin),
                                              c('N_intercept','N_moisture','N_deficit','N_moisture:deficit',
                                                'P_intercept','P_moisture','P_deficit','P_moisture:deficit',
                                                'SLA_intercept','SLA_moisture','SLA_deficit','SLA_moisture:deficit')])

subX <- as.data.table(plotByX[,c("deficit", "moisture")])
subX[,'moisture:deficit'] <- plotByX$moisture*plotByX$deficit

post <- postGibbsChains(betachains = output$chains$agibbs, 
                        burnin = output$burnin,
                        traitsToPlot = c('N','P','SLA') ,
                        predictorsToPlot = c('moisture','deficit','moisture:deficit'), 
                        onlySignificant = F, 
                        normalized = T, 
                        standardizedT = T,
                        sdTraits = sdTraits,
                        excludeIntercept  =F,
                        includeInteractions = F,
                        includeMainEffects = T,
                        exactPredictors = T)
traitBetas <- c("N_moisture", "N_deficit", "N_moisture:deficit")

siteClimateEffect <- function(traitBetas, post){
  betas <- t(post$chains[,traitBetas]  )
  xVec <- seq(-1,1, length.out = 1000)
  dryClimate <- data.frame(moisture = xVec,
                           deficit = -1,
                           moistureXdeficit = -xVec)
  
  wetClimate <- data.frame(moisture = xVec,
                           deficit = +1,
                           moistureXdeficit = xVec)
  
  drySite <- data.frame(deficit = xVec,
                        moisture = -1,
                        moistureXdeficit = -xVec)
  
  wetSite <- data.frame(deficit = xVec,
                        moisture = +1,
                        moistureXdeficit = xVec)
  
  SiteClimate <- list(xVec = xVec,
                      dryClimate = as.matrix(dryClimate)%*%betas,
                      wetClimate = as.matrix(wetClimate)%*%betas,
                      drySite = as.matrix(drySite)%*%betas,
                      wetSite = as.matrix(wetSite)%*%betas)
  
}

N.SiteClimate <- siteClimateEffect(c("N_moisture", "N_deficit", "N_moisture:deficit"), post)
P.SiteClimate <- siteClimateEffect(c("P_moisture", "P_deficit", "P_moisture:deficit"), post)
M.SiteClimate <- siteClimateEffect(c("SLA_moisture", "SLA_deficit", "SLA_moisture:deficit"), post)
xVec <- N.SiteClimate$xVec
probs <- c(.5, .005, .995)
plot(NA, xlim=c(-1,1), ylim=c(-1,1))

tmp <- apply(N.SiteClimate$dryClimate , 1, quantile, probs=probs)
lines(xVec, tmp[1,])
polygon(c(xVec, rev(xVec)), c(tmp[2,], rev(tmp[3,])))

tmp <- apply(N.SiteClimate$wetClimate , 1, quantile, probs=probs)
lines(xVec, tmp[1,])
polygon(c(xVec, rev(xVec)), c(tmp[2,], rev(tmp[3,])))

plot(NA, xlim=c(-1,1), ylim=c(-1,1))
tmp <- apply(P.SiteClimate$dryClimate , 1, quantile, probs=probs)
lines(xVec, tmp[1,])
polygon(c(xVec, rev(xVec)), c(tmp[2,], rev(tmp[3,])))

tmp <- apply(P.SiteClimate$wetClimate , 1, quantile, probs=probs)
lines(xVec, tmp[1,])
polygon(c(xVec, rev(xVec)), c(tmp[2,], rev(tmp[3,])))

plot(NA, xlim=c(-1,1), ylim=c(-1,1))
tmp <- apply(M.SiteClimate$dryClimate , 1, quantile, probs=probs)
lines(xVec, tmp[1,])
polygon(c(xVec, rev(xVec)), c(tmp[2,], rev(tmp[3,])))

tmp <- apply(M.SiteClimate$wetClimate , 1, quantile, probs=probs)
lines(xVec, tmp[1,])
polygon(c(xVec, rev(xVec)), c(tmp[2,], rev(tmp[3,])))

