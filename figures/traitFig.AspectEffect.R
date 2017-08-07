source('slopeAspectFunctions.r')
source('figures/traitPostAux.R')

png('figures/traitFig.AspectEffect.png', width = 6, height = 6, res =150, units='in')
par(mfrow=c(2,1), bty='n', xaxt='s', yaxt='s', mar=c(4,4,1,0), oma=c(.5,.5,.5,.5))
tBMu <- output$parameters$betaTraitMu[c('u1','u2','u3'),]/matrix(rep(apply(traitList$plotByTrait,2, sd), 3), nrow=3, byrow = T)
plotAspectEffect(tBMu[,4:6])
#mtext(text = 'Leaf traits', side = 3, cex = 1.2, line = -2, at = -3, adj = 0)

plotAspectEffect(tBMu[,7:9])
#mtext(text = 'Leaf habitats', side = 3, cex = 1.2, line = -2, at = -3, adj = 0)
dev.off()
