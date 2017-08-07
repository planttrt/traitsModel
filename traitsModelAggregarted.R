library(gjam)
library(repmis)
# d <- "https://github.com/jimclarkatduke/gjam/blob/master/forestTraits.RData?raw=True"
# source_data(d)

xdata <- forestTraits$xdata                          # n X Q
types <- forestTraits$traitTypes                     # 12 trait types 
sbyt  <- forestTraits$specByTrait                    # S X 12
pbys  <- gjamReZero(forestTraits$treesDeZero)        # n X S
pbys  <- gjamTrimY(pbys,5)$y                         # at least 5 plots
sbyt  <- sbyt[match(colnames(pbys),rownames(sbyt)),] # trait matrix matches ydata
identical(rownames(sbyt),colnames(pbys))

table(sbyt$leaf)      # four levels
table(sbyt$xylem)     # diffuse/tracheid vs ring-porous
table(sbyt$repro)     # two levels

tmp         <- gjamSpec2Trait(pbys, sbyt, types)

tl  <- list(plotByTrait = tmp$plotByCWM, 
            traitTypes = tmp$traitTypes,
            specByTrait = tmp$specByTrait)

rl  <- list(r = 8, N = 25)

ml  <- list(ng = 1000, 
            burnin = 200, 
            typeNames = 'CC', 
            holdoutN = 20,
            traitList = tl, 
            reductList = rl)

out <- gjam(formula = 
              ~temp + moisture + deficit + soil +u1 + u2 + u3 + 
              # I(moisture^2) +
              moisture*deficit + 
              moisture*soil +
              temp*soil 
            # +
            #   deficit*soil
            ,
            # formula = ~ temp + stdage + deficit*soil, 
            xdata = xdata, 
            ydata = pbys, 
            modelList = ml)


signif(out$parameters$betaTraitMu, 2)
signif(out$parameters$betaTraitSe, 2)
signif(cov2cor(out$parameters$sigmaTraitMu), 2)

save.image(paste('agg-output',
                 ml$ng/1000, ml$burnin/1000,
                 substring(make.names(Sys.time()),2),
                 '.RData', sep = '-'))
