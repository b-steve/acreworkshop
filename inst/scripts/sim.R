library(geoR)
load("ppws-mask.RData")
source("workshop-functions.R")

set.seed(1234)
canopy.height.ppws <- grf(nrow(ppws), grid = ppws, cov.pars = c(1, 100000))$data
elevation.ppws <- grf(nrow(ppws), grid = ppws, cov.pars = c(1, 50000))$data
forest.type.ppws <- grf(nrow(ppws), grid = ppws, cov.pars = c(1, 10000))$data
forest.type.ppws <- ifelse(forest.type.ppws > quantile(forest.type.ppws, 0.3),
                           "evergreen", "deciduous")
villages.ppws <- cbind(x = c(718469.221610262, 706335.190158707,
                             688836.639539097, 682067.127255598),
                       y = c(1395339.85660492, 1420563.3550554,
                             1409352.91129963, 1389734.63472704))
village.dist.ppws <- apply(calc.dists(villages.ppws, ppws), 1, min)

true.df <- data.frame(canopy.height = canopy.height.ppws,
                      elevation = elevation.ppws,
                      forest.type = forest.type.ppws,
                      village.dist = village.dist.ppws)

D.calc <- function(df){
    exp(-6.5 + 0.2*df$canopy.height - 0.6*(df$forest.type != "evergreen") +
        2*(1 - exp(-0.0005*(df$village.dist))))
}

D.ppws <- D.calc(true.df)
true.df$D <- D.ppws

s.ppws <- sim.pop(true.df, D.calc)

save(canopy.height.ppws, elevation.ppws, forest.type.ppws, villages.ppws, s.ppws, file = "ppws.RData")
