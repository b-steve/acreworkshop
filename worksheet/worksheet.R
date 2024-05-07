library(acreworkshop)

cov.df <- measure.covariates(skip.wait = TRUE)

plotcov(cov.df, "elevation")
plotcov(cov.df, "canopy.height")
plotcov(cov.df, "forest.type")


interpolated.df <- interpolate.covs(cov.df)

plotcov(cov.df, "elevation")
plotcov(interpolated.df, "elevation")
plotcov(cov.df, "canopy.height")
plotcov(interpolated.df, "canopy.height")
plotcov(cov.df, "forest.type")
plotcov(interpolated.df, "forest.type")

acreworkshop:::compare.cov.to.truth(interpolated.df)

survey.data <- conduct.survey(skip.wait = TRUE)

library(acre)

data <- read.acre(captures = survey.data$captures, survey.data$traps,
                  control.mask = list(buffer = 3000),
                  loc.cov = cov.df, dist.cov = list(villages = villages.df))

plot(data, type = "capt")
plot(data, type = "covariates")

fit1 <- fit.acre(data, detfn = "hhn")
fit2 <- fit.acre(data, detfn = "hhn", model = list(D = ~ canopy.height))
fit3 <- fit.acre(data, detfn = "hhn", model = list(D = ~ villages))
fit4 <- fit.acre(data, detfn = "hhn", model = list(D = ~ forest.type))
fit5 <- fit.acre(data, detfn = "hhn", model = list(D = ~ canopy.height + forest.type
                                                   + villages))
fit6 <- fit.acre(data, detfn = "hhn", model = list(D = ~ canopy.height + forest.type
                                                   + s(villages, k = 3)))

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)

AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(fit5)
AIC(fit6)

plot(fit1, type = "detfn")

plot(fit1, type = "Dsurf", new.data = ppws)
plot(fit2, type = "Dsurf", new.data = ppws)
plot(fit3, type = "Dsurf", new.data = ppws)
plot(fit4, type = "Dsurf", new.data = ppws)
plot(fit5, type = "Dsurf", new.data = ppws)
plot(fit6, type = "Dsurf", new.data = ppws)

acreworkshop:::compare.D.to.truth(fit2)
