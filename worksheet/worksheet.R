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

acreworkshop:::compare.to.truth(interpolated.df)

survey.data <- conduct.survey(skip.wait = TRUE)

library(acre)

data <- read.acre(captures = survey.data$captures, survey.data$traps,
                  control.mask = list(buffer = 3000),
                  loc.cov = cov.df, dist.cov = list(villages = villages.df))

plot(data, type = "capt")
plot(data, type = "covariates")

fit1 <- fit.acre(data, detfn = "hhn")
fit2 <- fit.acre(data, detfn = "hhn", model = list(D = ~ s(canopy.height, k = 4)))
fit3 <- fit.acre(data, detfn = "hhn", model = list(D = ~ s(canopy.height, k = 4) + s(villages, k = 3)))

summary(fit1)
summary(fit2)
summary(fit3)

plot(fit1, type = "Dsurf", new.data = ppws)
plot(fit2, type = "Dsurf", new.data = ppws)
plot(fit3, type = "Dsurf", new.data = ppws)

plot(fit1, type = "detfn")
plot(fit2, type = "detfn", add = TRUE, col = "red")

AIC(fit1)
AIC(fit2)
AIC(fit3)
