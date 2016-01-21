set.seed(2807)
library("flexmix")
data("NregFix", package = "flexmix")
Model <- FLXMRglm(~ x2 + x1)
fittedModel <- stepFlexmix(y ~ 1, model = Model, nrep = 3, k = 3,
                              data = NregFix, concomitant = FLXPmultinom(~ w))
fittedModel <- relabel(fittedModel, "model", "x1")
summary(refit(fittedModel))
Model2 <- FLXMRglmfix(fixed = ~ x2, nested = list(k = c(1, 2),
                                                  formula = c(~ 0, ~ x1)), varFix = TRUE)
fittedModel2 <- flexmix(y ~ 1, model = Model2,
                          cluster = posterior(fittedModel), data = NregFix,
                        concomitant = FLXPmultinom(~ w))
BIC(fittedModel)
BIC(fittedModel2)
summary(refit(fittedModel2))
