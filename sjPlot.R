library(sjPlot)
data(efc)
sji.viewSPSS(efc)
sjt.df(efc)
sjt.df(efc[1:20,1:5], alternateRowColors=TRUE,
       orderColumn="e42dep", describe=FALSE)
data(efc)
# retrieve variable and value labels
varlabs <- sji.getVariableLabels(efc)
# recveive first item of COPE-index scale
start <- which(colnames(efc)=="c82cop1")
# recveive last item of COPE-index scale
end <- which(colnames(efc)=="c90cop9")
# create data frame with COPE-index scale
df <- as.data.frame(efc[,c(start:end)])
colnames(df) <- varlabs[c(start:end)]
sjt.pca(df, showMSA=TRUE, showVariance=TRUE)
