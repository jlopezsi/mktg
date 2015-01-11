
getwd()
setwd("kchospital")
#"/kchospital/kchospital.txt"
kchospital <- read.table('../hospital/hospital.txt', header=T)
#"../kchspital/kcprefs.txt"
kcprefs <- read.table(file.choose(), header=T)
names(kchospital)
names(kcprefs)
