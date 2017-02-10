library(rugarch)

#ss <- knime.in$"Close"
#outOfSample = (NROW(ss)/5) -1#20 percent for test 

#spec <- ugarchspec(mean.model=list(armaOrder=c(0,0)), 
#	variance.model = list(model="fGARCH", submodel="GARCH", garchOrder=c(1,1)))
spec <- ugarchspec()

knime.in$"Prediction" <- 0

for(i in 1:NROW(knime.in)){
  mod <- try(fit <- ugarchfit(spec=spec, solver="hybrid", data= as.numeric(t(knime.in[i,(NCOL(knime.in)-1):2]))))
  if(!is(mod,'try-error'))
    knime.in[i, "Prediction"]<- fitted(ugarchforecast(fit, n.ahead=1))
}

#fit <- ugarchfit(spec=spec, data=ss, out.sample=outOfSample)
#fore <- ugarchforecast(fit, n.ahead=1, n.roll=outOfSample)

#plot(fore,which=2)

#knime.out <- t(mapply(c,fitted(fore),sigma(fore)))
knime.out <- knime.in