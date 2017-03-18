#lendo csv
#csv_dolar_puro<-read.csv('dados-Dolar.csv')
#csv_ibov_puro<-read.csv('dados-Ibov.csv')

#min max normalization

minMaxNormalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


dolar_minMax <- as.data.frame(lapply(csv_dolar,minMaxNormalize))
ibov_minMax <- as.data.frame(lapply(csv_ibov_puro,minMaxNormalize))


#min max denormaliztion

dolarminvec <- min(csv_dolar$Close)
dolarmaxvec <- max(csv_dolar$Close)
minMaxDenormalize <- function(x,minval,maxval) {
  x*(maxval-minval) + minval
}

den_dolar_minMax <- as.data.frame(Map(minMaxDenormalize,dolar_minMax,dolarminvec,dolarmaxvec))



#Z score normalization

dolarmeanvec <- sapply(csv_dolar_puro,mean)
dolarsdvec <- sapply(csv_dolar_puro,sd)

zScoreNormalize <- function(x){
  return((x-mean(x))/sd(x))
}

dolar_Z <- as.data.frame(lapply(csv_dolar_puro,zScoreNormalize))
ibov_Z <- as.data.frame(lapply(csv_ibov_puro,zScoreNormalize))


#Z score denormalization

zScoreDenormalize <- function(x, meanvec, sdvec){
  (x*sdvec) + meanvec
}


den_dolar_Z <- as.data.frame(Map(zScoreDenormalize,dolar_Z,dolarmeanvec,dolarsdvec))



#decimal normalization

decimalNormalize <- function(x) {
  return (x / 10^log10(max(x)))
}

dolar_decimal <- as.data.frame(lapply(csv_dolar_puro,decimalNormalize))
ibov_decimal <- as.data.frame(lapply(csv_ibov_puro,decimalNormalize))


#decimal denormalize

decimalDenormalize <- function(x,maxvec) {
  return (x*(10^log10(maxvec)))
}

den_dolar_decimal <- as.data.frame(Map(decimalDenormalize,dolar_decimal,dolarmaxvec))


