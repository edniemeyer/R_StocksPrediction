#lendo csv
csv_dolar_puro<-read.csv('dados-Dolar.csv')
csv_ibov_puro<-read.csv('dados-Ibov.csv')

#min max normalization
minMaxNormalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dolar_minMax <- as.data.frame(lapply(csv_dolar_puro,minMaxNormalize))
ibov_minMax <- as.data.frame(lapply(csv_ibov_puro,minMaxNormalize))


#min max denormaliztion
minvec <- sapply(csv_dolar_puro,min)
maxvec <- sapply(csv_dolar_puro,max)
minMaxDenormalize <- function(x,minval,maxval) {
  x*(maxval-minval) + minval
}

den_dolar_minMax <- as.data.frame(Map(minMaxDenormalize,dolar_minMax,minvec,maxvec))



#Z score normalization

dolar_Z <- as.data.frame( scale(csv_dolar_puro ))
ibov_Z <- as.data.frame( scale(csv_ibov_puro ))


#decimal normalization


decimalNormalize <- function(x) {
  return (x / 10^log10(max(x)))
}

dolar_decimal <- as.data.frame(lapply(csv_dolar_puro,decimalNormalize))
ibov_decimal <- as.data.frame(lapply(csv_ibov_puro,decimalNormalize))

