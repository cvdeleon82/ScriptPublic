library(base)
library(deSolve)
library(dfoptim)
library(nloptr)


x<-c(1900,1930,1950,1970,1990,2000,2010,2015)

#Tiempo de simulacion
x <- c(0,30,50,70,90,100,110,115);

#Longitud de peces
y <- c(0.7,1.2,3.1,6.9,8.2,8.6,8.8,8.9);



#La función nls minimiza via mínimos cuadrados no lineales la solución
#exacta del modelo
nlmod <- nls(y~ ((K*x_0)/(x_0+(K-x_0)*exp(-(r*x)))) ,start = list(x_0=0.7,r=0.1,K=150));nlmod

nlmod <- nls(y~  ((K*0.7)/(0.7+(K-0.7)*exp(-(r*x)))) ,start = list(r=0.1,K=150));nlmod

#Muestra los parámetros estimados
summary(nlmod)

confint(nlmod)

#deltaMethod(nlmod)

# Grafica la solucion numerica usando los parametros estimados versus los puntos
plot(x,y)
lines(x,predict(nlmod),lty=2,col="red",lwd=3)

