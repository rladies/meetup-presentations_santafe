## Leemos datos
#setwd("D:/R ladies/MujeresPrograman")
programadoras <- read.csv(file = "datos/programadorasnew.csv",header = TRUE)
## qué contienen
## nombres de las variables
names(programadoras)
## que contiene el dataset
head(programadoras)
## tamaño del dataset
dim(programadoras)

## cantidad de unidades académicas por anio
table(programadoras$anio)

################################################################

## GENERO

mujeres=tapply(programadoras$estMujeres,programadoras$anio,sum)
mujeres
varones=tapply(programadoras$estVarones,programadoras$anio,sum)
varones
plot(2010:2015,varones,ylab = "cantidad de estudiantes",xlab="año",ylim=c(11000,38000),col="green3",pch=19)
points(2010:2015,mujeres,col="purple3",pch=19)
legend(2014, 20000, c("mujeres", "varones"), col = c("purple3","green3"), pch = c(19,19), bg = "gray90")

## opiniones?
#################################################################
##TOTALES
tabla=matrix(c(mujeres,varones),2,6,byrow=TRUE)
rownames(tabla)=c("mujeres","varones")
colnames(tabla)=2010:2015
tabla
#barplot(tabla)
barplot(tabla, main="", xlab="año", col=c("purple3","green3"),
        legend = rownames(tabla)) 

### PROPORCIONES
prop.table(tabla,2)
tabla.porc=prop.table(tabla,2)*100
tabla.porc
barplot(tabla.porc[1,],main="Distribución de estudiantes mujeres",xlab="año", ylab="porentaje",col=c("purple3"))
##########################################################################################
##########################################################################################
## Y los nuevos inscriptos?

mujeres=tapply(programadoras$niMujeres,programadoras$anio,sum)
mujeres
varones=tapply(programadoras$niVarones,programadoras$anio,sum)
varones
###########################
##TOTALES
tabla=matrix(c(mujeres,varones),2,6,byrow=TRUE)
rownames(tabla)=c("mujeres","varones")
colnames(tabla)=2010:2015
tabla

barplot(tabla, main="", xlab="año", col=c("purple3","green3"),
        legend = rownames(tabla)) 

### PROPORCIONES
prop.table(tabla,2)
tabla.porc=prop.table(tabla,2)*100
tabla.porc
barplot(tabla.porc[1,],main="Distribución de nuevas inscriptas",xlab="año", ylab="porentaje",col=c("purple3"))

##################################################################################################
##################################################################################################
## Y las egresadas?

mujeres=tapply(programadoras$egMujeres,programadoras$anio,sum)
mujeres
varones=tapply(programadoras$egVarones,programadoras$anio,sum)
varones
###########################
##TOTALES
tabla=matrix(c(mujeres,varones),2,6,byrow=TRUE)
rownames(tabla)=c("mujeres","varones")
colnames(tabla)=2010:2015
tabla

barplot(tabla, main="", xlab="año", col=c("purple3","green3"),
        legend = rownames(tabla)) 

### PROPORCIONES
prop.table(tabla,2)
tabla.porc=prop.table(tabla,2)*100
tabla.porc
barplot(tabla.porc[1,],main="Distribución de egresadas",xlab="año", ylab="porcentaje",col=c("purple3"))

############################################################################
###########################################################################
# de las estudiantes mujeres, qué proporción de las ingresantes egresa?
### SIMPLIFICACION!! OJO,NO ESTA BIEN ESTRICTAMENTE!!!

mujeres.ni=tapply(programadoras$niMujeres,programadoras$anio,sum)
mujeres.ni

mujeres.egre=tapply(programadoras$egMujeres,programadoras$anio,sum)
mujeres.egre
prop.mujeres=mujeres.egre/mujeres.ni*100
prop.mujeres

## idem varones
varones.ni=tapply(programadoras$niVarones,programadoras$anio,sum)
varones.ni

varones.egre=tapply(programadoras$egVarones,programadoras$anio,sum)
varones.egre
prop.varones=varones.egre/varones.ni*100
prop.varones

##LO vemos en un gráfico

plot(2010:2015,prop.varones,ylab = "porcentaje de egresados",xlab="año",ylim=c(0,40),col="green3",pch=19)
points(2010:2015,prop.mujeres,col="purple3",pch=19)
legend(2010,10, c("mujeres", "varones"), col = c("purple3","green3"), pch = c(19,19))

plot(2010:2015,prop.varones,ylab = "porcentaje de egresados",xlab="año",ylim=c(0,40),col="green3",pch=19)
points(2010:2015,prop.mujeres,col="purple3",pch=19)
legend("bottomleft", c("mujeres", "varones"), col = c("purple3","green3"), pch = c(19,19),cex = 1.2, bty = "n", inset = c(0.1, 0.1))

#############################################################################
### CONCLUSION?