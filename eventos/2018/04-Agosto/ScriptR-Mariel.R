# Lectura de datos

setwd("C:/Users/Mariel/Dropbox/Maestria/2.3 Longitudinales")

data <- read.table("aclac.csv", header = T, sep=",", dec ="," )

head(data)



#Acomodar los datos
#formato tabla
library(reshape2)
data2 <- melt(data, id.vars = "id", measure.vars = c("lac0", "lac3", "lac6", "lac12"),
             value.name = "NivelAL", variable.name = "Tiempo")
#datos <- gather(data, "ocasion", "NivelAL", 3:6) otra forma de hacerlo
head(data2)

#agregue el tto y el tiempo numerico 
data2$tto <- (ifelse(data2$id > 126, 0,1))
data2$ttoo <- as.factor(data2$tto)
levels(data2$ttoo) <- c("No tratados", "Tratados")
data2$tiempo <- c(rep(0,140), rep(3,140), rep(6,140), rep(12,140))
data2$tiempo <- as.integer(data2$tiempo)
#data2$logNivelAL <- log(data2$NivelAL)
tail(data2)


#Medidas y graficos
library(dplyr)
data3=data2 %>%
  group_by(id) %>%
  summarise(media=mean(NivelAL), var=var(NivelAL), tto=mean(tto))
#data$var <- apply(data[,3:6],1,var)
#data$mean <- apply(data[,3:6],1,mean)

library(ggplot2)
ggplot(data3, aes(reorder(id, media), media, col=tto))+geom_point()
p <- ggplot(data3, aes(reorder(id, var), var, col=tto))+geom_point()
p

library(plotly)
ggplotly(p)

#resumen numerico
library(mosaic)
medidas <- cbind(favstats(NivelAL~tiempo+tto,data=data2),
                 tto=as.factor(c(0,0,0,0,1,1,1,1)), tiempo=c(0,3,6,12,0,3,6,12))



#MAtrices de varianzas y covarianzas por tto (si cambian cov por cor te da la de correlacion)
corrtt01 <- cov(data[1:126,3:6])   
corrtt02 <- cov(data[127:140,3:6]) 
corrtt <- cov(data[,3:6]) 


#grafico de dispersión
library(PerformanceAnalytics)
datos.reducido0 <- data[127:140,3:6] 
datos.reducido1 <- data[1:126,3:6]
chart.Correlation(datos.reducido0 , histogram=T, pch=19, 
                  main="Grafico 5: Matriz de dispersion y correlaciones(sin tratamiento)")
chart.Correlation(datos.reducido1 , histogram=T, pch=19, 
                  main="Grafico 5: Matriz de dispersion y correlaciones (con tratamiento)")
chart.Correlation(data[,3:6] , histogram=T, pch=19, 
                  main="Grafico 5: Matriz de dispersion y correlaciones")

#gráfico de perfiles individuales
ggplot(data=data2, aes(x=tiempo, y=NivelAL, group=id, col=ttoo)) +
  scale_x_continuous(breaks = c(0,3,6,12))+
  geom_line(show.legend = F)+
  geom_point(show.legend = F)+ 
  scale_color_brewer(palette="Set1")+
  facet_grid(.~ttoo)+
  ylab("Nivel ácido láctico (mmol/L)") +
  xlab("Tiempo (meses)") +
  stat_summary(aes(group=1), geom = "point",  fun.y = mean,
               size = 2, col="black") + 
  stat_summary(aes(group=1), geom = "line", fun.y = mean, size = 1, col="black") +
  labs(col="Tratamiento")
#  stat_smooth(aes(group=1), method = "lm", se = F,
#              formula = y ~ poly(x, 2), col="green")

#boxplot
ggplot(data2, aes(x=tiempo, y=NivelAL, group=tiempo, col=ttoo))+
    scale_x_continuous(breaks = c(0,3,6,12))+
  geom_boxplot(show.legend = F)+ facet_grid(.~ttoo)+ scale_color_brewer(palette="Set1")+
  ylab("Nivel ácido láctico (mmol/L)") +xlab("Tiempo (meses)")




#gráfico de perfiles promedio
o <- ggplot(medidas, aes(x=tiempo, y=mean, group=tto, color=tto))+
  geom_line(size=1)+geom_point()

o

# stat_summary(aes(x=tiempo, y=mean, group=tto, color=tto), geom = "point",  fun.y = mean,
#              size = 2, col="black") + 
# 


#####MODELOS--------------------(TODOS CON EFECTO ALEATORIO 
#####INTERCEPTO Y PENDIENTE)
#modelo maximal lineal 
lmm1 <- lme(NivelAL ~ tiempo*tto, data = data2, random = ~ 1+ tiempo | id, method = "ML" )
summary(lmm1)
plot(lmm1)

ggplot(data2, aes(x=tiempo, y=fitted(lmm1),group=id, color=tto))+
  geom_line(show.legend = F)+
  geom_point(show.legend = F)+
  facet_grid(.~tto)




# maximal cuadratico
lmm3 <- lme(NivelAL ~ tiempo+I(tiempo^2)+tiempo:tto+I(tiempo^2):tto,
            data = data2, random = ~ 1+ tiempo |id , method = "ML")
summary(lmm3)
ggplot(data2, aes(x=tiempo, y=fitted(lmm3),
                  group=id, col=tto))+geom_point()+geom_line()
plot(lmm3)


#  cuadratica sin interaccion
lmm5 <- lme(NivelAL ~ tiempo+I(tiempo^2)+tto, data = data2, 
            random = ~ 1+tiempo| id, method = "ML")
summary(lmm5)
ggplot(data2, aes(x=tiempo, y=fitted(lmm5),
                  group=id, color=tto))+geom_point()+geom_line()+
  facet_grid(.~tto)

#LINEAL SIN INTERACCION
library(nlme)
lmm4 <- lme(NivelAL ~ tiempo+tto, data = data2, 
            random = ~ 1+ tiempo | id, method = "ML" )
summary(lmm4)
plot(lmm4)

ggplot(data2, aes(x=tiempo, y=fitted(lmm4),group=id, color=tto))+
  geom_line(show.legend = F)+
  geom_point(show.legend = F)+
  facet_grid(.~tto)





# #con logaritmo
# #fijo lineal sin interaccion, random solo id
# lmm7 <- lme(logNivelAL~tiempo+tto, data = data2, random = ~ 1 | id) 
# summary(lmm7)
# ggplot(data2, aes(x=tiempo, y=fitted(lmm7),
#                   group=id, color=tto))+geom_point()+geom_line()+
#   facet_grid(.~tto)
# 



#comparaciones entre lineAL Y CUADRATICO SIN INTERACCION

anova(lmm3,lmm1) #con interaccion
anova(lmm4,lmm1) #2 lineales
anova(lmm3,lmm5) #dos cuadraticas
anova(lmm1, lmm4,lmm3,lmm5) 
anova(lmm4)
lmm3a <- update(lmm3, fixed=NivelAL~I(tiempo^2)*tto)
summary(lmm3a)



#me quedo con el modelo 1
lmm1a <- update(lmm1, fixed=log(NivelAL)~tto)
anova(lmm1a)
ggplot(data2, aes(x=tiempo, y=fitted(lmm1a),
                  group=id, color=tto))+geom_point()+geom_line()+
  facet_grid(.~tto)
summary(lmm1a)

lmm1a <- lme(NivelAL ~ tiempo+tto, data = data2,
             random = ~tiempo|id)
lmm1b <- lme(NivelAL ~ tiempo*tto, data = data2,
             random = ~tiempo|id,
             weights = varIdent(form= ~1| tto), method = "ML")


lmm1c <- lme(NivelAL ~ tiempo+tto, data = data2,
             random = ~tiempo|id,  method = "ML",
             weights =  varIdent(form= ~1|tto))
lmm1c.1 <- lme(NivelAL ~ tto, data = data2,
             random = ~tiempo|id,  method = "ML",
             weights =  varExp(form= ~tiempo|tto)
             )

anova(lmm1c,lmm1c.1)
summary(lmm1c)
anova(lmm1c)
plot(lmm1c.1, resid(., type="p") ~ fitted(.)|tto, abline = 0 )
plot(lmm1c)

shapiro.test(lmm1c$residuals)
