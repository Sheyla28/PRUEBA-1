#PRUEBA 1 INGENIER�A DE LA PRODUCCI�N 1
Datos <- read.csv("Tiempo Proceso.csv", header = TRUE, sep = ",")
names(Datos)

attach(Datos)

Informaci�n <- read.csv("Informaci�n.csv")
attach(Informaci�n)
names(Informaci�n)
data.frame(Informaci�n)

library(dplyr)
ls("package:dplyr")

#FRESADORA
data.frame(Informaci�n$"JL", data.frame(Informaci�n$Fresadora))


R�gimen_trabajo_Fresadora <- 8
R�gimen_trabajo_Fresadora

Horas_mtto_Fresadora <- 115
Horas_mtto_Fresadora

Cantidad_equipos_Fresadora <- 1
Cantidad_equipos_Fresadora

Imprevistos_Fresadora <- 150
Imprevistos_Fresadora

D�as_mes_Fresadora <- 24
D�as_mes_Fresadora

Horas_d�a_Fresadora <- 24
Horas_d�a_Fresadora

Meses_a�o_Fresadora <- 12
Meses_a�o_Fresadora

Vacaciones_Fresadora <- 24*R�gimen_trabajo_Fresadora*Cantidad_equipos_Fresadora
Vacaciones_Fresadora

#FONDO PRODUCTIVO TOTAL 
FPT <- D�as_mes_Fresadora*Horas_d�a_Fresadora*Cantidad_equipos_Fresadora*Meses_a�o_Fresadora 
FPT
#FONDO POR REQUERIMIENTOS TECNOLOGICOS  
FRT <- Horas_mtto_Fresadora*Cantidad_equipos_Fresadora
FRT
#FONDO POR REGIMEN LABORAL
FRL <- (R�gimen_trabajo_Fresadora*D�as_mes_Fresadora*Cantidad_equipos_Fresadora*11)+(Vacaciones_Fresadora)
FRL
#FONDO POR OTRAS CAUSAS
FOC <- Imprevistos_Fresadora*Cantidad_equipos_Fresadora
FOC
#FONDO PRODUCTIVO DISPONIBLE
FPD <- FPT-FRT-FRL-FOC 
FPD #h-e/a�o

#COEFICIENTE DE CARGA 
data.frame(Datos$"Plan_Anual", data.frame(Datos$Fresadora))
Pz_Ntij_Fresadora <- (Datos$"Plan_Anual")* (Datos$"Fresadora")
Pz_Ntij_Fresadora
sum(Pz_Ntij_Fresadora)

FPD_Np_Fresadora <- FPD*Cantidad_equipos_Fresadora
FPD_Np_Fresadora

Ccj_Fresadora <- sum(Pz_Ntij_Fresadora)/FPD_Np_Fresadora
Ccj_Fresadora

if(Ccj_Fresadora >= 0.85){
  "Masiva"
} else if(Ccj_Fresadora>=0.2 & Ccj_Fresadora<0.85){
  "Gran Serie"
}else if(Ccj_Fresadora>=0.08 & Ccj_Fresadora<0.2){
  "Mediana serie"
} else if(Ccj_Fresadora>=0.04 & Ccj_Fresadora<0.08){
  "Peque�a serie"
} else if(Ccj_Fresadora < 0.04){
  "Unitaria"
} else {
  "El n�mero es nulo"
}
print(Ccj_Fresadora)

#TORNO
data.frame(Informaci�n)
data.frame(Informaci�n$"JL", data.frame(Informaci�n$Torno))

R�gimen_trabajo_Torno <- 8
R�gimen_trabajo_Torno

Horas_mtto_Torno <- 167
Horas_mtto_Torno

Cantidad_equipos_Torno <- 3
Cantidad_equipos_Torno

Imprevistos_Torno <- 150
Imprevistos_Torno

D�as_mes_Torno <- 24
D�as_mes_Torno

Horas_d�a_Torno <- 24
Horas_d�a_Torno

Meses_a�o_Torno <- 12
Meses_a�o_Torno

Vacaciones_Torno <- 24*R�gimen_trabajo_Torno*Cantidad_equipos_Torno
Vacaciones_Torno

#FONDO PRODUCTIVO TOTAL 
FPT <- D�as_mes_Torno*Horas_d�a_Torno*Cantidad_equipos_Torno*Meses_a�o_Torno 
FPT
#FONDO POR REQUERIMIENTOS TECNOLOGICOS  
FRT <- Horas_mtto_Torno*Cantidad_equipos_Torno
FRT
#FONDO POR REGIMEN LABORAL
FRL <- (0)+(Vacaciones_Torno)
FRL
#FONDO POR OTRAS CAUSAS
FOC <- Imprevistos_Torno*Cantidad_equipos_Torno
FOC
#FONDO PRODUCTIVO DISPONIBLE
FPD <- FPT-FRT-FRL-FOC 
FPD #h-e/a�o

#COEFICIENTE DE CARGA 
data.frame(Datos$"Plan_Anual", data.frame(Datos$Torno))
Pz_Ntij_Torno <- (Datos$"Plan_Anual")* (Datos$"Torno")
Pz_Ntij_Torno
sum(Pz_Ntij_Torno)

FPD_Np_Torno <- FPD*Cantidad_equipos_Torno
FPD_Np_Torno

Ccj_Torno <- sum(Pz_Ntij_Torno)/FPD_Np_Torno
Ccj_Torno

if(Ccj_Torno >= 0.85){
  "Masiva"
} else if(Ccj_Torno>=0.2 & Ccj_Torno<0.85){
  "Gran Serie"
}else if(Ccj_Torno>=0.08 & Ccj_Torno<0.2){
  "Mediana serie"
} else if(Ccj_Torno>=0.04 & Ccj_Torno<0.08){
  "Peque�a serie"
} else if(Ccj_Torno < 0.04){
  "Unitaria"
} else {
  "El n�mero es nulo"
}
print(Ccj_Torno)

#PULIDORA
data.frame(Informaci�n)
data.frame(Informaci�n$"JL", data.frame(Informaci�n$Pulidora))


R�gimen_trabajo_Pulidora <- 8
R�gimen_trabajo_Pulidora

Horas_mtto_Pulidora <- 225
Horas_mtto_Pulidora

Cantidad_equipos_Pulidora <- 1
Cantidad_equipos_Pulidora

Imprevistos_Pulidora <- 150
Imprevistos_Pulidora

D�as_mes_Pulidora <- 24
D�as_mes_Pulidora

Horas_d�a_Pulidora <- 24
Horas_d�a_Pulidora

Vacaciones_Pulidora <- 24*R�gimen_trabajo_Pulidora*Cantidad_equipos_Pulidora
Vacaciones_Pulidora

Meses_a�o_Pulidora <- 12
Meses_a�o_Pulidora

#FONDO PRODUCTIVO TOTAL 
FPT <- D�as_mes_Pulidora*Horas_d�a_Pulidora*Cantidad_equipos_Pulidora*Meses_a�o_Pulidora
FPT
#FONDO POR REQUERIMIENTOS TECNOLOGICOS  
FRT <- Horas_mtto_Pulidora*Cantidad_equipos_Pulidora
FRT
#FONDO POR REGIMEN LABORAL
FRL <- (R�gimen_trabajo_Pulidora*D�as_mes_Pulidora*Cantidad_equipos_Pulidora*11)+(Vacaciones_Pulidora)
FRL
#FONDO POR OTRAS CAUSAS
FOC <- Imprevistos_Pulidora*Cantidad_equipos_Pulidora
FOC
#FONDO PRODUCTIVO DISPONIBLE
FPD <- FPT-FRT-FRL-FOC 
FPD #h-e/a�o

#COEFICIENTE DE CARGA 
data.frame(Datos$"Plan_Anual", data.frame(Datos$Pulidora))
Pz_Ntij_Pulidora <- (Datos$"Plan_Anual")* (Datos$"Pulidora")
Pz_Ntij_Pulidora
sum(Pz_Ntij_Pulidora)

FPD_Np_Pulidora <- FPD*Cantidad_equipos_Pulidora
FPD_Np_Pulidora

Ccj_Pulidora <- sum(Pz_Ntij_Pulidora)/FPD_Np_Pulidora
Ccj_Pulidora

if(Ccj_Pulidora >= 0.85){
  "Masiva"
} else if(Ccj_Pulidora>=0.2 & Ccj_Pulidora<0.85){
  "Gran Serie"
}else if(Ccj_Pulidora>=0.08 & Ccj_Pulidora<0.2){
  "Mediana serie"
} else if(Ccj_Pulidora>=0.04 & Ccj_Pulidora<0.08){
  "Peque�a serie"
} else if(Ccj_Pulidora < 0.04){
  "Unitaria"
} else {
  "El n�mero es nulo"
}
print(Ccj_Pulidora)


#COEFICIENTE DE OPERACIONES FIJADAS
#O
O <- count(data.frame(Datos$Fresadora)) + count(data.frame(Datos$Torno)) + count(data.frame(Datos$Pulidora))
O


#P
P <- Cantidad_equipos_Fresadora + Cantidad_equipos_Torno + Cantidad_equipos_Pulidora
P

Kof <- O/P
Kof
if(Kof <= 1){
  "Masiva"
} else if(Kof>1 & Kof<=10){
  "Gran Serie"
}else if(Kof>10 & Kof<=20){
  "Mediana serie"
} else if(Kof>20 & Kof<=40){
  "Peque�a serie"
} else if(Kof > 40){
  "Unitaria"
} else {
  "El n�mero es nulo"
}
print(Kof)
#CONCLUSION 
#EL TIEMPO DE PRODUCCI�N DE LA EMPRESA ES GRAN SERIE
#FIN Prueba 1
#Sheyla Flores

