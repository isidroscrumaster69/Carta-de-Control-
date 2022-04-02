CartaDeControl<-function(data){
  #definicion de Variables a Usar
i<-NULL
ix<-NULL
iR<-NULL
promx<-NULL
lc<-NULL
lcs<-NULL
lcsR<-NULL
lci<-NULL
lciR<-NULL
lcR<-NULL
R<-NULL
colorX<-NULL
colorR<-NULL
obs<-NULL
#contadores
contXP1_UP<-0
contXP1_down<-0
contXP2_ASC<-0
contXP2_DESC<-0
ContXP4<-0
contXP5<-0
#i de ciclo for
ip1<-NULL
ip2<-NULL
ip4<-NULL
ip5<-NULL
#banderas 
P1_UP<-FALSE
P1_down<-FALSE
p2_DESC<-FALSE
p2_ASC<-FALSE
P4<-FALSE
P5<-FALSE
j<-2
#constantes del apendice a usar
A2<-0.577
D3<-0
D4<-2.1144
#calculo del promedio de X y Rango
for (i in 1:nrow(data)){
  promx<-c(promx,mean(data[i,]))
  R<-c(R,max(data[i,])-min(data[i,]))
}
#calculo de los diferentes limites
lc[1:nrow(data)]<-mean(promx)
lcR[1:nrow(data)]<-mean(R)
lcs[1:nrow(data)]<-lc+A2*lcR
lci[1:nrow(data)]<-lc-A2*lcR
lciR[1:nrow(data)]<-D3*lcR
lcsR[1:nrow(data)]<-D4*lcR
#calculo de la zona C
cUP<-mean(promx)+sd(promx)
cdown<-mean(promx)-sd(promx)
#PATRON 1
for (ip1 in 1:length(promx)) {
  if(promx[ip1]>lc[ip1]){
    contXP1_UP<-contXP1_UP+1
    if(contXP1_UP>=8){
      P1_UP<-TRUE
    }
  }
  else{
    contXP1_UP<-0
  }
  
}
for (ip1 in 1:length(promx)) {
  if(promx[ip1]<lc[ip1]){
    contXP1_down<-contXP1_down+1
    if(contXP1_down>=8){
      P1_down<-TRUE
    }
  }
  else{
    contXP1_down<-0
  }
  
} 
#Patron 2 
for (ip2 in 1:length(promx)) {
  if(promx[ip2]<promx[j]){
    contXP2_ASC<-contXP2_ASC+1
    j<-j+1
    if(contXP2_ASC>=6){
      p2_ASC=TRUE
    }
    
  }else{
    contXP2_ASC<-0
  }
}

for (i in 1:length(promx)) {
  if(promx[ip2]<promx[j]){
    contXP2_DESC<-contXP2_DESC+1
    j<-j+1
    if(contXP2_DESC>=6){
      p2_DESC=TRUE
    }
    
  }else{
    contXP2_DESC<-0
  }
}

#patron 4
for (ip4 in 1:length(promx)) {
  if(promx[ip4]>cUP|promx[ip4]<cdown){
    ContXP4<-ContXP4+1
    if(ContXP4==8){
      P4<-TRUE
    }
  }
  else{
    ContXP4<-0
  }
}
#patron 5 
for (ip5 in 1:length(promx)) {
  if(promx[ip5]<cUP&promx[ip5]>lc[ip5]|promx[ip5]>cdown&promx[ip5]<lc[ip5]){
    contXP5<-contXP5+1
    if(contXP5==8){
      P5<-TRUE
    }
  }else{
    contXP5<-0
  }
}

#funcion para que se muestren las dos graficas
par(mfrow=c(1,2))
#elaboracion de la condicion en caso de que un punto se salga de los limites
for (ix in 1:length(promx)) {
  if(promx[ix]>lcs[ix]|promx[ix]<lci[ix]){
    colorX<-c(colorX,"RED")
    obs<-"FUERA DE CONTROL ESTADISTICO POR Puntos Fuera de los Limites"
  }else{
    colorX<-c(colorX,"BLACK")
  }
}
#elaboracion de la carta X
plot(promx,type="o",pch=16,ylim = c(-20,30),main = "Carta X",ylab = "X",xlab = "Subgrupo",sub="Isidro Matamoros",col=colorX)
abline(h=lc,col="GREEN")
abline(h=lcs,col="RED")
abline(h=lci,col="RED")
abline(h=cUP,col="BLUE")
abline(h=cdown,col="BLUE")
text(x = 2.7, y = 20,label = "Limite Superior:")
text(x = 10, y = 20,label = lcs)
text(x = 2.7, y = -10,label = "Limite Inferior:")
text(x = 10, y = -10,label = lci)
#condicion en caso de que un punto se salgo de los limites Carta R
for (iR in 1:length(R)) {
  if(R[iR]>lcsR[iR]|R[iR]<lciR[iR]){
    colorR<-c(colorR,"RED")
    
  }else{
    colorR<-c(colorR,"BLACK")
  }
}
#elaboracion de la grafica de carta R
plot(R,type = "o",pch=16,ylim=c(0,60),main = "Carta R",ylab = "Rango",xlab = "Subgrupo",,sub="Isidro Matamoros",col= colorR)
abline(h=lciR,col="RED")
abline(h=lcsR,col="RED")
abline(h=lcR,col="GREEN")
text(x = 2.7, y = 60,label = "Limite Superior:")
text(x = 8, y = 60,label = lcsR)
text(x = 2.8, y = 7,label = "Limite Inferior:")
text(x = 7, y = 5,label = lciR)
#prueba de patrones
if(P1_UP==TRUE |P1_down==TRUE){
  print("FUERA DE CONTROL POR PATRON 1")
}
if(p2_ASC==TRUE|p2_DESC==TRUE){
  print("FUERA DE CONTROL POR PATRON 2")
}
if(P4==TRUE){
  print("FUERA DE CONTROL POR PATRON 4")
}
if(P5==TRUE){
  print("FUERA DE CONTROL POR PATRON 5")
}

}
