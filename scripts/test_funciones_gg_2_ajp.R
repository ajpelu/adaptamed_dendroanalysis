
library(dplR)
library(tidyverse)

# Read pies and subset locality
pies <- readxl::read_excel("data/db_dendroadaptamed.xlsx", sheet = "tree_measurements") |>
  dplyr::filter(locality == "pinaster_low")



# RWL es la matriz de mediciones de TSAP (.rwl)

RWgraph <- function(RWL, a, b)
{
  RWL1=NULL
  #RWL$year=as.numeric(rownames(RWL))
  #
  for (i in 1:(ncol(RWL)-1))
  {
    serie=RWL[,c(i, ncol(RWL))]
    serie=subset(serie, !is.na(serie[,1]))
    #serie$plot=substr(colnames(serie[1]), a,a+1)
    serie$pie=as.numeric(substr(colnames(serie[1]), a,a+1))
    serie$core=substr(colnames(serie[1]), b,b+1)
    colnames(serie)=c("RW", "year", "pie", "core")
    RWL1=rbind(RWL1, serie)
  }
  RWL1<<-RWL1
}
#



# Format rwl
pilow_rw <- pilow |>
  pivot_longer(-year, values_to = "RW",
               values_drop_na = TRUE) |>
  mutate(treeid = str_remove(name, "JERL")) |>
  separate(treeid, into = c("tree", "core"), sep = 2)







|>
  separate(name, into = c("site", "id"), sep = 4) |>
  separate(id, into = c("tree", "coree"), sep = )



# Read
pilow <- read.rwl("data/raw/rwl/6-JER_PIN_LOW_cores.rwl")

# Year como columna
pilow$year <- as.numeric(rownames(pilow))

pilowRW=RWgraph(pilow, 5, 7)


rwl2df <- function(path) {
  o <- dplR::read.rwl(path) |> rownames_to_column(var = "year")
  out <- o |> pivot_longer(-year, values_to = "RW",
                           values_drop_na = TRUE) |>
    mutate(file = str_remove(basename(path), ".rwl"))
  return(out)
  }


rwl_files <- list.files(path = "data/raw/cronos", pattern = "\\cores.rwl$", full.names = TRUE, recursive = TRUE)


ii <- rwl_files |> purrr::map(~rwl2df(path = .x)) |> bind_rows()

ii <- rwl2df("data/raw/rwl/6-JER_PIN_LOW_cores.rwl") |> bind_row



write.csv(as.data.frame(
  cbind(p = rwl_files,
        bn = basename(rwl_files))), "output/files.csv")

pilow_rw <- pilow |>
  pivot_longer(-year, values_to = "RW",
               values_drop_na = TRUE) |>
  mutate(treeid = str_remove(name, "JERL")) |>
  separate(treeid, into = c("tree", "core"), sep = 2)




pilow=read.rwl("/Users/guigeiz/Documents/Proyectos/LIFE Granada Regino y Antonio/Muestreos Sierra Nevada 2020-2021/Granada LIFE UGR cronos finales/6-Jerez-Low_PINASTER/6-JER_PIN_LOW_cores.rwl")
pilow$year=as.numeric(rownames(pilow))
#RW
pilowRW=RWgraph(pilow, 5, 7)
pilowRW=aggregate(pilowRW[, c(1,3)], by=list(year=pilowRW$year, pie=pilowRW$pie), mean)
#write.csv(pilowRW, "/Users/guigeiz/Documents/Proyectos/LIFE Granada Regino y Antonio/Tablas y datos analisis/2RW_PIPI_low.csv")
RW_m2=aggregate(pilowRW[,c(3)], by=list(pilowRW$year), mean)
RW_s=aggregate(pilowRW[,c(3)], by=list(pilowRW$year), sd)
RW_l=aggregate(pilowRW[,c(3)], by=list(pilowRW$year), length)
colnames(RW_m2)=c("year", "RWmean")
colnames(RW_s)=c("year", "RWsd")
colnames(RW_l)=c("year", "n")






# compute BA
basalarea <- function(dbh, unit = "m2") {

  # Calculate basal area in cm2
  ba_cm2 <- (pi * dbh^2) / 4

  # Convert to specified units
  if (unit == "cm2") {
    ba <- ba_cm2
  } else if (unit == "m2") {
    ba <- ba_cm2 / 10000;
  } else {
    stop("Invalid unit. Please use 'cm2' or 'm2'.")
  }

  # Return the basal area
  return(ba)
}




pies <- pies |>
  mutate(ba = (pi*tree_dbh^2)/40000,
         baf = basalarea(tree_dbh, unit = "cm2"))




sum(pies$ba)*10000/(pi*(max(pies$tree_distance_m)^2)+(0.5*max(pies$tree_distance_m)^2)/nrow(pies))




pies$arbol=as.numeric(substr(pies$code_tree,1,2))


###Exploraci´ón datos parcelas LIFE sierra nevada
#2 PINUS PINASTER LOW, cuidado que era el archivo 6 en las carpetas
#lo puse aqu´í como 2 para que fueran por orden de altitud


library("dplR")


library(dplR)
#
pies=read.csv("/Users/guigeiz/Documents/Proyectos/LIFE Granada Regino y Antonio/Muestreos Sierra Nevada 2020-2021-2022/Pies_SN_LIFE.csv")
#
pies=subset(pies, localidad=="pinaster low")
summary(pies)
pies$BA=(pi*(pies$dbh^2))/40000  ### m2
pies$arbol=as.numeric(substr(pies$code_tree,1,2))
#
sum(pies$BA)*10000/(pi*(max(pies$distance_m)^2)+(0.5*max(pies$distance_m)^2)/nrow(pies))
nrow(pies)*10000/(pi*(max(pies$distance_m)^2)+(0.5*max(pies$distance_m)^2)/nrow(pies))
#
pies=pies[order(-pies$tree_height),]
piesA=pies[1:5,]
summary(piesA)
############## SCRIPTS ##############################
#################
#RWL es la matriz de mediciones de TSAP (.rwl)
#
RWgraph=function(RWL, a, b)
{
  RWL1=NULL
  #RWL$year=as.numeric(rownames(RWL))
  #
  for (i in 1:(ncol(RWL)-1))
  {
    serie=RWL[,c(i, ncol(RWL))]
    serie=subset(serie, !is.na(serie[,1]))
    #serie$plot=substr(colnames(serie[1]), a,a+1)
    serie$pie=as.numeric(substr(colnames(serie[1]), a,a+1))
    serie$core=substr(colnames(serie[1]), b,b+1)
    colnames(serie)=c("RW", "year", "pie", "core")
    RWL1=rbind(RWL1, serie)
  }
  RWL1<<-RWL1
}
#

####### BAI  #####
###MACRO PARA CALCULARSE BAI POR PARCELA
### CUIDADO CON SERIES QUE NO EMPIEZAN EN LA MISMA FECHA QUE EL FINAL (PUEDEN METER SESGO): por eso uso BAItree
### BAI calculado por árbol para evitar sesgo de series que no lleguen hasta el final ("flotantes")
##
BAItree=function(daps, rwls)
{
  BAIdata=NULL
  #
  for (i in 1:(ncol(rwls)-1))
  {
    Var=as.data.frame(cbind(as.numeric((rwls[,i])), as.numeric(rwls$year)))
    Var=subset(Var, !is.na(Var[,1]))
    colnames(Var)=c("Growth", "year")
    Var$age1=seq(1, nrow(Var), 1)
    #parcela2=as.numeric(substr(colnames(rwls[i]), 3,4))
    arbol1=as.numeric(substr(colnames(rwls[i]),5,6))
    checkage=subset(daps, arbol==arbol1)
    Var$dapcc=as.numeric(c(checkage$dbh))
    Var$dapsc=10*(Var$dapcc-(2*(0.0797*Var$dapcc+1.26 )))   ###pongo las ecuaciones de dar´ío de datos que tomamos en parcelas dap-CORTEZA
    Var$tree=arbol1
    #
    BAIdata=rbind(BAIdata, Var)
  }
  #
  BAIdatatree=aggregate(BAIdata, by=list(BAIdata$year, BAIdata$tree), mean)
  BAIdatatree=BAIdatatree[order(BAIdatatree$tree, BAIdatatree$year),-c(4, 8)]
  colnames(BAIdatatree)=c("year", "tree", "Growth", "age1", "dapcc", "dapsc")
  #
  arboles=aggregate(BAIdatatree[,2], by=list(arbol=BAIdatatree$tree), mean)
  arboles=arboles[,1]

  ###ahora me calculo el BAI por pie
  BAIdata2=NULL
  #
  for (i in arboles)
  {
    ##Me calculo el BAI ahora
    Var=subset(BAIdatatree, tree==i)
    Var=Var[rev(order(Var$year)),]
    Var$RWcorr=Var[,3]*Var$dapsc/(2*sum(Var[,3]))
    Var$BAI=ifelse(2*sum(Var[,3])<max(Var$dapsc),(pi/4)*(Var$dapsc**2-(Var$dapsc-2*Var[,3])**2)/100,(pi/4)*(Var$dapsc**2-(Var$dapsc-2*Var[,7])**2)/100)   ##en cm2, podrÌa hacerse con pi r cuadrado, pero lo pongo como Piovesan et al 2008
    #lo paso todo a cm ahora
    for (vv in 2:nrow(Var))
    {
      Var$dapsc[vv]=ifelse(2*sum(Var[,3])<max(Var$dapsc),Var$dapsc[vv-1]-2*Var[vv-1,3], Var$dapsc[vv-1]-2*Var[vv-1,7]) #en mm
      Var$BAI[vv]=ifelse(2*sum(Var[,3])<max(Var$dapsc),(pi/4)*(Var$dapsc[vv]**2-(Var$dapsc[vv]-2*Var[vv,3])**2)/100,(pi/4)*(Var$dapsc[vv]**2-(Var$dapsc[vv]-2*Var[vv,7])**2)/100)   ##en cm2, podrÌa hacerse con pi r cuadrado, pero lo pongo como Piovesan et al 2008
    }
    #
    BAIdata2=rbind(BAIdata2, Var)
  }
  #
  #BAIdata2$plot=parcela2
  #
  Muestra=NULL
  for (i in min(BAIdata2$year):max(BAIdata2$year))
  {
    numer=c(i, nrow(BAIdata2[BAIdata2$year==i,]))
    Muestra=rbind(Muestra,numer)
  }
  Muestra<-Muestra
  Muestra2<-Muestra[Muestra[,2]>=3,]  ###3 pies son como mínimo 6 radios
  #
  BAIdata2<-BAIdata2[BAIdata2$year>=min(Muestra2[,1]),]
  #
  BAIdata_sd=aggregate(BAIdata2[,8], by=list(year1=BAIdata2$year), sd)
  #BAIdata_sd$year=BAIdata_sd$Group.1
  BAIdata3=aggregate(BAIdata2[, 2:8], by=list(year=BAIdata2$year), mean)   ###los NA son omitidos (na.action) y cuando hay sólo un radio coge el que hay como la media
  #
  BAIdata_sd[,2]=BAIdata_sd[,2]/sqrt(Muestra2[,2])
  colnames(BAIdata_sd)=c("year", "BAIserror")
  #
  BAIdata<<-as.data.frame(merge(BAIdata3, BAIdata_sd, by.x="year", by.y="year"))
}


#### CALCULOS RW, BAI, IBIO

pilow=read.rwl("/Users/guigeiz/Documents/Proyectos/LIFE Granada Regino y Antonio/Muestreos Sierra Nevada 2020-2021/Granada LIFE UGR cronos finales/6-Jerez-Low_PINASTER/6-JER_PIN_LOW_cores.rwl")
pilow$year=as.numeric(rownames(pilow))
#RW
pilowRW=RWgraph(pilow, 5, 7)
pilowRW=aggregate(pilowRW[, c(1,3)], by=list(year=pilowRW$year, pie=pilowRW$pie), mean)
#write.csv(pilowRW, "/Users/guigeiz/Documents/Proyectos/LIFE Granada Regino y Antonio/Tablas y datos analisis/2RW_PIPI_low.csv")
RW_m2=aggregate(pilowRW[,c(3)], by=list(pilowRW$year), mean)
RW_s=aggregate(pilowRW[,c(3)], by=list(pilowRW$year), sd)
RW_l=aggregate(pilowRW[,c(3)], by=list(pilowRW$year), length)
colnames(RW_m2)=c("year", "RWmean")
colnames(RW_s)=c("year", "RWsd")
colnames(RW_l)=c("year", "n")
#
plot(RW_m2[,1],RW_m2[,2], type="l", ylim=c(0,10), xlab="Year", ylab=expression(bold(RW~(mm~year^-1))))
xx=c(min(RW_m2[,1]):max(RW_m2[,1]), rev(min(RW_m2[,1]):max(RW_m2[,1])))
yy=c(RW_m2[,2]-1.96*RW_s[,2]/sqrt(RW_l[,2]), rev(RW_m2[,2]+1.96*RW_s[,2]/sqrt(RW_l[,2])))
polygon(xx,yy, col=rgb(0,0,0,0.3), border=NA)
##
#### BAI
pilowBAI=BAItree(pies, pilow)
#write.csv(pilowBAI, "/Users/guigeiz/Documents/Proyectos/LIFE Granada Regino y Antonio/Tablas y datos analisis/2BAI_PIPI_low.csv")
#### Figure BAI
#
par(mgp=c(1.5,0.3,0), cex.lab=1, font.axis=2, font.lab=2, cex.lab=1, tck=0.2, tcl=0.2, las=1, mar=c(3,3,1,1))
#
plot(pilowBAI$year, pilowBAI$BAI, type="n", ylim=c(0, 10), xlim=c(1950,2022), xlab="Year", ylab=expression(bold(BAI~(cm^2~year^-1))))
vv=seq(min(pilowBAI$year), max(pilowBAI$year),1)
xxx=c(vv,rev(vv))
yyy=c((pilowBAI$BAI-(1.96*pilowBAI$BAIserror)), rev(pilowBAI$BAI+(1.96*pilowBAI$BAIserror)))
polygon(xxx, yyy, col=rgb(0,0,0,0.3), border=NA)
lines(pilowBAI$year, pilowBAI$BAI, ylim=c(0, 13), lwd=1, col="black")
#

########## INCREMENTO BIOMASA
############################################
######## estimate  AGB  #############
########################################################################################
# Montero, G., Ruiz-Peinado, R. and Munoz, M., 2005. Producción de biomasa
# y fijación de CO2 por los bosques españoles.
# INIA-Instituto Nacional de Investigación y Tecnología Agraria y Alimentaria.
########################################################################################


Biomass<-function(dbh, SEE,a, b){
  SEE_total=SEE
  CF_total=exp((SEE_total^2)/2)
  biomass= CF_total*(exp(a))*dbh^b
  return(biomass)
}



########### IBIO
IBIOpipi=function(daps, rwls, carbono_perc, SEE, a, b, SEE1, a1, b1, SEE2, a2, b2)    ###a, b parámetros alometrís dap, pp es la parcela de donde se saca la distancia para calcular valores por m2
{
  BIOdata=NULL
  #
  for (i in 1:(ncol(rwls)-1))
  {
    Var=as.data.frame(cbind(as.numeric((rwls[,i])), as.numeric(rwls$year)))
    Var=subset(Var, !is.na(Var[,1]))
    colnames(Var)=c("Growth", "year")
    Var$age1=seq(1, nrow(Var), 1)
    arbol1=as.numeric(substr(colnames(rwls[i]),5,6))
    checkage=subset(daps, arbol==arbol1)
    Var$dapcc=as.numeric(checkage$dbh)
    Var$dapsc=10*(Var$dapcc-(2*(0.0797*Var$dapcc+1.26 )))   ###pongo las ecuaciones de dar´ío de datos que tomamos en parcelas dap-CORTEZA
    #dapsc en mm porque growth esta en mm
    Var$tree=arbol1
    #
    BIOdata=rbind(BIOdata, Var)
  }
  #
  BIOdatatree=aggregate(BIOdata, by=list(BIOdata$year, BIOdata$tree), mean)
  #
  BIOdatatree=BIOdatatree[order(BIOdatatree$tree, BIOdatatree$year),-c(4, 8)]
  colnames(BIOdatatree)=c("year", "tree", "Growth", "age1", "dapcc", "dapsc")
  #
  arboles=aggregate(BIOdatatree[,2], by=list(arbol=BIOdatatree$tree), mean)
  arboles=arboles[,1]
  #
  ###ahora me calculo el IBIO por pie
  BIOdata2=NULL
  #
  for (i in arboles)
  {
    ##
    Var=subset(BIOdatatree, tree==i)
    Var=Var[rev(order(Var$year)),]
    Var$RWcorr=Var[,3]*Var$dapsc/(2*sum(Var[,3]))   # en mm
    Var$dapcc_t1=(ifelse(2*sum(Var[,3])<max(Var$dapsc),0.1*(Var$dapsc-2*Var$Growth), 0.1*(Var$dapsc-2*Var$RWcorr))+2.52)/0.8406   ###en cm el dap otra vez, supongo la relación corteza-diámetro ad-hoc PIHA SN
    # dap en las formulas en cm
    BT1=Biomass(Var$dapcc, SEE, a,b)-Biomass(Var$dapcc_t1, SEE, a,b)
    BT=Biomass(Var$dapcc, SEE, a,b)
    ST1=Biomass(Var$dapcc, SEE1, a1,b1)-Biomass(Var$dapcc_t1, SEE1, a1,b1)
    ST=Biomass(Var$dapcc, SEE1, a1,b1)
    IBR=Biomass(Var$dapcc, SEE2, a2,b2)-Biomass(Var$dapcc_t1, SEE2, a2,b2)
    BR=Biomass(Var$dapcc, SEE2, a2,b2)
    #
    Var$IBT_ag=BT1*carbono_perc*1000   ##en g C /año, biomasa a´érea total
    Var$IST=ST1*carbono_perc*1000   ##en g C /año, biomasa a´érea total
    Var$IBR=IBR*carbono_perc*1000  ###RA´ÍCES (pagina 33 Montero et al. tabla coeficientes)
    Var$BT_ag=BT*carbono_perc*1000
    Var$ST=ST*carbono_perc*1000
    Var$BR=BR*carbono_perc*1000
    #
    for (vv in 2:nrow(Var))
    {
      Var$dapsc[vv]=ifelse(2*sum(Var[,3])<max(Var$dapsc),Var$dapsc[vv-1]-2*Var[vv-1,3], Var$dapsc[vv-1]-2*Var[vv-1,7]) #en mm
      Var$dapcc[vv]=((0.1*Var$dapsc[vv])+2.52)/0.8406  ## en cm, y hay que meter en cm el dap en la formula de corteza que sale en mm, cuidado!
      Var$dapcc_t1[vv]=(ifelse(2*sum(Var[,3])<max(Var$dapsc),0.1*(Var$dapsc[vv]-2*Var$Growth[vv]),0.1*(Var$dapsc[vv]-2*Var$RWcorr[vv]))+2.52)/0.8406  ##en cm
      #
      BT1[vv]=Biomass(Var$dapcc[vv], SEE, a,b)-Biomass(Var$dapcc_t1[vv],SEE, a,b)
      BT[vv]=Biomass(Var$dapcc[vv], SEE, a,b)
      ST1[vv]=Biomass(Var$dapcc[vv],SEE1, a1,b1)-Biomass(Var$dapcc_t1[vv], SEE1, a1,b1)
      ST[vv]=Biomass(Var$dapcc[vv], SEE1, a1,b1)
      IBR[vv]=Biomass(Var$dapcc[vv], SEE2, a2,b2)-Biomass(Var$dapcc_t1[vv], SEE2, a2,b2)
      BR[vv]=Biomass(Var$dapcc[vv], SEE2, a2,b2)
      #
      Var$IBT_ag[vv]=BT1[vv]*carbono_perc*1000   ##en g C /año, biomasa a´érea total
      Var$IST[vv]=ST1[vv]*carbono_perc*1000   ##en g C /año, biomasa a´érea total
      Var$IBR[vv]=IBR[vv]*carbono_perc*1000  ###RA´ÍCES (pagina 33 Montero et al. tabla coeficientes)
      Var$BT_ag[vv]=BT[vv]*carbono_perc*1000
      Var$ST[vv]=ST[vv]*carbono_perc*1000
      Var$BR[vv]=BR[vv]*carbono_perc*1000
    }
    #
    #plot(Var$year, Var$IBIO_stem, type="l", xlim=c(1959, 2017))
    #lines(Var$year, Var$IBIO_root, col="red")
    #par(new=TRUE)
    #plot(BAI2$year, BAI2$BAI, ylim=c(0, 11), lwd=3, col="blue", type="l")

    BIOdata2=rbind(BIOdata2, Var)
  }
  #
  #BIOdata2$plot=parcela2
  #
  if(FALSE)
  {
    Muestra=NULL
    for (i in min(BIOdata2$year):max(BIOdata2$year))
    {
      numer=c(i, nrow(BIOdata2[BIOdata2$year==i,]))
      Muestra=rbind(Muestra,numer)
    }
    Muestra<-Muestra
    Muestra2<-Muestra[Muestra[,2]>=3,]  ###3 pies son como mínimo 6 radios
    #
    BAIdata2<-BAIdata2[BAIdata2$year>=min(Muestra2[,1]),]
    #
  }
  #
  BIOdata=aggregate(BIOdata2[,9:14], by=list(year1=BIOdata2$year), sum)
  #
  BIOdata$IBT_ag_m2=BIOdata$IBT_ag*1/(pi*(max(daps$distance_m)^2)+(0.5*max(daps$distance_m)^2)/nrow(daps))   ###por m2 TOTAL ABOVEGROUND
  BIOdata$IST_m2=BIOdata$IST*1/(pi*(max(daps$distance_m)^2)+(0.5*max(daps$distance_m)^2)/nrow(daps))   ###por m2 FUSTE (STEM)
  BIOdata$IBR_m2=BIOdata$IBR*1/(pi*(max(daps$distance_m)^2)+(0.5*max(daps$distance_m)^2)/nrow(daps))  ### por m2 RA´ÍCES (ROOTS)
  BIOdata$BT_m2=BIOdata$BT_ag*1/(pi*(max(daps$distance_m)^2)+(0.5*max(daps$distance_m)^2)/nrow(daps))   ###
  BIOdata$ST_m2=BIOdata$ST*1/(pi*(max(daps$distance_m)^2)+(0.5*max(daps$distance_m)^2)/nrow(daps)) ###por m2
  BIOdata$BR_m2=BIOdata$BR*1/(pi*(max(daps$distance_m)^2)+(0.5*max(daps$distance_m)^2)/nrow(daps))
  #
  return(BIOdata)
}


### ABI
IBIO2=IBIOpipi(pies, pilow, 0.5, SEE=0.173491, a=-3.00347, b=2.49641, SEE1=0.191593, a1=-3.43957, b1=2.56636, SEE2=0.160225, a2=-3.85184, b2=2.37592)

#write.csv(IBIO2,"/Users/guigeiz/Documents/Proyectos/LIFE Granada Regino y Antonio/Tablas y datos analisis/2BIO_PIPI_low.csv" )
plot(IBIO2$year1, IBIO2$IBT_ag_m2, type="l")
par(new=TRUE, yaxt="n")
plot(pilowBAI$year, pilowBAI$BAI, col="red", type="l")
par(yaxt="s")
axis(4)
cor.test(IBIO2$IBT_ag_m2, pilowBAI$BAI)





daps, rwls


# preparo los datos de RW

# Leo rwls p

rwls

rw



dbh_trees <- pies |>
  dplyr::select(code_tree, dbh) |>
  mutate(code_tree = as.numeric(str_remove(code_tree, "POM")))

g <- rw |> inner_join(dbh_trees, by = c("tree" = "code_tree"))


t1 <- g |> group_by(sp_elev, tree, year) |>
  summarise(RW = mean(RW),
            dbh = mean(dbh)) |>
  filter(tree == 1)


P. sylvestris
Var$dapsc=10*(Var$dapcc-(2*(0.0809*Var$dapcc-0.00416)))

P. halepensis
Var$dapsc=(10*Var$dapcc-(2*(1.6143*Var$dapcc+7.6431 )))

P. pinaster
Var$dapsc=10*(Var$dapcc-(2*(0.0797*Var$dapcc+1.26 )))
Var$dapsc=10*(Var$dapcc-(2*(0.0797*Var$dapcc+1.26 )))

P. nigra









### ahora me calculo el BAI por pie
BAIdata2 <- NULL
#
for (i in arboles)
{
  ## Me calculo el BAI ahora
  Var <- subset(BAIdatatree, tree == i)
  Var <- Var[rev(order(Var$year)), ]

  Var$RWcorr <- Var[, 3] * Var$dapsc / (2 * sum(Var[, 3]))
  Var$BAI <- ifelse(2 * sum(Var[, 3]) < max(Var$dapsc), (pi / 4) * (Var$dapsc**2 - (Var$dapsc - 2 * Var[, 3])**2) / 100, (pi / 4) * (Var$dapsc**2 - (Var$dapsc - 2 * Var[, 7])**2) / 100) ## en cm2, podrÌa hacerse con pi r cuadrado, pero lo pongo como Piovesan et al 2008
  # lo paso todo a cm ahora
  for (vv in 2:nrow(Var))
  {
    Var$dapsc[vv] <- ifelse(2 * sum(Var[, 3]) < max(Var$dapsc), Var$dapsc[vv - 1] - 2 * Var[vv - 1, 3], Var$dapsc[vv - 1] - 2 * Var[vv - 1, 7]) # en mm
    Var$BAI[vv] <- ifelse(2 * sum(Var[, 3]) < max(Var$dapsc), (pi / 4) * (Var$dapsc[vv]**2 - (Var$dapsc[vv] - 2 * Var[vv, 3])**2) / 100, (pi / 4) * (Var$dapsc[vv]**2 - (Var$dapsc[vv] - 2 * Var[vv, 7])**2) / 100) ## en cm2, podrÌa hacerse con pi r cuadrado, pero lo pongo como Piovesan et al 2008
  }
  #
  BAIdata2 <- rbind(BAIdata2, Var)













  ###
  library(dplR)


  # read rwl core
  pisyhigh = read.rwl("data/raw/cronos/11-Portugos_HIGH_PISY/11-PH_cores.rwl")

  pisyhigh_tree = read.rwl("data/raw/cronos/11-Portugos_HIGH_PISY/11-PH_trees.rwl")

  # stats
  a <- rwl.report(pisyhigh)
  b <- rwl.stats(pisyhigh_tree)
  mean(b$mean)
  sens1(pisyhigh_tree)
  get_period <- function(rwl, site) {
    x <- rwl.report(rwl)
    data.frame(cbind(site = site,
                     start = x$firstYear,
                     end = x$lastYear))
  }




  get_period(pisyhigh, site = "sylvestris_high")










  ###Exploraci´ón datos parcelas LIFE sierra nevada
  #11 PINUS SYLVESTRIS HIGH

  library(dplR)

  pies=read.csv("/Users/guigeiz/Documents/Proyectos/LIFE Granada Regino y Antonio/Muestreos Sierra Nevada 2020-2021-2022/Pies_SN_LIFE.csv")
  #
  pies=subset(pies, localidad=="POR_HIGH_SYLVESTRIS")
  summary(pies)
  pies$BA=(pi*(pies$dbh^2))/40000  ### m2
  pies$arbol=as.numeric(substr(pies$code_tree,4,5))
  pies=pies[order(pies$arbol),]
  #
  sum(pies$BA)*10000/(pi*(max(pies$distance_m)^2)+(0.5*max(pies$distance_m)^2)/nrow(pies))
  nrow(pies)*10000/(pi*(max(pies$distance_m)^2)+(0.5*max(pies$distance_m)^2)/nrow(pies))
  #
  pies=pies[order(-pies$tree_height),]
  piesA=pies[1:5,]
  summary(piesA)
  ############## SCRIPTS ##############################
  #################
  #RWL es la matriz de mediciones de TSAP (.rwl)
  #
  RWgraph=function(RWL, a, b)
  {
    RWL1=NULL
    #RWL$year=as.numeric(rownames(RWL))
    #
    for (i in 1:(ncol(RWL)-1))
    {
      serie=RWL[,c(i, ncol(RWL))]
      serie=subset(serie, !is.na(serie[,1]))
      #serie$plot=substr(colnames(serie[1]), a,a+1)
      serie$pie=as.numeric(substr(colnames(serie[1]), a,a+1))
      serie$core=substr(colnames(serie[1]), b,b+1)
      colnames(serie)=c("RW", "year", "pie", "core")
      RWL1=rbind(RWL1, serie)
    }
    RWL1<<-RWL1
  }
  #

  # pisyhigh=read.rwl("/Users/guigeiz/Documents/Proyectos/LIFE Granada Regino y Antonio/Muestreos Sierra Nevada 2020-2021/Granada LIFE UGR cronos finales/11-Portugos_HIGH_PISY/11-PH_cores.rwl")
  pisyhigh=read.rwl("data/raw/cronos/11-Portugos_HIGH_PISY/11-PH_cores.rwl")

  pisyhigh$year=as.numeric(rownames(pisyhigh))
  #RW
  pisyhighRW=RWgraph(pisyhigh, 3, 5)
  pisyhighRW=aggregate(pisyhighRW[, c(1,3)], by=list(year=pisyhighRW$year, pie=pisyhighRW$pie), mean)
  #write.csv(pisyhighRW, "/Users/guigeiz/Documents/Proyectos/LIFE Granada Regino y Antonio/Tablas y datos analisis/11RW_PISY_high.csv")

  RW_m2=aggregate(pisyhighRW[,c(3)], by=list(pisyhighRW$year), mean)
  RW_s=aggregate(pisyhighRW[,c(3)], by=list(pisyhighRW$year), sd)
  RW_l=aggregate(pisyhighRW[,c(3)], by=list(pisyhighRW$year), length)
  colnames(RW_m2)=c("year", "RWmean")
  colnames(RW_s)=c("year", "RWsd")
  colnames(RW_l)=c("year", "n")
  #
  plot(RW_m2[,1],RW_m2[,2], type="l",xlim=c(1950,2022), ylim=c(0,10), xlab="Year", ylab=expression(bold(RW~(mm~year^-1))))
  xx=c(min(RW_m2[,1]):max(RW_m2[,1]), rev(min(RW_m2[,1]):max(RW_m2[,1])))
  yy=c(RW_m2[,2]-1.96*RW_s[,2]/sqrt(RW_l[,2]), rev(RW_m2[,2]+1.96*RW_s[,2]/sqrt(RW_l[,2])))
  polygon(xx,yy, col=rgb(0,0,0,0.3), border=NA)
  ##
  #### BAI
  ####### BAI  #####
  ###MACRO PARA CALCULARSE BAI POR PARCELA
  ### CUIDADO CON SERIES QUE NO EMPIEZAN EN LA MISMA FECHA QUE EL FINAL (PUEDEN METER SESGO): por eso uso BAItree
  ### BAI calculado por árbol para evitar sesgo de series que no lleguen hasta el final ("flotantes")
  ##
  #### CALCULOS RW, BAI, IBIO


  BAItree=function(daps, rwls)
  {
    BAIdata=NULL
    #
    for (i in 1:(ncol(rwls)-1))
    {
      Var=as.data.frame(cbind(as.numeric((rwls[,i])), as.numeric(rwls$year)))
      Var=subset(Var, !is.na(Var[,1]))
      colnames(Var)=c("Growth", "year")
      Var$age1=seq(1, nrow(Var), 1)
      #parcela2=as.numeric(substr(colnames(rwls[i]), 3,4))
      arbol1=as.numeric(substr(colnames(rwls[i]),3,4))
      checkage=subset(daps, arbol==arbol1)
      Var$dapcc=as.numeric(c(checkage$dbh))
      Var$dapsc=10*(Var$dapcc-(2*(0.0809*Var$dapcc-0.00416)))   ###me la pas´ó dar´ío con datos de las parcelas que tomamos, lo paso todo a mm
      Var$tree=arbol1
      #
      BAIdata=rbind(BAIdata, Var)
    }
    #
    BAIdatatree=aggregate(BAIdata, by=list(BAIdata$year, BAIdata$tree), mean)
    BAIdatatree=BAIdatatree[order(BAIdatatree$tree, BAIdatatree$year),-c(4, 8)]
    colnames(BAIdatatree)=c("year", "tree", "Growth", "age1", "dapcc", "dapsc")
    #
    arboles=aggregate(BAIdatatree[,2], by=list(arbol=BAIdatatree$tree), mean)
    arboles=arboles[,1]

    ###ahora me calculo el BAI por pie
    BAIdata2=NULL
    #
    for (i in arboles)
    {
      ##Me calculo el BAI ahora
      Var=subset(BAIdatatree, tree==i)
      Var=Var[rev(order(Var$year)),]
      Var$RWcorr=Var[,3]*Var$dapsc/(2*sum(Var[,3]))
      Var$BAI=ifelse(2*sum(Var[,3])<max(Var$dapsc),(pi/4)*(Var$dapsc**2-(Var$dapsc-2*Var[,3])**2)/100,(pi/4)*(Var$dapsc**2-(Var$dapsc-2*Var[,7])**2)/100)   ##en cm2, podrÌa hacerse con pi r cuadrado, pero lo pongo como Piovesan et al 2008
      #lo paso todo a cm ahora
      for (vv in 2:nrow(Var))
      {
        Var$dapsc[vv]=ifelse(2*sum(Var[,3])<max(Var$dapsc),Var$dapsc[vv-1]-2*Var[vv-1,3], Var$dapsc[vv-1]-2*Var[vv-1,7]) #en mm
        Var$BAI[vv]=ifelse(2*sum(Var[,3])<max(Var$dapsc),(pi/4)*(Var$dapsc[vv]**2-(Var$dapsc[vv]-2*Var[vv,3])**2)/100,(pi/4)*(Var$dapsc[vv]**2-(Var$dapsc[vv]-2*Var[vv,7])**2)/100)   ##en cm2, podrÌa hacerse con pi r cuadrado, pero lo pongo como Piovesan et al 2008
      }
      #
      BAIdata2=rbind(BAIdata2, Var)
    }
    #
    #BAIdata2$plot=parcela2
    #
    Muestra=NULL
    for (i in min(BAIdata2$year):max(BAIdata2$year))
    {
      numer=c(i, nrow(BAIdata2[BAIdata2$year==i,]))
      Muestra=rbind(Muestra,numer)
    }
    Muestra<-Muestra
    Muestra2<-Muestra[Muestra[,2]>=3,]  ###3 pies son como mínimo 6 radios
    #
    BAIdata2<-BAIdata2[BAIdata2$year>=min(Muestra2[,1]),]
    #
    BAIdata_sd=aggregate(BAIdata2[,8], by=list(year1=BAIdata2$year), sd)
    #BAIdata_sd$year=BAIdata_sd$Group.1
    BAIdata3=aggregate(BAIdata2[, 2:8], by=list(year=BAIdata2$year), mean)   ###los NA son omitidos (na.action) y cuando hay sólo un radio coge el que hay como la media
    #
    BAIdata_sd[,2]=BAIdata_sd[,2]/sqrt(Muestra2[,2])
    colnames(BAIdata_sd)=c("year", "BAIserror")
    #
    BAIdata<<-as.data.frame(merge(BAIdata3, BAIdata_sd, by.x="year", by.y="year"))
  }


  ### BAI
  pisyhighBAI=BAItree(pies, pisyhigh)
  #write.csv(pisyhighBAI, "/Users/guigeiz/Documents/Proyectos/LIFE Granada Regino y Antonio/Tablas y datos analisis/11BAI_PISY_high.csv")

  #### Figure BAI
  #
  par(mgp=c(1.5,0.3,0), cex.lab=1, font.axis=2, font.lab=2, cex.lab=1, tck=0.2, tcl=0.2, las=1, mar=c(3,3,1,1))
  #
  plot(pisyhighBAI$year, pisyhighBAI$BAI, type="n", ylim=c(0, 15), xlim=c(1950,2022), xlab="Year", ylab=expression(bold(BAI~(cm^2~year^-1))))
  vv=seq(min(pisyhighBAI$year), max(pisyhighBAI$year),1)
  xxx=c(vv,rev(vv))
  yyy=c((pisyhighBAI$BAI-(1.96*pisyhighBAI$BAIserror)), rev(pisyhighBAI$BAI+(1.96*pisyhighBAI$BAIserror)))
  polygon(xxx, yyy, col=rgb(0,0,0,0.3), border=NA)
  lines(pisyhighBAI$year, pisyhighBAI$BAI, ylim=c(0, 13), lwd=1, col="black")
  #

  ############################################
  ######## estimate  AGB  #############
  ########################################################################################
  # Montero, G., Ruiz-Peinado, R. and Munoz, M., 2005. Producción de biomasa
  # y fijación de CO2 por los bosques españoles.
  # INIA-Instituto Nacional de Investigación y Tecnología Agraria y Alimentaria.
  ########################################################################################
  Biomass<-function(dbh, SEE,a, b){
    SEE_total=SEE
    CF_total=exp((SEE_total^2)/2)
    biomass= CF_total*(exp(a))*dbh^b
    return(biomass)
  }
  ########### IBIO
  IBIOpisy=function(daps, rwls, carbono_perc, SEE, a, b, SEE1, a1, b1, SEE2, a2, b2)    ###a, b parámetros alometrís dap, pp es la parcela de donde se saca la distancia para calcular valores por m2
  {
    BIOdata=NULL
    #
    for (i in 1:(ncol(rwls)-1))
    {
      Var=as.data.frame(cbind(as.numeric((rwls[,i])), as.numeric(rwls$year)))
      Var=subset(Var, !is.na(Var[,1]))
      colnames(Var)=c("Growth", "year")
      Var$age1=seq(1, nrow(Var), 1)
      arbol1=as.numeric(substr(colnames(rwls[i]),3,4))
      checkage=subset(daps, arbol==arbol1)
      Var$dapcc=as.numeric(checkage$dbh)
      Var$dapsc=10*(Var$dapcc-(2*(0.0809*Var$dapcc-0.00416)))   ###pongo las ecuaciones de dar´ío de datos que tomamos en parcelas dap-CORTEZA
      #dapsc en mm porque growth esta en mm
      Var$tree=arbol1
      #
      BIOdata=rbind(BIOdata, Var)
    }
    #
    BIOdatatree=aggregate(BIOdata, by=list(BIOdata$year, BIOdata$tree), mean)
    #
    BIOdatatree=BIOdatatree[order(BIOdatatree$tree, BIOdatatree$year),-c(4, 8)]
    colnames(BIOdatatree)=c("year", "tree", "Growth", "age1", "dapcc", "dapsc")
    #
    arboles=aggregate(BIOdatatree[,2], by=list(arbol=BIOdatatree$tree), mean)
    arboles=arboles[,1]
    #
    ###ahora me calculo el IBIO por pie
    BIOdata2=NULL
    #
    for (i in arboles)
    {
      ##
      Var=subset(BIOdatatree, tree==i)
      Var=Var[rev(order(Var$year)),]
      Var$RWcorr=Var[,3]*Var$dapsc/(2*sum(Var[,3]))   # en mm
      Var$dapcc_t1=(ifelse(2*sum(Var[,3])<max(Var$dapsc),0.1*(Var$dapsc-2*Var$Growth), 0.1*(Var$dapsc-2*Var$RWcorr))-0.00832)/0.8382   ###en cm el dap otra vez, supongo la relación corteza-diámetro ad-hoc PIHA SN
      Var$dapcc_t1=ifelse(Var$dapcc_t1<0, 0, Var$dapcc_t1)  ##a veces en las estimaciones el año 1 salian negativas, asumo que entonces el año 0 era 0 el diametro, en este caso
      # dap en las formulas en cm
      BT1=Biomass(Var$dapcc, SEE, a,b)-Biomass(Var$dapcc_t1, SEE, a,b)
      BT=Biomass(Var$dapcc, SEE, a,b)
      ST1=Biomass(Var$dapcc, SEE1, a1,b1)-Biomass(Var$dapcc_t1, SEE1, a1,b1)
      ST=Biomass(Var$dapcc, SEE1, a1,b1)
      IBR=Biomass(Var$dapcc, SEE2, a2,b2)-Biomass(Var$dapcc_t1, SEE2, a2,b2)
      BR=Biomass(Var$dapcc, SEE2, a2,b2)
      #
      Var$IBT_ag=BT1*carbono_perc*1000   ##en g C /año, biomasa a´érea total
      Var$IST=ST1*carbono_perc*1000   ##en g C /año, biomasa a´érea total
      Var$IBR=IBR*carbono_perc*1000  ###RA´ÍCES (pagina 33 Montero et al. tabla coeficientes)
      Var$BT_ag=BT*carbono_perc*1000
      Var$ST=ST*carbono_perc*1000
      Var$BR=BR*carbono_perc*1000
      #
      for (vv in 2:nrow(Var))
      {
        Var$dapsc[vv]=ifelse(2*sum(Var[,3])<max(Var$dapsc),Var$dapsc[vv-1]-2*Var[vv-1,3], Var$dapsc[vv-1]-2*Var[vv-1,7]) #en mm
        Var$dapcc[vv]=((0.1*Var$dapsc[vv])-0.00832)/0.8382  ## en cm, y hay que meter en cm el dap en la formula de corteza que sale en mm, cuidado!
        Var$dapcc_t1[vv]=(ifelse(2*sum(Var[,3])<max(Var$dapsc),0.1*(Var$dapsc[vv]-2*Var$Growth[vv]),0.1*(Var$dapsc[vv]-2*Var$RWcorr[vv]))-0.00832)/0.8382  ##en cm
        Var$dapcc_t1[vv]=ifelse(Var$dapcc_t1[vv]<0, 0, Var$dapcc_t1[vv])
        #
        BT1[vv]=Biomass(Var$dapcc[vv], SEE, a,b)-Biomass(Var$dapcc_t1[vv],SEE, a,b)
        BT[vv]=Biomass(Var$dapcc[vv], SEE, a,b)
        ST1[vv]=Biomass(Var$dapcc[vv],SEE1, a1,b1)-Biomass(Var$dapcc_t1[vv], SEE1, a1,b1)
        ST[vv]=Biomass(Var$dapcc[vv], SEE1, a1,b1)
        IBR[vv]=Biomass(Var$dapcc[vv], SEE2, a2,b2)-Biomass(Var$dapcc_t1[vv], SEE2, a2,b2)
        BR[vv]=Biomass(Var$dapcc[vv], SEE2, a2,b2)
        #
        Var$IBT_ag[vv]=BT1[vv]*carbono_perc*1000   ##en g C /año, biomasa a´érea total
        Var$IST[vv]=ST1[vv]*carbono_perc*1000   ##en g C /año, biomasa a´érea total
        Var$IBR[vv]=IBR[vv]*carbono_perc*1000  ###RA´ÍCES (pagina 33 Montero et al. tabla coeficientes)
        Var$BT_ag[vv]=BT[vv]*carbono_perc*1000
        Var$ST[vv]=ST[vv]*carbono_perc*1000
        Var$BR[vv]=BR[vv]*carbono_perc*1000
      }
      #
      #plot(Var$year, Var$IBIO_stem, type="l", xlim=c(1959, 2017))
      #lines(Var$year, Var$IBIO_root, col="red")
      #par(new=TRUE)
      #plot(BAI2$year, BAI2$BAI, ylim=c(0, 11), lwd=3, col="blue", type="l")

      BIOdata2=rbind(BIOdata2, Var)
    }
    #
    #BIOdata2$plot=parcela2
    #
    if(FALSE)
    {
      Muestra=NULL
      for (i in min(BIOdata2$year):max(BIOdata2$year))
      {
        numer=c(i, nrow(BIOdata2[BIOdata2$year==i,]))
        Muestra=rbind(Muestra,numer)
      }
      Muestra<-Muestra
      Muestra2<-Muestra[Muestra[,2]>=3,]  ###3 pies son como mínimo 6 radios
      #
      BAIdata2<-BAIdata2[BAIdata2$year>=min(Muestra2[,1]),]
      #
    }
    #
    BIOdata=aggregate(BIOdata2[,9:14], by=list(year1=BIOdata2$year), sum)
    #
    BIOdata$IBT_ag_m2=BIOdata$IBT_ag*1/(pi*(max(daps$distance_m)^2)+(0.5*max(daps$distance_m)^2)/nrow(daps))   ###por m2 TOTAL ABOVEGROUND
    BIOdata$IST_m2=BIOdata$IST*1/(pi*(max(daps$distance_m)^2)+(0.5*max(daps$distance_m)^2)/nrow(daps))   ###por m2 FUSTE (STEM)
    BIOdata$IBR_m2=BIOdata$IBR*1/(pi*(max(daps$distance_m)^2)+(0.5*max(daps$distance_m)^2)/nrow(daps))  ### por m2 RA´ÍCES (ROOTS)
    BIOdata$BT_m2=BIOdata$BT_ag*1/(pi*(max(daps$distance_m)^2)+(0.5*max(daps$distance_m)^2)/nrow(daps))   ###
    BIOdata$ST_m2=BIOdata$ST*1/(pi*(max(daps$distance_m)^2)+(0.5*max(daps$distance_m)^2)/nrow(daps)) ###por m2
    BIOdata$BR_m2=BIOdata$BR*1/(pi*(max(daps$distance_m)^2)+(0.5*max(daps$distance_m)^2)/nrow(daps))
    #
    return(BIOdata)
  }
  ### ABI
  IBIO11=IBIOpisy(pies, pisyhigh, 0.5, SEE=0.246887, a=-2.50275, b=2.41194, SEE1=0.290498, a1=-3.80519, b1=2.70808, SEE2=0.283615, a2=-4.56044, b2=2.62841)
  #write.csv(IBIO11,"/Users/guigeiz/Documents/Proyectos/LIFE Granada Regino y Antonio/Tablas y datos analisis/11BIO_PISY_high.csv" )
  plot(IBIO11$year1, IBIO11$IBT_ag_m2, type="l", xlim=c(1950, 2022))
  par(new=TRUE, yaxt="n")
  plot(pisyhighBAI$year, pisyhighBAI$BAI, col="red", type="l", xlim=c(1950, 2022))
  par(yaxt="s")
  axis(4)
  cor.test(IBIO11$IBT_ag_m2[3:nrow(IBIO11)], pisyhighBAI$BAI)



  library(dplR)

  psyl_high <- read.rwl("data/raw/cronos/11-Portugos_HIGH_PISY/11-PH_cores.rwl")

  nrow(psyl_high)
  ncol(psyl_high)
  colnames(psyl_high)
  plot(psyl_high, plot.type = "spag")
  rwl.report(psyl_high)

  s <- rwl.stats(psyl_high)
  mean(s$mean)
  mean(s$median)
  mean(s$ar1)

  ```{r}
  #RW
  ### Cuando tenga todas, puedo hacer lo siguiente:
  # Leer todos los rw_tree y luego hacer esto de forma bulk y plotearlos
  avg_year <- rw_tree |>
    group_by(year) |>
    summarise(rw_mean = mean(rw, na.rm = TRUE),
              rw_sd = sd(rw, na.rm = TRUE),
              n = length(rw)) |>
    mutate(sp_elev = sp_elev) |>
    rowwise() |>
    mutate(
      upper = rw_mean + (1.96*rw_sd)/sqrt(n),
      lower = rw_mean - (1.96*rw_sd)/sqrt(n))
  ###


  RW_m2=aggregate(pisymedRW[,c(3)], by=list(pisymedRW$year), mean)
  RW_s=aggregate(pisymedRW[,c(3)], by=list(pisymedRW$year), sd)
  RW_l=aggregate(pisymedRW[,c(3)], by=list(pisymedRW$year), length)
  colnames(RW_m2)=c("year", "RWmean")
  colnames(RW_s)=c("year", "RWsd")
  colnames(RW_l)=c("year", "n")
  #
  plot(RW_m2[,1],RW_m2[,2], type="l",xlim=c(1950,2022), ylim=c(0,10), xlab="Year", ylab=expression(bold(RW~(mm~year^-1))))
  xx=c(min(RW_m2[,1]):max(RW_m2[,1]), rev(min(RW_m2[,1]):max(RW_m2[,1])))
  yy=c(RW_m2[,2]-1.96*RW_s[,2]/sqrt(RW_l[,2]), rev(RW_m2[,2]+1.96*RW_s[,2]/sqrt(RW_l[,2])))
  polygon(xx,yy, col=rgb(0,0,0,0.3), border=NA)
  ##
  ```











