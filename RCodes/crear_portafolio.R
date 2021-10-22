crear_portafolio<-function(portafolio){
  
  vector=read_xls(paste("Informacion/VectorAnaliticoMD.xls",sep=""))
  vector=read.xlsx2(paste("Informacion/VectorAnaliticoMD.xls",sep=""),sheetName="MD")
  vector$Llave=paste(vector$TIPO.VALOR,"_",vector$EMISORA,"_",vector$SERIE,sep="")
  
  if(unique(is.numeric(substr(vector$FECHA.VCTO[match(portafolio$Llave,vector$Llave)][!is.na(vector$FECHA.VCTO[match(portafolio$Llave,vector$Llave)])],7,10)))){
    vector$FECHA.VCTO=as.Date(vector$FECHA.VCTO,"%d/%m/%Y")
  }else{
    vector$FECHA.VCTO=as.Date(vector$FECHA.VCTO,"%Y-%m-%d")
  }
  
  Tipovaluacion=data.frame(read_excel("Informacion/Supuestos_Generales_Activo.xlsx", sheet = "Tipo_Descuento",skip = 0))# %>% head(n = numvalores_dinero)
  portafolio$tc=as.numeric(as.character(vector$CUPON.ACTUAL[match(portafolio$Llave,vector$Llave)]))/100
  portafolio$st=as.numeric(as.character(vector$SOBRETASA[match(portafolio$Llave,vector$Llave)]))/100
  portafolio$tc2=0
  portafolio$tc2[!as.character(vector$REGLA.CUPON[match(portafolio$Llave,vector$Llave)])%in%c("0","Tasa Fija","Nota de Tasa Flotante")]=portafolio$st[!as.character(vector$REGLA.CUPON[match(portafolio$Llave,vector$Llave)])%in%c("0","Tasa Fija","Nota de Tasa Flotante")]
  portafolio$tc2[is.na(as.character(vector$REGLA.CUPON[match(portafolio$Llave,vector$Llave)]))]=0
  portafolio$tc2[portafolio$TV%in%c("LD","IM","IQ")]=0
  portafolio$pc=as.character(vector$FREC..CPN[match(portafolio$Llave,vector$Llave)])
  portafolio$pc[portafolio$pc=="NA"]=0
  portafolio$pc=as.numeric(gsub("Dias","",portafolio$pc))
  unique(portafolio$pc)
  portafolio$vn=as.numeric(as.character(vector$VALOR.NOMINAL[match(portafolio$Llave,vector$Llave)]))
  portafolio$precio_sucio=as.numeric(as.character(vector$PRECIO.SUCIO[match(portafolio$Llave,vector$Llave)]))
  portafolio$dxv=as.numeric(vector$FECHA.VCTO[match(portafolio$Llave,vector$Llave)]-as.Date(paste(diaval,"/",mesval,"/",añoval,sep=""),"%d/%m/%Y"))
  portafolio$año_Venc=as.numeric(substr(as.character(vector$FECHA.VCTO[match(portafolio$Llave,vector$Llave)]),1,4))
  portafolio$moneda=as.character(vector$MONEDA.EMISION[match(portafolio$Llave,vector$Llave)])
  unique(portafolio$moneda)
  portafolio$moneda[portafolio$moneda=="USD" ]="USD"
  portafolio$moneda[portafolio$moneda=="MPS" ]="MXN"
  portafolio$moneda[portafolio$moneda=="MUD" ]="UDI"
  portafolio$dtcv=as.numeric(as.character(vector$DIAS.TRANSC..CPN[match(portafolio$Llave,vector$Llave)]))
  portafolio$dtcv[is.na(portafolio$dtcv)]=0
  portafolio$cxc=as.numeric(as.character(vector$CUPONES.X.COBRAR[match(portafolio$Llave,vector$Llave)]))
  portafolio$cxc[portafolio$cxc=="NA"]=0
  portafolio$rc=as.character(vector$REGLA.CUPON[match(portafolio$Llave,vector$Llave)])
  portafolio$rc[portafolio$rc%in%c("0","Tasa Fija","Nota de Tasa Flotante")]="Fija"
  portafolio$rc[is.na(portafolio$rc)]=""
  portafolio$rc[!(portafolio$rc%in%c("Fija",""))]="Revisable"
  portafolio$vna=as.numeric(as.character(vector$VALOR.NOMINAL.ACTUALIZADO[match(portafolio$Llave,vector$Llave)]))
  portafolio$amor=portafolio$vna-portafolio$vn
  portafolio$amor[is.na(portafolio$amor)]=""
  portafolio$cupon=portafolio$TITULOS*portafolio$vn*portafolio$tc*portafolio$pc/360
  portafolio$valornominal=portafolio$vn*portafolio$TITULOS
  portafolio$plazo_pag_cupon[portafolio$pc!=0]="Cada"
  portafolio$plazo_duracion_cupon[portafolio$pc!=0]= portafolio$pc[portafolio$pc!=0]
  portafolio$plazo_temporalidad_cupon=""
  Aux=as.character(vector$REGLA.CUPON[match(portafolio$Llave,vector$Llave)])
  Aux[is.na(Aux)]="0"
  portafolio$Tasa_Referencia_1[Aux=="TIIE28"]="TIIE"
  portafolio$Plazo_TR1[Aux=="TIIE28"]=28
  portafolio$Tasa_Referencia_2[Aux=="TIIE28"]="TIIE"
  portafolio$Plazo_TR2[Aux=="TIIE28"]=28
  portafolio$Tasa_Referencia_1[Aux=="IRMXP-FGub-28"]="CETE"
  portafolio$Plazo_TR1[Aux=="IRMXP-FGub-28"]=28
  portafolio$Tasa_Referencia_2[Aux=="IRMXP-FGub-28"]="TPFG"
  portafolio$Plazo_TR2[Aux=="IRMXP-FGub-28"]=1
  portafolio$Tasa_Referencia_1[Aux=="IRMXP-FGub-91"]="CETE"
  portafolio$Plazo_TR1[Aux=="IRMXP-FGub-91"]=91
  portafolio$Tasa_Referencia_2[Aux=="IRMXP-FGub-91"]="TPFG"
  portafolio$Plazo_TR2[Aux=="IRMXP-FGub-91"]=1
  portafolio$Tasa_Referencia_1[Aux=="CETE182"]="CETE"
  portafolio$Plazo_TR1[Aux=="CETE182"]=182
  portafolio$Tasa_Referencia_2[Aux=="CETE182"]="UDI"
  portafolio$Plazo_TR2[Aux=="CETE182"]=1
  portafolio$Tasa_Referencia_1[Aux=="Fondeo Bancario"]="TPFB"
  portafolio$Plazo_TR1[Aux=="Fondeo Bancario"]=1
  portafolio$Tasa_Referencia_2[Aux=="Fondeo Bancario"]="TPFB"
  portafolio$Plazo_TR2[Aux=="Fondeo Bancario"]=1
  portafolio$Tasa_Referencia_1[Aux=="TREASURY 5Y BENCHMARK"]="Treasury"
  portafolio$Plazo_TR1[Aux=="TREASURY 5Y BENCHMARK"]=5
  portafolio$Tasa_Referencia_2[Aux=="TREASURY 5Y BENCHMARK"]="Treasury"
  portafolio$Plazo_TR2[Aux=="TREASURY 5Y BENCHMARK"]=5
  portafolio$Curva_Valuacion=as.character(vector$SUBYACENTE[match(portafolio$Llave,vector$Llave)])
  portafolio$Curva_Valuacion[portafolio$Curva_Valuacion=="TIIE28"]="TIIE"
  portafolio$Curva_Valuacion[portafolio$Curva_Valuacion=="Bonos M Bruta(Yield)"]="BonoM_Yield"
  portafolio$Curva_Valuacion[portafolio$Curva_Valuacion=="Real Bruta (Yield) "]="RealBrutaY"
  portafolio$Curva_Valuacion[portafolio$Curva_Valuacion=="Zero Real Bruta"]="RealBrutaZ"
  portafolio$Curva_Valuacion[portafolio$Curva_Valuacion=="AAA"]="Pagares_AAA"
  portafolio$Curva_Valuacion[portafolio$Curva_Valuacion=="Libor"]="Libor_IRS"
  portafolio$Curva_Valuacion[portafolio$TV=="IM"]="IM"
  portafolio$Curva_Valuacion[portafolio$TV=="IQ"]="IQ"
  portafolio$Curva_Valuacion[portafolio$TV=="LD"]="LD"
  portafolio$Curva_Valuacion[portafolio$TV=="IS"]="IS"
  portafolio$Curva_Valuacion[portafolio$TV=="M"]="BonoM_Yield"
  portafolio$Curva_Valuacion[portafolio$TV=="BI"]="BI"
  portafolio$Curva_Valuacion[portafolio$TV%in%c("D1","D1SP")]="UMS"
  portafolio$Curva_Valuacion[portafolio$TV%in%c("D2","D2SP")]="Libor_IRS"
  portafolio$Curva_Valuacion[portafolio$TV%in%c("D6","D6SP")]="Libor_IRS"
  portafolio$vn[portafolio$vn==0]=NA
  portafolio$Formula_Valuacion=paste(Tipovaluacion$Tipo_Instrumento[match(portafolio$TV,Tipovaluacion$Tipo_Valor)],ifelse(is.na(Tipovaluacion$Cupon[match(portafolio$TV,Tipovaluacion$Tipo_Valor)]),"",Tipovaluacion$Cupon[match(portafolio$TV,Tipovaluacion$Tipo_Valor)]),"_",Tipovaluacion$Tipo_Interes[match(portafolio$TV,Tipovaluacion$Tipo_Valor)],sep="")
  portafolio$Valor=Tipovaluacion$Valor[match(portafolio$TV,Tipovaluacion$Tipo_Valor)]
  portafolio$Di=Tipovaluacion$Di[match(portafolio$TV,Tipovaluacion$Tipo_Valor)]
  portafolio$P=Tipovaluacion$P[match(portafolio$TV,Tipovaluacion$Tipo_Valor)]
  portafolio$Amortizacion=ifelse(portafolio$vn!=portafolio$vna,1,0)
  portafolio[is.na(portafolio)]=""
  portafolio$tc2[portafolio$rc=="Revisable" & !portafolio$TV%in%c("LD","IM","IQ") & portafolio$Tasa_Referencia_1%in%c("TIIE","Treasury")]=portafolio$tc2[portafolio$rc=="Revisable" & !portafolio$TV%in%c("LD","IM","IQ") & portafolio$Tasa_Referencia_1%in%c("TIIE","Treasury")]+as.numeric(as.character(vector$PRECIO.SUCIO[vector$EMISORA=="TIIE" &vector$SERIE=="28"]))/100
  return(portafolio)
}
