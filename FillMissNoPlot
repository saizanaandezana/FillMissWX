FillMissWX=function(declat, declon,StnRadius,minstns,date_min,date_max,method,alfa=2){
  station_data=ghcnd_stations()
  while(StnRadius<2000){
    print(StnRadius)
    stns=meteo_distance(
      station_data=station_data,
      lat=declat, long=declon,
      units = "deg",
      radius = StnRadius,
      limit = NULL
    )
    if(length(unique(stns$id))>minstns){break()}
    StnRadius=StnRadius*1.414
  }
  ustns=unique(data.frame(id=stns$id,distance=stns$distance,elevation=stns$elevation)[c("id", "distance","elevation")])
  ustns$idNum=substring(ustns$id, 3,3)
  #ustns$idNum=as.vector(ustns$idNum)
  ustns$source=NA
  #ustns$source=as.character(ustns$source)
  for(k in 1:nrow(ustns)){
    if(ustns$idNum[k]=="0"){
      ustns$source[k]="unspecified"
    }
    if (ustns$idNum[k]=="1"){
      ustns$source[k]="CoCoRaHS" 
    }
    if (ustns$idNum[k]=="C"){
      ustns$source[k]="COOP" 
    }
    if (ustns$idNum[k]=="E"){
      ustns$source[k]="ECA&D non-blended dataset" 
    }
    if (ustns$idNum[k]=="M"){
      ustns$source[k]="World Meteorological Organization ID" 
    }
    if (ustns$idNum[k]=="N"){
      ustns$source[k]="National Meteorological/Hydrological Center" 
    }  
    
    if (ustns$idNum[k]=="R"){
      ustns$source[k]="RAWS" 
    }  
    if (ustns$idNum[k]=="S"){
      ustns$source[k]="USNRCS-SNOTEL" 
    }
    if (ustns$idNum[k]=="W"){
      ustns$source[k]="WBAN" 
    }
  }
  
  WXStn=ustns[order(ustns$distance),]$id[1]
  WXDistance=ustns[order(ustns$distance),]$distance[1]
  WXElev=ustns$elevation[1]
  WXSource=ustns$source[1]
  modeldata=meteo_pull_monitors(
    monitors=WXStn,
    keep_flags = FALSE,
    date_min = date_min,
    date_max = date_max,
    var = c("TMAX","TMIN","PRCP")
  )
  if(!("prcp" %in% colnames(modeldata))){
    modeldata$prcp=NA
    
  }
  if(!("tmax" %in% colnames(modeldata))){
    modeldata$tmax=NA
    
  } 
  if(!("tmin" %in% colnames(modeldata))){
    modeldata$tmin=NA
  }
  modeldata$tmaxid=WXStn
  modeldata$tminid=WXStn
  modeldata$prcpid=WXStn
  modeldata$tmaxDis=WXDistance
  modeldata$tminDis=WXDistance
  modeldata$prcpDis=WXDistance
  modeldata$prcpElev=WXElev
  modeldata$tmaxElev=WXElev
  modeldata$tminElev=WXElev
  modeldata$PrcSource=WXSource
  modeldata$tmaxSource=WXSource
  modeldata$tminSource=WXSource
  #####Base plot(distance vs date) func for both IDW nd Closest#
  WXDataplot=modeldata
  
  if (method == "IDW"){
    #################################################
    modeldata$DenPrcp=0;modeldata$DenTmin=0;modeldata$DenTmax=0
    if("prcp" %in% colnames(modeldata)){
      modeldata$prcp=modeldata$prcp*(1/(modeldata$prcpDis^alfa))
      modeldata$DenPrcp[!is.na(modeldata$prcp)]=(1/(modeldata$prcpDis^alfa))
      modeldata$prcpElev[is.na(modeldata$prcp)]=NA
      modeldata$prcpElev[!is.na(modeldata$prcp)]=WXElev*(1/(modeldata$prcpDis^alfa))
      
    } else {
      modeldata$prcp=NA
    }
    if("tmax" %in% colnames(modeldata)){
      modeldata$tmax=modeldata$tmax*(1/(modeldata$tmaxDis^alfa))
      modeldata$DenTmax[!is.na(modeldata$tmax)]=(1/(modeldata$tmaxDis^alfa))
      modeldata$tmaxElev[is.na(modeldata$tmax)]=NA
      modeldata$tmaxElev[!is.na(modeldata$tmax)]=WXElev*(1/(modeldata$tmaxDis^alfa))
      
    } else {
      modeldata$tmax=NA
    }
    
    if("tmin" %in% colnames(modeldata)){
      modeldata$tmin=modeldata$tmin*(1/(modeldata$tminDis^alfa))
      modeldata$DenTmin[!is.na(modeldata$tmin)]=(1/(modeldata$tminDis^alfa))
      modeldata$tminElev[is.na(modeldata$tmin)]=NA
      modeldata$tminElev[!is.na(modeldata$tmin)]=WXElev*(1/(modeldata$tminDis^alfa))
    } else{
      modeldata$tmin=NA
    }
    ################################################
    for (i in 2:length(ustns[,1])){
      WXStn=ustns[order(ustns$distance),]$id[i]
      WXDistance=ustns[order(ustns$distance),]$distance[i]
      WXElev=ustns$elevation[i]
      WXSource=ustns$source[i]
      WXData=try(meteo_pull_monitors(
        monitors=WXStn,
        keep_flags = FALSE,
        date_min = date_min,
        date_max = date_max,
        var = c("TMAX","TMIN","PRCP")
      ))
      if (length(WXData$id)>5){
        modeldata=merge(modeldata,WXData,by.x=c("date"),by.y=c("date"),all = T)
        if("prcp.y" %in% colnames(modeldata)){
          modeldata$DenPrcp[is.na(modeldata$DenPrcp)]=0
          modeldata$DenPrcp[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]=modeldata$DenPrcp[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]+(1/(WXDistance^alfa))
          modeldata$DenPrcp[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]=modeldata$DenPrcp[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]+(1/(WXDistance^alfa))
          modeldata$prcpElev[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]=modeldata$prcpElev[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]+WXElev*(1/(WXDistance^alfa))
          modeldata$prcpElev[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]=WXElev*(1/(WXDistance^alfa))
          
          modeldata$prcp.x[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]=modeldata$prcp.x[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]+
            modeldata$prcp.y[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]*(1/(WXDistance^alfa))
          modeldata$prcp.x[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]=modeldata$prcp.y[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]*(1/(WXDistance^alfa))
          modeldata=subset(modeldata,select = -c(prcp.y))
        }
        if("tmax.y" %in% colnames(modeldata)){
          modeldata$DenTmax[is.na(modeldata$DenTmax)]=0
          modeldata$DenTmax[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]=modeldata$DenTmax[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]+(1/(WXDistance^alfa))
          modeldata$DenTmax[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]=modeldata$DenTmax[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]+(1/(WXDistance^alfa))      
          modeldata$tmaxElev[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]=modeldata$tmaxElev[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]+WXElev*(1/(WXDistance^alfa))
          modeldata$tmaxElev[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]=WXElev*(1/(WXDistance^alfa))         
          
          modeldata$tmax.x[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]=modeldata$tmax.x[!is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]+
            modeldata$tmax.y[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]*(1/(WXDistance^alfa))
          modeldata$tmax.x[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]=modeldata$tmax.y[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]*(1/(WXDistance^alfa))
          
          
          modeldata=subset(modeldata,select = -c(tmax.y))
        }
        if("tmin.y" %in% colnames(modeldata)){      
          modeldata$DenTmin[is.na(modeldata$DenTmin)]=0
          modeldata$DenTmin[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]=modeldata$DenTmin[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]+(1/(WXDistance^alfa))
          modeldata$DenTmin[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]=modeldata$DenTmin[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]+(1/(WXDistance^alfa))
          modeldata$tminElev[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]=modeldata$tminElev[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]+WXElev*(1/(WXDistance^alfa))
          modeldata$tminElev[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]=WXElev*(1/(WXDistance^alfa))             
          
          
          modeldata$tmin.x[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]=modeldata$tmin.x[!is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]+
            modeldata$tmin.y[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]*(1/(WXDistance^alfa))
          modeldata$tmin.x[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]=modeldata$tmin.y[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]*(1/(WXDistance^alfa))
          
          modeldata=subset(modeldata,select = -c(tmin.y))
        }
        
        modeldata=subset(modeldata,select = -c(id.y))
        colnames(modeldata)[2]="id"
        colnames(modeldata)[which(colnames(modeldata)=="prcp.x")]="prcp"
        colnames(modeldata)[which(colnames(modeldata)=="tmax.x")]="tmax"
        colnames(modeldata)[which(colnames(modeldata)=="tmin.x")]="tmin"
      } 
    }
    modeldata$MaxTemp=as.numeric(modeldata$tmax)/10/modeldata$DenTmax# Converting to C
    modeldata$MinTemp=as.numeric(modeldata$tmin)/10/modeldata$DenTmin# Converting to C
    modeldata$P=as.numeric(modeldata$prcp)/10/modeldata$DenPrcp # Converting to mm
    modeldata$prcpElevation=modeldata$prcpElev/modeldata$DenPrcp ###elevation of prcp station
    modeldata$tmaxElevation=modeldata$tmaxElev/modeldata$DenTmax
    modeldata$tminElevation=modeldata$tminElev/modeldata$DenTmin
    WXDataplot1=merge(modeldata, WXDataplot, by="date")
    WXDataplot1$weightedP=(1/WXDataplot1$prcpDis.y^alfa)/WXDataplot1$DenPrcp
    WXDataplot1$weightedTMX=(1/WXDataplot1$tmaxDis.y^alfa)/WXDataplot1$DenTmax
    WXDataplot1$weightedTMN=(1/WXDataplot1$tminDis.y^alfa)/WXDataplot1$DenTmin
    
    #WXDataplot1$weightedTMX=((1/WXDataplot1$tmaxDis.y^alfa))/WXDataplot1$DenTmax

    return(modeldata)
    
  }
  
  #############################################################################
  
  
  #################################################################################
  if(method == "closest"){
    for (i in 2:length(ustns[,1])){
      WXStn=ustns[order(ustns$distance),]$id[i]
      WXDistance=ustns[order(ustns$distance),]$distance[i]
      WXElev=ustns$elevation[i]
      WXSource=ustns$source[i]
      WXData=try(meteo_pull_monitors(
        monitors=WXStn,
        keep_flags = FALSE,
        date_min = date_min,
        date_max = date_max,
        var = c("TMAX","TMIN","PRCP")
      ))
      if (length(WXData$id)>5){
        modeldata=merge(modeldata,WXData,by.x=c("date"),by.y=c("date"),all = T)
        if("prcp.y" %in% colnames(modeldata)){
          
          modeldata$prcpid[is.na(modeldata$prcp.x)]=WXStn
          modeldata$prcpDis[is.na(modeldata$prcp.x)]=WXDistance
          modeldata$prcpElev[is.na(modeldata$prcp.x)]=WXElev
          modeldata$PrcSource[is.na(modeldata$prcp.x)]=WXSource
          
          
          modeldata$prcp.x[is.na(modeldata$prcp.x)]=modeldata$prcp.y[is.na(modeldata$prcp.x)]
          modeldata=subset(modeldata,select = -c(prcp.y))
        }
        if("tmin.y" %in% colnames(modeldata)){
          
          modeldata$tminid[is.na(modeldata$tmin.x)]=WXStn
          modeldata$tminDis[is.na(modeldata$tmin.x)]=WXDistance
          modeldata$tminElev[is.na(modeldata$tmin.x)]=WXElev
          modeldata$tminSource[is.na(modeldata$tmin.x)]=WXSource
          
          modeldata$tmin.x[is.na(modeldata$tmin.x)]=modeldata$tmin.y[is.na(modeldata$tmin.x)]
          modeldata=subset(modeldata,select = -c(tmin.y))
        }
        if("tmax.y" %in% colnames(modeldata)){
          
          modeldata$tmaxid[is.na(modeldata$tmax.x)]=WXStn
          modeldata$tmaxDis[is.na(modeldata$tmax.x)]=WXDistance
          modeldata$tmaxElev[is.na(modeldata$tmax.x)]=WXElev
          modeldata$tmaxSource[is.na(modeldata$tmax.x)]=WXSource
          modeldata$tmax.x[is.na(modeldata$tmax.x)]=modeldata$tmax.y[is.na(modeldata$tmax.x)]
          modeldata=subset(modeldata,select = -c(tmax.y))
        } 
        modeldata=subset(modeldata,select = -c(id.y))
        colnames(modeldata)[2]="id"
        colnames(modeldata)[which(colnames(modeldata)=="prcp.x")]="prcp"
        colnames(modeldata)[which(colnames(modeldata)=="tmax.x")]="tmax"
        colnames(modeldata)[which(colnames(modeldata)=="tmin.x")]="tmin"
      } 
    }
    colnames(modeldata)[which(colnames(modeldata)=="prcpElev")]="prcpElevation"
    colnames(modeldata)[which(colnames(modeldata)=="tmaxElev")]="tmaxElevation"
    colnames(modeldata)[which(colnames(modeldata)=="tminElev")]="tminElevation"
    modeldata$MaxTemp=as.numeric(modeldata$tmax)/10# Converting to C
    modeldata$MinTemp=as.numeric(modeldata$tmin)/10 # Converting to C
    modeldata$P=as.numeric(modeldata$prcp)/10 # Converting to mm
    return(modeldata)
  }
}
