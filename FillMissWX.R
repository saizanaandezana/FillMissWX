FillMissWX=function(declat, declon,StnRadius,date_min,date_max,method){
  if (method == "IDW"){
    
    stns=meteo_distance(
      station_data=ghcnd_stations(),
      lat=declat,
      long=declon,
      units = "deg",
      radius = StnRadius,
      limit = NULL
    )
    ustns=unique(data.frame(id=stns$id,distance=stns$distance)[c("id", "distance")])
    WXStn=ustns[order(ustns$distance),]$id[1]
    WXDistance=ustns[order(ustns$distance),]$distance[1]
    modeldata=meteo_pull_monitors(
      monitors=WXStn,
      keep_flags = FALSE,
      date_min = date_min,
      date_max = date_max,
      var = c("TMAX","TMIN","PRCP")
    )
    modeldata$tmaxid=WXStn
    modeldata$tminid=WXStn
    modeldata$prcpid=WXStn
    modeldata$tmaxDis=WXDistance
    modeldata$tminDis=WXDistance
    modeldata$prcpDis=WXDistance
    if("prcp" %in% colnames(modeldata)){
      modeldata$prcp=modeldata$prcp*(1/WXDistance)
    } else {
      modeldata$prcp=NA
    }
    if("tmax" %in% colnames(modeldata)){
      modeldata$tmax=modeldata$tmax*(1/WXDistance)
    } else {
      modeldata$tmax=NA
    }
    
    if("tmin" %in% colnames(modeldata)){
      modeldata$tmin=modeldata$tmin*(1/WXDistance)
    } else{
      modeldata$tmin=NA
    }
    modeldata$DenPrcp=0
    modeldata$DenTmin=0
    modeldata$DenTmax=0
    if("prcp" %in% colnames(modeldata)){
      modeldata$DenPrcp[!is.na(modeldata$prcp)]=1/WXDistance
    }
    if("tmax" %in% colnames(modeldata)){
      modeldata$DenTmax[!is.na(modeldata$tmax)]=1/WXDistance
    }
    if("tmax" %in% colnames(modeldata)){
      modeldata$DenTmin[!is.na(modeldata$tmin)]=1/WXDistance
    }
    for (i in 2:length(ustns[,1])){
      WXStn=ustns[order(ustns$distance),]$id[i]
      WXDistance=ustns[order(ustns$distance),]$distance[i]
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
          modeldata$DenPrcp[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]=modeldata$DenPrcp[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]+(1/WXDistance)
          modeldata$DenPrcp[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]=modeldata$DenPrcp[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]+1/WXDistance
        }
        if("tmax.y" %in% colnames(modeldata)){
          modeldata$DenTmax[is.na(modeldata$DenTmax)]=0
          modeldata$DenTmax[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]=modeldata$DenTmax[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]+(1/WXDistance)
          modeldata$DenTmax[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]=modeldata$DenTmax[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]+1/WXDistance       
        }
        if("tmin.y" %in% colnames(modeldata)){      
          modeldata$DenTmin[is.na(modeldata$DenTmin)]=0
          modeldata$DenTmin[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]=modeldata$DenTmin[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]+(1/WXDistance)
          modeldata$DenTmin[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]=modeldata$DenTmin[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]+1/WXDistance
        }
        if("prcp.y" %in% colnames(modeldata)){     
          modeldata$prcpid[is.na(modeldata$prcp.x)]=WXStn
          modeldata$prcpDis[is.na(modeldata$prcp.x)]=WXDistance
          modeldata$prcp.x[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]=modeldata$prcp.x[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]+
            modeldata$prcp.y[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]*(1/WXDistance)
          modeldata$prcp.x[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]=modeldata$prcp.y[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]*(1/WXDistance)
          modeldata=subset(modeldata,select = -c(prcp.y))
        }
        if("tmin.y" %in% colnames(modeldata)){
          
          modeldata$tminid[is.na(modeldata$tmin.x)]=WXStn
          modeldata$tminDis[is.na(modeldata$tmin.x)]=WXDistance
          modeldata$tmin.x[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]=modeldata$tmin.x[!is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]+
            modeldata$tmin.y[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]*(1/WXDistance)
          modeldata$tmin.x[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]=modeldata$tmin.y[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]*(1/WXDistance)
          modeldata=subset(modeldata,select = -c(tmin.y))
        }
        if("tmax.y" %in% colnames(modeldata)){
          
          modeldata$tmaxid[is.na(modeldata$tmax.x)]=WXStn
          modeldata$tmaxDis[is.na(modeldata$tmax.x)]=WXDistance
          modeldata$tmax.x[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]=modeldata$tmax.x[!is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]+
            modeldata$tmax.y[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]*(1/WXDistance)
          modeldata$tmax.x[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]=modeldata$tmax.y[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]*(1/WXDistance)
          modeldata=subset(modeldata,select = -c(tmax.y))
        } 
        modeldata=subset(modeldata,select = -c(id.y))
        #View(modeldata3)
        colnames(modeldata)[2]="id"
        colnames(modeldata)[which(colnames(modeldata)=="prcp.x")]="prcp"
        colnames(modeldata)[which(colnames(modeldata)=="tmax.x")]="tmax"
        colnames(modeldata)[which(colnames(modeldata)=="tmin.x")]="tmin"
        
        #if ((sum(is.na(modeldata$prcp)))<5&(sum(is.na(modeldata$tmax)))<5){
        #  stop("Enough")
        #}
      } 
    }
    modeldata$MaxTemp=as.numeric(modeldata$tmax)/10/modeldata$DenTmax# Converting to C
    modeldata$MinTemp=as.numeric(modeldata$tmin)/10/modeldata$DenTmin# Converting to C
    modeldata$P=as.numeric(modeldata$prcp)/10/modeldata$DenPrcp # Converting to mm
    return(modeldata)
  }
 if(method == "closest"){
     stns=meteo_distance(
       station_data=ghcnd_stations(),
       lat=declat,
       long=declon,
       units = "deg",
       radius = StnRadius,
       limit = NULL
     )
     ustns=unique(data.frame(id=stns$id,distance=stns$distance)[c("id", "distance")])
     WXStn=ustns[order(ustns$distance),]$id[1]
     WXDistance=ustns[order(ustns$distance),]$distance[1]
     modeldata=meteo_pull_monitors(
       monitors=WXStn,
       keep_flags = FALSE,
       date_min = date_min,
       date_max = date_max,
       var = c("TMAX","TMIN","PRCP")
     )
     modeldata$tmaxid=WXStn
     modeldata$tminid=WXStn
     modeldata$prcpid=WXStn
     modeldata$tmaxDis=WXDistance
     modeldata$tminDis=WXDistance
     modeldata$prcpDis=WXDistance
     for (i in 2:length(ustns[,1])){
       WXStn=ustns[order(ustns$distance),]$id[i]
       WXDistance=ustns[order(ustns$distance),]$distance[i]
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
           modeldata$prcp.x[is.na(modeldata$prcp.x)]=modeldata$prcp.y[is.na(modeldata$prcp.x)]
           modeldata=subset(modeldata,select = -c(prcp.y))
         }
         if("tmin.y" %in% colnames(modeldata)){
           
           modeldata$tminid[is.na(modeldata$tmin.x)]=WXStn
           modeldata$tminDis[is.na(modeldata$tmin.x)]=WXDistance
           modeldata$tmin.x[is.na(modeldata$tmin.x)]=modeldata$tmin.y[is.na(modeldata$tmin.x)]
           modeldata=subset(modeldata,select = -c(tmin.y))
         }
         if("tmax.y" %in% colnames(modeldata)){
           
           modeldata$tmaxid[is.na(modeldata$tmax.x)]=WXStn
           modeldata$tmaxDis[is.na(modeldata$tmax.x)]=WXDistance
           modeldata$tmax.x[is.na(modeldata$tmax.x)]=modeldata$tmax.y[is.na(modeldata$tmax.x)]
           modeldata=subset(modeldata,select = -c(tmax.y))
         } 
         modeldata=subset(modeldata,select = -c(id.y))
         #View(modeldata3)
         colnames(modeldata)[2]="id"
         colnames(modeldata)[which(colnames(modeldata)=="prcp.x")]="prcp"
         colnames(modeldata)[which(colnames(modeldata)=="tmax.x")]="tmax"
         colnames(modeldata)[which(colnames(modeldata)=="tmin.x")]="tmin"
         
       } 
     }
     modeldata$MaxTemp=as.numeric(modeldata$tmax)/10# Converting to C
     modeldata$MinTemp=as.numeric(modeldata$tmin)/10 # Converting to C
     modeldata$P=as.numeric(modeldata$prcp)/10 # Converting to mm
     return(modeldata)
   }
   
 }
