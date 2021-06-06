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
     WXDataplot=meteo_pull_monitors(
       monitors=WXStn,
       keep_flags = FALSE,
       date_min = date_min,
       date_max = date_max,
       var = c("TMAX","TMIN","PRCP")
     )
     
     WXDataplot$tmaxid=WXStn
     WXDataplot$tminid=WXStn
     WXDataplot$prcpid=WXStn
     WXDataplot$tmaxDis=WXDistance
     WXDataplot$tminDis=WXDistance
     WXDataplot$prcpDis=WXDistance
     ################prcp_plot
     prcp_plot=ggplot(WXDataplot, aes(x=date,y=prcpDis, col=prcpid))+
       geom_point(size=0.5)+
       xlim(as.Date(c(date_min,date_max)))+
       ylim(0,StnRadius)+
       labs(title = 'Precipitation', x = 'Date', y = 'Distance (km)')
     ####################tmax plot
     tmax_plot=ggplot(WXDataplot, aes(x=date,y=tmaxDis, col=tmaxid))+
       geom_point(size=0.5)+
       xlim(as.Date(c(date_min,date_max)))+
       ylim(0,StnRadius)+
       labs(title = 'Maximum Temperature', x = 'Date', y = 'Distance (km)')
     ####################################tmin plot
     tmin_plot=ggplot(WXDataplot, aes(x=date,y=tminDis, col=tminid))+
       geom_point(size=0.5)+
       xlim(as.Date(c(date_min,date_max)))+
       ylim(0,StnRadius)+
       labs(title = 'Minimum Temperature', x = 'Date', y = 'Distance (km)')
     for (i in 2:length(ustns[,1])){
       WXStn=ustns[order(ustns$distance),]$id[i]
       WXDistance=ustns[order(ustns$distance),]$distance[i]
       WXDataplot=meteo_pull_monitors(
         monitors=WXStn,
         keep_flags = FALSE,
         date_min = date_min,
         date_max = date_max,
         var = c("TMAX","TMIN","PRCP")
       )
       if("prcp" %in% colnames(WXDataplot)){
         WXDataplot$prcpid=WXStn
         WXDataplot$prcpDis=WXDistance
         prcp_plotFinal=prcp_plot+geom_point(aes(x=date,y=prcpDis, col=prcpid),data=WXDataplot,size=0.5)
         prcp_plot=prcp_plotFinal}
       if("tmax" %in% colnames(WXDataplot)){
         WXDataplot$tmaxid=WXStn
         WXDataplot$tmaxDis=WXDistance
         tmax_plotFinal=tmax_plot+geom_point(aes(x=date,y=tmaxDis, col=tmaxid),data=WXDataplot,size=0.5)
         tmax_plot=tmax_plotFinal}  
       if("tmin" %in% colnames(WXDataplot)){
         WXDataplot$tminid=WXStn
         WXDataplot$tminDis=WXDistance
         tmin_plotFinal=tmin_plot+geom_point(aes(x=date,y=tminDis, col=tminid),data=WXDataplot,size=0.5)
         tmin_plot=tmin_plotFinal}  
     }
     plot(tmax_plot)
     plot(tmin_plot)
     plot(prcp_plot)
  #################################################
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
        colnames(modeldata)[2]="id"
        colnames(modeldata)[which(colnames(modeldata)=="prcp.x")]="prcp"
        colnames(modeldata)[which(colnames(modeldata)=="tmax.x")]="tmax"
        colnames(modeldata)[which(colnames(modeldata)=="tmin.x")]="tmin"
        
      } 
    }
    modeldata$MaxTemp=as.numeric(modeldata$tmax)/10# Converting to C
    modeldata$MinTemp=as.numeric(modeldata$tmin)/10 # Converting to C
    modeldata$P=as.numeric(modeldata$prcp)/10 # Converting to mm
    plot_prcp=ggplot(modeldata, aes(date,prcpDis, col=prcpid))+
      geom_point()+
      labs(title = 'Precipitation', x = 'Date', y = 'Distance of station from the outlet (km)')
    plot(plot_prcp)
    prcp_distance=ggplot(modeldata, aes(date,P, col=prcpid))+
      geom_point()+
      labs(title = 'Precipitation', x = 'Date', y = 'Precipitation (mm)')
    plot(prcp_distance)
    ##############Tmax
   plot_tmax= ggplot(modeldata, aes(date,tmaxDis, col=tmaxid))+
      geom_point()+
      labs(title = 'Maximum Temperature', x = 'Date', y = 'Distance of station from the outlet (km)')
   plot(plot_tmax)
    tmax_distance=ggplot(modeldata, aes(date,MaxTemp, col=tmaxid))+
      geom_point()+
      labs(title = 'Maximum Temperature', x = 'Date', y = 'Maximum Temperature (C)')
    plot(tmax_distance)
    ######MinTemp
    plot_tmin=ggplot(modeldata, aes(date,tminDis, col=tminid))+
      geom_point()+
      labs(title = 'Minimum Temperature', x = 'Date', y = 'Distance of station from the outlet (km)')
    plot(plot_tmin)
   tmin_distance= ggplot(modeldata, aes(date,MinTemp, col=tminid))+
      geom_point()+
      labs(title = 'Minimum Temperature', x = 'Date', y = 'Minimum Temperature (C)')
   plot(tmin_distance)
    return(modeldata)
  }
}
