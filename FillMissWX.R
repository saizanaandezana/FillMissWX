FillMissWX=function(declat, declon,StnRadius,date_min,date_max,method,alfa=2){
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
  #####Base plot(distance vs date) func for both IDW nd Closest#
  WXDataplot=modeldata
  
  if (method == "IDW"){
    #################################################
    modeldata$DenPrcp=0;modeldata$DenTmin=0;modeldata$DenTmax=0
    if("prcp" %in% colnames(modeldata)){
      modeldata$prcp=modeldata$prcp*(1/(modeldata$prcpDis^alfa))
      modeldata$DenPrcp[!is.na(modeldata$prcp)]=(1/(modeldata$prcpDis^alfa))
    } else {
      modeldata$prcp=NA
    }
    if("tmax" %in% colnames(modeldata)){
      modeldata$tmax=modeldata$tmax*(1/(modeldata$tmaxDis^alfa))
      modeldata$DenTmax[!is.na(modeldata$tmax)]=(1/(modeldata$tmaxDis^alfa))
    } else {
      modeldata$tmax=NA
    }
    
    if("tmin" %in% colnames(modeldata)){
      modeldata$tmin=modeldata$tmin*(1/(modeldata$tminDis^alfa))
      modeldata$DenTmin[!is.na(modeldata$tmin)]=(1/(modeldata$tminDis^alfa))
    } else{
      modeldata$tmin=NA
    }
    ################################################
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
          modeldata$DenPrcp[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]=modeldata$DenPrcp[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]+(1/(WXDistance^alfa))
          modeldata$DenPrcp[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]=modeldata$DenPrcp[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]+(1/(WXDistance^alfa))
          modeldata$prcp.x[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]=modeldata$prcp.x[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]+
            modeldata$prcp.y[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]*(1/(WXDistance^alfa))
          modeldata$prcp.x[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]=modeldata$prcp.y[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]*(1/(WXDistance^alfa))
          modeldata=subset(modeldata,select = -c(prcp.y))
        }
        if("tmax.y" %in% colnames(modeldata)){
          modeldata$DenTmax[is.na(modeldata$DenTmax)]=0
          modeldata$DenTmax[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]=modeldata$DenTmax[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]+(1/(WXDistance^alfa))
          modeldata$DenTmax[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]=modeldata$DenTmax[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]+(1/(WXDistance^alfa))      
          modeldata$tmax.x[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]=modeldata$tmax.x[!is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]+
            modeldata$tmax.y[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]*(1/(WXDistance^alfa))
          modeldata$tmax.x[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]=modeldata$tmax.y[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]*(1/(WXDistance^alfa))
          modeldata=subset(modeldata,select = -c(tmax.y))
        }
        if("tmin.y" %in% colnames(modeldata)){      
          modeldata$DenTmin[is.na(modeldata$DenTmin)]=0
          modeldata$DenTmin[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]=modeldata$DenTmin[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]+(1/(WXDistance^alfa))
          modeldata$DenTmin[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]=modeldata$DenTmin[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]+(1/(WXDistance^alfa))
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
    WXDataplot1=merge(modeldata, WXDataplot, by="date")
    WXDataplot1$weightedP=(((1/WXDataplot1$prcpDis.y^alfa)*(WXDataplot1$prcp.y/10))/WXDataplot1$DenPrcp)
    WXDataplot1$weightedTMX=(((1/WXDataplot1$tmaxDis.y^alfa)*(WXDataplot1$tmax.y/10))/WXDataplot1$DenTmax)
    WXDataplot1$weightedTMN=(((1/WXDataplot1$tminDis.y^alfa)*(WXDataplot1$tmin.y/10))/WXDataplot1$DenTmin)
    
    
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
    
    prcp_plotweight1=ggplot(data = WXDataplot1 ,aes(x=date,y=weightedP, col=prcpid.y))+
      geom_point(size=0.5)+
      xlim(as.Date(c(date_min,date_max)))+
      labs(title = 'Precipitation', x = 'Date', y = 'Weighted Precipitation (mm)')
    ####################tmax plot
    tmax_plotweight1=ggplot(WXDataplot1, aes(x=date,y=weightedTMX, col=tmaxid.y))+
      geom_point(size=0.5)+
      xlim(as.Date(c(date_min,date_max)))+
      labs(title = 'Maximum Temperature', x = 'Date', y = 'Weighted Maximum Temperature (C)')
    ####################################tmin plot
    
    tmin_plotweight1=ggplot(WXDataplot1, aes(x=date,y=weightedTMN, col=tminid.y))+
      geom_point(size=0.5)+
      xlim(as.Date(c(date_min,date_max)))+
      labs(title = 'Minimum Temperature', x = 'Date', y = 'Weighted Minimum Temperature (C)')
    
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
      
      WXDataplot$prcpid=WXStn
      WXDataplot$prcpDis=WXDistance
      WXDataplot$tmaxid=WXStn
      WXDataplot$tmaxDis=WXDistance
      WXDataplot$tminid=WXStn
      WXDataplot$tminDis=WXDistance 
      
      WXDataplot1=merge(modeldata, WXDataplot, by="date")
      
      if("prcp" %in% colnames(WXDataplot)){
        
        prcp_plotFinal=prcp_plot+geom_point(aes(x=date,y=prcpDis, col=prcpid),data=WXDataplot,size=0.5)
        prcp_plot=prcp_plotFinal
        
        WXDataplot1$weightedP=(((1/WXDataplot1$prcpDis.y^alfa)*(WXDataplot1$prcp.y/10))/WXDataplot1$DenPrcp)
        
        prcp_plotweight=prcp_plotweight1+geom_point(data = WXDataplot1 ,aes(x=date,y=weightedP, col=prcpid.y),size=0.5)
        prcp_plotweight1=prcp_plotweight
        
      }
      
      if("tmax" %in% colnames(WXDataplot)){
        
        tmax_plotFinal=tmax_plot+geom_point(aes(x=date,y=tmaxDis, col=tmaxid),data=WXDataplot,size=0.5)
        tmax_plot=tmax_plotFinal
        
        WXDataplot1$weightedTMX=(((1/WXDataplot1$tmaxDis.y^alfa)*(WXDataplot1$tmax.y/10))/WXDataplot1$DenTmax)
        tmax_plotweight=tmax_plotweight1+geom_point(data=WXDataplot1, aes(x=date,y=weightedTMX, col=tmaxid.y),size=0.5)       
        tmax_plotweight1=tmax_plotweight
      }
      if("tmin" %in% colnames(WXDataplot)){
        
        tmin_plotFinal=tmin_plot+geom_point(aes(x=date,y=tminDis, col=tminid),data=WXDataplot,size=0.5)
        tmin_plot=tmin_plotFinal
        
        WXDataplot1$weightedTMN=(((1/WXDataplot1$tminDis.y^alfa)*(WXDataplot1$tmin.y/10))/WXDataplot1$DenTmin)
        tmin_plotweight=tmin_plotweight1+geom_point(data=WXDataplot1, aes(x=date,y=weightedTMN, col=tminid.y),size=0.5)       
        tmin_plotweight1=tmin_plotweight
      }  
    }
    plot(prcp_plotweight1)
    plot(prcp_plot)
    plot(tmax_plotweight1)
    plot(tmax_plot)
    plot(tmin_plotweight1)
    plot(tmin_plot)
    return(modeldata)
    
  }
  if(method == "closest"){
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
      expand_limits(y=0)+
      labs(title = 'Precipitation', x = 'Date', y = 'Distance of station from the outlet (km)')
    plot(plot_prcp)
    prcp_distance=ggplot(modeldata, aes(date,P, col=prcpid))+
      geom_point()+
      labs(title = 'Precipitation', x = 'Date', y = 'Precipitation (mm)')
    plot(prcp_distance)
    ##############Tmax
    plot_tmax= ggplot(modeldata, aes(date,tmaxDis, col=tmaxid))+
      geom_point()+
      expand_limits(y=0)+
      labs(title = 'Maximum Temperature', x = 'Date', y = 'Distance of station from the outlet (km)')
    plot(plot_tmax)
    tmax_distance=ggplot(modeldata, aes(date,MaxTemp, col=tmaxid))+
      geom_point()+
      labs(title = 'Maximum Temperature', x = 'Date', y = 'Maximum Temperature (C)')
    plot(tmax_distance)
    ######MinTemp
    plot_tmin=ggplot(modeldata, aes(date,tminDis, col=tminid))+
      geom_point()+
      expand_limits(y=0)+
      labs(title = 'Minimum Temperature', x = 'Date', y = 'Distance of station from the outlet (km)')
    plot(plot_tmin)
    tmin_distance= ggplot(modeldata, aes(date,MinTemp, col=tminid))+
      geom_point()+
      labs(title = 'Minimum Temperature', x = 'Date', y = 'Minimum Temperature (C)')
    plot(tmin_distance)
    return(modeldata)
  }
}
