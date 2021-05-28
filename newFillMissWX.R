
flowgage_id="04282650" 
flowgage=get_usgs_gage(flowgage_id,begin_date = "2010-01-01",
                       end_date = "2022-01-01")
#declat=flowgage$declat
#declon=flowgage$declon
#StnRadius=30
#date_min="2010-01-01"
#date_max="2021-01-01"

NewFillMissWX=function(declat, declon,StnRadius,date_min,date_max){
  
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
  modeldata$prcp=modeldata$prcp*(1/WXDistance)
  modeldata$tmax=modeldata$tmax*(1/WXDistance)
  modeldata$tmin=modeldata$tmin*(1/WXDistance)
  modeldata$DenPrcp=0
  modeldata$DenTmin=0
  modeldata$DenTmax=0
  modeldata$DenPrcp[!is.na(modeldata$prcp)]=1/WXDistance
  modeldata$DenTmax[!is.na(modeldata$tmax)]=1/WXDistance
  modeldata$DenTmin[!is.na(modeldata$tmin)]=1/WXDistance
###########################################
  for (i in 2:length(ustns[,1])){
    #i=3
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
      modeldata$DenPrcp[is.na(modeldata$DenPrcp)]=1/WXDistance
      modeldata$DenTmax[is.na(modeldata$DenTmax)]=1/WXDistance
      modeldata$DenTmin[is.na(modeldata$DenTmin)]=1/WXDistance
      if("prcp.y" %in% colnames(modeldata)){
        
        modeldata$prcpid[is.na(modeldata$prcp.x)]=WXStn
        modeldata$prcpDis[is.na(modeldata$prcp.x)]=WXDistance
        modeldata$prcp.x[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]=modeldata$prcp.x[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]+
        modeldata$prcp.y[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]*(1/WXDistance)
        modeldata$prcp.x[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]=modeldata$prcp.y[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]*(1/WXDistance)
        modeldata$DenPrcp[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]=modeldata$DenPrcp[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]+(1/WXDistance)
        modeldata$DenPrcp[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]=modeldata$DenPrcp[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]+1/WXDistance
        modeldata=subset(modeldata,select = -c(prcp.y))
      }
      if("tmin.y" %in% colnames(modeldata)){
        
        modeldata$tminid[is.na(modeldata$tmin.x)]=WXStn
        modeldata$tminDis[is.na(modeldata$tmin.x)]=WXDistance
        modeldata$tmin.x[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]=modeldata$tmin.x[!is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]+
          modeldata$tmin.y[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]*(1/WXDistance)
        modeldata$tmin.x[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]=modeldata$tmin.y[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]*(1/WXDistance)
        modeldata$DenTmin[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]=modeldata$DenTmin[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]+(1/WXDistance)
        modeldata$DenTmin[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]=modeldata$DenTmin[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]+1/WXDistance
        modeldata=subset(modeldata,select = -c(tmin.y))
      }
      if("tmax.y" %in% colnames(modeldata)){
        
        modeldata$tmaxid[is.na(modeldata$tmax.x)]=WXStn
        modeldata$tmaxDis[is.na(modeldata$tmax.x)]=WXDistance
        modeldata$tmax.x[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]=modeldata$tmax.x[!is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]+
          modeldata$tmax.y[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]*(1/WXDistance)
        modeldata$tmax.x[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]=modeldata$tmax.y[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]*(1/WXDistance)
        modeldata$DenTmax[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]=modeldata$DenTmax[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]+(1/WXDistance)
        modeldata$DenTmax[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]=modeldata$DenTmax[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]+1/WXDistance       
       
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

newLOCFer=NewFillMissWX(declat=flowgage$declat, declon=flowgage$declon,
                   StnRadius=30,date_min="2010-01-01",date_max="2021-01-01")
LOCFer=FillMissWX(declat=flowgage$declat, declon=flowgage$declon,
                  StnRadius=30,date_min="2010-01-01",date_max="2021-01-01")
################################Prec
plot(newLOCFer$date,newLOCFer$P, type="l",xlab="Date", ylab="Precipitation (mm)")
lines(LOCFer$date,LOCFer$P,col="red")
legend("topleft",legend = c("Filling With Inverse Distance Weighting","Filling Without Inverse Distance Weighting"),col = c("black","red"),horiz=TRUE, lwd=1.25,cex=0.8,bty="n",inset=0)
########################Max Temp
plot(newLOCFer$date,newLOCFer$MaxTemp,type="l",xlab="Date",ylab="Maximum Temperature (c)")
lines(LOCFer$date,LOCFer$MaxTemp, col="red")
legend("topleft",legend = c("Filling With Inverse Distance Weighting","Filling Without Inverse Distance Weighting"),col = c("black","red"),horiz=TRUE, lwd=1.25,cex=0.8,bty="n",inset=0)
##################################MIn Temp
plot(newLOCFer$date,newLOCFer$MinTemp,type="l", xlab="Date",ylab="Minimum Temperature")
lines(LOCFer$date,LOCFer$MinTemp,col="red")
legend("topleft",legend = c("Filling With Inverse Distance Weighting","Filling Without Inverse Distance Weighting"),col = c("black","red"),horiz=TRUE, lwd=1.25,cex=0.8,bty="n",inset=0)

##############Precipitation
ggplot(newLOCFer, aes(date,prcpDis, col=prcpid))+
  geom_point()+
  labs(title = 'Precipitation', x = 'Date', y = 'Distance of station from the outlet (km)')

ggplot(newLOCFer, aes(date,P, col=prcpid))+
  geom_point()+
  labs(title = 'Precipitation', x = 'Date', y = 'Precipitation (mm)')

