zones_shp = readOGR("/Users/henrygomory/Documents/Research/BARI/Zoning/Data Files (No Year)/Zoning Subdistricts Shapefile/","Zones_with_NSA_R_ID",stringsAsFactors = F)
xwalk = read.csv("Documents/Research/BARI/Zoning/Data Files (No Year)/Updated Common IDs 9-16-2015.csv",stringsAsFactors=F)
clearances = read.csv("Documents/Research/BARI/Zoning/Zoning 2016/Zoning_Clearances.csv",stringsAsFactors=F)
clearances_t = data.frame(t(clearances),stringsAsFactors = F)
names(clearances_t) = gsub(" ","",trim(clearances_t[1,]))
clearances_t = clearances_t[-1,]
clearances_t$Subdistrict.ID = trim(gsub("X","",row.names(clearances_t)))

zones = zones_shp@data
zones.1 = merge(zones,xwalk,by="cartodb_id",all.x=T)
#there is already a zone_
zones.1$ZONE_<-NULL
zones.1$DISTRICT<-NULL
zones.1$MAPNO<-NULL
zones.1$ARTICLE<-NULL

zones.1$Subdistrict.ID = trim(zones.1$Subdistrict.ID)
zones.2 = merge(zones.1,clearances_t,by="Subdistrict.ID",all.x=T)
sum(is.na(zones.2$Cinema))
#this merge is not working quite right because the zoning clearances file is missing data for some subdistrict IDs
     
          
#clean a column
zones.2$subdistric <- str_to_title(str_replace_all(zones.2$subdistric, "Micellaneous", "Miscellaneous"))


original_names = names(zones.2)
zoning_columns = c(17:176)
#shorten the names
for (num in zoning_columns) {
  name = names(zones.2)[num]
  if (nchar(name)>63) {
    names(zones.2)[num] = paste(substr(name,1,40),
                                substr(name,nchar(name)-20,nchar(name)),
                                sep="...")
  }
}
sum(duplicated(names(zones.2)))
newNames = names(zones.2)[zoning_columns]
write.csv(data.frame(long_name = original_names,
                     short_name = names(zones.2)),"/Users/henrygomory/Documents/Research/BARI/Zoning/Zoning 2016/Output/FullVariableNames.new.csv",row.names=F)


#new base variables: sums and means of allowed, conditional, and forbidden uses, and zoning score
zones.2$AllowedUses <- apply(zones.2[zoning_columns]=='A', 1, sum, na.rm=T)
zones.2$ConditionalUses <- apply(zones.2[zoning_columns]=='C' | zones.2[zoning_columns]=='A*', 1, sum, na.rm=T)
zones.2$ForbiddenUses <- apply(zones.2[zoning_columns]=='F', 1, sum, na.rm=T)

#add region data
region_xwalk = read.csv("/Users/henrygomory/Documents/Research/BARI/Zoning/Data Files (No Year)/region_xwalk.csv",stringsAsFactors=F)
zones.3 = merge(zones.2,region_xwalk,by="cartodb_id")

#order vars and write
first_vars = c("Subdistrict.ID","zone_","district","mapno","article","subdistric","unique_cod","cartodb_id","Shapearea", "Region","NSA_NAME","BOSNA_R_ID",
              "AllowedUses","ConditionalUses","ForbiddenUses")
zones.3 = zones.3[,c(first_vars,setdiff(names(zones.3),first_vars))]

write.csv(zones.3,"/Users/henrygomory/Documents/Research/BARI/Zoning/Zoning 2016/Output/Zoning.Record.2016.csv",row.names = F)

zones_shp.1 = merge(zones_shp,zones.3[,c("cartodb_id",setdiff(names(zones.3),names(zones_shp@data)))],by="cartodb_id",all.x=T)
writeOGR(zones_shp.1, "/Users/henrygomory/Documents/Research/BARI/Zoning/Zoning 2016/Output/Zoning.Record.2016.shp/","Zoning.Record.2016",driver="ESRI Shapefile",overwrite_layer=TRUE)



zones.3$isOpen =  ifelse(!is.na(zones.3$subdistric) & zones.3$subdistric=="Open Space",1,0)

#annoyingly, sqldf doesn't like datasets with periods in the names
zones_3 = zones.3
zones.3.district = sqldf("select district, sum(isOpen) 'numOpen', count(district) 'totalzones',sum(Shapearea) 'totalArea' from zones_3 group by district")
zones.3.district$RatioOpen = zones.3.district$numOpen/zones.3.district$totalzones

zones.3.NSA = sqldf("select NSA_NAME, sum(isOpen) 'numOpen', count(district) 'totalzones',sum(Shapearea) 'totalArea' from zones_3 group by NSA_NAME")
zones.3.NSA$RatioOpen = zones.3.NSA$numOpen/zones.3.NSA$totalzones
rm(zones_3)

#get percent allowed of each type of zoning, either with all zones weighted equally or weighted by area
for (name in newNames) {
  agg.district = aggregate(cbind( (zones.3[,name]=="A")*zones.3$Shapearea,
                  (zones.3[,name]=="A")*1),
                  by=list(zones.3$district),FUN=sum)
  names(agg.district)<-c("district",paste(name,"_byArea",sep=""),paste(name,"_byZone",sep=""))
  zones.3.district = merge(zones.3.district,agg.district,by="district")
  zones.3.district[,paste(name,"_byArea",sep="")]=zones.3.district[,paste(name,"_byArea",sep="")]/zones.3.district$totalArea
  zones.3.district[,paste(name,"_byZone",sep="")]=zones.3.district[,paste(name,"_byZone",sep="")]/zones.3.district$totalzones
  
  agg.NSA = aggregate(cbind( (zones.3[,name]=="A")*zones.3$Shapearea,
                             (zones.3[,name]=="A")*1),
                      by=list(zones.3$NSA_NAME),FUN=sum)
  names(agg.NSA)<-c("NSA_NAME",paste(name,"_byArea",sep=""),paste(name,"_byZone",sep=""))  
  zones.3.NSA = merge(zones.3.NSA,agg.NSA,by="NSA_NAME")
  zones.3.NSA[,paste(name,"_byArea",sep="")]=zones.3.NSA[,paste(name,"_byArea",sep="")]/zones.3.NSA$totalArea
  zones.3.NSA[,paste(name,"_byZone",sep="")]=zones.3.NSA[,paste(name,"_byZone",sep="")]/zones.3.NSA$totalZones
}


write.csv(zones.3.district,"/Users/henrygomory/Documents/Research/BARI/Zoning/Zoning 2016/Output/Zoning.District.2016.csv",row.names=F)

write.csv(zones.3.NSA,"/Users/henrygomory/Documents/Research/BARI/Zoning/Zoning 2016/Output/Zoning.NSA.2016.csv",row.names=F)

NSA.shp = getNSAsShp() 
NSA.shp.1 = merge(NSA.shp,zones.3.NSA,by="NSA_NAME",all.x=T)
writeOGR(NSA.shp.1, "/Users/henrygomory/Documents/Research/BARI/Zoning/Zoning 2016/Output/Zoning.NSA.2016.shp/","Zoning.NSA.2016",driver="ESRI Shapefile",overwrite_layer=TRUE)

 