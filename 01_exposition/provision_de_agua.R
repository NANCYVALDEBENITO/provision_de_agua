
#install.packages("name")
library(ncdf4)
#load some packages
library(chron)
library(lattice)
library(RColorBrewer)
library(maptools)
library(rgdal)
library(sp)
library(raster)

##netcdf precipitation
ncpath  <- "/home/nvaldebenito/Documentos/01_provision_de_agua/01_exposition/"
ncname  <- "pr_CL-09_MPI-M-MPI-ESM-MR_historical_r1i1p1_CR2-RegCM4-6_v4_mon_19950101-19981231"
ncfname <- paste(ncpath, ncname, ".nc", sep="")  #variable 
dname   <- "pr"
ncin    <- nc_open(ncfname)
lon     <- ncvar_get(ncin,"lon")
nlon    <- dim(lon)
lat     <- ncvar_get(ncin,"lat")
nlat    <- dim(lat)
#get time
time    <- ncvar_get(ncin,"time")
tunits  <- ncatt_get(ncin,"time","units")
nt      <- dim(time)
#nt      <- 2
#get 
tmp_array   <- ncvar_get(ncin,dname)
pr<- data.frame(tmp_array)
#fix(pr)
d1name      <- ncatt_get(ncin,dname,"long_name")
dunits      <- ncatt_get(ncin,dname,"units")
fillvalue   <- ncatt_get(ncin,dname,"_FillValue")

#Convert time -- split the time units string into fields
##Time variable in new units 
tustr   <- strsplit(tunits$value," ")
tdstr   <- strsplit(unlist(tustr)[3], "-")
tmonth  <- as.integer(unlist(tdstr)[2])
tday    <- as.integer(unlist(tdstr)[3])
tyear   <- as.integer(unlist(tdstr)[1])
time_real <- as.chron(time, origin=c(tmonth,tday,tyear))
#print(time_real)
#class(time_real)
time_real <- as.character(time_real)
#class(time_real)

##Netcdf clean fillvalues
#Replace netCDF fill values with NA's
tmp_array[tmp_array==fillvalue$value] <- NA
#class(tmp_array)
#print(tmp_array)
#Can be gotten by determining the length of a vector of values representing one slice from the brick, omitting the NA values
length(na.omit(as.vector(tmp_array[,,1])))
precipitation_value<-data.frame()
#get a single slice or layer(January)
##Plot map one value january
for(m in 1:nt){
#nt
#m           <- 3
tmp_slice   <- tmp_array[,,m]

##Check map in netcdf plot
#image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

##Open shapepoints or shapepolygon
com<-readShapePoints('w_code_qsta_1001002.shp')
#com<- readShapePoly('catchments_chile_cag_v2.shp')
Tabla1     <- data.frame(com)
#fix(Tabla1)
#coordinates(Tabla1)<-c("coords.x1","coords.x2")
#latitude    <- Tabla1$coords.x1
#longitude   <- Tabla1$coords.x2

r<- raster(nrows=nlon, ncols=nlat, 
xmn=min(lat),xmx=max(lat),
ymn=min(lon),ymx=max(lon),crs=NA)
print(r)

r<- setValues(r,tmp_slice)
print(r)
r2<- t(flip(r,direction="x"))

pr_crop <- crop(r2,com)
pr_crop <- mask(pr_crop,com)

###Plot data netcdf about shape
plot(pr_crop,main=m)
plot(com, add=T)


##Save results how raster
#for (i in (1:length(pr_crop))){
#writeRaster(pr_crop, filename="precipitation_raster", format="GTiff", overwrite=TRUE)
#}


##Create points 
data        <- rasterToPoints(pr_crop)
#head(data)
dat         <- data.frame(data)
#print(data)
#fix(dat)

latitude    <- dat$y
longitude   <- dat$x
precipitation<- dat$layer
#print(precipitation)
precipitation_value<-rbind(precipitation_value,precipitation)
}

precipitation<-t(precipitation_value)
#reshape the array into vector
tmp_vec_long    <- as.vector(precipitation)
tmp_mat<- matrix(tmp_vec_long, nrow=length(longitude),ncol=m)
#create new dataframe
i<-1:m
tmp_df02    <- data.frame(cbind(longitude, latitude,tmp_mat))
#time_2<-NULL

#time_2<-rbind(time_2,chron(time,origin=c(tmonth,tday,tyear)))
#print(time_2)
names(tmp_df02) <- c("lon","lat", time_real[i]) #change de name variable time with chron

##Create a new shapepoints
#shape<-SpatialPointsDataFrame(data=tmp_df02)
#writePolygonsShape(pr_crop,shape, "precipitation_shape.shp")



###write out the dataframe in csv format
 
csvpath     <- "/home/nvaldebenito/Documentos/01_provision_de_agua/01_exposition/01_results/"
csvname     <-  "pr_CL-09_1976_month.csv"
csvfile     <-  paste(csvpath,csvname,sep="")
write.table(na.omit(tmp_df02),csvfile,row.names=FALSE, sep=",")


##Monthly data for process

january <- c()                      # January by point for nt/12 year 
for (n in 1:length(longitude)){     # quantity of points in lat lon
for (j in seq(3, nt, by=12)){       # columns in dataframe to read
for (m in 1:length(j)){
#print(tmp_df02[n,j])                # rows,columns matrix
test    <-t(tmp_df02[n,j])  
#print(test)                         # array
january <-union(january,test[m])    # matrix 1x1
}}}
print("January")
print(january)

february <- c()
for (n in 1:length(longitude)){     # quantity of points in lat lon
for (j in seq(4, nt, by=12)){       # columns in dataframe to read
for (m in 1:length(j)){
#print(tmp_df02[n,j])                # rows,columns matrix
test    <-t(tmp_df02[n,j])  
#print(test)                         # array
february <-union(february,test[m])    # matrix 1x1
}}}
print("February")
print(february)

march <- c()
for (n in 1:length(longitude)){     # quantity of points in lat lon
for (j in seq(5, nt, by=12)){       # columns in dataframe to read
for (m in 1:length(j)){
#print(tmp_df02[n,j])                # rows,columns matrix
test    <-t(tmp_df02[n,j])  
#print(test)                         # array
march <-union(march,test[m])    # matrix 1x1
}}}
print("March")
print(march)

april <- c()
for (n in 1:length(longitude)){     # quantity of points in lat lon
for (j in seq(6, nt, by=12)){       # columns in dataframe to read
for (m in 1:length(j)){
#print(tmp_df02[n,j])                # rows,columns matrix
test    <-t(tmp_df02[n,j])  
#print(test)                         # array
april <-union(april,test[m])    # matrix 1x1
}}}
print("April")
print(april)

may <- c()
for (n in 1:length(longitude)){     # quantity of points in lat lon
for (j in seq(7, nt, by=12)){       # columns in dataframe to read
for (m in 1:length(j)){
#print(tmp_df02[n,j])                # rows,columns matrix
test    <-t(tmp_df02[n,j])  
#print(test)                         # array
may <-union(may,test[m])    # matrix 1x1
}}}
print("May")
print(may)

june <- c()
for (n in 1:length(longitude)){     # quantity of points in lat lon
for (j in seq(8, nt, by=12)){       # columns in dataframe to read
for (m in 1:length(j)){
#print(tmp_df02[n,j])                # rows,columns matrix
test    <-t(tmp_df02[n,j])  
#print(test)                         # array
june <-union(june,test[m])    # matrix 1x1
}}}
print("June")
print(june)

july <- c()
for (n in 1:length(longitude)){     # quantity of points in lat lon
for (j in seq(9, nt, by=12)){       # columns in dataframe to read
for (m in 1:length(j)){
#print(tmp_df02[n,j])                # rows,columns matrix
test    <-t(tmp_df02[n,j])  
#print(test)                         # array
july <-union(july,test[m])    # matrix 1x1
}}}
print("July")
print(july)

august <- c()
for (n in 1:length(longitude)){     # quantity of points in lat lon
for (j in seq(10, nt, by=12)){       # columns in dataframe to read
for (m in 1:length(j)){
#print(tmp_df02[n,j])                # rows,columns matrix
test    <-t(tmp_df02[n,j])  
#print(test)                         # array
august <-union(august,test[m])    # matrix 1x1
}}}
print("August")
print(august)

september <- c()
for (n in 1:length(longitude)){     # quantity of points in lat lon
for (j in seq(11, nt, by=12)){       # columns in dataframe to read
for (m in 1:length(j)){
#print(tmp_df02[n,j])                # rows,columns matrix
test    <-t(tmp_df02[n,j])  
#print(test)                         # array
september <-union(september,test[m])    # matrix 1x1
}}}

print("September")
print(september)

october <- c()
for (n in 1:length(longitude)){     # quantity of points in lat lon
for (j in seq(12, nt, by=12)){       # columns in dataframe to read
for (m in 1:length(j)){
#print(tmp_df02[n,j])                # rows,columns matrix
test    <-t(tmp_df02[n,j])  
#print(test)                         # array
october <-union(october,test[m])    # matrix 1x1
}}}
print("October")
print(october)

november <- c()
for (n in 1:length(longitude)){     # quantity of points in lat lon
for (j in seq(13, nt+12, by=12)){       # columns in dataframe to read
for (m in 1:length(j)){
#print(tmp_df02[n,j])                # rows,columns matrix
test    <-t(tmp_df02[n,j])  
#print(test)                         # array
november <-union(november,test[m])    # matrix 1x1
}}}
print("November")
print(november)

december <- c()
for (n in 1:length(longitude)){     # quantity of points in lat lon
for (j in seq(14, nt+12, by=12)){       # columns in dataframe to read
for (m in 1:length(j)){
#print(tmp_df02[n,j])                # rows,columns matrix
test    <-t(tmp_df02[n,j])  
#print(test)                         # array
december <-union(december,test[m])    # matrix 1x1
}}}
print("December")
print(december)


##Calculate mean and standart desviation

mean_month <- function(month){ 
    month_mean <- c()
    #month_desv <- c()
    for (k in seq(0,length(month)-nt/12,by=nt/12)){
    test<- c()
    for (l in 1+k:((nt/12-1)+k)){                           # equivalent by total years 
    test=union(test,month[l])                         # test is equivalent to dates by point  
    }
    print(test)
    month_mean<-union(month_mean,mean(test))        # average january months by total years
    #month_desv<-union(month_desv,sd(test))
    rm(test)

    print(month_mean)
    length(month_mean) #total equivalent to quantity of points
    #print(month_desv)
    }
    return(month_mean)
    
}

desv_month <- function(month){ 
    #month_mean <- c();
    month_desv <- c()
    for (k in seq(0,length(month)-nt/12,by=nt/12)){
    test<- c()
    for (l in 1+k:((nt/12-1)+k)){                           # equivalent by total years 
    test=union(test,month[l])                         # test is equivalent to dates by point  
    }
    print(test)
    #month_mean<-union(month_mean,mean(test))        # average january months by total years
    month_desv<-union(month_desv,sd(test))
    rm(test)

    #print(str(month)+str("average"))
    #print(month_mean)
    #length(month_mean) #total equivalent to quantity of points
    print(month_desv)}
    return(month_desv)
    
}

january_mean <- c()
january_mean <- union(january_mean,mean_month(january))
january_desv<-c()
january_desv<-union(january_desv,desv_month(january))

february_mean <- c()
february_mean <- union(february_mean,mean_month(february))
february_desv<-c()
february_desv<-union(february_desv,desv_month(february))

march_mean <- c()
march_mean <- union(march_mean,mean_month(march))
march_desv<-c()
march_desv<-union(march_desv,desv_month(march))

april_mean <- c()
april_mean <- union(april_mean,mean_month(april))
april_desv<-c()
april_desv<-union(april_desv,desv_month(april))

may_mean <- c()
may_mean <- union(may_mean,mean_month(may))
may_desv<-c()
may_desv<-union(may_desv,desv_month(may))

june_mean <- c()
june_mean <- union(june_mean,mean_month(june))
june_desv<-c()
june_desv<-union(june_desv,desv_month(june))

july_mean <- c()
july_mean <- union(july_mean,mean_month(july))
july_desv<-c()
july_desv<-union(july_desv,desv_month(july))

august_mean <- c()
august_mean <- union(august_mean,mean_month(august))
august_desv<-c()
august_desv<-union(august_desv,desv_month(august))

september_mean <- c()
september_mean <- union(september_mean,mean_month(september))
september_desv<-c()
september_desv<-union(september_desv,desv_month(september))

october_mean <- c()
october_mean <- union(october_mean,mean_month(october))
october_desv<-c()
october_desv<-union(october_desv,desv_month(october))

november_mean <- c()
november_mean <- union(november_mean,mean_month(november))
november_desv<-c()
november_desv<-union(november_desv,desv_month(november))

december_mean <- c()
december_mean <- union(december_mean,mean_month(december))
december_desv<-c()
december_desv<-union(december_desv,desv_month(december))

##Create new dataframe

new_date    <- data.frame(cbind(longitude, latitude, january_mean, february_mean, march_mean, april_mean, may_mean, june_mean, july_mean,august_mean, september_mean, october_mean, november_mean, december_mean,january_desv, february_desv, march_desv, april_desv, may_desv, june_desv, july_desv, august_desv, september_desv, october_desv, november_desv, december_desv))
#fix(new_date)


csvpath     <- "/home/nvaldebenito/Documentos/01_provision_de_agua/01_exposition/01_results/"
csvname     <-  "pr_CL-09_1976_monthly_statistics.csv"
csvfile     <-  paste(csvpath,csvname,sep="")
write.table(na.omit(new_date),csvfile,row.names=FALSE, sep=",")


##Calculate IEP month to month
#tmp_df02 dataframe lon lat precipitation 12 months for year 3 ... nt 
#new_date dataframe lon lat monthly mean and monthly standart desviation  3 january 4 february ..    15

calculate_IEP <- function(month,month_mean,month_desv){
    IEP_month=c()    
    for (j in  seq(1,length(month),by=1)){ #8 
    
    m=nt/12; #4
    n=rep(1:length(month_mean),each=m)

    #print(j)
    #print(n[j])
    # equivalent by total years 
    IEP=(month[j] - month_mean[n[j]])/month_desv[n[j]]
    IEP_month = union(IEP, IEP_month)
    
    }
    return(IEP_month)


}

IEP_january<-c()
IEP_january<-union(IEP_january,calculate_IEP(january, january_mean, january_desv))
IEP_february<-c()
IEP_february<-union(IEP_february,calculate_IEP(february, february_mean, february_desv))
IEP_march<-c()
IEP_march<-union(IEP_march,calculate_IEP(march, march_mean, march_desv))
IEP_april<-c()
IEP_april<-union(IEP_april,calculate_IEP(april, april_mean, april_desv))
IEP_may<-c()
IEP_may<-union(IEP_may,calculate_IEP(may, may_mean, may_desv))
IEP_june<-c()
IEP_june<-union(IEP_june,calculate_IEP(june, june_mean, june_desv))
IEP_july<-c()
IEP_july<-union(IEP_july,calculate_IEP(july, july_mean, july_desv))
IEP_august<-c()
IEP_august<-union(IEP_august,calculate_IEP(august, august_mean, august_desv))
IEP_september<-c()
IEP_september<-union(IEP_september,calculate_IEP(september, september_mean, september_desv))
IEP_october<-c()
IEP_october<-union(IEP_october,calculate_IEP(october, october_mean, october_desv))
IEP_november<-c()
IEP_november<-union(IEP_november,calculate_IEP(november, november_mean, november_desv))
IEP_december<-c()
IEP_december<-union(IEP_december,calculate_IEP(december, december_mean, december_desv))



IEP<- data.frame(cbind( latitude, longitude, IEP_january, IEP_february, IEP_march, IEP_april, IEP_may, IEP_june, IEP_july, IEP_august, IEP_september, IEP_october, IEP_november, IEP_december))

csvpath     <- "/home/nvaldebenito/Documentos/01_provision_de_agua/01_exposition/01_results/"
csvname     <-  "IEP_calculated.csv"
csvfile     <-  paste(csvpath,csvname,sep="")
write.table(na.omit(IEP),csvfile,row.names=FALSE, sep=",")


##Finding months in drought by latitude and longitude
#The values are between -2.5 to -1.5, look for IEP < -1 

mdrought <- function(IEP_month){
	lat <-c()
	for (j in  seq(1,length(IEP_month),by=1)){ #8
    
		if (IEP_month[j] < -1) {
		
			lat <- append(lat,1)
		}	
		else {
	    
	    	lat <- append(lat,0)
		}

		
		}
	return(lat)
}
mdrought_january<-c()
mdrought_january<-append(mdrought_january,mdrought(IEP_january))
mdrought_february<-c()
mdrought_february<-append(mdrought_february,mdrought(IEP_february))
mdrought_march<-c()
mdrought_march<-append(mdrought_march,mdrought(IEP_march))
mdrought_april<-c()
mdrought_april<-append(mdrought_april,mdrought(IEP_april))
mdrought_may<-c()
mdrought_may<-append(mdrought_may,mdrought(IEP_may))
mdrought_june<-c()
mdrought_june<-append(mdrought_june,mdrought(IEP_june))
mdrought_july<-c()
mdrought_july<-append(mdrought_july,mdrought(IEP_july))
mdrought_august<-c()
mdrought_august<-append(mdrought_august,mdrought(IEP_august))
mdrought_september<-c()
mdrought_september<-append(mdrought_september,mdrought(IEP_september))
mdrought_october<-c()
mdrought_october<-append(mdrought_october,mdrought(IEP_october))
mdrought_november<-c()
mdrought_november<-append(mdrought_november,mdrought(IEP_november))
mdrought_december<-c()
mdrought_december<-append(mdrought_december,mdrought(IEP_december))



mdrought_data<- data.frame(cbind( latitude, longitude, mdrought_january, mdrought_february, mdrought_march, mdrought_april, mdrought_may, mdrought_june, mdrought_july, mdrought_august, mdrought_september, mdrought_october, mdrought_november, mdrought_december))

csvpath     <- "/home/nvaldebenito/Documentos/01_provision_de_agua/01_exposition/01_results/"
csvname     <-  "mdrought_calculated.csv"
csvfile     <-  paste(csvpath,csvname,sep="")
write.table(na.omit(mdrought_data),csvfile,row.names=FALSE, sep=",")



o=length(latitude)
m=nt/12; #4
n=rep(0:m*o,each=o)
j=rep(seq(1,nt,by=1),each=12) #111 .. 222... 333...



print(n)
print(j)


print(seq(1,32,by=8))
print(rep(seq(1,32,by=8),each=12))

#sequences for create points with total drougth months

print(rep(seq(1+1,32+1,by=8),each=12))
print(rep(seq(1+2,32+2,by=8),each=12))
#...
print(rep(seq(1+8,32+8,by=8),each=12))

point<-c()

for (k in seq(1,8,by=1)){
	point <- append(point,paste("punto",k,sep="_"))
	print(point)

}
print(point) #file names

exposition<-c()
for (i in seq(1,length(point),by=1)){
	
	n <- seq(0,length(point)-1,by=1)
	
	j <-seq(1+n[i],32+n[i],by=8)
	print(j)
	data<-sum(mdrought_january[j]+mdrought_february[j],mdrought_march[j],mdrought_april[j],mdrought_may[j],mdrought_june[j],
	mdrought_july[j],mdrought_august[j],mdrought_september[j],mdrought_october[j],mdrought_november[j],mdrought_december[j])
	print(data)
    exposition<-append(exposition,data)

		
}


print(exposition)

exposition_data<- data.frame(cbind( latitude, longitude, exposition))

csvpath     <- "/home/nvaldebenito/Documentos/01_provision_de_agua/01_exposition/01_results/"
csvname     <-  "exposition.csv"
csvfile     <-  paste(csvpath,csvname,sep="")
write.table(na.omit(exposition_data),csvfile,row.names=FALSE, sep=",")


#com<-readShapePoints('w_code_qsta_1001002.shp')