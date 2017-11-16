

library(raster)
library(rgdal)
library(magrittr)
library(stringr)
library(lubridate)


#define input files
indir = "D:/work/proj/npp/20171107/hdf/1080x2160"
outdir = "D:/work/proj/npp/20171107/tif/1080x2160"
nppdir = "D:/work/proj/npp/20171107/annual_npp"
meanannual = "D:/work/proj/npp/20171107/mean_annual"




# convert hdf files to tif ------------------------------------------------
hdffiles = list.files(indir, ".hdf$", full.names=T, recursive = T) #find hdf files
tiffiles = sub(indir, outdir, hdffiles) %>% # define tif names
  sub(".hdf", ".tif", .)


#loop through files and convert them 
for(i in 1:length(tiffiles)){
  cmd = paste("gdal_translate -of GTiff", 
              "-a_srs EPSG:4326",
              "-a_nodata -9999",
              "-a_ullr -180 90 180 -90",
              #"-ot Int16",
              hdffiles[i], tiffiles[i])
  shell(cmd)
}


#separate files by year month and figure out how many days are in each month/year
bname = basename(tiffiles)
year = as.numeric(str_sub(bname, 6,9))
startday = as.numeric(str_sub(bname, 10,12))
endday = startday[2:length(startday)]
change = which(endday > 330)+1
end = change
for(i in 1:length(end)){
  end[i] = ifelse(leap_year(year[end[i]]), 367, 366)
}
endday[change] = end
ndays = endday - startday # this is a list with the number of days in each month - one per npp monthly average file

length(ndays) == length(tiffiles)

#calculate annual npp/year - each file is month-mean, so the unit of each file is: net primary production (units of mg C / m**2 / day) based on the standard vgpm algorithm
uni = unique(year) # make a list of unique years

# for each unique year calcuate and annual sum
for(y in 1:length(uni)){
  print(paste0(y,"/",length(uni)))
  print(uni[y])
  # pull out this unique year files
  these = which(year==uni[y]) 
  tifs = tiffiles[these] # these are the files that belong to a year - months 
  nd = ndays[these] # these are the number of days in each month for this year
  # for each month in this particular year do stuff
  for(i in 1:length(tifs)){
    print(paste0(i,"/",length(tifs)))
    r = raster(tifs[i])
    r[is.na(r)] = 0 # set NA to 0 so that when we add month totals together NA is not propogated
    r = r*nd[i] # get the month total npp by mutiplying the monthing average by the number of days in the month
    if(i == 1){s = r} else{s = s+r} # add this month to an accumulating year total npp raster
  }
  writeRaster(s, paste0(nppdir,"/npp_1080x2160_",uni[y],".tif")) # write out the year total npp file
}

#find the annual npp files
yearfiles = list.files(nppdir, ".tif$", full.names = T)

#calulate the mean annual npp - total all the annual files in the for loop
for(i in 1:length(yearfiles)){
  print(paste0(i,"/",length(yearfiles)))
  r = raster(yearfiles[i])
  r[values(r) == 0] = NA # ????????????????? not sure why changing to NA 
  if(i == 1){s = r} else{s = s+r}
}
mgnpp = s/length(yearfiles) # divide the totaled annual files and divide by the number of years
writeRaster(mgnpp, paste0(meanannual,"/npp_1080x2160_mean_annual_mg.tif"), overwrite=T)

#calculate gross primary productivity
mggpp = mgnpp*3 # convert to gpp
writeRaster(mggpp, paste0(meanannual,"/gpp_1080x2160_mean_annual_mg.tif"), overwrite=T)

#convert to grams
ggpp = mggpp/1000
writeRaster(ggpp, paste0(meanannual,"/gpp_1080x2160_mean_annual_g.tif"), overwrite=T)
