
library(raster)
library(ggplot2)
library(dplyr)
library(palr)
library(rasterVis)

meanAnnualGPPfile = "D:/work/proj/npp/20171107/mean_annual/gpp_1080x2160_mean_annual_g.tif"
distFile = "D:/work/proj/npp/20171107/mean_annual/land_water_dist_geo.tif"
meanAnnualGPPfileFresh = "D:/work/proj/npp/20171107/freshwater/StreamProductivity.csv"
width = 5


# read in freshwater
ggppFresh = read.csv(meanAnnualGPPfileFresh)

# read in the file and get the number of rows
ggpp = raster(meanAnnualGPPfile)
nRow = nrow(ggpp)

# bind the rows into latitude bands
fromRow = seq(1, nRow, 6*width)
toRow = seq(6*width, nRow, 6*width)
length(fromRow) == length(toRow)
lats = seq(90, -90, -180/nRow)
fromLat = lats[fromRow]
toLat = c(fromLat[2:length(fromLat)], -90)
label = paste0(fromLat,' : ',toLat)

hemi = midLat = vector()
for(i in 1:length(toLat)){
  mid = ((fromLat[i]-toLat[i])/2)+toLat[i]
  midLat = c(midLat,mid)
  hem = 'North'
  if(mid < 0){
    hem = 'South'
  }
  hemi = c(hemi, hem)
}



fromLatAbs = abs(fromLat)
toLatAbs = abs(toLat)
midLatAbs = abs(midLat)
labelAbs = paste0(fromLatAbs,' : ',toLatAbs)

latInfo = data.frame(fromRow, toRow, fromLat, toLat, midLat, label,
                     fromLatAbs, toLatAbs, midLatAbs, labelAbs, hemi)



# fresh water df
dfSalt = dfFresh = data.frame(mean=NA, stdev=NA, median=NA, q1=NA, q3=NA, 
                              fromLat=latInfo$fromLat, toLat=latInfo$toLat, midLat=latInfo$midLat, label=latInfo$label,
                              fromLatAbs=latInfo$fromLatAbs, toLatAbs=latInfo$toLatAbs, midLatAbs=latInfo$midLatAbs, labelAbs=latInfo$labelAbs,
                              hemi = latInfo$hemi)
dfFresh$water = 'Fresh'
dfSalt$water = 'Salt'


for(i in 1:nrow(dfFresh)){
  theseRows = filter(ggppFresh, Lat_Deg <= dfFresh$fromLat[i] & Lat_Deg > dfFresh$toLat[i])
  mean_ = mean(theseRows$Annual_GPP, na.rm=T)
  sd_ = sd(theseRows$Annual_GPP, na.rm=T)
  medq = summary(theseRows$Annual_GPP, na.rm=T)
  if(sum(!is.finite(c(mean_, sd_))) < 2){
    dfFresh$mean[i] = mean_
    dfFresh$stdev[i] = sd_
    dfFresh$median[i] = medq[3]
    dfFresh$q1[i] = medq[2]
    dfFresh$q3[i] = medq[5]
  }
}


# bring in the freshwater
# test = mutate(ggppFresh, label=cut(Lat_Deg, seq(90, -90, -width), rev(paste0(fromLat,' - ',toLat)))) %>%
#   group_by(label) %>% 
#   summarise(mean = mean(Annual_GPP, na.rm=T), stdev=sd(Annual_GPP, na.rm=T)) %>%
#   mutate(water = "Fresh")


# get salt water data
for(i in 1:nrow(latInfo)){
  print(paste0(latInfo$fromLat[i],'-',latInfo$toLat[i]))
  ggppBin = ggpp[latInfo$fromRow[i]:latInfo$toRow[i],]
  medq = summary(ggppBin, na.rm=T)
  dfSalt$mean[i] = mean(ggppBin, na.rm=T)
  dfSalt$stdev[i] = sd(ggppBin, na.rm=T)
  dfSalt$median[i] = medq[3]
  dfSalt$q1[i] = medq[2]
  dfSalt$q3[i] = medq[5]
}

# plot gpp as function of latitude
# dfSalt = na.omit(dfSalt)
# latP = ggplot(dfSalt, aes(x=midLat, y=mean))+
#   geom_ribbon(aes(ymin = mean - stdev, ymax = mean + stdev), fill = "grey70") +
#   geom_line()+
#   ylab("Carbon (g/m^2/year)")+
#   xlab("Latitude (degrees)")+
#   theme_bw()
# 
# latP



# combine fresh and salt
combined = rbind(dfFresh, dfSalt)

combined = mutate(combined, class=paste(hemi, water))

# set label as factor
combined$label = as.factor(combined$label)
combined$label = factor(combined$label, levels = rev(latInfo$label))

combined$labelAbs = as.factor(combined$labelAbs)
combined$labelAbs = factor(combined$label, levels = rev(latInfo$labelAbs))

combined$class = as.factor(combined$class)
combined$hemi = as.factor(combined$hemi)
combined$water = as.factor(combined$water)



# plot gpp as function of latitude LINES MEAN
# latP = ggplot(combined, aes(x=midLat, y=mean, color=water))+
#   #geom_ribbon(aes(ymin = mean - stdev, ymax = mean + stdev, fill=water), alpha=0.1) +
#   geom_point()+
#   geom_line(size=2)+
#   ylab("Carbon (g/m^2/year)")+
#   xlab("Latitude (degrees)")+
#   theme_bw()
# 
# latP

dodge = position_dodge(3)

#plot gpp as function of latitude POINTS MEAN BY WATER TWO NO HEMI
latP = ggplot(combined, aes(x=midLat, y=mean, color=water))+
  #geom_ribbon(aes(ymin = mean - stdev, ymax = mean + stdev, fill=water), alpha=0.1) +
  geom_errorbar(aes(ymin = mean - stdev, ymax = mean + stdev, color=water), width=0.1, position = dodge, alpha=0.8) +
  geom_point(size=4, position = dodge, alpha=0.8)+
  ylab("Carbon (g/m^2/year)")+
  xlab("Latitude (degrees)")+
  guides(color=guide_legend(title="Water Type"))+
  theme_bw()
latP

#plot gpp as function of latitude POINTS MEDIAN BY WATER NO HEMI
latP = ggplot(combined, aes(x=midLat, y=median, color=water))+
  #geom_ribbon(aes(ymin = mean - stdev, ymax = mean + stdev, fill=water), alpha=0.1) +
  geom_errorbar(aes(ymin = q1, ymax = q3, color=water), width=0.1, position = dodge, alpha=0.8) +
  geom_point(size=4, position = dodge, alpha=0.8)+
  ylab("Carbon (g/m^2/year)")+
  xlab("Latitude (degrees)")+
  guides(color=guide_legend(title="Water Type"))+
  theme_bw()
latP

###################################################
# HEMI COMBINED
###################################################

######## NO FACETS ########

#plot gpp as function of latitude POINTS MEAN
latP = ggplot(combined, aes(x=midLatAbs, y=mean, color=class))+
  #geom_ribbon(aes(ymin = mean - stdev, ymax = mean + stdev, fill=water), alpha=0.1) +
  geom_errorbar(aes(ymin = mean - stdev, ymax = mean + stdev, color=class), width=0.1, position = dodge, alpha=0.8) +
  geom_point(size=4, position = dodge, alpha=0.8)+
  ylab("Carbon (g/m^2/year)")+
  xlab("Latitude (degrees)")+
  guides(color=guide_legend(title="Group"))+
  theme_bw()
latP

#plot gpp as function of latitude POINTS MEDIAN
latP = ggplot(combined, aes(x=midLatAbs, y=median, color=class))+
  #geom_ribbon(aes(ymin = mean - stdev, ymax = mean + stdev, fill=water), alpha=0.1) +
  geom_errorbar(aes(ymin = q1, ymax = q3, color=class), width=0.1, position = dodge, alpha=0.8) +
  geom_point(size=4, position = dodge, alpha=0.8)+
  ylab("Carbon (g/m^2/year)")+
  xlab("Latitude (degrees)")+
  guides(color=guide_legend(title="Group"))+
  theme_bw()
latP


#plot gpp as function of latitude POINTS MEAN
latP = ggplot(combined, aes(x=midLatAbs, y=mean, color=class))+
  #geom_ribbon(aes(ymin = mean - stdev, ymax = mean + stdev, fill=water), alpha=0.1) +
  geom_errorbar(aes(ymin = mean - stdev, ymax = mean + stdev, color=class), width=0.1, position = dodge, alpha=0.8) +
  geom_point(size=4, position = dodge, alpha=0.8)+
  geom_smooth(method='lm',formula=y~x, se=FALSE)+
  ylab("Carbon (g/m^2/year)")+
  xlab("Latitude (degrees)")+
  guides(color=guide_legend(title="Group"))+
  theme_bw()
latP

#plot gpp as function of latitude POINTS MEDIAN
latP = ggplot(combined, aes(x=midLatAbs, y=median, color=class))+
  #geom_ribbon(aes(ymin = mean - stdev, ymax = mean + stdev, fill=water), alpha=0.1) +
  geom_errorbar(aes(ymin = q1, ymax = q3, color=class), width=0.1, position = dodge, alpha=0.8) +
  geom_point(size=4, position = dodge, alpha=0.8)+
  geom_smooth(method='lm',formula=y~x, se=FALSE)+
  ylab("Carbon (g/m^2/year)")+
  xlab("Latitude (degrees)")+
  guides(color=guide_legend(title="Group"))+
  theme_bw()
latP

######## FACETS ########

#plot gpp as function of latitude POINTS MEAN
latP = ggplot(combined, aes(x=midLatAbs, y=mean, color=hemi))+
  #geom_ribbon(aes(ymin = mean - stdev, ymax = mean + stdev, fill=water), alpha=0.1) +
  geom_errorbar(aes(ymin = mean - stdev, ymax = mean + stdev, color=hemi), width=0.1, position = dodge, alpha=0.8) +
  geom_point(size=4, position = dodge, alpha=0.8)+
  facet_grid(~water)+
  ylab("Carbon (g/m^2/year)")+
  xlab("Latitude (degrees)")+
  guides(color=guide_legend(title="Hemisphere"))+
  theme_bw()
latP

#plot gpp as function of latitude POINTS MEDIAN
latP = ggplot(combined, aes(x=midLatAbs, y=median, color=hemi))+
  #geom_ribbon(aes(ymin = mean - stdev, ymax = mean + stdev, fill=water), alpha=0.1) +
  geom_errorbar(aes(ymin = q1, ymax = q3, color=hemi), width=0.1, position = dodge, alpha=0.8) +
  geom_point(size=4, position = dodge, alpha=0.8)+
  facet_grid(~water)+
  ylab("Carbon (g/m^2/year)")+
  xlab("Latitude (degrees)")+
  guides(color=guide_legend(title="Hemisphere"))+
  theme_bw()
latP


######## FACETS WITH REGRESSION ########

#plot gpp as function of latitude POINTS MEAN
latP = ggplot(combined, aes(x=midLatAbs, y=mean, color=hemi))+
  #geom_ribbon(aes(ymin = mean - stdev, ymax = mean + stdev, fill=water), alpha=0.1) +
  geom_errorbar(aes(ymin = mean - stdev, ymax = mean + stdev, color=hemi), width=0.1, position = dodge, alpha=0.8) +
  geom_point(size=4, position = dodge, alpha=0.8)+
  geom_smooth(method='lm',formula=y~x, se=FALSE)+
  facet_grid(~water)+
  ylab("Carbon (g/m^2/year)")+
  xlab("Latitude (degrees)")+
  guides(color=guide_legend(title="Hemisphere"))+
  theme_bw()
latP

#plot gpp as function of latitude POINTS MEDIAN
latP = ggplot(combined, aes(x=midLatAbs, y=median, color=hemi))+
  #geom_ribbon(aes(ymin = mean - stdev, ymax = mean + stdev, fill=water), alpha=0.1) +
  geom_errorbar(aes(ymin = q1, ymax = q3, color=hemi), width=0.1, position = dodge, alpha=0.8) +
  geom_point(size=4, position = dodge, alpha=0.8)+
  geom_smooth(method='lm',formula=y~x, se=FALSE)+
  facet_grid(~water)+
  ylab("Carbon (g/m^2/year)")+
  xlab("Latitude (degrees)")+
  guides(color=guide_legend(title="Hemisphere"))+
  theme_bw()
latP









#####################################################
# histograms
#####################################################

df = data.frame(values=NA, label=NA, labelAbs=NA, hemi=NA, midLatAbs=NA)

for(i in 1:nrow(latInfo)){
  print(latInfo$label[i])
  ggppBin = ggpp[latInfo$fromRow[i]:latInfo$toRow[i],]
  addDF = data.frame(values=ggppBin, label=latInfo$label[i], labelAbs=latInfo$labelAbs[i], hemi=latInfo$hemi[i], midLatAbs=latInfo$midLatAbs[i])
  df = rbind(df, addDF)
}

dfClean = na.omit(df)
nrow(dfClean)
dfClean$label = as.factor(dfClean$label)
dfClean$label = factor(dfClean$label, levels = rev(latInfo$label))

dfClean$labelAbs = as.factor(dfClean$labelAbs)
dfClean$labelAbs = factor(dfClean$labelAbs, levels = rev(latInfo$labelAbs))




# boxplot
plt = ggplot(dfClean, aes(label, values))+
  geom_boxplot()+
  ylab("Carbon (g/m^2/year)")+
  xlab("Latitude (degrees)")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust = 0.5))
plt

# boxplot log10 y axis
plt = ggplot(dfClean, aes(label, values))+
  geom_boxplot()+
  scale_y_log10()+
  ylab("Carbon (g/m^2/year)")+
  xlab("Latitude (degrees)")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust = 0.5))
plt

###################################################
# HEMI COMBINED
################################################### 

# boxplot
plt = ggplot(dfClean, aes(as.factor(midLatAbs), values, fill=hemi))+
  geom_boxplot(position=position_dodge(width=0.6))+
  ylab("Carbon (g/m^2/year)")+
  xlab("Latitude (degrees)")+
  guides(fill=guide_legend(title="Hemisphere"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust = 0.5))
plt

# boxplot log10 y axis
plt = ggplot(dfClean, aes(as.factor(midLatAbs), values, fill=hemi))+
  geom_boxplot(position=position_dodge(width=0.6))+
  scale_y_log10()+
  ylab("Carbon (g/m^2/year)")+
  xlab("Latitude (degrees)")+
  guides(fill=guide_legend(title="Hemisphere"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust = 0.5))
plt









################################################################
## raster and distance stuff
################################################################



distR = raster(distFile)


#plot map of mean annual gpp
myTheme = rasterTheme(region = chlPal(255))
levelplot(ggpp, zscaleLog=TRUE, margin=F, par.settings = myTheme, maxpixels=1000000) #, maxpixels = 10000000
1080*2160




samp = sampleRandom(ggpp, 100000, na.rm=T, cells=T, xy=T)
ggppDist = as.data.frame(cbind(samp, dist=distR[samp[,1]]))
colnames(ggppDist) = c("cell", "x", "y", "ggpp", "dist")

# plot it
plt = ggplot(ggppDist, aes(dist, ggpp))+
  geom_point(alpha=0.01)+
  ylab("Carbon (g/m^2/year)")+
  xlab("Degrees from Land (distance)")+
  theme_bw()
plt




# fresh water df
dfdistggpplat = data.frame(y=NA, ggpp=NA, dist=NA, midLat=NA)


for(i in 1:length(latInfo$midLat)){
  print(i)
  theseRows = filter(ggppDist, y <= latInfo$fromLat[i] & y > latInfo$toLat[i]) %>%
    select(y, ggpp, dist)
  if(nrow(theseRows) == 0){next}
  theseRows$midLat = rep(latInfo$midLat[i], nrow(theseRows))
  dfdistggpplat = rbind(dfdistggpplat, theseRows)
}

dfdistggpplatClean = dfdistggpplat[2:nrow(dfdistggpplat),]

dfdistggpplatClean$midLat = factor(dfdistggpplatClean$midLat, levels = rev(latInfo$midLat))

# plot it with facets on latMid
plt = ggplot(dfdistggpplatClean, aes(dist, ggpp))+
  geom_point(alpha=0.01)+
  facet_grid(.~midLat)+
  ylab("Carbon (g/m^2/year)")+
  xlab("Degrees from Land (distance)")+
  theme_bw()
plt













