library(foreign)

options(scipen = 999)
options(stringsAsFactors = F)

set.seed(1234)

#function for convex hull and area
chullArea = function(x,y,df) {
  testHull = chull(x=x, y=y)
  
  dfhull = df[testHull,]
  hullarea = abs(polyarea(x=dfhull$LON, y=dfhull$LAT))
  
  areaConf = 1/hullarea
  
  return(areaConf)
}


##reopen and prepare the crime df
dfc = read.csv('D:\\Work\\CrimeZillow\\OH_data\\Crime\\crime_gc_midway.csv', header=T)

dfc$LAT_MED = 0
dfc$LON_MED = 0
dfc$LAT_RAN = 0
dfc$LON_RAN = 0
dfc$LAT = NULL
dfc$LON = NULL

dfc$CONF = 0
dfc$CONF_AREA = 0

#let's make dfp a lot smaller
dfp = read.dbf('D:\\Work\\CrimeZillow\\OH_data\\Cin_parcels\\cin_par_DD.dbf')

dfp = data.frame(dfp$ADDRNO, dfp$ADDRST, dfp$ADDRSF, dfp$LAT, dfp$LON)
colnames(dfp) = c('ADD_NUM', 'ADD_ST', 'ADD_SF', 'LAT' ,'LON')
dfp = dfp[complete.cases(dfp$ADD_NUM),]
dfp = dfp[complete.cases(dfp$ADD_ST),]

dfp$ADD_NUM = as.character(dfp$ADD_NUM)
dfp = unique(dfp)

#and now let's write the GC script
#create a list of numbers
numList = c('1','2','3','4','5','6','7','8','9')

for (i in 1:nrow(dfc)) {
  #get the identifiers for a given row
  tnum = dfc$ADD_NUM[i]
  tst = dfc$ADD_ST[i]
  tsf = dfc$ADD_SF[i]
  
  #create split number in case needed
  tnumSplit = strsplit(tnum, '')[[1]]
  
  #create temp df used in both if statements below
  tdf = dfp[dfp$ADD_ST==tst & dfp$ADD_SF==tsf,]
  tdf = tdf[complete.cases(tdf),]
  #and let's make sure it's not empty
  if (nrow(tdf)==0) {
    next
  }
  
  ##st and sf are fine.. need to check different states for the num
  #missing number case
  if (tnum==0 | substr(tnum,1,1)=='X') {
    #select random address
    tran = sample(nrow(tdf),1)
    dfc$LAT_RAN[i] = tdf$LAT[tran][1]
    dfc$LON_RAN[i] = tdf$LON[tran][1]
    
    #and now median address
    sortedNums = sort(tdf$ADD_NUM)
    tmed = sortedNums[ceiling(length(sortedNums)/2)]
    dfc$LAT_MED[i] = tdf[tdf$ADD_NUM==tmed,]$LAT[1]
    dfc$LON_MED[i] = tdf[tdf$ADD_NUM==tmed,]$LON[1]
    
    #add confidence (higher number = less confidence)
    dfc$CONF[i] = 1/nrow(tdf)
    dfc$CONF_AREA[i] = chullArea(tdf$LON, tdf$LAT, tdf)
  }
  
  #regular case (30XX)
  else if (tnumSplit[1] %in% numList) {
    #let's build the numbers we know
    knownNum = tnumSplit[1]
    for (j in 2:length(tnumSplit)) {
      if (tnumSplit[j] %in% numList | tnumSplit[j]==0) {
        knownNum = paste(knownNum,tnumSplit[j],sep='')
      }
      else if (tnumSplit[j]=='X') {
        break
      }
    }
    #now knownNum is the numbers we know, we have to filter by that
    tdf = tdf[startsWith(tdf$ADD_NUM,knownNum),]
    tdf = tdf[complete.cases(tdf),]
    #and now check again for no match
    if (nrow(tdf)==0) {
      next
    }
    
    #confidence record
    dfc$CONF[i] = 1/nrow(tdf)
    dfc$CONF_AREA[i] = chullArea(tdf$LON, tdf$LAT, tdf)
    
    #now we repeat random and median operations to fill in lat/lon
    #random
    tran = sample(nrow(tdf),1)
    dfc$LAT_RAN[i] = tdf$LAT[tran][1]
    dfc$LON_RAN[i] = tdf$LON[tran][1]
    
    #median
    sortedNums = sort(tdf$ADD_NUM)
    tmed = sortedNums[ceiling(length(sortedNums)/2)]
    dfc$LAT_MED[i] = tdf[tdf$ADD_NUM==tmed,]$LAT[1]
    dfc$LON_MED[i] = tdf[tdf$ADD_NUM==tmed,]$LON[1]
  }
}

#clean the results (remove no match data) and write them
dfc2 = dfc[dfc$CONF>0,]
write.csv(dfc2, 'D:\\Work\\CrimeZillow\\OH_data\\Crime\\crime_gc_spatial_final.csv')
