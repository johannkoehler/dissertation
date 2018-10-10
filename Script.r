# uncomment these two lines to really start from scratch
#cat('\014') # clear console
#rm(list=ls()) # remove saved objects
library(magrittr)
library(data.table)
library(ggplot2)
library(slam)
library(skmeans)
source("Importer.r")

# Import ------------------------------------------------------------------

# check if saved database exists before importing
# if underlying data change run importer again
if(file.exists('out/wok2dbl.RData')) {
	load('out/wok2dbl.RData')
	dat<-wok2dbl
	rm(wok2dbl)
} else {
	dat<-wok2dbl.f(dir = 'WoS.TextFiles.19Sept2018.61551/', out = 'Out')
}

# delete o (order) column
# if we ever need to remember first author, we would need this back
dat[,o:=NULL]

# inspect cited references field
head(dat[field=="CR"], 50)

# key the database on the field to let me query things quickly by field
setkey(dat,field,id)

# Data Audit --------------------------------------------------------------

dat['UT'][duplicated(val),val] %>% cat(sep='\n')
# remove duplicates
dat<-dat[!duplicated(dat)]

# Descriptives ------------------------------------------------------------

# average page numbers and number of works cited by year
# original data in long format, so need to "reshape" to wide format
# long to wide: dcast
# wide to long: melt <- should never need to do this
des<-dcast(data = dat[c("PY", "PG", "NR")]
					 ,formula = id~field
					 ,value.var = "val"
					# ,fun.aggregate = function(x) x[1] # if duplicates, take only first. No longer needed after de-duplication.
)[,`:=`(
	NR=as.numeric(NR) # all data treated as characters on import, so convert to numbers
	,PG=as.numeric(PG)
	,PY=as.numeric(PY)
)]

# aggregate individual records into means by year, so one row per year
des<-des[,.(cites=mean(NR),pages=mean(PG)),by=PY]

# plot and save
ggplot(data=melt(des,id.vars = "PY")) + geom_line(aes(x=PY,y=value,color=variable))
ggsave("citespages.png")


# Edgelist ---------------------------------------------------
# TODO scan titles of articles that are dropped due to not having any citations
setkey(dat,field)
el<-dat['CR',!'field']

keep<-el[,table(val)]
keep<-keep[keep>1]

# sniff test for highly cited items
keep[keep>500] %>% sort(decreasing=T)
prop.table(keep)[keep>500] %>% `*`(100) %>% sort(decreasing=T)

# we want to drop all isolates
setkey(el,val)
el<-el[names(keep)]

# number of citing articles (senders, UT) remaining
el$id %>% unique %>% length
# number of cited articles (receivers, CR) remaining
el$val %>% unique %>% length

# Sparse Matrix -----------------------------------------------------------

# convert CR to factor
el[,`:=`(
	id=factor(id)
	,val=factor(val)
	)]

# make sparse matrix as cited (rows) by citing (columns)
# rows are clustered, columns are "coordinates" determining position of each row
# to cluster citing articles, switch "val" and "id", i.e. transpose matrix
# clustering takes longer the more dimensions...
# ...so clustering cited by citing is easier (matrix is longer than it is wide)
sm<-el[,simple_triplet_matrix(as.numeric(val),as.numeric(id),rep(1,nrow(el)))]
sm

# Kmeans ------------------------------------------------------------------

system.time(km<-skmeans(sm,100,method = 'pclust'))

# count of size of each cluster
km$cluster %>% table
# as percentage of all items
km$cluster %>% table %>% prop.table %>% `*`(100) %>% round(3)

# view top 6 prototypes in each cluster (items nearest center of cluster)
pr<-km$prototypes %>% apply(1,function(x) order(x,decreasing = T) %>% head(6)) %>% data.frame
pr %<>% lapply(function(x) levels(el$val)[x])
names(pr)<-as.character(1:length(pr))
View(pr)

# Secondary Analysis ------------------------------------------------------

# create membership table with year data attached
setkey(dat,field)
m<-merge(dat['CR',.(id,cr=val)],dat['PY',.(id,py=as.integer(val))]) %>% merge(data.table(cr=el$val %>% levels,m=km$cluster),by = 'cr')

# time series of counts by membership and year
ts<-m[,.N,by=.(m,py)] %>% setkey(m,py)
ts[,m:=factor(m)]
# add scaled count as proportion, to compare clusters of very different sizes
ts[,p:=prop.table(N),by=m]

oy<-ts[,.(my=weighted.mean(py,N)),by=m][,c(oldest=which.min(my) %>% as.character,youngest=which.max(my) %>% as.character)]
oy

# plot time series, just oldest and youngest cluster
setkey(ts,m)
ggplot(ts[oy],aes(x=py,y=p,color=m)) + geom_line(size=1) + guides(color=F)
ggsave('oldyoung.png')
# prototypes of oldest and youngest
# note that the "age" here is not the age of the reference, but the dates in which it was cited
# may be interesting to subtract reference date from publication date to measure recency bias over time
pr[oy]
