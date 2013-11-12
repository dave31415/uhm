#setup variables, globals, libraries etc

library(xlsx)
library(data.table)
library(reshape2)
library(ggplot2)
library(RMySQL)
library(R2HTML)

datadir=paste(rootdir,"data/",sep="")
plotdir=paste(rootdir,"plots/",sep="")

big.file=paste(datadir,"UHM/july.xls",sep="")
html.file.orig=paste(plotdir,"plots.html",sep="")
html.file=paste(plotdir,"maternity.html",sep="")

dopng=F
textsize=18 #use a larger default textsize
#use_theme=theme_bw
use_theme=theme_gray
theme_set(use_theme(base_size=textsize))

catchment=c("Mirebalais","Saut d'Eau","Savanette")

source(paste(rootdir,"uhm/io.R",sep=""))
source(paste(rootdir,"uhm/util.R",sep=""))
