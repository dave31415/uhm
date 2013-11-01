#pih
#TODO: fix issues with dots in xlsx names. Change to underscores so msql 
#doesn't change them

library(xlsx)
library(data.table)
library(ggplot2)
library(RMySQL)

datadir="/Users/davej/TW/PIH/data/"
plotdir="/Users/davej/TW/PIH/plots/"
big.file=paste(datadir,"UHM/july.xls",sep="")

dopng=T
textsize=18 #use a larger default textsize
#use_theme=theme_bw
use_theme=theme_gray
theme_set(use_theme(base_size=textsize))

catchment=c("Mirebalais","Saut d'Eau","Savanette")

read.uhm<-function(tab="patients",write.mysql=F,read.mysql=T,over.write=F){
   #read patient tab from spreadsheet (default) 
   #TODO: generalize this to other tabs   
   #optionally, write to mysql database 
   #optionally, read from mysql database 
   #the sheets we have with mapping to number
   start=Sys.time()
   db="uhm"
   user="root"
   #the table name that will exist in MySQL
   suff=""
   tab.mysql=paste(tab,suff,sep="")
   sheets=list(patients=6,checkins=7,consultations=8,pivot_diagnoses=9,
      diagnoses=10,visits=11,hospitalization=12,vitals=13,pivot_surgery=14,
      postOpNote1=15,postOpNote2=16)
   if (!(tab %in% names(sheets))) {
      print("sheetlist")
      print(sheets)
      stop(paste("tab",tab,"not in sheet list"))
   }
   sheet=sheets[[tab]]
   start.read=Sys.time()
   if (read.mysql) {
      print(paste("Reading from MySQL, Table:",tab.mysql))
      con=dbConnect(MySQL(),user=user,db=db)
      if(!(tab.mysql %in% dbListTables(con))) {
         stop(paste("Table",tab.mysql,"not in mysql"))
      }
      data=data.table(dbReadTable(con,tab.mysql))
      dbDisconnect(con)
   } else {
      print("Reading from spreadsheet")
      data=data.table(read.xlsx(big.file,sheet))
      print(paste(nrow(data),"rows"))
   }
   print("Read time")
   print(Sys.time()-start.read)
   if (write.mysql) {
      start.write=Sys.time()
      print(paste("Writing to MySQL, Table: ",tab.mysql))
      con=dbConnect(MySQL(),user=user,db=db)
      if(tab.mysql %in% dbListTables(con)) {
         if (over.write==F){
            stop(paste("Table",tab.mysql,"already exist, use over.write=T to over-write"))
         } else {
            print(paste("Over-writing table",tab.mysql))
            dbRemoveTable(con,tab.mysql)
         }
      }
      dbWriteTable(con,tab.mysql,data)
      dbDisconnect(con)
      print("Write time")
      print(Sys.time()-start.write)
   }
   print("Total time")
   print(Sys.time()-start)
   return(data)
}

get.maternity<-function(pat=read.uhm(tab="patients")){
   #Get all maternity patients and relevant columns
   Maternity=pat[Maternity_patient_==TRUE,
                 list(gender,age_at_reg,commune)]
   #total number of maternity patients
   Tot=nrow(Maternity)
   #add inside.catchment column
   Maternity[,inside.catchment:=commune %in% catchment]   
   #add age groups TODO: check these 
   breaks=c(0,5,14,49,150)
   Maternity[,age.group:=cut(age_at_reg,br=breaks)]
}

get.womans.triage<-function(diag=read.uhm(tab="diagnoses")){
   #TODO: check this logic with Phil, looks to give 
   #numbers different from spreadsheet
   #TODO, Woman's Triage has a funny character in it, why, issue with mysql,R,excel ?
   return(diag[name %in% c("Women\x92s Triage","Woman's Clinic"),])
}

maternity<-function(pat=read.uhm(tab="patients")){
   #replace excel formulas like
   #=COUNTIFS(patients!$L$2:$L$12000,"="&"F",patients!$Z$2:$Z$12000,"1")
   
   #Get all maternity patients and relevant columns
   Maternity=get.maternity()
   
   #calculate tables
   #using data.table functionality
   Male.Fem=Maternity[,.N,by=gender]
   #OR like this, standard R functionality
   #Male.Fem=table(Maternity[,gender])
   Age.Groups=Maternity[,.N,by=age.group]
   Geo.Orig=Maternity[,.N,by=inside.catchment]
   
   #add percentages
   Male.Fem[,Percent:=round(100*N/Tot)]
   Age.Groups[,Percent:=round(100*N/Tot)]
   Geo.Orig[,Percent:=round(100*N/Tot)]
   #print out information
   line="---------------------------"
   print(Male.Fem)
   print(line)
   print(Age.Groups)
   print(line)
   print(Geo.Orig)
   
   #make some plots
   mf.plot<-ggplot(Mat,aes(factor(1),fill=gender))+geom_bar()+coord_polar("y")+ylab("")+xlab("")
   age.plot<-ggplot(Mat,aes(factor(1),fill=age.group))+geom_bar()+coord_polar("y")+ylab("")+xlab("")
   #put together into one panel?
   multiplot(mf.plot,age.plot,cols=1)
}

womans.triage.top10<-function(){
   wt=get.womans.triage()
   counts=wt[,.N,by=diagnosis_coded_en]
   top.ten=counts[order(-N),][1:10,]
   top.ten=na.omit(top.ten)
   print(top.ten)
   #TODO: add an ordered factor so it plots the bars in right order                            
   top.ten[,diag:=diagnosis_coded_en]
   top.ten.plot<-ggplot(top.ten,aes(diag,N))+geom_bar(stat="identity")
   top.ten.plot=top.ten.plot+xlab("Diagnosis")
   top.ten.plot=top.ten.plot+ylab("Number")+coord_flip()
   #opts(axis.text.x  = theme_text(angle=35,hjust = 1,vjust =1))
   
   #print(top.ten.plot)
   #use showplot instead and show how it can be saved to file
   show_plot(top.ten.plot,dopng=dopng,file="top.ten.women",
             width=1200,height=600)
   return(top.ten)
}

load.all.mysql<-function(){
   #read all tabs from the spreadsheet and write them all to mysql
   #after that, read the mysql tables in just to check things
   
   sheets=list(
               patients=6,checkins=7,consultations=8,
               #pivot_diagnoses=9,
               diagnoses=10,visits=11,hospitalization=12,
               vitals=13,
               #pivot_surgery=14,
               postOpNote1=15,
               postOpNote2=16)
   
   for (tab in names(sheets)) {
      print(tab)
      d=read.uhm(tab,write.mysql=T)
      d2=read.uhm(tab,read.mysql=T)
   }
}

multiplot <- function(..., plotlist=NULL, cols) {
   #borrowed from somewhere online
   #calllike this multiplot(p1,p1,p3...,cols=2)
   require(grid)   
   # Make a list from the ... arguments and plotlist
   plots <- c(list(...), plotlist)
   numPlots = length(plots)
   # Make the panel
   plotCols = cols                          # Number of columns of plots
   plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
   # Set up the page
   grid.newpage()
   pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
   vplayout <- function(x, y)
      viewport(layout.pos.row = x, layout.pos.col = y)

   # Make each plot, in the correct location
   for (i in 1:numPlots) {
      curRow = ceiling(i/plotCols)
      curCol = (i-1) %% plotCols + 1
      print(plots[[i]], vp = vplayout(curRow, curCol ))
   }
}

make.ordered.fac<-function(x){
   #TODO, finish this
}

wrap <- function(x,wid=10) {paste(strwrap(x,width=wid), collapse = "\n")}

show_plot<-function(p="Nothing to Plot",dopng=F,file="TemporaryPlot",extra=NULL,
                    sep="_",verb=0,width=800,height=700,dir=plotdir) {
   
   #a useful utility function for plotting or just printing to window
   #p<-qplot(c(0,1))
   #show_plot(p,dopng=T)
   if (!dopng) {
      print(p)
      #and nothing else
   } else {
      #uses global plotdir
      wmessage=paste("Warning, no file provided. Printing to :",file)
      if (file == "TemporaryPlot") print(wmessage)
      
      ex=""
      if (! is.null(extra)) {
         #extra stuff to join in with underscores
         ex=paste(sep,paste(extra,collapse=sep),sep="")
      }
      print(length(ex))
      outfile=paste(dir,file,ex,".png",sep="")
      if (verb > 0) print(paste("Writing to file:",outfile))
      png(outfile,width=width,height=height)
      print(p)
      dev.off()
   }
}

