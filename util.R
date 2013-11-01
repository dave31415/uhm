#utility functions

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

order.fac<-function(x){
   #order a factor so that ggplot puts it in right order
   y <- factor(x, 
               levels=names(sort(table(x), 
                                 decreasing=TRUE)),ordered=T)
   return(y)
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
