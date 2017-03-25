library(sqldf)
library(flexclust)
library(stringr)
library(ggplot2)
########################################################

# Multiple plot function made available for public use
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


sql<-function(str){
  sqldf()
  ret<-sqldf(str,drv="SQLite")
  sqldf()
  return(ret)
}


nearest<-function(col,k_nearest){
  #Get the indices of the k smallest values of a vector
  order(col, decreasing = F)[1:k_nearest]
}


#The following is my own nearest neighbor algorithm
#created to change the distance metric
my_knn<-function(train,test,cl,k,dist_mat,P=2){
  #Nearest neighbor alg. for binary response
  #distances can be "euclidean","maximum",
  #manhattan, canberra, binary, or minkowski
  
  #if minkowski, then P is the power of the Minkowski distance
  
  #each row contains indices from the training set of the nearest neighbors
  #for each instance in the testing set
  nearest_neighbors<-apply(dist_mat[,],2,nearest,k_nearest=k)
  
  if(k==1){
    nearest_neighbors<-t(as.matrix(nearest_neighbors))
    responses<-data.frame(matrix(cl[nearest_neighbors[,]],ncol = k,byrow = T))
    pred<-as.factor(responses[,1])
  }else {
    
    #each row nor contains the responses of the k nearest neighbors
    responses<-data.frame(matrix(cl[nearest_neighbors[,]],ncol = k,byrow = T))
    
    response_cts_temp<-apply(
      responses,1,
      function(x){sum(as.numeric(as.character((as.factor(unlist(x))))))})
    
    response_cts<-data.frame(
      spam=response_cts_temp,nonspam=k-response_cts_temp)
    
    #Break ties randomly
    ties<-response_cts[response_cts$spam==k/2,]
    add_spam<-rbinom(n=nrow(ties),size = 1,prob=0.5)
    add_ham<-1-add_spam
      
    ties$spam<-ties$spam+add_spam
    ties$nonspam<-ties$nonspam+add_ham
    
    response_cts[response_cts$spam==k/2,]<-ties
    #Breaking ties finished
    
    pred<-as.factor((response_cts$spam>response_cts$nonspam)*1) 
  }
  return(pred)
}


simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

# Write a prediction function
pred = function(nn, dat) {
    yhat = round(compute(nn, dat)$net.result)
    return(yhat)
}

tracePlot<-function(title_str,dat){
  ggplot(dat,aes(x=iteration,y=dat[,2]))+
  geom_line()+
  theme_minimal()+
  theme(text=element_text(family="Times"))+
  ggtitle(title_str)+
  theme(plot.title=element_text(face="bold",
                                  hjust=-.08,vjust=2,colour="#3C3C3C",size=17))+
  theme(axis.title.y=element_text(size=11,
                                    colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,
                                    colour="#535353",face="bold",vjust=-.5))+
  ylab('Sample Value')+
  xlab('Index')
}
  
  
  
  
