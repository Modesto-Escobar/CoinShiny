gcoin<-function(Data, variables, Characteristics=NULL, color="$", shape="$",minimum=5, p=.5, Bonferroni=FALSE, size=30, lwidth=5, layout="fruchertman.reingold", palette=NA) {
  require(igraph)
  D <- subset(Data, select=variables)
  R=data.matrix(D)
  Q<-R[,colSums(R)>=minimum]
  varlist<-colnames(Q)
  N<-Haberman(Q)
  ifelse(Bonferroni,b<-ncol(N)*(ncol(N)-1)/2,b<-1)
  A<-Adjacency(N, p, b, nrow(D))
  if (p==1) A[crossprod(Q)==0]<-0
  if (layout=="kamada.kawai") A[A!=0]<-A[A!=0]-min(A)+1E-6
  G<-graph.adjacency(A, weighted=T, mode="undirected")
  G<-simplify(G)
  if (is.data.frame(Characteristics)) {
     rownames(Characteristics)<-Characteristics$variable #Possible parameter
     Characteristics<-Characteristics[varlist,]
     if(shape!="$") V(G)$shape<-as.character(Characteristics[V(G),shape])
     V(G)$label<-as.character(Characteristics[V(G),"label"]) #Possible parameter
     if (color!="$") {
       if (is.na(match(Characteristics[1,color],colors()))) {
          V(G)$color<-pColor(subset(Characteristics,select=color),palette)
       } else V(G)$color<-as.character(Characteristics[V(G),color])
     }
     else V(G)$color<-"azure"
  }
  V(G)$size<-sqrt(colSums(data.matrix(Q))/max(colSums(data.matrix(Q)))*size*20)
  egam<-pmax(log(E(G)$weight-min(E(G)$weight)+1),.1)
  egam<-egam/(4*max(egam))+.25
  E(G)$color <- rgb(0.5, 0.5, 0, egam)
  E(G)$width <- egam*lwidth
  plot(G, layout=sLayout(G,layout))
  return(G)
}
## Routine from Data-frame to adjacency Matrix
Haberman<-function (Q, minimum=5) {
  L<-colSums(Q)
  M<-crossprod(Q)
  n=nrow(Q)
  E<-tcrossprod(L)/n
  N<-((M-E)/sqrt(E))/sqrt(tcrossprod(1-(L/n)))
  return(N)
}
Frequency<-function (D, minimum=5) {
  Q=data.matrix(D)
  Q<-Q[,colSums(Q)>=minimum]
  L<-colSums(Q)
  M<-crossprod(Q)
  return(M)
}

Adjacency<- function (N, p, b, n) {
  A<-N
  A[(1-pt(N,n))>=(p/b)]<-0
  diag(A)<-0
  return(A)
} 

Edge<- function(Data, variables, Characteristics=NULL, var="variable", lab="label", directed=FALSE, haberman=FALSE, min=1) {
  D <- subset(Data, select=variables)
  ifelse(haberman,Matrix<-Haberman(D,min),Matrix<-Frequency(D,min))
  Mat<-NULL
  if(is.null(Characteristics)){
    for(i in seq(1:ncol(Matrix))) {
      ifelse(directed,top<-ncol(Matrix),top<-i)
      for (j in seq(1:top)) {
        if (i!=j & Matrix[i,j]>0) Mat<-(c(Mat,c(colnames(Matrix)[i],
                                                colnames(Matrix)[j],
                                                Matrix[i,j])))
      }
    }
  }
  else {
    for(i in seq(1:ncol(Matrix))) {
        ifelse(directed,top<-ncol(Matrix),top<-i)
        for (j in seq(1:top)) {
          if (i!=j & Matrix[i,j]>0) Mat<-(c(Mat,c(as.character(Characteristics[[lab]][Characteristics[[var]]==colnames(Matrix)[i]]),
                                              as.character(Characteristics[[lab]][Characteristics[[var]]==colnames(Matrix)[j]]),
                                              Matrix[i,j])))
          }
     }
  }
  Mat<-matrix(Mat,ncol=3,byrow=TRUE)
  colnames(Mat)<-c("Source","Target", "Weight")
  Mat<-as.data.frame(Mat)
  Mat["Weight"]<-apply(Mat["Weight"],1,as.numeric)
  return(Mat)
}

sLayout<-function(x, layout) {
  switch(layout,
         circle = layout.circle(x),
         fruchterman.reingold = layout.fruchterman.reingold(x),
         gem = layout.gem(x),
         kamada.kawai = layout.kamada.kawai(x),
         random = layout.random(x),
         reingold.tilford = layout.reingold.tilford(x),
         star = layout.star(x))
}

pColor<-function(x, palette=NA) {
  x<-as.numeric(as.factor(as.matrix(x)))
  y=rep(NA,length(x))
    if (is.na(palette[1])) palette=c("azure", "pink", "limegreen", "red2", "steelblue", "sienna", "aquamarine3", "yellow","maroon", "white")
    for (i in 1:max(x)) {
         y[which(x==i)]<-palette[i]
    }
  return(y)
}
