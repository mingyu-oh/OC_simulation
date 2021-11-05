########################################
#### 1. Simulation Function WITH Label
########################################

myfun_label <- function(Delta, SDRatio,PointSize, xref, yref){
M = c("Minmax","SD2","SD3","TI9099","TI9599","TOST1.5","TOST2","TOST3","PI99")
nseq = seq(4,30,by=1)
tmp = as.data.frame(matrix(NA,length(M)*length(nseq),2,byrow=T))
colnames(tmp) = c("FN","FP")
tmp$method = M
tmp$N = rep(nseq,each=length(M))
class = matrix(0,41,41)
colnames(class) = seq(0,4,by=0.1)
rownames(class) = seq(0,4,by=0.1)
  for(mi in 1:length(seq_)){
    for(sdi in 1:length(seq_)){
      class[mi,sdi] = (SDRatio/Delta)*seq_[mi]+seq_[sdi] <= SDRatio
    }}
  
  f = as.data.frame(matrix(NA,length(M),2,byrow=T))
  for(ni in 1:length(nseq)){
    for(i in 1:length(M)){
      f[i,1] = mean(c(1-res[,,i,ni])[c(class)==1])
      f[i,2] = mean(c(res[,,i,ni])[c(class)==0])
    }
    start = 1+length(M)*(ni-1)
    tmp[start:(start+length(M)-1),1:2] = f
  }
  
  d1 = tmp[tmp[,3]==M[1],]
  d2 = tmp[tmp[,3]==M[2],]
  d3 = tmp[tmp[,3]==M[3],]
  d4 = tmp[tmp[,3]==M[4],]
  d5 = tmp[tmp[,3]==M[5],]
  d6 = tmp[tmp[,3]==M[6],]
  d7 = tmp[tmp[,3]==M[7],]
  d8 = tmp[tmp[,3]==M[8],]
  d9 = tmp[tmp[,3]==M[9],]
  
  colnames(d1)= c("FN","FP","method","Minmax") 
  colnames(d2)= c("FN","FP","method","SD2") 
  colnames(d3)= c("FN","FP","method","SD3") 
  colnames(d4)= c("FN","FP","method","TI9099") 
  colnames(d5)= c("FN","FP","method","TI9599") 
  colnames(d6)= c("FN","FP","method","TOST") 
  colnames(d7)= c("FN","FP","method","TOST2") 
  colnames(d8)= c("FN","FP","method","TOST3") 
  colnames(d9)= c("FN","FP","method","PI99") 
  
  p = ggplot()+
    geom_point(data= d1, aes(x=FP, y=FN, color=Minmax),size=PointSize) +
    scale_color_gradient(low="orange", high="red") +
    new_scale_color() + 
    
    geom_point(data= d2, aes(x=FP, y=FN, color=SD2),size=PointSize) +
    scale_color_gradient(low="lightblue", high="#3C5488B2") +
    new_scale_color() +
    
    geom_point(data= d3, aes(x=FP, y=FN, color=SD3),size=PointSize) +
    scale_color_gradient(low="orange", high="brown") +
    new_scale_color() +
    
    geom_point(data= d4, aes(x=FP, y=FN, color=TI9099),size=PointSize) +
    scale_color_gradient(low="lightgreen", high="#7CAE00") +
    new_scale_color() +
    
    geom_point(data= d5, aes(x=FP, y=FN, color=TI9599),size=PointSize) +
    scale_color_gradient(low="#C77CFF", high="purple") +
    new_scale_color() +
    
    geom_point(data= d6, aes(x=FP, y=FN, color=TOST),size=PointSize) +
    scale_color_gradient(low="#8491B4B2", high="black") +
    new_scale_color() +
    
    geom_point(data= d7, aes(x=FP, y=FN, color=TOST2),size=PointSize, show.legend = F) +
    scale_color_gradient(low="#8491B4B2", high="black") +
    new_scale_color() +
    
    geom_point(data= d8, aes(x=FP, y=FN, color=TOST3),size=PointSize, show.legend = F) +
    scale_color_gradient(low="#8491B4B2", high="black") +
    new_scale_color() +
    
    geom_point(data= d9, aes(x=FP, y=FN, color=PI99),size=PointSize) +
    scale_color_gradient(low="#00BFC4", high="blue") + 
    geom_text() +
    # annotate("text", label = "0.05", x = 0.025, y = -0.025, size = 4, colour = "black",fontface=1)+
    # annotate("text", label = "0.10", x = 0.125, y = -0.025, size = 4, colour = "black",fontface=1)+
    
    annotate("text", label = "MinMax", x = mean(0.1+d1[,2]), y = mean(d1[,1]), size = 5, colour = "black",fontface=2)+
    annotate("text", label = "SD2", x = mean(0.1+d2[,2]), y = mean(d2[,1]), size = 5, colour = "black",fontface=2)+
    annotate("text", label = "SD3", x = mean(0.1+d3[,2]), y = mean(d3[,1]), size = 5, colour = "black",fontface=2)+
    annotate("text", label = "TI9099", x = mean(0.1+d4[,2]), y = mean(d4[,1]), size = 5, colour = "black",fontface=2)+
    annotate("text", label = "TI9599", x = mean(0.1+d5[,2]), y = mean(d5[,1]), size = 5, colour = "black",fontface=2)+
    annotate("text", label = "TOST 1.5", x = mean(0.1+d6[,2]), y = mean(d6[,1]), size = 5, colour = "black",fontface=2)+
    annotate("text", label = "TOST 2", x = mean(0.1+d7[,2]), y = mean(d7[,1]), size = 5, colour = "black",fontface=2)+
    annotate("text", label = "TOST 3", x = mean(0.1+d8[,2]), y = mean(d8[,1]), size = 5, colour = "black",fontface=2)+
    annotate("text", label = "PI99", x = mean(0.1+d9[,2]), y = mean(d9[,1]), size = 5, colour = "black",fontface=2)+ 
    
    scale_x_continuous(breaks=seq(0,1,by=0.1)) +
    scale_y_continuous(breaks=seq(0,1,by=0.1)) +
    # geom_vline(xintercept = 0, size = 1.2)+
    # geom_hline(yintercept = 0, size = 1.2)+
    geom_vline(xintercept = xref, color='red')+
    geom_hline(yintercept = yref, color='red')+ 
    labs(x = 'Average False Accept Rate (FP)',
         y = 'Average False Accept Rate (FN)',
         title = 'Simulation Results :')+
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))+theme_bw()+
 
    theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top"), legend.box="horizontal",
    legend.background = element_rect(fill=F),
    legend.box.just = "right",
    legend.margin = margin( ))+
    theme(plot.margin=unit(c(2,2,2,2),"cm"), plot.title = element_text(size = 20),
          axis.title.x = element_text(size = 15), 
          axis.title.y = element_text(size = 15))
  return(p)
}


########################################
#### 2. Simulation Function WITHOUT Label
########################################
myfun_nolabel <- function(Delta, SDRatio,PointSize, xref, yref){
M = c("Minmax","SD2","SD3","TI9099","TI9599","TOST1.5","TOST2","TOST3","PI99")
nseq = seq(4,30,by=1)
tmp = as.data.frame(matrix(NA,length(M)*length(nseq),2,byrow=T))
colnames(tmp) = c("FN","FP")
tmp$method = M
tmp$N = rep(nseq,each=length(M))
class = matrix(0,41,41)
colnames(class) = seq(0,4,by=0.1)
rownames(class) = seq(0,4,by=0.1)
  for(mi in 1:length(seq_)){
    for(sdi in 1:length(seq_)){
      class[mi,sdi] = (SDRatio/Delta)*seq_[mi]+seq_[sdi] <= SDRatio
    }}
  
  f = as.data.frame(matrix(NA,length(M),2,byrow=T))
  for(ni in 1:length(nseq)){
    for(i in 1:length(M)){
      f[i,1] = mean(c(1-res[,,i,ni])[c(class)==1])
      f[i,2] = mean(c(res[,,i,ni])[c(class)==0])
    }
    start = 1+length(M)*(ni-1)
    tmp[start:(start+length(M)-1),1:2] = f
  }
  
  d1 = tmp[tmp[,3]==M[1],]
  d2 = tmp[tmp[,3]==M[2],]
  d3 = tmp[tmp[,3]==M[3],]
  d4 = tmp[tmp[,3]==M[4],]
  d5 = tmp[tmp[,3]==M[5],]
  d6 = tmp[tmp[,3]==M[6],]
  d7 = tmp[tmp[,3]==M[7],]
  d8 = tmp[tmp[,3]==M[8],]
  d9 = tmp[tmp[,3]==M[9],]
  
  colnames(d1)= c("FN","FP","method","Minmax") 
  colnames(d2)= c("FN","FP","method","SD2") 
  colnames(d3)= c("FN","FP","method","SD3") 
  colnames(d4)= c("FN","FP","method","TI9099") 
  colnames(d5)= c("FN","FP","method","TI9599") 
  colnames(d6)= c("FN","FP","method","TOST") 
  colnames(d7)= c("FN","FP","method","TOST2") 
  colnames(d8)= c("FN","FP","method","TOST3") 
  colnames(d9)= c("FN","FP","method","PI99") 
  
  p = ggplot()+
    geom_point(data= d1, aes(x=FP, y=FN, color=Minmax),size=PointSize)+
    scale_color_gradient(low="orange", high="red") +
    new_scale_color() + 
    
    geom_point(data= d2, aes(x=FP, y=FN, color=SD2),size=PointSize) +
    scale_color_gradient(low="lightblue", high="#3C5488B2") +
    new_scale_color() +
    
    geom_point(data= d3, aes(x=FP, y=FN, color=SD3),size=PointSize) +
    scale_color_gradient(low="orange", high="brown") +
    new_scale_color() +
    
    geom_point(data= d4, aes(x=FP, y=FN, color=TI9099),size=PointSize) +
    scale_color_gradient(low="lightgreen", high="#7CAE00") +
    new_scale_color() +
    
    geom_point(data= d5, aes(x=FP, y=FN, color=TI9599),size=PointSize) +
    scale_color_gradient(low="#C77CFF", high="purple") +
    new_scale_color() +
    
    geom_point(data= d6, aes(x=FP, y=FN, color=TOST),size=PointSize) +
    scale_color_gradient(low="#8491B4B2", high="black") +
    new_scale_color() +
    
    geom_point(data= d7, aes(x=FP, y=FN, color=TOST2),size=PointSize, show.legend = F) +
    scale_color_gradient(low="#8491B4B2", high="black") +
    new_scale_color() +
    
    geom_point(data= d8, aes(x=FP, y=FN, color=TOST3),size=PointSize, show.legend = F) +
    scale_color_gradient(low="#8491B4B2", high="black") +
    new_scale_color() +
    
    geom_point(data= d9, aes(x=FP, y=FN, color=PI99),size=PointSize)+
    scale_color_gradient(low="#00BFC4", high="blue")+ 
    geom_text() +
    # annotate("text", label = "0.05", x = 0.025, y = -0.025, size = 4, colour = "black",fontface=1)+
    # annotate("text", label = "0.10", x = 0.125, y = -0.025, size = 4, colour = "black",fontface=1)+

    scale_x_continuous(breaks=seq(0,1,by=0.1)) +
    scale_y_continuous(breaks=seq(0,1,by=0.1)) +
    
    geom_vline(xintercept = xref, color='red') +
    geom_hline(yintercept = yref, color='red') + 
    labs(x = 'Average False Accept Rate (FP)',
         y = 'Average False Accept Rate (FN)',
         title = 'Simulation Results')+
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1.0)) + theme_bw()+
    
    theme(
      legend.position = c(0.98, 0.98),
      legend.justification = c("right", "top"), legend.box="horizontal",
      legend.box.just = "right",
      legend.margin = margin())+
    theme(plot.margin=unit(c(2,2,2,2),"cm"), plot.title = element_text(size = 20),
          axis.title.x = element_text(size = 15), 
          axis.title.y = element_text(size = 15))
  return(p)
}



########################################
#### 3. SummaryTable Function 
########################################
myfun_tab <- function(Delta, SDRatio){
  M = c("Minmax","SD2","SD3","TI 9099","TI 9599","TOST 1.5","TOST 2","TOST 3","PI 99")
  nseq = seq(4,30,by=1)
  tmp = as.data.frame(matrix(NA,length(M)*length(nseq),2,byrow=T))
  colnames(tmp) = c("FN","FP")
  tmp$method = M
  tmp$N = rep(nseq,each=length(M))
  
  class = matrix(0,41,41)
  colnames(class) = seq(0,4,by=0.1)
  rownames(class) = seq(0,4,by=0.1)
  for(mi in 1:length(seq_)){
    for(sdi in 1:length(seq_)){
      class[mi,sdi] = (SDRatio/Delta)*seq_[mi]+seq_[sdi] <= SDRatio
    }}
  
  f = as.data.frame(matrix(NA,length(M),2,byrow=T))
  for(ni in 1:length(nseq)){
    for(i in 1:length(M)){
      f[i,1] = mean(c(1-res[,,i,ni])[c(class)==1])
      f[i,2] = mean(c(res[,,i,ni])[c(class)==0])
    }
    start = 1+length(M)*(ni-1)
    tmp[start:(start+length(M)-1),1:2] = f
  }
  
  d1 = tmp[tmp[,3]==M[1],]
  d2 = tmp[tmp[,3]==M[2],]
  d3 = tmp[tmp[,3]==M[3],]
  d4 = tmp[tmp[,3]==M[4],]
  d5 = tmp[tmp[,3]==M[5],]
  d6 = tmp[tmp[,3]==M[6],]
  d7 = tmp[tmp[,3]==M[7],]
  d8 = tmp[tmp[,3]==M[8],]
  d9 = tmp[tmp[,3]==M[9],]
  
  R = round(seq(0.05,0.4,by=0.05),2)
  FP_res = matrix(NA,length(M),length(R))
  
  for(aaa in 1:length(M)){
    for(bbb in 1:length(R)){
      
      #fn = mget(paste0("d",aaa))[[1]]$FN
      d = mget(paste0("d",aaa))[[1]]
      
      w = nseq[which(d$FP < R[bbb])]
      
      if(length(w) != 0 & (d$FP[1] > d$FP[10])){
        FP_res[aaa,bbb] = min(w)
      }else if(length(w) != 0 & (d$FP[1] <= d$FP[10]))
      {
        FP_res[aaa,bbb] = max(w)
      }else{
        FP_res[aaa,bbb] = 0
      }
    }}
  colnames(FP_res) = R
  rownames(FP_res) = M
  data.frame(as.character(c(1,FP_res)))
return(FP_res)  
}

 
