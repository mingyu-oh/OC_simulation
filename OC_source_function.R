myfun_label <- function(Delta, SDRatio){
  
  tmp = as.data.frame(matrix(NA,length(M)*length(nseq),2,byrow=T))
  colnames(tmp) = c("FN","FP")
  tmp$method = M
  tmp$N = rep(nseq,each=length(M))
  
  class = matrix(0,41,41)
  colnames(class) = seq(0,4,by=0.1)
  rownames(class) = seq(0,4,by=0.1)
  for(mi in 1:length(seq)){
    for(sdi in 1:length(seq)){
      class[mi,sdi] = (SDRatio/Delta)*seq[mi]+seq[sdi] <= SDRatio
    }}
  
  f = as.data.frame(matrix(NA,length(M),2,byrow=T))
  for(ni in 1:length(nseq)){
    for(i in 1:length(M)){
      f[i,1] = mean(c(1-RESULT[,,i,ni])[c(class)==1])
      f[i,2] = mean(c(RESULT[,,i,ni])[c(class)==0])
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
  
  
  
  size_point=2
  
  p = ggplot()+
    geom_point(data= d1, aes(x=FP, y=FN, color=Minmax),size=size_point) +
    scale_color_gradient(low="purple", high="blue")+
    new_scale_color() + 
    
    geom_point(data= d2, aes(x=FP, y=FN, color=SD2),size=size_point) +
    scale_color_gradient(low="yellow", high="brown")+
    new_scale_color() +
    
    geom_point(data= d3, aes(x=FP, y=FN, color=SD3),size=size_point) +
    scale_color_gradient(low="orange", high="brown")+
    new_scale_color() +
    
    geom_point(data= d4, aes(x=FP, y=FN, color=TI9099),size=size_point) +
    scale_color_gradient(low="lightgreen", high="green")+
    new_scale_color() +
    
    geom_point(data= d5, aes(x=FP, y=FN, color=TI9599),size=size_point) +
    scale_color_gradient(low="brown", high="orange")+
    new_scale_color() +
    
    geom_point(data= d6, aes(x=FP, y=FN, color=TOST),size=size_point) +
    scale_color_gradient(low="white", high="brown")+
    new_scale_color() +
    
    geom_point(data= d7, aes(x=FP, y=FN, color=TOST2),size=size_point, show.legend = F) +
    scale_color_gradient(low="white", high="brown")+
    new_scale_color() +
    
    geom_point(data= d8, aes(x=FP, y=FN, color=TOST3),size=size_point, show.legend = F) +
    scale_color_gradient(low="white", high="brown")+
    new_scale_color() +
    
    geom_point(data= d9, aes(x=FP, y=FN, color=PI99),size=size_point) +
    scale_color_gradient(low="pink", high="purple")+
    # geom_label(label="TOST 1.5SD", x=0.2, y=0.5,label.size = 0.35,color = "black")+
    geom_text() +
    annotate("text", label = "0.05", x = 0.025, y = -0.025, size = 4, colour = "black",fontface=1)+
    annotate("text", label = "0.10", x = 0.125, y = -0.025, size = 4, colour = "black",fontface=1)+
    
    
    annotate("text", label = "MinMax", x = mean(0.05+d1[,2]), y = mean(d1[,1]), size = 5, colour = "black",fontface=2)+
    annotate("text", label = "2SD", x = mean(0.05+d2[,2]), y = mean(d2[,1]), size = 5, colour = "black",fontface=2)+
    annotate("text", label = "3SD", x = mean(0.05+d3[,2]), y = mean(d3[,1]), size = 5, colour = "black",fontface=2)+
    annotate("text", label = "TI9099", x = mean(0.05+d4[,2]), y = mean(d4[,1]), size = 5, colour = "black",fontface=2)+
    annotate("text", label = "TI9599", x = mean(0.05+d5[,2]), y = mean(d5[,1]), size = 5, colour = "black",fontface=2)+
    annotate("text", label = "TOST 1.5", x = mean(0.05+d6[,2]), y = mean(d6[,1]), size = 5, colour = "black",fontface=2)+
    annotate("text", label = "TOST 2", x = mean(0.05+d7[,2]), y = mean(d7[,1]), size = 5, colour = "black",fontface=2)+
    annotate("text", label = "TOST 3", x = mean(0.05+d8[,2]), y = mean(d8[,1]), size = 5, colour = "black",fontface=2)+
    annotate("text", label = "PI99", x = mean(0.05+d9[,2]), y = mean(d9[,1]), size = 5, colour = "black",fontface=2)+ 
    
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=13,color="darkred"))+
    geom_vline(xintercept = 0, size = 1.2)+
    geom_hline(yintercept = 0, size = 1.2)+
    geom_vline(xintercept = c(0.05,0.1),color='red')+
    geom_hline(yintercept =  c(0.1,0.05),color='red')+
    labs(x = 'Average False Accept Rate(FP)',
         y = 'Average False Accept Rate(FN)',
         title = 'Simulation Results')+
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1.0))+ theme_bw()+
    guides(fill = guide_colourbar(barwidth = 0.7,barheight = 0.3)) 
  return(p)
}



####
myfun_nolabel <- function(Delta, SDRatio){
  
  tmp = as.data.frame(matrix(NA,length(M)*length(nseq),2,byrow=T))
  colnames(tmp) = c("FN","FP")
  tmp$method = M
  tmp$N = rep(nseq,each=length(M))
  
  class = matrix(0,41,41)
  colnames(class) = seq(0,4,by=0.1)
  rownames(class) = seq(0,4,by=0.1)
  for(mi in 1:length(seq)){
    for(sdi in 1:length(seq)){
      class[mi,sdi] = (SDRatio/Delta)*seq[mi]+seq[sdi] <= SDRatio
    }}
  
  f = as.data.frame(matrix(NA,length(M),2,byrow=T))
  for(ni in 1:length(nseq)){
    for(i in 1:length(M)){
      f[i,1] = mean(c(1-RESULT[,,i,ni])[c(class)==1])
      f[i,2] = mean(c(RESULT[,,i,ni])[c(class)==0])
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
  
  
  
  size_point=2
  
  p = ggplot()+
    geom_point(data= d1, aes(x=FP, y=FN, color=Minmax),size=size_point) +
    scale_color_gradient(low="purple", high="blue")+
    new_scale_color() + 
    
    geom_point(data= d2, aes(x=FP, y=FN, color=SD2),size=size_point) +
    scale_color_gradient(low="yellow", high="brown")+
    new_scale_color() +
    
    geom_point(data= d3, aes(x=FP, y=FN, color=SD3),size=size_point) +
    scale_color_gradient(low="orange", high="brown")+
    new_scale_color() +
    
    geom_point(data= d4, aes(x=FP, y=FN, color=TI9099),size=size_point) +
    scale_color_gradient(low="lightgreen", high="green")+
    new_scale_color() +
    
    geom_point(data= d5, aes(x=FP, y=FN, color=TI9599),size=size_point) +
    scale_color_gradient(low="brown", high="orange")+
    new_scale_color() +
    
    geom_point(data= d6, aes(x=FP, y=FN, color=TOST),size=size_point) +
    scale_color_gradient(low="white", high="brown")+
    new_scale_color() +
    
    geom_point(data= d7, aes(x=FP, y=FN, color=TOST2),size=size_point, show.legend = F) +
    scale_color_gradient(low="white", high="brown")+
    new_scale_color() +
    
    geom_point(data= d8, aes(x=FP, y=FN, color=TOST3),size=size_point, show.legend = F) +
    scale_color_gradient(low="white", high="brown")+
    new_scale_color() +
    
    geom_point(data= d9, aes(x=FP, y=FN, color=PI99),size=size_point) +
    scale_color_gradient(low="pink", high="purple")+
    # geom_label(label="TOST 1.5SD", x=0.2, y=0.5,label.size = 0.35,color = "black")+
    geom_text() +
    annotate("text", label = "0.05", x = 0.025, y = -0.025, size = 4, colour = "black",fontface=1)+
    annotate("text", label = "0.10", x = 0.125, y = -0.025, size = 4, colour = "black",fontface=1)+

    
    geom_vline(xintercept = 0, size = 1.2)+
    geom_hline(yintercept = 0, size = 1.2)+
    geom_vline(xintercept = c(0.05,0.1),color='red',label="0.05")+
    geom_hline(yintercept =  c(0.1,0.05),color='red')+
    labs(x = 'Average False Accept Rate(FP)',
         y = 'Average False Accept Rate(FN)',
         title = 'Simulation Results')+
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1.0))+ theme_bw()+
    guides(fill = guide_colourbar(barwidth = 0.7,
                                  barheight = 0.3)) 
  return(p)
}
