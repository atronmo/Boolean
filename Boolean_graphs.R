domain = function(r0,col="yellow"){
  par(pty="s")
  lim = c(-r0-1,r0+1)
  plot(NULL,NULL,pch=19,xlim=lim,ylim=lim,asp=1,xlab='',ylab='',main =bquote("The domain "*Z)) 
  draw.circle(0,0,r0,col='black')
  
}
dispbool = function(x_proj,r0,rectangle_range,pdf_name){
  # resetPar()
  lim = r0+1
  if(is.null(pdf_name)){
    par(mai=c(0,0,0,0))
    par(bg='white')
    plot(x_proj[,1],x_proj[,2],xlim=c(-lim,lim),ylim=c(-lim,lim),xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.axis=1.0,asp=1,type='n');
    draw.circle(0,0,r0,col='black')
    for(i in seq(x_proj[,1])){
      draw.circle(x_proj[i,1],x_proj[i,2],x_proj[i,3],col="yellow",border = "yellow")
    }
    rect(xleft = -rectangle_range[1]/2,xright = rectangle_range[1]/2,ybottom = -rectangle_range[2]/2,ytop = rectangle_range[2]/2, border ="red",col = rgb(0,0,1.0,alpha=0),lwd = 2)
  }
  else{
    pdf(pdf_name,width=lim,height=lim)
    par(mai=c(0,0,0,0))
    plot(x_proj[,1],x_proj[,2],xlim=c(-lim,lim),ylim=c(-lim,lim),xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.axis=1.0,asp=1,type='n');
    draw.circle(0,0,r0,col='black')
    for(i in seq(x_proj[,1])){
      draw.circle(x_proj[i,1],x_proj[i,2],x_proj[i,3],col="yellow",border = "yellow")
    }
    rect(xleft = -rectangle_range[1]/2,xright = rectangle_range[1]/2,ybottom = -rectangle_range[2]/2,ytop = rectangle_range[2]/2, border ="red",col = rgb(0,0,1.0,alpha=0),lwd = 2)
    dev.off()
  }
}
dispbool2 = function(x_proj,r0,rectangle_range,pdf_name){
  # resetPar()
  if(is.null(pdf_name)){
    par(mai=c(0,0,0,0))
    #
    par(bg='black')
    plot(x_proj[,1],x_proj[,2],xlim=c(-rectangle_range[1]/2,rectangle_range[1]/2),ylim=c(-rectangle_range[2]/2,rectangle_range[2]/2),xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.axis=1.0,asp=1,type='n');
    symbols(x_proj[,1],x_proj[,2],circles=x_proj[,3],inches=F,add=T,fg='yellow',bg='yellow') 
    
  }
  else{
    pdf(pdf_name,width=rectangle_range[1],height=rectangle_range[2])
    par(mai=c(0,0,0,0))
    #
    par(bg='black')
    plot(x_proj[,1],x_proj[,2],xlim=c(-rectangle_range[1]/2,rectangle_range[1]/2),ylim=c(-rectangle_range[2]/2,rectangle_range[2]/2),xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.axis=1.0,asp=1,type='n');
    symbols(x_proj[,1],x_proj[,2],circles=x_proj[,3],inches=F,add=T,fg='yellow',bg='yellow') 
    dev.off()
  }
  
}
dispbool3 = function(x_proj,rectangle_range,ech,pdf_name){
  # resetPar()
  if(is.null(pdf_name)){
    par(mai=c(0,0,0,0))
    par(bg='black')
    plot(x_proj[,1],x_proj[,2],xlim=c(-rectangle_range[1]/2,rectangle_range[1]/2),ylim=c(-rectangle_range[2]/2,rectangle_range[2]/2),xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.axis=1.0,asp=1,type='n');
    symbols(x_proj[,1],x_proj[,2],circles=x_proj[,3],inches=F,add=T,fg='yellow',bg='yellow')
    points(ech[,1],ech[,2],pch=1,col=ifelse(ech[,3] == 1,"blue","orange"),cex=1.5,lwd=3)
  }
  else{
    pdf(pdf_name,width=rectangle_range[1],height=rectangle_range[2])
    par(mai=c(0,0,0,0))
    #
    par(bg='black')
    plot(x_proj[,1],x_proj[,2],xlim=c(-rectangle_range[1]/2,rectangle_range[1]/2),ylim=c(-rectangle_range[2]/2,rectangle_range[2]/2),xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.axis=1.0,asp=1,type='n');
    symbols(x_proj[,1],x_proj[,2],circles=x_proj[,3],inches=F,add=T,fg='yellow',bg='yellow')
    points(ech[,1],ech[,2],pch=1,col=ifelse(ech[,3] == 1,"blue","orange"),cex=1.5,lwd=3)
    dev.off()
  }
  
}
dispbool4 = function(x_proj,rectangle_range,ech,pdf_name){
  # resetPar()
  if(is.null(pdf_name)){
    par(mai=c(0,0,0,0))
    #
    par(bg='gray50')
    plot(x_proj[,1],x_proj[,2],xlim=c(-rectangle_range[1]/2,rectangle_range[1]/2),ylim=c(-rectangle_range[2]/2,rectangle_range[2]/2),xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.axis=1.0,asp=1,type='n');
    # symbols(x_proj[,1],x_proj[,2],circles=x_proj[,3],inches=F,add=T,fg='yellow',bg='yellow') 
    points(ech[,1],ech[,2],pch=1,col=ifelse(ech[,3] == 1,"blue","orange"),cex=1.5,lwd=3)
  }
  else{
    pdf(pdf_name,width=rectangle_range[1],height=rectangle_range[2])
    par(mai=c(0,0,0,0))
    #
    par(bg='gray50')
    plot(x_proj[,1],x_proj[,2],xlim=c(-rectangle_range[1]/2,rectangle_range[1]/2),ylim=c(-rectangle_range[2]/2,rectangle_range[2]/2),xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.axis=1.0,asp=1,type='n');
    # symbols(x_proj[,1],x_proj[,2],circles=x_proj[,3],inches=F,add=T,fg='yellow',bg='yellow') 
    points(ech[,1],ech[,2],pch=1,col=ifelse(ech[,3] == 1,"blue","orange"),cex=1.5,lwd=3)
    dev.off()
  }
  
}
hist_iterative_boolean_sim = function(tet,r0,a,iterative_point,pdf_name){
  
  tetaD = tet*pi*(r0^2+2*r0/a+2/a^2)
  
  Objects = iterative_point[[2]]
  Init_Objects = iterative_point[[3]]
  X = 1:length(Objects)
  df = cbind(X,Objects,Init_Objects)
  df = data.frame(df)
  # names(df) = c("index","Objects","Init. Objects")
  df <- melt(df,id.vars = 'X', variable.name = 'Type')
  #lines calculation
  n_object_limit = floor(0.05*Init_Objects[1])
  it_n_object_limit = min(which(Init_Objects<n_object_limit))
  vector_n_object = Objects[it_n_object_limit:length(Objects)]
  
  group.colors = c("green","red","black")
  base_size = 15
  base_line_size=2
  fontsize_size = 16
  title_size = 16
  # limits_x = range(Objects,na.rm = TRUE)
  limits_x = range(0,length(Objects))
  division_quadriculado_x = 5
  # limits_y = c(0,max(Objects[,-1],na.rm = TRUE))
  limits_y = c(0,1500)
  division_quadriculado_y = 5
  
  
  ggplot(df,aes(X,value,color = Type)) +
    theme_light(base_size = base_size,base_line_size = base_line_size) +
    labs(title = "", 
         x = bquote("Iterations"),y = bquote("Number of Objects"))+
    theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_x_continuous(breaks = floor(seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x)),limits = c(limits_x[1]-0.05,limits_x[2]+0.05)) + 
    scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1])) +
    theme(legend.position = "none")+
    geom_line() +
    scale_colour_manual(values=group.colors)+
    scale_linetype_manual(values=2)+
    scale_size_manual(values=2)+
    # geom_hline(yintercept=mean(vector_n_object)+3*sqrt(var(vector_n_object)), linetype="dashed", color = "orange")+
    # geom_hline(yintercept=mean(vector_n_object)-3*sqrt(var(vector_n_object)), linetype="dashed", color = "orange")+
    # geom_hline(yintercept=mean(vector_n_object), linetype="dashed", color = "blue")+
    # geom_hline(yintercept=n_object_limit, linetype="dashed", color = "black")+
    geom_vline(xintercept = it_n_object_limit, linetype="solid", color = "black")+theme(legend.position="bottom")
  ggsave(paste0(pdf_name,"_Nit.pdf"))
}

plot_map_prob = function(csv_name,rectangle_range){
  map_prob_graph = "map_prob_graphs"
  library("RColorBrewer")
  col = brewer.pal(n = 3, name = "YlOrRd")
  grid_total = expand.grid(x=seq(-rectangle_range[1]/2,rectangle_range[1]/2,length.out = 400),
                           y=seq(-rectangle_range[2]/2,rectangle_range[2]/2, length.out = 300))
  #prob_plot
  matrix_list_indicatrice = read.csv(paste0(map_prob_graph,"/",csv_name,"_indicatrice.csv"))
  matrix_list_indicatrice = matrix_list_indicatrice[,-1]
  matrix_list_indicatrice = matrix_list_indicatrice[,-c(1:2)]
  apply_matrix_list_indicatrice = apply(matrix_list_indicatrice,1,mean)
  matrix_list = db.create(z1 = apply_matrix_list_indicatrice,nx=c(400,300), x0=c(-rectangle_range[1]/2,-rectangle_range[2]/2), dx=c(rectangle_range[1]/400,rectangle_range[2]/300))
  matrix_list = db.locate(matrix_list,2:3,"x")
  matrix_list = db.locate(matrix_list,4,"z")
  png(paste0(map_prob_graph,"/",csv_name,"_map_probability.png"),width=rectangle_range[1]*100,height=(rectangle_range[2]+1)*100)
  par(mai=c(0,0,0,0))
  plot(matrix_list,zlim=c(0,1),xlim=c(-rectangle_range[1]/2,rectangle_range[1]/2),ylim=c(-rectangle_range[2]/2,rectangle_range[2]/2),xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.axis=1.0,asp=1,type='n',main = "")
  smallplot=c(0.2,0.8,0.04,0.061)
  image.plot(legend.only=T,zlim=range(c(0,1),na.rm=T),
             col=rg.colors(),smallplot=smallplot,horizontal = TRUE)
  dev.off()
  
  #interval 2sigmas
  output_binaire = numeric(2)
  interval = c(0.7-2*sqrt(0.7*0.3/500),0.7+2*sqrt(0.7*0.3/500))
  interval_2sigma = c(0,c(0.7-2*sqrt(0.7*0.3/500),0.7+2*sqrt(0.7*0.3/500)),1)
  cut_interval = c(0,1,2)[cut(apply_matrix_list_indicatrice,breaks = interval_2sigma,include.lowest = TRUE)]
  write.csv(table(cut(apply_matrix_list_indicatrice,breaks = interval_2sigma,include.lowest = TRUE))/sum(table(cut(apply_matrix_list_indicatrice,breaks = interval_2sigma,include.lowest = TRUE))),file = paste0(map_prob_graph,"/",csv_name,"_binary_sigmas_lantu"))
  
  matrix_list = db.add(matrix_list,binare=cut_interval)
  
  png(paste0(map_prob_graph,"/",csv_name,"_map_binare_2_sigmas_lantu.png"),width=rectangle_range[1]*100,height=(rectangle_range[2])*100)
  par(mai=c(0,0,0,0))
  plot(matrix_list,zlim=c(0,2),xlim=c(-rectangle_range[1]/2,rectangle_range[1]/2),
       ylim=c(-rectangle_range[2]/2,rectangle_range[2]/2),xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.axis=1.0,asp=1,type='n',main = "",col = col)
  # smallplot=c(.90,0.92,0.2,0.8)
  dev.off()
  
  
  # png(paste0(map_prob_graph,"/",csv_name,"_map_binare_2_sigmas.png"),width=rectangle_range[1]*100,height=(rectangle_range[2])*100)
  # par(mai=c(0,0,0,0))
  # plot(matrix_list,zlim=c(0,1),xlim=c(-rectangle_range[1]/2,rectangle_range[1]/2),
  #      ylim=c(-rectangle_range[2]/2,rectangle_range[2]/2),xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.axis=1.0,asp=1,type='n',main = "",col = c("orange","blue"))
  # # smallplot=c(.90,0.92,0.2,0.8)
  # dev.off()
  # output_binaire[1] = sum(matrix_list$binare)/length(matrix_list$binare)
  
  #interval 3sigmas
  interval = c(0.7-3*sqrt(0.7*0.3/500),0.7+3*sqrt(0.7*0.3/500))
  # matrix_list = db.add(matrix_list,binare1=ifelse(z1>=interval[1] &z1<=interval[2] ,1,0))
  # png(paste0(map_prob_graph,"/",csv_name,"_map_binare_3_sigmas.png"),width=rectangle_range[1]*100,height=(rectangle_range[2])*100)
  # par(mai=c(0,0,0,0))
  # plot(matrix_list,zlim=c(0,1),xlim=c(-rectangle_range[1]/2,rectangle_range[1]/2),
  #      ylim=c(-rectangle_range[2]/2,rectangle_range[2]/2),xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.axis=1.0,asp=1,type='n',main = "",col = c("orange","blue"))
  # # smallplot=c(.90,0.92,0.2,0.8)
  # dev.off()
  # output_binaire[2] = sum(matrix_list$binare1)/length(matrix_list$binare1)
  # write.csv(output_binaire,paste0(map_prob_graph,"/",csv_name,"_binary_sigmas"))
  
  #interval_lantu
  
  
  return(output_binaire)
}

hist_ind_sim_iterative = function(tet,r0,a,input_csv){
  # Iterations################################################
  tetaD = tet*pi*(r0^2+2*r0/a+2/a^2)
  map_prob_graph = "map_prob_graphs"

  object_matrix = read.csv(paste0(map_prob_graph,"/",csv_name,"_n_object.csv"))
  object_matrix_mean = apply(object_matrix[,-1],1,mean,na.rm =TRUE)
  object_init_matrix = read.csv(paste0(map_prob_graph,"/",csv_name,"_n_object_init.csv"))
  object_matrix_init_mean = apply(object_init_matrix[,-1],1,mean,na.rm =TRUE)

  # df = cbind(object_matrix,object_init_matrix[,-1])
  # df = cbind(df,object_matrix_mean,object_matrix_init_mean)
  # df = data.frame(df)
  # df <- melt(df ,  id.vars = 'X', variable.name = 'series')
  # group.colors = c(rep("green",(dim(object_matrix)[2]-1)),rep("red",(dim(object_matrix)[2]-1)),"black","black")
  # size = c(rep(0.1,(dim(object_matrix)[2]-1)),rep(0.1,(dim(object_matrix)[2]-1)),10,10)
  # lty = c(rep("longdash",(dim(object_matrix)[2]-1)),rep("longdash",(dim(object_matrix)[2]-1)),"solid","solid")
  # base_size = 15
  # base_line_size=2
  # fontsize_size = 16
  # title_size = 16
  # # limits_x = range(object_matrix,na.rm = TRUE)
  # limits_x = range(0,70000)
  # division_quadriculado_x = 5
  # # limits_y = c(0,max(object_matrix[,-1],na.rm = TRUE))
  # limits_y = c(0,1500)
  # division_quadriculado_y = 5
  # 
  # 
  # ggplot(df,aes(X,value,color = series)) +
  #   theme_light(base_size = base_size,base_line_size = base_line_size) +
  #   theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #   scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = c(limits_x[1]-0.05,limits_x[2]+0.05)) + 
  #   scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
  #   coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1])) +
  #   theme(legend.position = "none")+
  #   geom_line() +
  #   scale_colour_manual(values=group.colors)+
  #   scale_linetype_manual(values=lty)+
  #   scale_size_manual(values=size)+
  #   geom_hline(yintercept=tetaD+2*sqrt(tetaD), linetype="dashed", color = "orange")+
  #   geom_hline(yintercept=tetaD-2*sqrt(tetaD), linetype="dashed", color = "orange")+
  #   geom_hline(yintercept=tetaD, linetype="dashed", color = "blue")
  # 
  # ggsave(paste0(map_prob_graph,"/graph_",input_csv,"_Nit_.pdf"))
  
  
  #N_init###########################################################################################
  df = as.matrix(object_init_matrix)
  df = df[,-1]
  df = df[1,]
  df = as.data.frame(df)
  
  base_size = 15
  base_line_size=2
  fontsize_size = 16
  title_size = 16
  # limits_x = range(object_matrix,na.rm = TRUE)
  limits_x = range(0,1500)
  division_quadriculado_x = 5
  # limits_y = c(0,max(object_matrix[,-1],na.rm = TRUE))
  limits_y = c(0,dim(df)[1])
  division_quadriculado_y = 5
  #histograms
  ggplot(data=df, aes(x = df)) +
    labs(title = bquote("Histogram: Number of Initial Objects"), 
         x = bquote("Number of Initial Objects"),y = bquote(Count)) +
    geom_histogram(colour="black", fill="white") +
    theme_light(base_size = base_size,base_line_size = base_line_size) +
    theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = mean(df[,"df"]) , col = "orange",lwd = 1.5)  +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = c(limits_x[1]-1,limits_x[2]+1)) +
    scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5)+
    coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1]))
  
  ggsave(paste0(map_prob_graph,"/hist_",input_csv,"_N_obj_init.pdf"))
  
  #N_iterations####################################################################################
  df = as.matrix(object_init_matrix)
  df = df[,-1]
  df = colSums(!is.na(df))
  df = as.data.frame(df)
  
  base_size = 15
  base_line_size=2
  fontsize_size = 16
  title_size = 16
  # limits_x = range(object_matrix,na.rm = TRUE)
  limits_x = c(0,150000)
  division_quadriculado_x = 5
  # limits_y = c(0,max(object_matrix[,-1],na.rm = TRUE))
  limits_y = c(0,dim(df)[1])
  division_quadriculado_y = 5
  #histograms
  ggplot(data=df, aes(x = df)) +
    labs(title = bquote("Histogram: N. Iteration"), 
         x = bquote("Number of Iterations"),y = bquote(Count)) +
    geom_histogram(colour="black", fill="white") +
    theme_light(base_size = base_size,base_line_size = base_line_size) +
    theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = mean(df[,"df"]) , col = "orange",lwd = 1.5)  +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = c(limits_x[1]-1,limits_x[2]+1)) +
    scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5)+
    coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1]))
  
  ggsave(paste0(map_prob_graph,"/hist_",input_csv,"_N_init.pdf"))
  
  
  
  #N_iteration_check###################################################################################################################
  df = read.csv(paste0(map_prob_graph,"/",csv_name,"_n_object.csv"))
  
  df_vector = apply(df[,-1],2,function(x)x[max(which(!is.na(x)))])
  df_vector = cbind(seq(df_vector),df_vector)
  df_vector = as.data.frame(df_vector)
  
  #interval check
  interval_check = c(tetaD-2*sqrt(tetaD),tetaD+2*sqrt(tetaD))
  
  base_size = 15
  base_line_size=2
  fontsize_size = 16
  title_size = 16
  group.colors = ifelse(df_vector[,2]>=interval_check[1]&df_vector[,2]<=interval_check[2],"black","red")
  # limits_x = range(object_matrix,na.rm = TRUE)
  limits_x = c(0,dim(df_vector)[1])
  division_quadriculado_x = 5
  # limits_y = c(0,max(object_matrix[,-1],na.rm = TRUE))
  limits_y = floor(c(tetaD-2*sqrt(tetaD)-100,tetaD+2*sqrt(tetaD)+100))
  division_quadriculado_y = 5
  ggplot(df_vector,aes(x = V1,y = df_vector)) +
    labs(title = bquote("Number of Objects at the Final Iteration"), 
         x = bquote("Independent Simulation"),y = bquote("Number of Objects")) +
    theme_light(base_size = base_size,base_line_size = base_line_size) +
    theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = c(limits_x[1]-0.05,limits_x[2]+0.05)) + 
    scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1])) +
    theme(legend.position = "none")+
    geom_point(color = group.colors) + 
    geom_hline(yintercept=tetaD+2*sqrt(tetaD), linetype="dashed", color = "orange")+
    geom_hline(yintercept=tetaD-2*sqrt(tetaD), linetype="dashed", color = "orange")+
    geom_hline(yintercept=tetaD, linetype="dashed", color = "blue")
  ggsave(paste0(map_prob_graph,"/N_objects_final_iteration_",input_csv,".pdf"))
  
  #processing time
  df = read.csv(paste0(map_prob_graph,"/",csv_name,"_time_processing.csv"))
  df = df[,-1]
  df = as.data.frame(df)
  
  
  base_size = 15
  base_line_size=2
  fontsize_size = 16
  title_size = 16
  limits_x = c(0,2)
  # limits_x = c(0,10)
  division_quadriculado_x = 5
  # limits_y = c(0,max(object_matrix[,-1],na.rm = TRUE))
  # limits_x = range(object_matrix,na.rm = TRUE)
  limits_y = c(0,dim(df)[1])
  division_quadriculado_y = 5
  #histograms
  ggplot(data=df, aes(x = df)) +
    labs(title = bquote("Histogram: Time Processing Iterative Sim. [min]"), 
         x = bquote("Time Processing [min]"),y = bquote(Count)) +
    geom_histogram(colour="black", fill="white") +
    theme_light(base_size = base_size,base_line_size = base_line_size) +
    theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = mean(df[,"df"]) , col = "orange",lwd = 1.5)  +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = c(limits_x[1]-limits_x[1]/division_quadriculado_x,limits_x[2]+limits_x[1]/division_quadriculado_x)) +
    scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5)+
    coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1]))
  
  ggsave(paste0(map_prob_graph,"/hist_",input_csv,"_time_processing.pdf"))
  
  
}

hist_ind_sim_sequential = function(tet,r0,a,input_csv){
  #Iterations################################################
  tetaD = tet*pi*(r0^2+2*r0/a+2/a^2)
  map_prob_graph = "map_prob_graphs"
  
 
  #processing time
  df = read.csv(paste0(map_prob_graph,"/",csv_name,"_time_processing.csv"))
  df = df[,-1]
  df = as.data.frame(df)
  
  
  base_size = 15
  base_line_size=2
  fontsize_size = 16
  title_size = 16
  # limits_x = c(0,10)
  limits_x = c(0,2)
  division_quadriculado_x = 5
  # limits_y = c(0,max(object_matrix[,-1],na.rm = TRUE))
  division_quadriculado_y = 5
  # limits_x = range(object_matrix,na.rm = TRUE)
  limits_y = c(0,dim(df)[1])
  division_quadriculado_y = 5
  #histograms
  ggplot(data=df, aes(x = df)) +
    labs(title = bquote("Histogram: Time Processing Sequential Sim. [min]"), 
         x = bquote("Time Processing [min]"),y = bquote(Count)) +
    geom_histogram(colour="black", fill="white") +
    theme_light(base_size = base_size,base_line_size = base_line_size) +
    theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = mean(df[,"df"]) , col = "orange",lwd = 1.5)  +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = c(limits_x[1]-1/division_quadriculado_x,limits_x[2]+1/division_quadriculado_x)) +
    scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5)+
    coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1]))
  
  ggsave(paste0(map_prob_graph,"/hist_",input_csv,"_time_processing.pdf"))
  
  
}

plot_dgZ = function(a,r0){
  ggplot(data.frame(x=c(0.1, 10)), aes(x)) + 
    stat_function(fun=function(x) (a^2*x^2+4*a*x+6)/(a*(a^2*x^2+2*a*x+2))) + 
    labs(title = bquote(E*"{"*dG^Z*(R[o])*"} with "*a*" = "*.(a)),
         x = bquote(R[o]), y = bquote(E*"{"*dG^Z*(R[o])*"}")) +
    theme_bw(base_size = 15) + xlim(0,10) + ylim(1/a,3/a) + geom_point(aes(x=r0, y=(a^2*r0^2+4*a*r0+6)/(a*(a^2*r0^2+2*a*r0+2))), colour="orange",cex = 6) +
    # geom_vline(xintercept = r0,linetype="dotted",col="blue", size=1.5) 
    geom_hline(yintercept = 3/a,linetype="dotted",col="orange", size=1.5)+
    geom_hline(yintercept = 1/a,linetype="dotted",col="orange", size=1.5)
  ggsave(paste0("dGZR.pdf"))
  
  xlim = c(0.1,10)
  ylim = c(1,9)
  zlim = c(0,2)
  M <- mesh(seq(xlim[1],xlim[2], length.out = 100),seq(ylim[1],ylim[2], length.out = 100))
  R0_plot <- M$x ; a_plot <- M$y
  x <- R0_plot
  y <- a_plot
  z <- (R0_plot^2*a_plot^2+4*R0_plot*a_plot+6)/(a_plot*(R0_plot^2*a_plot^2+2*R0_plot*a_plot+2))
  persp3D(x, y, z, colvar = z, axes = TRUE,colkey = TRUE,clim = c(range(z)), 
          box = TRUE, bty = "b2", phi = 25, theta = 145,facets = TRUE,xlab=bquote("Ro"),ylab ="a",zlab = "",main=bquote("3D Surface"),
          xlim = xlim, 
          ylim = ylim, 
          zlim = zlim,
          border = NA, shade = .4,ticktype="detailed",scale= TRUE, expand = 0.5)
  par(xpd = TRUE)
  text3D(xlim[1], ylim[2], zlim[2], expression(E*"{"*dG^Z*"}"), add = T)
  
  M_scatter = mesh(seq(xlim[1], xlim[2], length.out = 300),a)
  u <- M_scatter$x ; v <- M_scatter$y
  scatter3D(x = M_scatter$x,y = M_scatter$y,z = (u^2*v^2+4*u*v+6)/(v*(u^2*v^2+2*u*v+2)),add= TRUE, col = "red",type = "l",lwd = 4)
  points3D(x = r0,y = a,z = (r0^2*a^2+4*r0*a+6)/(a*(r0^2*a^2+2*r0*a+2)),add = TRUE, cex = 5, col = "orange",full = TRUE,pch = 16)
  
}

plot_variogram_grid_boolean = function(vector_rep,csv_name,rectangle_range,a,tet){
  map_prob_graph = "map_prob_graphs"
  vario_grid_list = lapply(X = vector_rep,function(i){
    vario_read = vario.read(filename = paste0(map_prob_graph,"/",csv_name,"_vario_",i))
    return(vario_read)
  })
  N = length(vario_grid_list)
  avr_vario_grid_list = vario.average(vario_grid_list)
  #Horizontal Direction
  pdf(paste0(map_prob_graph,"/",csv_name,"_vario_0.pdf"))
  plot(NULL,NULL,ylim=c(0,0.3),xlim=c(0,rectangle_range[1]/2),main = paste0(""),
       xlab = "Range",ylab = "Sill",xaxs = "i",yaxs = "i")
  for(i in seq(N)){plot(vario_grid_list[[i]],add=T,lty = 2, lwd =1,col = "gray",idir0 = 1,varline = FALSE)}
  plot(avr_vario_grid_list,add=T,lty = 1, lwd =2,col = "black",idir0 = 1,varline = FALSE)
  curve(expr = variogram_x_h(x,tet,a),add=T,col=2,lwd=2,from = 0, to = rectangle_range[1]/2)
  legend("bottomright", legend=c("Sim", "Average","Model"),
         col=c("gray","black","red"), lty=c(2,1,1), lwd = c(1,2,2), cex=0.8)
  dev.off()
  #Vertical Direction
  pdf(paste0(map_prob_graph,"/",csv_name,"_vario_90.pdf"))
  plot(NULL,NULL,ylim=c(0,0.3),xlim=c(0,rectangle_range[2]/2),main = "",
       xlab = "Range",ylab = "Sill",xaxs = "i",yaxs = "i")
  for(i in seq(N)){plot(vario_grid_list[[i]],add=T,lty = 2, lwd =1,col = "gray",idir0 = 2,varline = FALSE)}
  plot(avr_vario_grid_list,add=T,lty = 1, lwd =2,col = "black",idir0 = 2,varline = FALSE)
  curve(expr = variogram_x_h(x,tet,a),add=T,col=2,lwd=2,from = 0, to = rectangle_range[2]/2)
  legend("bottomright", legend=c("Sim", "Average","Model"),
         col=c("gray","black","red"), lty=c(2,1,1), lwd = c(1,2,2), cex=0.8)
  dev.off()
  return("FIN")
}
q3gamma = function(p,a=a,r0=r0){
  p_i = c(a^2*r0^2,2*a*r0,2) # probas du mélange
  den = sum(p_i)
  output = p_i[1]/den*qgamma(p,1,a)+p_i[2]/den*qgamma(p,2,a)+ p_i[3]/den*qgamma(p,3,a)
  return(output)
}
#QQ PLOT
qq_plot = function(tet,r0,a,radius,csv_name){
  map_prob_graph = "map_prob_graphs"
  Z <- radius      # random sample from exponential distribution
  p <- ppoints(100)    # 100 equally spaced points on (0,1), excluding endpoints
  q <- quantile(Z,p=p) # percentiles of the sample distribution
  
  pdf(paste0(map_prob_graph,"/QQ_PLOT_",csv_name,".pdf"))
  par(pty="s")
  plot(q3gamma(p=p,a=a,r0=r0) ,q, main="",
       xlab="Theoretical Quantiles",ylab="Sample Quantiles",ylim = c(0,1),xlim=c(0,1))
  abline(a = 0,b = 1,col = "blue",lty=2, lwd = 2)
  dev.off()
}

#QQ_plot to compare exponential vs melange 3 gamma at different domain sizes
qq_plot_thesis_theoretical = function(tet,a){
  map_prob_graph = "map_prob_graphs"
  Z <- rexp(n = 1e6,rate = a)      # random sample from exponential distribution
  p <- ppoints(100)    # 100 equally spaced points on (0,1), excluding endpoints
  q <- quantile(Z,p=p) # percentiles of the sample distribution
  r0_vector = c(0,0.5,5,5e10)
  r0_legend = r0_vector[-length(r0_vector)]
  r0_legend = c(r0_legend,"Infinity")
  col = rg.colors(number = length(r0_vector),rank = 1)
  # pdf(paste0(map_prob_graph,"/QQ_PLOT_",csv_name,".pdf"))
  par(pty="s")
  plot(NULL,NULL,xlab="Theoretical Quantiles",ylab="Sample Quantiles",ylim = c(0,2),xlim=c(0,2),main="")
  for(r0 in 1:length(r0_vector)){
    points(q3gamma(p = p,a = a,r0 = r0_vector[r0]),q, add = TRUE,col = col[r0],type = 'p',lwd = 2)
  }
  
  
  
  legend("bottomright", legend=paste0("r0 = ",r0_legend),
       col=col, lty=1, cex=1)
  abline(a = 0,b = 1,col = "blue",lty=2, lwd = 2)
  
  
  
  # dev.off()
  
}


plots_charging_the_data = function(){
  #iterative method
  #pour le plot
  #N_iteration
  base_size = 15
  base_line_size=2
  fontsize_size = 16
  title_size = 16
  limits_x = c(1.5e5,4.5e5)
  division_quadriculado_x = 10
  limits_y = c(0,500)
  division_quadriculado_y = 10
  
  #plot
  hist_data_it_random = read.table("map_prob_graphs/map_prob_random_iterative_iteration.csv");hist_data_it_random = unname(unlist(hist_data_it_random))
  
  ggplot(data=as.data.frame(hist_data_it_random), aes(x = hist_data_it_random)) + 
    labs(title = bquote("Histogram: N. Iterations Random"), 
         x = bquote("Number of Iterations"),y = bquote(Count))  + geom_histogram(colour="black", fill="white") +
    theme_light(base_size = base_size,base_line_size = base_line_size)+ theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = mean(hist_data_it_random) , col = "orange",lwd = 1.5)  +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = limits_x) + scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5) + coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1]))
  ggsave(paste0(map_prob_graph,"/map_prob_random_iterative_Nit.pdf"))
  
  hist_data_it_line = read.table("map_prob_graphs/map_prob_line_iterative_iteration.csv");hist_data_it_line = unname(unlist(hist_data_it_line))
  ggplot(data=as.data.frame(hist_data_it_line), aes(x = hist_data_it_line)) + 
    labs(title = bquote("Histogram: N. Iterations Line"), 
         x = bquote("Number of Iterations"),y = bquote(Count))  + geom_histogram(colour="black", fill="white") +
    theme_light(base_size = base_size,base_line_size = base_line_size)+ theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = mean(hist_data_it_line) , col = "orange",lwd = 1.5)  +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = limits_x) + scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5) + coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1]))
  ggsave(paste0(map_prob_graph,"/map_prob_line_iterative_Nit.pdf"))
  
  #N_initobject
  base_size = 15
  base_line_size=2
  fontsize_size = 16
  title_size = 16
  limits_x = c(0,2e3)
  division_quadriculado_x = 10
  limits_y = c(0,500)
  division_quadriculado_y = 10
  #plot
  hist_data_nob_random = read.table("map_prob_graphs/map_prob_random_iterative_i_object.csv");hist_data_nob_random = unname(unlist(hist_data_nob_random))
  
  ggplot(data=as.data.frame(hist_data_nob_random), aes(x = hist_data_nob_random)) + 
    labs(title = bquote("Histogram: N. Initial Objects Random"), x = bquote("Number of Initial Objects"),y = bquote(Count))  + 
    geom_histogram(colour="black", fill="white") +
    theme_light(base_size = base_size,base_line_size = base_line_size)+ theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = mean(hist_data_nob_random) , col = "orange",lwd = 1.5)  +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = limits_x) + scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5) + coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1]))
  ggsave(paste0(map_prob_graph,"/map_prob_random_iterative_Nobj.pdf"))
  
  
  hist_data_nob_line = read.table("map_prob_graphs/map_prob_line_iterative_i_object.csv")
  
  hist_data_nob_line = read.table("map_prob_graphs/map_prob_line_iterative_i_object.csv");hist_data_nob_line = unname(unlist(hist_data_nob_line))
  ggplot(data=as.data.frame(hist_data_nob_line), aes(x = hist_data_nob_line)) + labs(title = bquote("Histogram: N. Initial Objects Line"), 
                                                                                     x = bquote("Number of Initial Objects"),y = bquote(Count))  + geom_histogram(colour="black", fill="white") +
    theme_light(base_size = base_size,base_line_size = base_line_size)+ theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = mean(hist_data_nob_line) , col = "orange",lwd = 1.5)  +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = limits_x) + scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5) + coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1]))
  ggsave(paste0(map_prob_graph,"/map_prob_line_iterative_Nobj.pdf"))
  
  #time processing
  
  base_size = 15
  base_line_size=2
  fontsize_size = 16
  title_size = 16
  limits_x = c(0,10)
  division_quadriculado_x = 10
  limits_y = c(0,500)
  division_quadriculado_y = 10
  
  hist_time_random = read.table("map_prob_graphs/map_prob_iterative_random_time_processing");hist_time_random = unname(unlist(hist_time_random))
  ggplot(data=as.data.frame(hist_time_random), aes(x = hist_time_random)) + 
    labs(title = bquote("Histogram: Time Processing Iterative Sim. Random [min]"), 
         x = bquote("Time Processing [min]"),y = bquote(Count)) + geom_histogram(colour="black", fill="white") +
    theme_light(base_size = base_size,base_line_size = base_line_size)+ theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = mean(hist_time_random) , col = "orange",lwd = 1.5)  +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = limits_x) + scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5) + coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1]))
  ggsave(paste0(map_prob_graph,"/map_prob_Iterative_random_time_processing.pdf"))
  
  hist_time_line = read.table("map_prob_graphs/map_prob_iterative_line_time_processing");hist_time_line = unname(unlist(hist_time_line))
  ggplot(data=as.data.frame(hist_time_line), aes(x = hist_time_line)) + 
    labs(title = bquote("Histogram: Time Processing Iterative Sim. line [min]"), 
         x = bquote("Time Processing [min]"),y = bquote(Count)) + geom_histogram(colour="black", fill="white") +
    theme_light(base_size = base_size,base_line_size = base_line_size)+ theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = mean(hist_time_line) , col = "orange",lwd = 1.5)  +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = limits_x) + scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5) + coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1]))
  ggsave(paste0(map_prob_graph,"/map_prob_Iterative_line_time_processing.pdf"))
  
  
  #sequential
  
  base_size = 15
  base_line_size=2
  fontsize_size = 16
  title_size = 16
  limits_x = c(0,10)
  division_quadriculado_x = 10
  limits_y = c(0,500)
  division_quadriculado_y = 10
  
  hist_time_random = read.table("map_prob_graphs/map_prob_sequential_random_time_processing");hist_time_random = unname(unlist(hist_time_random))
  ggplot(data=as.data.frame(hist_time_random), aes(x = hist_time_random)) + 
    labs(title = bquote("Histogram: Time Processing sequential Sim. Random [min]"), 
         x = bquote("Time Processing [min]"),y = bquote(Count)) + geom_histogram(colour="black", fill="white") +
    theme_light(base_size = base_size,base_line_size = base_line_size)+ theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = mean(hist_time_random) , col = "orange",lwd = 1.5)  +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = limits_x) + scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5) + coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1]))
  ggsave(paste0(map_prob_graph,"/map_prob_sequential_random_time_processing.pdf"))
  
  hist_time_line = read.table("map_prob_graphs/map_prob_sequential_line_time_processing");hist_time_line = unname(unlist(hist_time_line))
  ggplot(data=as.data.frame(hist_time_line), aes(x = hist_time_line)) + 
    labs(title = bquote("Histogram: Time Processing sequential Sim. line [min]"), 
         x = bquote("Time Processing [min]"),y = bquote(Count)) + geom_histogram(colour="black", fill="white") +
    theme_light(base_size = base_size,base_line_size = base_line_size)+ theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = mean(hist_time_line) , col = "orange",lwd = 1.5)  +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = c(limits_x[1]-0.5,limits_x[2])) + scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5) + coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1]))
  ggsave(paste0(map_prob_graph,"/map_prob_sequential_line_time_processing.pdf"))
  
  #curve
  
  
  
}

