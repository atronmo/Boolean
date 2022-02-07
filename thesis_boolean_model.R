#rm(list=ls())
library("plotrix")  
library("grid")
library("parallel")
library("doParallel")
library("RGeostats")
library("ggplot2")
library("plot3D")
library("ggplot2")
library("plyr")
library("reshape2")
source("Boolean_graphs.R")
source("fn_particle.R")
library("fields")
rec.list <- function(len){
  if(length(len) == 1){
    vector("list", len)
  } else {
    lapply(1:len[1], function(...) rec.list(len[-1]))
  }
}

#points in the domain Z
N_points_1 = function(n,r0,x_proj,R){
  #conditional simulation (points in all the dilatation domain)
  n=50 # nombre d'échantillons
  rech = r0*sqrt(runif(n)) #rayon dans le domain Z
  al = 2*pi*runif(n) #angle
  ech = cbind(rech*cos(al),rech*sin(al)) #projection
  hit = rep(0,n) #vector hit (which one hit the the objects from the 
  #Non-conditional simulation)
  for(i in 1:N){
    hit = hit + (sqrt((ech[,1]-x_proj[i,1])^2+(ech[,2]-x_proj[i,2])^2)<R[i]) 
  }
  hit = as.numeric(hit>0)	#which one hit (does not matter how many time a point 
  ech = cbind(ech,hit)
  
}
#points in the domain D
N_points_2 = function(n,x_proj,R,rectangle_range){
  #conditional simulation (points in all the dilatation domain)
  #set.seed(12345)
  ech = cbind(runif(n,-rectangle_range[1]/2,rectangle_range[1]/2),runif(n,-rectangle_range[2]/2,rectangle_range[2]/2))
  hit = rep(0,n) #vector hit (which one hit the the objects from the 
  #Non-conditional simulation)
  for(i in 1:N){
    hit = hit + (sqrt((ech[,1]-x_proj[i,1])^2+(ech[,2]-x_proj[i,2])^2)<R[i]) 
  }
  hit = as.numeric(hit>0)	#which one hit (does not matter how many time a point 
  ech = cbind(ech,hit)
  
}

#CS
#initialization
initialization = function(r0,rectangle_range,ech,set.seed){
  xcond = NULL #projection of the points for the initialisation
  Rcond = NULL	 # rayon from this points
  hit = ech[,3]
  J=sum(hit)  
  n = dim(ech)[1]
  cond = rep(0,n) 
  Ncond = 0 # nombre d'objets
  if(!is.null(set.seed)){set.seed = set.seed}
  while(J>0){
    u=runif(1)
    Rtest = (u<cum_pi[1])*rexp(1,a)+(u>cum_pi[1] & u<cum_pi[2])*rgamma(1,2,a) + 
      (u>cum_pi[2])*rgamma(1,3,a)
    altest = 2*pi*runif(1)
    mtest = (Rtest+r0)*sqrt(runif(1))
    xtest = cbind(mtest*cos(altest),mtest*sin(altest))
    test = (sqrt((ech[,1]-xtest[1])^2+(ech[,2]-xtest[2])^2)<Rtest)
    if (sum(test[hit==1])>0 & sum(test[hit==0])==0){ # on teste si l'objet recouvre un point de 
      Rcond = c(Rcond,Rtest)
      xcond = rbind(xcond,xtest)
      cond = cond + test
      Ncond = Ncond+1
    }
    J = sum(hit[hit==1] - (cond[hit==1]>0))
    
  }
  xcond = cbind(xcond,Rcond)
  # les disques générés
  return(xcond)
}###

iterative_boolean_sim = function(tet,r0,a,ech,set.seed,init_matrix){
  #iteration number
  tetaD = tet*pi*(r0^2+2*r0/a+2/a^2) #expected value
  Rcond = init_matrix[,3]
  xcond = init_matrix[,1:2]
  hit = ech[,3]
  NN =  Ncond = dim(xcond)[1] #number of objects from the conditional simulation in each iteration
  M1=M2= xcond
  
  y=M1[,1]%in%M2[,1]
  y_cont = length(y[y==TRUE])
  n_obt_limit = floor(0.05*y_cont)
  n_it_obt_limit = NULL
  n_obt_obt_limit = NULL
  tau = 9e10
  i=1
  if(!is.null(set.seed)){set.seed = set.seed}
  while(TRUE){
    #condition all initial object erased
    #if(y_cont[i]==0){break}
    if(y_cont[i]==n_obt_limit & tau==9e10){tau = i}
    if(i >=3*tau){break}
    pcum = c(tetaD/(tetaD+Ncond+1), Ncond/(tetaD+Ncond)) # probas d'ajouter un 
    #objet, d'en oter un
    pcum = cumsum(pcum)
    u = runif(1)
    if (u<pcum[1]){ # on ajoute un objet, similaire à l'initialisation
      altest = 2*pi*runif(1)
      Rtest = (u<cum_pi[1])*rexp(1,a)+(u>cum_pi[1] & u<cum_pi[2])*rgamma(1,2,a) + 
        (u>cum_pi[2])*rgamma(1,3,a)
      mtest = (Rtest+r0)*sqrt(runif(1))
      xtest = cbind(mtest*cos(altest),mtest*sin(altest))
      test = (sqrt((ech[,1]-xtest[1])^2+(ech[,2]-xtest[2])^2)<Rtest)
      if (sum(test[hit==0])==0){
        Rcond = c(Rcond,Rtest)
        xcond = rbind(xcond,xtest)
        Ncond = Ncond+1
      }	
    }
    else if (u<pcum[2]){ # on ote un objet
      obj = sample(1:Ncond,1)
      test = (sqrt((ech[hit==1,1]-xcond[obj,1])^2+(ech[hit==1,2]-xcond[obj,2])^2)<
                Rcond[obj])
      if (sum(test)==0){ # s'il ne recouvre pas de point, on l'ote
        Rcond = Rcond[-obj]
        xcond = xcond[-obj,]
        Ncond = Ncond-1
      }
      else { # s'il recouvre un ou plusieurs points conditionnant, on vérifie si 
        #le(s) point(s) est (sont) recouvert(s) par un autre objet
        ind = which(test)
        test2 = 0
        for (j in ind){
          test2 = test2 + as.numeric(sum(sqrt((ech[(hit==1),1][j]-xcond[-obj,1])^2+
                                                (ech[(hit==1),2][j]-xcond[-obj,2])^2)<
                                           Rcond[-obj])>0) 
          # y a-t-il au moins un disque qui recouvre chaque point de ind ? 
          #([-obj] = tous les objets sauf obj
        }
        if (test2==length(ind)){ # si tous les points conditionnants sont 
          #préservés, on fait
          Rcond = Rcond[-obj]
          xcond = xcond[-obj,]
          Ncond = Ncond-1
        }
      }
    }
    NN = c(NN,Ncond)
    i = i+1
    y=xcond[,1]%in%M2[,1]
    y_cont = c(y_cont,length(y[y==TRUE]))
    # print(paste0("Quantité des objects initial: ",y_cont[i],", quantité des objets total: ",NN[i]," quantité des iterations: ",i))
  }
  xcond = cbind(xcond,Rcond)
  return(list(xcond,NN,y_cont))
}

#############################SEQUENTIAL

sequential_avoiding_boolean_sim = function(tet,r0,a,ech){
  #avoiding functional
  p = newParticule(NULL)
  x_proj = NCS(tet = tet,a = a,r0 = r0)
  test=numeric(dim(x_proj)[1])
  for(j in 1:(dim(x_proj)[1])){
    if(as.numeric(sum(sqrt((ech[,1]-x_proj[j,1])^2+(ech[,2]-x_proj[j,2])^2)<x_proj[j,3])>0)){test[j] = 1}
  }
  x_proj = x_proj[test==0,,drop = F]
  if(length(x_proj) == 0){return(NULL)}
  for(j in 1:(dim(x_proj)[1])){p = newParticule(data = x_proj[j,],par = p)}
  return(p)
}


sequential_hitting_boolean_sim_checking_background = function(tet,r0,a,ech,K,i,ech_avoiding,parallel = FALSE,ncores,particle = FALSE,p=NULL){
  creating_object = function(k,tet,t0,a,i,ech_avoiding,ech,particle,p){
    set.seed(NULL)
    N = rpois(1,tet*pi*(2/a^2)) #selon l'expression de la premiere partie
    if(N == 0){
      if(particle == TRUE){return(p[[k]])}
      else{return(NULL)}
    }
    # rayons des disques
    R = rgamma(N,3,a)
    # implantations	
    al = 2*pi*runif(N) #angle en polaires
    m = R*sqrt(runif(N)) #rayon en polaires
    x_proj = cbind(m*cos(al)+ech[i,1],m*sin(al)+ech[i,2]) #projection aux axis cartesiennes
    x_proj = as.matrix(cbind(x_proj,R,k))
    
    test=numeric(dim(x_proj)[1])
    for(j in 1:(dim(x_proj)[1])){
      if(as.numeric(sum(sqrt((ech_avoiding[,1]-x_proj[j,1])^2+(ech_avoiding[,2]-x_proj[j,2])^2)<x_proj[j,3])>0)){test[j] = 1}
    }
    x_proj = x_proj[test==0,,drop = F]
    if(length(x_proj) == 0){
      if(particle == TRUE){return(p[[k]])}
      else{return(NULL)}}
    if(particle == TRUE){
      for(j in 1:(dim(x_proj)[1])){p[[k]] = newParticule(data = x_proj[j,],id = k,idx = k,par = p[[k]])}
      return(p[[k]])
    }
    else{return(x_proj)}
  }
  if(parallel == FALSE){
    if(particle == TRUE){
      ech_hitting = lapply(X = seq(K),creating_object,tet,t0,a,i,ech_avoiding,ech,particle = TRUE,p)
      
    }
    else{ech_hitting = lapply(X = seq(K),creating_object,tet,t0,a,i,ech_avoiding,ech,particle = FALSE)}
  }
  else{
    if(particle == TRUE){
      ech_hitting = mclapply(X = seq(K),mc.preschedule = TRUE,mc.cores = ncores,creating_object,tet,t0,a,i,ech_avoiding,ech,particle = TRUE,p)
    }
    else{ech_hitting = mclapply(X = seq(K),mc.preschedule = TRUE,mc.cores = ncores,
                           creating_object,tet,t0,a,i,ech_avoiding,ech,particle = FALSE)}
  }
  return(ech_hitting)
}




sequential_hitting_boolean_sim = function(tet,r0,a,ech,K,parallel = FALSE){
  # avoiding = sequential_avoiding_boolean_sim(tet,r0,a,ech,pdf_name = "avoiding_functional")
  ech_avoiding =  ech[ech[,3]==0,]
  ech_hitting =  rec.list(K)
  order_hitting = sample(which(ech[,3]==1))
  idx_matrix = matrix(0,nrow = K,ncol = length(order_hitting))
  for(t in seq(order_hitting)){
  # for(t in 1:1){
    i = order_hitting[t]
    #this function add objects: it erases the objects proposed that hit the background points
    if(parallel == TRUE){
      ncores = detectCores()-1
      cl = makeCluster(ncores)
      dummy = registerDoParallel(cl)
      ech_hitting_list_prop = sequential_hitting_boolean_sim_checking_background(tet,r0,a,ech,K,i,ech_avoiding,parallel = parallel,ncores,particle = FALSE)
      stopCluster(cl)
    }
    else{ech_hitting_list_prop = sequential_hitting_boolean_sim_checking_background(tet = tet,r0 = r0,a = a,ech = ech,K = K,i = i,ech_avoiding = ech_avoiding,particle = FALSE)}
    #merging
    for(k in 1:K){
      if((is.null(ech_hitting_list_prop[[k]]) | length(ech_hitting_list_prop[[k]])==0) & t == 1){next}
      ech_hitting_list_prop[[k]] = rbind(ech_hitting_list_prop[[k]],ech_hitting[[k]])
    }
    idx = sapply(ech_hitting_list_prop, function(k){
      if((is.null(k) | length(k)==0) & t == 1){return(0)}
      #traditional method
      test = numeric(dim(ech[i,])[1]) #vector hit (which one hit the the objects from the 
      #Non-conditional simulation)
      for(j in 1:(dim(k)[1])){test = test + (sqrt((ech[i,1]-k[j,1])^2+(ech[i,2]-k[j,2])^2)<k[j,3])}
      
      #selon moi ça marche egalement
      # test = as.matrix(sqrt(outer(ech[i,1],k[,1],"-")^2+outer(ech[i,2],k[,2],"-")^2))
      # #if foreground is overlapped 
      # test = sapply(1:(dim(test)[2]),function(j){return(test[,j]<k[j,3])})
      # #how many objects cover ci
      # test = apply(as.matrix(test),2,sum)
      
      #is this number greater than 1
      if(sum(test)>0){return(1)}
      else{return(0)}
    })
    if(sum(idx)==0){cat("ERROR: NOT ENOUGHT PATICLES ! ");break}
    idx = sample(x = 1:K,replace = TRUE,size = K,prob = idx)
    # ech_hitting_list_prop_prop = rec.list(K)
    # for(j in seq(K)){ech_hitting_list_prop_prop[[j]] = ech_hitting_list_prop[[idx[j]]]}
    # for(j in seq(K)){ech_hitting[[j]] = ech_hitting_list_prop_prop[[j]]}
    
    for(j in seq(K)){ech_hitting[[j]] = ech_hitting_list_prop[[idx[j]]]}
    #adding one element to the avoiding
    ech_avoiding = rbind(ech_avoiding,ech[i,])
    # rm(ech_hitting_list_prop_prop)
  }
  return(ech_hitting)
}

sequential_simulation = function(tet,r0,a,ech,K,parallel){
  avoiding = sequential_avoiding_boolean_sim(tet = tet,r0 = r0,a = a,ech = ech)
  hitting = sequential_hitting_boolean_sim(tet = tet,r0 = r0,
                                           a = a,ech = ech,K = K)
  set.seed(NULL)
  return(list(t(buildSimu(avoiding)),hitting[[sample(x = 1:K,size = 1)]]))
  
  
}

map_prob_iterative_NCS = function(tet,r0,a,ech,vector_rep,
                              rectangle_range,csv_name,NCS_Boolean = TRUE){
  map_prob_graph = "map_prob_graphs"
  dir.create(paste0(map_prob_graph),showWarnings = FALSE)
  #init
  grid_total = expand.grid(x=seq(-rectangle_range[1]/2,rectangle_range[1]/2,length.out = 400),
                           y=seq(-rectangle_range[2]/2,rectangle_range[2]/2, length.out = 300))
  ncores = detectCores()-1
  cl = makeCluster(ncores)
  dummy = registerDoParallel(cl)
  set.seed=NULL
  liste_sim_iterative = mclapply(X = vector_rep, mc.cores = ncores,function(rep){
    if(NCS_Boolean == TRUE){ech_NCS = check_points_NCS(tet,a,r0,ech)}
    else{ech_NCS = ech}
    init_time = Sys.time()
    init_matrix = initialization(r0,rectangle_range,ech_NCS,set.seed = FALSE)
    iterative_point = iterative_boolean_sim(tet,r0,a,ech_NCS,set.seed,init_matrix)
    end_time = Sys.time()
    hit2 = rep(0,dim(grid_total)[1])
    for(i in seq(dim(iterative_point[[1]])[1])){
      hit2 = hit2 + (sqrt((grid_total[,1]-iterative_point[[1]][i,1])^2+(grid_total[,2]-iterative_point[[1]][i,2])^2)<iterative_point[[1]][i,3])
    }
    hit2 = as.numeric(hit2>0)
    #write.csv()
    return(list(hit2,iterative_point,as.numeric(difftime(time1 = end_time,time2 = init_time,units = "mins"))))
  })
  stopCluster(cl)
  
  #information of Grid
  matrix_list = do.call(cbind, lapply(liste_sim_iterative,"[[",1))
  write.csv(cbind(grid_total,matrix_list),paste0(map_prob_graph,"/",csv_name,"_indicatrice.csv"))
  rm(matrix_list)
  
  #saving all rows
  matrix_list = do.call(rbind, lapply(lapply(liste_sim_iterative,"[[",2),"[[",1))
  write.csv(matrix_list,paste0(map_prob_graph,"/",csv_name,"_iterative.csv"))
  rm(matrix_list)  
  
  #information about the objects population
  object_matrix = sapply(lapply(lapply(liste_sim_iterative,"[[",2),"[[",2), '[', seq(max(lengths(lapply(lapply(liste_sim_iterative,"[[",2),"[[",2)))))
  write.csv(object_matrix,paste0(map_prob_graph,"/",csv_name,"_n_object.csv"))
  rm(object_matrix)
  #information about the initial objects
  n_obj_init = sapply(lapply(lapply(liste_sim_iterative,"[[",2),"[[",3), '[', seq(max(lengths(lapply(lapply(liste_sim_iterative,"[[",2),"[[",3)))))
  write.csv(n_obj_init,paste0(map_prob_graph,"/",csv_name,"_n_object_init.csv"))
  rm(object_matrix)
  #time simulation
  time_processing = sapply(liste_sim_iterative,"[[",3)
  write.csv(time_processing,paste0(map_prob_graph,"/",csv_name,"_time_processing.csv"))
  return("FIN")
}


map_prob_sequential_NCS = function(tet,r0,a,ech,vector_rep,
                                   rectangle_range,csv_name,K,NCS_Boolean){
  map_prob_graph = "map_prob_graphs"
  dir.create(paste0(map_prob_graph),showWarnings = FALSE)
  grid_total = expand.grid(x=seq(-rectangle_range[1]/2,rectangle_range[1]/2,length.out = 400),
                           y=seq(-rectangle_range[2]/2,rectangle_range[2]/2, length.out = 300))
  ncores = detectCores()-1
  cl = makeCluster(ncores)
  dummy = registerDoParallel(cl)
  # liste_sim_sequential = lapply(X = vector_rep, function(rep){
  gc()
  liste_sim_sequential = mclapply(X = vector_rep,mc.cores = ncores, function(rep){
    if(NCS_Boolean == TRUE){ech_NCS = check_points_NCS(tet,a,r0,ech)}
    else{ech_NCS = ech}
    init_time = Sys.time()
    sequential_point = sequential_simulation(tet,r0,a,ech_NCS,K,parallel = FALSE)
    # sequential_point = sequential_simulation_particule(tet = tet,r0 = r0,a = a,ech = ech_NCS,K = K)
    end_time = Sys.time()
    #avoiding
    hit_avoiding = rep(0,dim(grid_total)[1])
    for(i in seq(dim(sequential_point[[1]])[1])){
      hit_avoiding = hit_avoiding + (sqrt((grid_total[,1]-sequential_point[[1]][,1:3][i,1])^2+(grid_total[,2]-sequential_point[[1]][,1:3][i,2])^2)<
                                       sequential_point[[1]][,1:3][i,3])
    }
    hit_avoiding = as.numeric(hit_avoiding>0)
    #hitting
    hit_hitting = rep(0,dim(grid_total)[1])
    for(i in seq(dim(sequential_point[[2]])[1])){
      hit_hitting = hit_hitting + (sqrt((grid_total[,1]-sequential_point[[2]][,1:3][i,1])^2+(grid_total[,2]-sequential_point[[2]][,1:3][i,2])^2)<
                                     sequential_point[[2]][,1:3][i,3])
    }
    hit_hitting = as.numeric(hit_hitting>0)
    #mixing
    hit2 = rep(0,dim(grid_total)[1])
    for(i in seq(dim(rbind(sequential_point[[1]],sequential_point[[2]][,1:3]))[1])){
      hit2 = hit2 + (sqrt((grid_total[,1]-rbind(sequential_point[[1]],sequential_point[[2]][,1:3])[i,1])^2+(grid_total[,2]-rbind(sequential_point[[1]],sequential_point[[2]][,1:3])[i,2])^2)<
                       rbind(sequential_point[[1]],sequential_point[[2]][,1:3])[i,3])
    }
    hit2 = as.numeric(hit2>0)
    return(list(hit_avoiding,hit_hitting,hit2,sequential_point,as.numeric(difftime(time1 = end_time,time2 = init_time,units = "mins"))))
  })
  
  stopCluster(cl)
  #information of Grid
  matrix_list = do.call(cbind, lapply(liste_sim_sequential,"[[",1))
  write.csv(cbind(grid_total,matrix_list),paste0(map_prob_graph,"/",csv_name,"_avoiding_indicatrice.csv"))
  rm(matrix_list)
  
  matrix_list = do.call(cbind, lapply(liste_sim_sequential,"[[",2))
  write.csv(cbind(grid_total,matrix_list),paste0(map_prob_graph,"/",csv_name,"_hitting_indicatrice.csv"))
  rm(matrix_list)
  
  matrix_list = do.call(cbind, lapply(liste_sim_sequential,"[[",3))
  write.csv(cbind(grid_total,matrix_list),paste0(map_prob_graph,"/",csv_name,"_sequential_indicatrice.csv"))
  rm(matrix_list)  
  
  matrix_list = do.call(rbind, lapply(lapply(liste_sim_sequential,"[[",4),"[[",1))
  write.csv(matrix_list,paste0(map_prob_graph,"/",csv_name,"_sequential_avoiding.csv"))
  rm(matrix_list)  
  
  matrix_list = do.call(rbind, lapply(lapply(liste_sim_sequential,"[[",4),"[[",2))
  write.csv(matrix_list,paste0(map_prob_graph,"/",csv_name,"_sequential_hitting.csv"))
  rm(matrix_list)  
  
  
  #time simulation
  time_processing = sapply(liste_sim_sequential,"[[",5)
  write.csv(time_processing,paste0(map_prob_graph,"/",csv_name,"_time_processing.csv"))
  rm(liste_sim_sequential)
  gc()
  return("FIN")
  
}


map_prob_sequential_old = function(tet,r0,a,ech,vector_rep,pdf_name,rectangle_range,K){
  map_prob_graph = "map_prob_graphs"
  dir.create(paste0(map_prob_graph),showWarnings = FALSE)
  grid_total = expand.grid(x=seq(-rectangle_range[1]/2,rectangle_range[1]/2,length.out = 400),
                           y=seq(-rectangle_range[2]/2,rectangle_range[2]/2, length.out = 300))
  ncores = detectCores()-1
  cl = makeCluster(ncores)
  dummy = registerDoParallel(cl)
  # liste_grid = lapply(X = vector_rep, function(rep){
  liste_grid = mclapply(X = vector_rep, mc.cores = ncores,mc.preschedule = TRUE,function(rep){
    init_time = Sys.time()
    xcond = sequential_simulation(tet = tet,r0,a,ech,K,plot = FALSE)
    end_time = Sys.time()
    #avoiding
    hit_avoiding = rep(0,dim(grid_total)[1])
    for(i in seq(dim(xcond[[1]])[1])){
      hit_avoiding = hit_avoiding + (sqrt((grid_total[,1]-xcond[[1]][i,1])^2+(grid_total[,2]-xcond[[1]][i,2])^2)<xcond[[1]][i,3])
    }
    hit_avoiding = as.numeric(hit_avoiding>0)
    #hitting
    hit_hitting = rep(0,dim(grid_total)[1])
    sample_hitting = sample(x = 1:K,size = 1)
    for(i in seq(dim(xcond[[2]][[sample_hitting]])[1])){
      hit_hitting = hit_hitting + (sqrt((grid_total[,1]-xcond[[2]][[sample_hitting]][i,1])^2+(grid_total[,2]-xcond[[2]][[sample_hitting]][i,2])^2)<xcond[[2]][[sample_hitting]][i,3])
    }
    hit_hitting = as.numeric(hit_hitting>0)
    #both
    hit_both = rep(0,dim(grid_total)[1])
    xcond_both = rbind(xcond[[1]],xcond[[2]][[sample_hitting]][,1:3])
    for(i in seq(dim(xcond_both)[1])){
      hit_both = hit_both + (sqrt((grid_total[,1]-xcond_both[i,1])^2+(grid_total[,2]-xcond_both[i,2])^2)<xcond_both[i,3])
    }
    hit_both = as.numeric(hit_both>0)
    
    return(list(hit_avoiding,hit_hitting,hit_both,as.numeric(difftime(time1 = end_time,time2 = init_time,units = "mins"))))
  })
  stopCluster(cl)
  #AVOIDING
  matrix_list_avoiding = do.call(cbind, lapply(liste_grid,"[[",1))
  write.csv(cbind(grid_total,matrix_list_avoiding),paste0(map_prob_graph,"/",pdf_name,"_avoiding.csv"))
  
  matrix_list_avoiding = cbind(grid_total,apply(matrix_list_avoiding,1,mean))
  matrix_list_avoiding = db.create(matrix_list_avoiding[,3],nx=c(400,300), x0=c(-rectangle_range[1]/2,-rectangle_range[2]/2), dx=c(rectangle_range[1]/400,rectangle_range[2]/300))
  matrix_list_avoiding = db.locate(matrix_list_avoiding,2:3,"x")
  matrix_list_avoiding = db.locate(matrix_list_avoiding,4,"z")
  
  #hitting
  matrix_list_hitting = do.call(cbind, lapply(liste_grid,"[[",2))
  write.csv(cbind(grid_total,matrix_list_hitting),paste0(map_prob_graph,"/",pdf_name,"_hitting.csv"))
  
  matrix_list_hitting = cbind(grid_total,apply(matrix_list_hitting,1,mean))
  matrix_list_hitting = db.create(matrix_list_hitting[,3],nx=c(400,300), x0=c(-rectangle_range[1]/2,-rectangle_range[2]/2), dx=c(rectangle_range[1]/400,rectangle_range[2]/300))
  matrix_list_hitting = db.locate(matrix_list_hitting,2:3,"x")
  matrix_list_hitting = db.locate(matrix_list_hitting,4,"z")
  #both
  #hitting
  matrix_list_both = do.call(cbind, lapply(liste_grid,"[[",3))
  write.csv(cbind(grid_total,matrix_list_both),paste0(map_prob_graph,"/",pdf_name,"_both.csv"))
  
  matrix_list_both = cbind(grid_total,apply(matrix_list_both,1,mean))
  matrix_list_both = db.create(matrix_list_both[,3],nx=c(400,300), x0=c(-rectangle_range[1]/2,-rectangle_range[2]/2), dx=c(rectangle_range[1]/400,rectangle_range[2]/300))
  matrix_list_both = db.locate(matrix_list_both,2:3,"x")
  matrix_list_both = db.locate(matrix_list_both,4,"z")
  #time
  hist_time=  do.call(c, lapply(liste_grid,"[[",4))
  write(hist_time,paste0(map_prob_graph,"/",pdf_name,"_time_processing"))
  #hist_time_random = read.table("map_prob_graphs/map_prob_sequential_random_time_processing");hist_time_random = unname(unlist(hist_time_random))
  #hist_time_line = read.table("map_prob_graphs/map_prob_sequential_line_time_processing");hist_time_line = unname(unlist(hist_time_line))
  #plot avoiding
  png(paste0(map_prob_graph,"/",pdf_name,"_avoiding.png"),width=rectangle_range[1]*100,height=rectangle_range[2]*100)
  par(mai=c(0,0,0,0))
  plot(matrix_list_avoiding,zlim=c(0,1),xlim=c(-rectangle_range[1]/2,rectangle_range[1]/2),ylim=c(-rectangle_range[2]/2,rectangle_range[2]/2),xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.axis=1.0,asp=1,type='n',main = "")
  # points(ech[,1],ech[,2],pch=1,col=ifelse(ech[,3] == 1,"blue","orange"),cex=1.5,lwd=3)
  dev.off()
  #plot hitting
  png(paste0(map_prob_graph,"/",pdf_name,"_hitting.png"),width=rectangle_range[1]*100,height=rectangle_range[2]*100)
  par(mai=c(0,0,0,0))
  plot(matrix_list_hitting,zlim=c(0,1),xlim=c(-rectangle_range[1]/2,rectangle_range[1]/2),ylim=c(-rectangle_range[2]/2,rectangle_range[2]/2),xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.axis=1.0,asp=1,type='n',main = "")
  # points(ech[,1],ech[,2],pch=1,col=ifelse(ech[,3] == 1,"blue","orange"),cex=1.5,lwd=3)
  dev.off()
  #plot both
  png(paste0(map_prob_graph,"/",pdf_name,"_both.png"),width=rectangle_range[1]*100,height=rectangle_range[2]*100)
  par(mai=c(0,0,0,0))
  plot(matrix_list_both,zlim=c(0,1),xlim=c(-rectangle_range[1]/2,rectangle_range[1]/2),ylim=c(-rectangle_range[2]/2,rectangle_range[2]/2),xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.axis=1.0,asp=1,type='n',main = "")
  # points(ech[,1],ech[,2],pch=1,col=ifelse(ech[,3] == 1,"blue","orange"),cex=1.5,lwd=3)
  dev.off()
  
  base_size = 15
  base_line_size=2
  fontsize_size = 16
  title_size = 16
  ggplot(data=as.data.frame(hist_time_random), aes(x = hist_time)) + 
    labs(title = bquote("Histogram: Time Processing Sequential Sim. Random [min]"), 
         x = bquote("Time Processing [min]"),y = bquote(Count)) + geom_histogram(colour="black", fill="white") +
    theme_light(base_size = base_size,base_line_size = base_line_size)+ theme(plot.title = element_text(size=title_size)) +
    geom_vline(xintercept = mean(hist_time_random) , col = "orange",lwd = 1.5)  +
    scale_x_continuous(breaks = seq(0,10,by=1),limits = c(-0.25,10.25)) + scale_y_continuous(breaks = seq(0,500,by=100),limits = c(0,510))+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5) + coord_fixed(1/50)
  # par(pty = "s")
  # ggsave(paste0(map_prob_graph,"/",pdf_name,"_time_processing.pdf"))
  ggsave(paste0(map_prob_graph,"/map_prob_sequential_random_time_processing.pdf"))
  
  ggplot(data=as.data.frame(hist_time_line), aes(x = hist_time_line)) + 
    labs(title = bquote("Histogram: Time Processing Sequential Sim. Line [min]"), 
         x = bquote("Time Processing [min]"),y = bquote(Count)) + geom_histogram(colour="black", fill="white") +
    theme_light(base_size = base_size,base_line_size = base_line_size)+ theme(plot.title = element_text(size=title_size)) +
    geom_vline(xintercept = mean(hist_time_line) , col = "orange",lwd = 1.5)  +
    scale_x_continuous(breaks = seq(0,10,by=1),limits = c(-0.25,10.25)) + scale_y_continuous(breaks = seq(0,500,by=100),limits = c(0,510))+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5) + coord_fixed(1/50)
  
  # ggsave(paste0(map_prob_graph,"/",pdf_name,"_time_processing.pdf"))
  ggsave(paste0(map_prob_graph,"/map_prob_sequential_line_time_processing.pdf"))
  
  return("FIN")
  
}


correlation_curve_map_of_probability = function(ech){
  #inversion just from the Y axis
  #add vector lantu mean
  mean_iterative = read.csv("map_prob_graphs/mean_iterative_random")
  mean_iterative = mean_iterative[,-1]
  
  mean_sequentual = read.csv("map_prob_graphs/map_prob_sequential_random_both.csv")
  mean_sequentual = mean_sequentual[,-c(1:3)]
  mean_sequentual = apply(mean_sequentual,1,mean)
  
  PF_iterative = as.data.frame(cbind(mean_iterative,mean_sequentual))
  #pour le plot
  base_size = 15
  base_line_size=2
  fontsize_size = 16
  title_size = 16
  limits_x = c(0,1)
  division_quadriculado_x = 10
  limits_y = c(0,1)
  division_quadriculado_y = 10
  ggplot(data=PF_iterative, aes(x = mean_iterative, y = mean_sequentual)) +
    labs(title = bquote("Point Cloud with Random Points: Iterative vs Sequential"),
         x = paste0("Iterative"), y = paste0("Sequential"))+
    geom_point() + 
    theme_light(base_size = base_size,base_line_size = base_line_size)+ theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = mean(hist_data_it_random) , col = "orange",lwd = 1.5)  +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = limits_x) + scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5) + coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1]))+
    geom_abline(intercept = 0, slope = 1, col = "orange",lwd = 1.5) +
    annotation_custom(grobTree(textGrob(lm_eqn(PF_iterative,x = mean_iterative, y = mean_sequentual), x=0.005,  y=0.965, hjust=0,
                                        gp=gpar(col="darkred", fontsize=fontsize_size, fontface="italic"))))
  ggsave(paste0("map_prob_graphs/Point_cloud_random_points.pdf"),device = png)
  ####LINE
  mean_iterative = read.csv("map_prob_graphs/mean_iterative_line")
  mean_iterative = mean_iterative[,-1]
  
  mean_sequentual = read.csv("map_prob_graphs/map_prob_sequential_line_both.csv")
  mean_sequentual = mean_sequentual[,-c(1:3)]
  mean_sequentual = apply(mean_sequentual,1,mean)
  
  PF_iterative = as.data.frame(cbind(mean_iterative,mean_sequentual))
  ggplot(data=PF_iterative, aes(x = mean_iterative, y = mean_sequentual)) +
    labs(title = bquote("Point Cloud with Line Point: Iterative vs Sequential"),
         x = paste0("Iterative"), y = paste0("Sequential"))+
    geom_point() + 
    theme_light(base_size = base_size,base_line_size = base_line_size)+ theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = mean(hist_data_it_random) , col = "orange",lwd = 1.5)  +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = limits_x) + scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5) + coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1]))+
    geom_abline(intercept = 0, slope = 1, col = "orange",lwd = 1.5) +
    annotation_custom(grobTree(textGrob(lm_eqn(PF_iterative,x = mean_iterative, y = mean_sequentual), x=0.005,  y=0.965, hjust=0,
                                        gp=gpar(col="darkred", fontsize=fontsize_size, fontface="italic"))))
  ggsave(paste0("map_prob_graphs/Point_cloud_line_points.pdf"),device = png)
  
  
  #time processing comp
  #iterative
  hist_time_iterative_random = read.table("map_prob_graphs/map_prob_iterative_random_time_processing");hist_time_iterative_random = unname(unlist(hist_time_iterative_random))
  # hist_time_iterative_random = as.data.frame(cbind(hist_time_iterative_random,"it_random"))
  
  hist_time_iterative_line = read.table("map_prob_graphs/map_prob_iterative_line_time_processing");hist_time_iterative_line = unname(unlist(hist_time_iterative_line))
  # hist_time_iterative_line = cbind(hist_time_iterative_line,"it_line")
  #sequential
  hist_time_sequential_random = read.table("map_prob_graphs/map_prob_sequential_random_time_processing");hist_time_sequential_random = unname(unlist(hist_time_sequential_random))
  # hist_time_sequential_random=as.data.frame(hist_time_sequential_random)
  # hist_time_sequential_random = as.data.frame(cbind(hist_time_sequential_random,"seq_random"))
  # hist_time_sequential_random[,2] = as.factor(hist_time_sequential_random[,2])
  
  hist_time_sequential_line = read.table("map_prob_graphs/map_prob_sequential_line_time_processing");hist_time_sequential_line = unname(unlist(hist_time_sequential_line))
  # hist_time_sequential_line=as.data.frame(hist_time_sequential_line)
  # hist_time_sequential_line = cbind(hist_time_sequential_line,"it_line")
  # hist_time_sequential_line[,2] = as.factor(hist_time_sequential_line[,2])
  
  #mix
  #random
  base_size = 15
  base_line_size=2
  fontsize_size = 16
  title_size = 16
  limits_x = c(0,10)
  division_quadriculado_x = 10
  limits_y = c(0,500)
  division_quadriculado_y = 10
  hist_time_random = c(hist_time_iterative_random,hist_time_sequential_random)
  method = factor(x = c(rep("Iterative",length(hist_time_iterative_random)),rep("Sequential",length(hist_time_sequential_random))))
  hist_time_random=data.frame(hist_time_random,method)
  mu <- ddply(hist_time_random, "method", summarise, grp.mean=mean(hist_time_random))
  #plot  
  ggplot(data=hist_time_random, aes(x = hist_time_random, color = method,fill = method)) + 
    labs(title = bquote("Histogram: Time Processing Sim. random [min]"), 
         x = bquote("Time Processing [min]"),y = bquote(Count)) + geom_histogram(position="identity", alpha=0.5,binwidth = 0.5) +
    theme_light(base_size = base_size,base_line_size = base_line_size)+ theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position="bottom") +
    geom_vline(data=mu, aes(xintercept=grp.mean, color=method),lwd = 2) +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = limits_x) + scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1]))
  # stat_bin(aes(y=..count..+10, label=ifelse(..count..==0,"",..count..), label=..count..), geom="text", binwidth=0.5)
  
  ggsave(paste0(map_prob_graph,"/map_prob_random_time_processing.pdf"))
  #line
  hist_time_line = c(hist_time_iterative_line,hist_time_sequential_line)
  method = factor(x = c(rep("Iterative",length(hist_time_iterative_line)),rep("Sequential",length(hist_time_sequential_line))))
  hist_time_line=data.frame(hist_time_line,method)
  mu <- ddply(hist_time_line, "method", summarise, grp.mean=mean(hist_time_line))
  #plot  
  ggplot(data=hist_time_line, aes(x = hist_time_line, color = method,fill = method)) + 
    labs(title = bquote("Histogram: Time Processing Sim. line [min]"), 
         x = bquote("Time Processing [min]"),y = bquote(Count)) + geom_histogram(position="identity", alpha=0.5,binwidth = 0.5) +
    theme_light(base_size = base_size,base_line_size = base_line_size)+ theme(plot.title = element_text(size=title_size),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position="bottom") +
    geom_vline(data=mu, aes(xintercept=grp.mean, color=method),lwd = 2)  +
    scale_x_continuous(breaks = seq(limits_x[1],limits_x[2],by=(limits_x[2]-limits_x[1])/division_quadriculado_x),limits = c(limits_x[1]-0.5,limits_x[2])) + scale_y_continuous(breaks = seq(limits_y[1],limits_y[2],by=(limits_y[2]-limits_y[1])/division_quadriculado_y),limits = limits_y)+
    coord_fixed(ratio = (limits_x[2]-limits_x[1])/(limits_y[2]-limits_y[1]))
  # stat_bin(aes(y=..count..+10, label=ifelse(..count..==0,"",..count..), label=..count..), geom="text", binwidth=0.5)
  ggsave(paste0(map_prob_graph,"/map_prob_line_time_processing.pdf"))
  
  
}


integrand = function(x,h,a){
  ifelse(h<=2*x,2*(x^2)*(acos(h/(2*x))-(h/(2*x)*sqrt(1-(h^2/(4*x^2)))))*a*exp(-a*x),0)
}
integral_kr_h = function(h,a){
  
  result = numeric(length(h))
  for(i in 1:length(h)){
    result[i] = integrate(f = integrand,lower = h[i]/2,upper =
                            Inf,h=h[i],a=a)$value
  }
  return(result)
}
variogram_x_h = function(h,tet,a){
  vario = exp(-tet*integral_kr_h(h = 0,a))*(1-exp(-tet*(integral_kr_h(h=0,a)-integral_kr_h(h = h,a))))
  return(vario)
}

#we compute the vario.grid with the CSV
variogram_grid_boolean = function(csv_name,rectangle_range){
  map_prob_graph = "map_prob_graphs"
  #charger les donnes
  matrix_list_indicatrice = read.csv(paste0(map_prob_graph,"/",csv_name,"_indicatrice.csv"))
  matrix_list_indicatrice = matrix_list_indicatrice[,-1]
  matrix_list_indicatrice = matrix_list_indicatrice[,-c(1:2)]
  matrix_list = db.create(matrix_list_indicatrice,nx=c(400,300), x0=c(-rectangle_range[1]/2,-rectangle_range[2]/2), dx=c(rectangle_range[1]/400,rectangle_range[2]/300))
  matrix_list = db.locate(matrix_list,2:3,"x")
  
  ncores = detectCores()-1
  cl = makeCluster(ncores)
  dummy = registerDoParallel(cl)
  gc()
  vlist = mclapply(seq(dim(matrix_list_indicatrice)[2]),mc.cores = ncores,function(i){
    matrix_list = db.locerase(matrix_list,"z")
    matrix_list = db.locate(matrix_list,3+i,"z")
    vario = vario.grid(matrix_list,nlag = c(rectangle_range[1]*100/2,rectangle_range[2]*100/2))
    vario.write(vario = vario,filename = paste0(map_prob_graph,"/",csv_name,"_vario_",i))
    return(vario)}
    )
  rm(vlist)
  gc()
  return("FIN")
  # v_graph = vario.average(vlist)
  # plot(v_graph,idir0 = 1,lwd = 2,lty = 1, add = TRUE, varline = FALSE)
}


sequential_hitting_boolean_sim_particle = function(tet,r0,a,ech,K){
  parallel_K = function(x){
    N = rpois(1,tet*pi*(2/a^2)) #selon l'expression de la premiere partie
    if(N == 0){return(NULL)}
    # rayons des disques
    R = rgamma(N,3,a)
    # implantations	
    al = 2*pi*runif(N) #angle en polaires
    m = R*sqrt(runif(N)) #rayon en polaires
    x_proj = cbind(m*cos(al)+ech[i,1],m*sin(al)+ech[i,2]) #projection aux axis cartesiennes
    x_proj = as.matrix(cbind(x_proj,R))
    test= numeric(N)
    for(j in seq(N)){
      if(as.numeric(sum(sqrt((ech_avoiding[,1]-x_proj[j,1])^2+(ech_avoiding[,2]-x_proj[j,2])^2)<R[j])>0)){test[j] = 1}
    }
    x_proj = x_proj[test==0,,drop=F]
    if(length(x_proj)==0){return(NULL)}
    for(j in 1:(dim(x_proj)[1])){p[[x]] = newParticule(data = x_proj[j,],id = x,idx = x,par = p[[x]])}
    return(p[[x]])
  }
  ech_avoiding =  ech[ech[,3]==0,]
  # ech_hitting =  rec.list(K)
  order_hitting = sample(which(ech[,3]==1))
  p = NULL
  for(k in 1:K){p = c(p,newParticule(NULL))}
  p_prop = list()
  for(t in seq(order_hitting)){
  # for(t in 1:2){#test
    i=order_hitting[t]
    p_part = lapply(1:K,parallel_K)
    idx = sapply(p_part, function(k){
      if(is.null(k)){return(0)}
      if(length(k)==0){return(0)}
      k_test = t(buildSimu(k))
      test = numeric(dim(k_test)[1])
      for(j in 1:length(test)){
        if(as.numeric(sum(sqrt((ech[order_hitting[t],1]-k_test[j,1])^2+(ech[order_hitting[t],2]-k_test[j,2])^2)<k_test[j,3])>0)){test[j] = 1}
      }
      if(sum(test)>0){return(1)}
      else{return(0)}
    })
    if(sum(idx)==0){cat("ERROR: NOT ENOUGHT PATICLES ! ");break}
    idx = sample(x = 1:K,replace = TRUE,size = K,prob = idx)
    
    p_prop = p_part
    for(j in 1:K){p[[j]] = p_prop[[idx[j]]]}
    #adding one element to the avoiding
    ech_avoiding = rbind(ech_avoiding,ech[order_hitting[t],])
    p_prop = list()
    
  }
  return(p)
}

sequential_hitting_boolean_sim_particle_2 = function(tet,r0,a,ech,K){
  parallel_K = function(x){
    N = rpois(1,tet*pi*(2/a^2)) #selon l'expression de la premiere partie
    if(N == 0){return(p[[x]])}
    # rayons des disques
    R = rgamma(N,3,a)
    # implantations	
    al = 2*pi*runif(N) #angle en polaires
    m = R*sqrt(runif(N)) #rayon en polaires
    x_proj = cbind(m*cos(al)+ech[i,1],m*sin(al)+ech[i,2]) #projection aux axis cartesiennes
    x_proj = as.matrix(cbind(x_proj,R))
    #checking and erasing background
    test = as.matrix(sqrt(outer(ech_avoiding[,1],x_proj[,1],"-")^2+outer(ech_avoiding[,2],x_proj[,2],"-")^2))
    test = sapply(1:N,function(i){
      test1 = test[,i]<x_proj[i,3]
      return(test1)
    })
    test = apply(as.matrix(test),2,sum)
    # test= numeric(N)
    # for(j in seq(N)){
    #   if(as.numeric(sum(sqrt((ech_avoiding[,1]-x_proj[j,1])^2+(ech_avoiding[,2]-x_proj[j,2])^2)<R[j])>0)){test[j] = 1}
    # }
    x_proj = x_proj[test==0,,drop=F]
    
    if(length(x_proj)==0){return(p[[x]])}
    for(j in 1:(dim(x_proj)[1])){p[[x]] = newParticule(data = x_proj[j,],id = x,idx = x,par = p[[x]])}
    return(p[[x]])
  }
  ech_avoiding =  ech[ech[,3]==0,]
  # ech_hitting =  rec.list(K)
  order_hitting = sample(which(ech[,3]==1))
  p = NULL
  for(k in 1:K){p = c(p,newParticule(NULL))}
  p_prop = list()
  for(t in seq(order_hitting)){
  # for(t in 1:2){#test
    i=order_hitting[t]
    
    p_part = sequential_hitting_boolean_sim_checking_background(tet = tet,r0 = r0,a = a,ech = ech,K = K,i = i,ech_avoiding = ech_avoiding,particle = TRUE,p = p)
    
    # p_part = lapply(1:K,parallel_K)
    
    #########################
    idx = sapply(p_part, function(k){
      k = t(buildSimu(k))
      if((is.null(k) | length(k)==0) & t == 1){return(0)}
      # test = sqrt(outer(k[,1],ech[i,1],"-")^2+outer(k[,2],ech[i,2],"-")^2)<k[,3]
      test = as.matrix(sqrt(outer(ech[i,1],k[,1],"-")^2+outer(ech[i,2],k[,2],"-")^2))

      test = sapply(1:(dim(test)[2]),function(i){
        test1 = test[,i]<k[i,3]
        return(test1)
      })
      test = apply(as.matrix(test),2,sum)
      if(sum(test)>0){return(1)}
      else{return(0)}
    })
    # 
    #option LENT, TROP LENT: buildsimu each time = lent
    # idx = sapply(p_part, function(k){
    #   if((is.null(t(buildSimu(k))) | length(t(buildSimu(k)))==0) & t == 1){return(0)}
    #   # test = sqrt(outer(t(buildSimu(k))[,1],ech[i,1],"-")^2+outer(t(buildSimu(k))[,2],ech[i,2],"-")^2)<t(buildSimu(k))[,3]
    #   test = as.matrix(sqrt(outer(ech[i,1],t(buildSimu(k))[,1],"-")^2+outer(ech[i,2],t(buildSimu(k))[,2],"-")^2))
    #   
    #   test = sapply(1:(dim(test)[2]),function(i){
    #     test1 = test[,i]<t(buildSimu(k))[i,3]
    #     return(test1)
    #   })
    #   test = apply(as.matrix(test),2,sum)
    #   
    #   
    #   if(sum(test)>0){return(1)}
    #   else{return(0)}
    # })
   
    
    
    if(sum(idx)==0){cat("ERROR: NOT ENOUGHT PATICLES ! ");break}
    idx = sample(x = 1:K,replace = TRUE,size = K,prob = idx)
    
    #p_prop = p_part
    for(j in 1:K){p[[j]] = p_part[[idx[j]]]}
    #adding one element to the avoiding
    ech_avoiding = rbind(ech_avoiding,ech[order_hitting[t],])
    p_part = list()
    
  }
  return(p)
}

sequential_simulation_particule = function(tet,r0,a,ech,K){
  avoiding = sequential_avoiding_boolean_sim(tet = tet,r0 = r0,a = a,ech = ech)
  hitting = sequential_hitting_boolean_sim_particle_2(tet = tet,r0 = r0,a = a,ech = ech,K = K)
  
  return(list(t(buildSimu(avoiding)),t(buildSimu(hitting[[sample(x = 1:K,size = 1)]]))))
  
  
}
#Faire N simulations non-conditionnelles
NCS = function(tet,a,r0){
  set.seed(NULL)
  N = rpois(1,tet*pi*(r0^2+2*r0/a+2/a^2)) #selon l'expression de la premiere partie
  den = a^2*r0^2+2*a*r0+2 #denominateur des probabilité du melange de la distr. 
  p_i = c(a^2*r0^2,2*a*r0,2)/den # probas du mélange
  cum_pi = cumsum(p_i) #distribution cumulée des probas du mélange
  #pour générer les rayons
  u=runif(N)
  R = (u<cum_pi[1])*rgamma(N,1,a)+(u>cum_pi[1] & u<cum_pi[2])*rgamma(N,2,a) + 
    (u>cum_pi[2])*rgamma(N,3,a)
  # implantations	
  al = 2*pi*runif(N) #angle en polaires
  m = (R+r0)*sqrt(runif(N)) #rayon en polaires
  x = cbind(m*cos(al),m*sin(al)) #projection aux axis cartesiennes
  if(is.null(x)){return(NULL)}
  return(cbind(x,R))
}

#Check if the points are background or foreground from a set of points
check_points_NCS = function(tet,a,r0,ech){
  x = NCS(tet = tet,a = a,r0 = r0)
  #it is foreground or background
  hit = rep(0,dim(ech)[1]) #vector hit (which one hit the the objects from the 
  #Non-conditional simulation)
  for(i in 1:(dim(x)[1])){hit = hit + (sqrt((ech[,1]-x[i,1])^2+(ech[,2]-x[i,2])^2)<x[i,3])}
  hit = as.numeric(hit>0)
  return(cbind(ech[,1:2],hit))
}
CSV_compute_N_NCS = function(tet,r0,a,vector_rep,rectangle_range,csv_name){
  map_prob_graph = "map_prob_graphs"
  dir.create(map_prob_graph,showWarnings = FALSE)
  grid_total = expand.grid(x=seq(-rectangle_range[1]/2,rectangle_range[1]/2,length.out = 400),
                           y=seq(-rectangle_range[2]/2,rectangle_range[2]/2, length.out = 300))
  result_compute_N_NCS = compute_N_NCS(tet,a,r0,vector_rep)
  gc()
  ncores = detectCores()-1
  cl = makeCluster(ncores)
  dummy = registerDoParallel(cl)
  grid_computed = mclapply(result_compute_N_NCS,mc.cores = ncores, function(liste){
    #alternativa: demasiada RAM por gran matriz hit2
    # hit2 = as.matrix(sqrt(outer(grid_total[,1],liste[,1],"-")^2+outer(grid_total[,2],liste[,2],"-")^2))
    # hit2 = sapply(1:(dim(hit2)[2]),function(i){
    #   test1 = hit2[,i]<liste[i,3]
    #   return(test1)
    # })
    # hit2 = apply(as.matrix(hit2),1,sum)
    # hit2 = as.numeric(hit2>0)
    #optimizado: mucha ram igual
    # hit2 = sapply(1:(dim(liste)[1]),function(i){
    #   hit2 = sqrt(outer(grid_total[,1],liste[i,1],"-")^2+outer(grid_total[,2],liste[i,2],"-")^2)
    #   hit2 = hit2[,1]<liste[i,3]
    #   return(hit2)
    # })
    # hit2 = apply(as.matrix(hit2),1,sum)
    # hit2 = as.numeric(hit2>0)

    # #functional
    hit2 = rep(0,dim(grid_total)[1])
    for(i in seq(dim(liste)[1])){
      hit2 = hit2 + (sqrt((grid_total[,1]-liste[i,1])^2+(grid_total[,2]-liste[i,2])^2)<liste[i,3])
    }
    hit2 = as.numeric(hit2>0)
    return(hit2)

  })
  stopCluster(cl)
  grid_computed = do.call(cbind,grid_computed)
  write.csv(cbind(grid_total,grid_computed),paste0(map_prob_graph,"/",csv_name,"_indicatrice.csv"))
  write.csv()
  return(list(grid_computed,result_compute_N_NCS))
}

compute_N_NCS = function(tet,a,r0,vector_rep){
  ncores = detectCores()-1
  cl = makeCluster(ncores)
  dummy = registerDoParallel(cl)
  # liste_grid = lapply(X = vector_rep, function(rep){
  gc()
  result = mclapply(X = vector_rep, mc.cores = ncores,mc.preschedule = TRUE,function(rep){
    result_NCS = NCS(tet,a,r0)
  })
  return(result)
}
