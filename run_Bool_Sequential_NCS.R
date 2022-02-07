#RUN KATRINA
#parameters
source("thesis_boolean_model.R")
tet = 10 #density Poisson point process
a = 7.22 #exponential parameter
r0 = 6  #radius of the champ Z
rectangle_range = c(8,6)
# génération du nombre d'objets
N = rpois(1,tet*pi*(r0^2+2*r0/a+2/a^2)) #selon l'expression de la premiere partie

# rayons des disques

den = a^2*r0^2+2*a*r0+2 #denominateur des probabilité du melange de la distr. 
#gamma
p_i = c(a^2*r0^2,2*a*r0,2)/den # probas du mélange

cum_pi = cumsum(p_i) #distribution cumulée des probas du mélange
vector_rep = 1:3
NCS_Boolean = TRUE
# for(K in c(200,400,800)){
for(K in 200){
  ech = read.table("pc")
  ech[,1] = ech[,1]-rectangle_range[1]/2
  ech[,2] = ech[,2]-rectangle_range[2]/2
  csv_name = ifelse(NCS_Boolean==TRUE,paste0("Bool_sequential_random_NCS_K_",K),paste0("Bool_sequential_random_K_",K))
  dir.create(csv_name)
  setwd(csv_name)
  # #cache pour eviter un betise avec les plot_map
  map_prob_sequential_NCS(tet = tet,r0 = r0,a = a,ech = ech,vector_rep = vector_rep,rectangle_range = rectangle_range,csv_name = csv_name,K = K,NCS_Boolean = NCS_Boolean)
  radius = c(read.csv(file = paste0("map_prob_graphs/",csv_name,"_sequential_avoiding.csv"))[,4],read.csv(file = paste0("map_prob_graphs/",csv_name,"_sequential_hitting.csv"))[,4])
  qq_plot(tet=tet,r0=r0,a = a,radius = radius,csv_name = csv_name)
  rm(radius)
  plot_map_prob(csv_name = paste0(csv_name,"_avoiding"),rectangle_range)
  plot_map_prob(csv_name = paste0(csv_name,"_hitting"),rectangle_range)
  plot_map_prob(csv_name = paste0(csv_name,"_sequential"),rectangle_range)
  hist_ind_sim_sequential(tet = tet,r0 = r0,a = a,input_csv = paste0(csv_name,"_sequential"))
  
  # variogram_grid_boolean(csv_name = paste0(csv_name,"_sequential"),rectangle_range)
  # plot_variogram_grid_boolean(vector_rep = vector_rep,a = a,tet = tet,csv_name = paste0(csv_name,"_sequential"),rectangle_range = rectangle_range)
  setwd("..")
  
  ech = read.table("pcl")
  ech[,1] = ech[,1]-rectangle_range[1]/2
  ech[,2] = ech[,2]-rectangle_range[2]/2
  csv_name = ifelse(NCS_Boolean==TRUE,paste0("Bool_sequential_line_NCS_K_",K),paste0("Bool_sequential_line_K_",K))
  dir.create(csv_name)
  setwd(csv_name)
  # #cache pour eviter un betise avec les plot_map
  map_prob_sequential_NCS(tet = tet,r0 = r0,a = a,ech = ech,vector_rep = vector_rep,rectangle_range = rectangle_range,csv_name = csv_name,K = K,NCS_Boolean = NCS_Boolean)
  radius = c(read.csv(file = paste0("map_prob_graphs/",csv_name,"_sequential_avoiding.csv"))[,4],read.csv(file = paste0("map_prob_graphs/",csv_name,"_sequential_hitting.csv"))[,4])
  qq_plot(tet=tet,r0=r0,a = a,radius = radius,csv_name = csv_name)
  rm(radius)
  plot_map_prob(csv_name = paste0(csv_name,"_avoiding"),rectangle_range)
  plot_map_prob(csv_name = paste0(csv_name,"_hitting"),rectangle_range)
  plot_map_prob(csv_name = paste0(csv_name,"_sequential"),rectangle_range)
  hist_ind_sim_sequential(tet = tet,r0 = r0,a = a,input_csv = paste0(csv_name,"_sequential"))
  
  # variogram_grid_boolean(csv_name = paste0(csv_name,"_sequential"),rectangle_range)
  # plot_variogram_grid_boolean(vector_rep = vector_rep,a = a,tet = tet,csv_name = paste0(csv_name,"_sequential"),rectangle_range = rectangle_range)
  setwd("..")
}
