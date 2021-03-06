#RUN KATRINA
#parameters
source("thesis_boolean_model.R")
tet = 10 #density Poisson point process
a = 7.22 #exponential parameter
r0 = 6  #radius of the champ Z
rectangle_range = c(8,6)
# génération du nombre d'objets
N = rpois(1,tet*pi*(r0^2+2*r0/a+2/a^2)) #selon l'expression de la premiere partie
den = a^2*r0^2+2*a*r0+2 #denominateur des probabilité du melange de la distr. 
#gamma
p_i = c(a^2*r0^2,2*a*r0,2)/den # probas du mélange
cum_pi = cumsum(p_i) #distribution cumulée des probas du mélange

#ITERATIVE:
vector_rep = 1:500
NCS_Boolean = FALSE
ech = read.table("pc")
ech[,1] = ech[,1]-rectangle_range[1]/2
ech[,2] = ech[,2]-rectangle_range[2]/2
csv_name = paste0("Bool_iterative_random")
dir.create(csv_name)
setwd(csv_name)
list_sim=map_prob_iterative_NCS(tet = tet,r0 = r0,a = a,ech = ech,vector_rep = vector_rep,rectangle_range = rectangle_range,csv_name = csv_name,NCS_Boolean = NCS_Boolean)
plot_map_prob(csv_name = paste0(csv_name),rectangle_range)
hist_ind_sim_iterative(tet = tet,r0 = r0,a = a,input_csv = paste0(csv_name))
vario_grid_list = variogram_grid_boolean(csv_name,rectangle_range)
plot_variogram_grid_boolean(a = a,tet=tet,vario_grid_list,csv_name = paste0(csv_name),rectangle_range)
radius = do.call(rbind,lapply(lapply(list_sim,"[[",2),"[[",1))[,3]
qq_plot(tet=tet,r0=r0,a = a,radius = radius,csv_name = csv_name)
rm(list_sim)
rm(vario_grid_list)
setwd("..")

#ITERATIVE: NCS activated
ech = read.table("pcl")
ech[,1] = ech[,1]-rectangle_range[1]/2
ech[,2] = ech[,2]-rectangle_range[2]/2
csv_name = paste0("Bool_iterative_line")
dir.create(csv_name)
setwd(csv_name)
list_sim=map_prob_iterative_NCS(tet = tet,r0 = r0,a = a,ech = ech,vector_rep = vector_rep,rectangle_range = rectangle_range,csv_name = csv_name,NCS_Boolean = NCS_Boolean)
plot_map_prob(csv_name = paste0(csv_name),rectangle_range)
hist_ind_sim_iterative(tet = tet,r0 = r0,a = a,input_csv = paste0(csv_name))
vario_grid_list = variogram_grid_boolean(csv_name,rectangle_range)
plot_variogram_grid_boolean(a = a,tet=tet,vario_grid_list,csv_name = paste0(csv_name),rectangle_range)
radius = do.call(rbind,lapply(lapply(list_sim,"[[",2),"[[",1))[,3]
qq_plot(tet=tet,r0=r0,a = a,radius = radius,csv_name = csv_name)
rm(list_sim)
rm(vario_grid_list)
setwd("..")

