#RUN KATRINA
#parameters
source("thesis_boolean_model.R")
tet = 10 #density Poisson point process
a = 7.22 #exponential parameter
r0 = 6  #radius of the champ Z
den = a^2*r0^2+2*a*r0+2 #denominateur des probabilité du melange de la distr.
rectangle_range = c(8,6)

NCS_Boolean = TRUE
vector_rep = 1:500


NCS_Boolean_point_fixe = FALSE
if(NCS_Boolean == TRUE){csv_name = ifelse(NCS_Boolean_point_fixe == TRUE,paste0("Bool_iterative_line_NCS"),"Bool_iterative_NCS_all_random")}
if(NCS_Boolean == FALSE){csv_name = paste0("Bool_iterative_line")}
dir.create(csv_name)
setwd(csv_name)
# #cache pour eviter un betise avec les plot_map
map_prob_iterative_NCS(tet = tet,r0 = r0,a = a,ech = ech,vector_rep = vector_rep,rectangle_range = rectangle_range,
                       csv_name = csv_name,NCS_Boolean = NCS_Boolean,NCS_Boolean_point_fixe=NCS_Boolean_point_fixe)
qq_plot(tet=tet,r0=r0,a = a,radius = read.csv(file = paste0("map_prob_graphs/",csv_name,"_iterative.csv"))[,4],csv_name = csv_name)
plot_map_prob(csv_name = paste0(csv_name),rectangle_range)
hist_ind_sim_iterative(tet = tet,r0 = r0,a = a,input_csv = paste0(csv_name))

variogram_grid_boolean(csv_name = paste0(csv_name),rectangle_range)
plot_variogram_grid_boolean(vector_rep = vector_rep,a = a,tet = tet,csv_name = paste0(csv_name),rectangle_range = rectangle_range)
setwd("..")




NCS_Boolean_point_fixe = TRUE
ech = read.table("pc")
ech[,1] = ech[,1]-rectangle_range[1]/2
ech[,2] = ech[,2]-rectangle_range[2]/2
if(NCS_Boolean == TRUE){csv_name = ifelse(NCS_Boolean_point_fixe == TRUE,paste0("Bool_iterative_random_NCS"),"Bool_iterative_NCS_all_random")}
if(NCS_Boolean == FALSE){csv_name = paste0("Bool_iterative_random")}
dir.create(csv_name)
setwd(csv_name)
# #cache pour eviter un betise avec les plot_map
map_prob_iterative_NCS(tet = tet,r0 = r0,a = a,ech = ech,vector_rep = vector_rep,rectangle_range = rectangle_range,
                        csv_name = csv_name,NCS_Boolean = NCS_Boolean,NCS_Boolean_point_fixe=NCS_Boolean_point_fixe)
qq_plot(tet=tet,r0=r0,a = a,radius = read.csv(file = paste0("map_prob_graphs/",csv_name,"_iterative.csv"))[,4],csv_name = csv_name)
plot_map_prob(csv_name = paste0(csv_name),rectangle_range)
hist_ind_sim_iterative(tet = tet,r0 = r0,a = a,input_csv = paste0(csv_name))

variogram_grid_boolean(csv_name = paste0(csv_name),rectangle_range)
plot_variogram_grid_boolean(vector_rep = vector_rep,a = a,tet = tet,csv_name = paste0(csv_name),rectangle_range = rectangle_range)
setwd("..")

NCS_Boolean_point_fixe = TRUE
ech = read.table("pcl")
ech[,1] = ech[,1]-rectangle_range[1]/2
ech[,2] = ech[,2]-rectangle_range[2]/2
if(NCS_Boolean == TRUE){csv_name = ifelse(NCS_Boolean_point_fixe == TRUE,paste0("Bool_iterative_line_NCS"),"Bool_iterative_NCS_all_random")}
if(NCS_Boolean == FALSE){csv_name = paste0("Bool_iterative_line")}
dir.create(csv_name)
setwd(csv_name)
# #cache pour eviter un betise avec les plot_map
map_prob_iterative_NCS(tet = tet,r0 = r0,a = a,ech = ech,vector_rep = vector_rep,rectangle_range = rectangle_range,
                       csv_name = csv_name,NCS_Boolean = NCS_Boolean,NCS_Boolean_point_fixe=NCS_Boolean_point_fixe)
qq_plot(tet=tet,r0=r0,a = a,radius = read.csv(file = paste0("map_prob_graphs/",csv_name,"_iterative.csv"))[,4],csv_name = csv_name)
plot_map_prob(csv_name = paste0(csv_name),rectangle_range)
hist_ind_sim_iterative(tet = tet,r0 = r0,a = a,input_csv = paste0(csv_name))

variogram_grid_boolean(csv_name = paste0(csv_name),rectangle_range)
plot_variogram_grid_boolean(vector_rep = vector_rep,a = a,tet = tet,csv_name = paste0(csv_name),rectangle_range = rectangle_range)
setwd("..")



