##RUN KATRINA
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
#map_prob_NCS
vector_rep = 1:3
csv_name = "Bool_NCS"
dir.create(csv_name)
setwd(csv_name)
CSV_compute_N_NCS(tet,r0,a,vector_rep,rectangle_range,csv_name)
plot_map_prob(csv_name = csv_name,rectangle_range)
radius = read.csv(file = paste0("map_prob_graphs/",csv_name,"_NCS.csv"))[,4]
qq_plot(tet=tet,r0=r0,a = a,radius = radius,csv_name = csv_name)
rm(radius)
variogram_grid_boolean(csv_name,rectangle_range)
plot_variogram_grid_boolean(vector_rep = vector_rep,a = a,tet = tet,csv_name = paste0(csv_name),rectangle_range = rectangle_range)
setwd("..")


