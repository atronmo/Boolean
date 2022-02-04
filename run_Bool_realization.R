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

#pour générer les rayons
u=runif(N)
set.seed = 12345
R = (u<cum_pi[1])*rgamma(N,1,a)+(u>cum_pi[1] & u<cum_pi[2])*rgamma(N,2,a) + 
  (u>cum_pi[2])*rgamma(N,3,a)

# implantations	
al = 2*pi*runif(N) #angle en polaires
m = (R+r0)*sqrt(runif(N)) #rayon en polaires

x_proj = cbind(m*cos(al),m*sin(al)) #projection aux axis cartesiennes
x_proj = cbind(x_proj,R)

#domain(r0)
#NCS
dispbool(x_proj,r0,rectangle_range,pdf_name = "NCS_Z.pdf")
dispbool2(x_proj,r0,rectangle_range,pdf_name = "NCS.pdf")

#conditioning points
n = 100
ech = N_points_2(n = n,x_proj = x_proj,R = R,rectangle_range = rectangle_range)
#points de Lantu

dispbool3(x_proj,rectangle_range,ech,pdf_name = "NCS_point.pdf")
dispbool4(x_proj = x_proj,rectangle_range = rectangle_range,ech = ech,pdf_name = "points.pdf")
########################################

#ITERATIVE ####
#random point
ech = read.table("pc")
ech[,1] = ech[,1]-rectangle_range[1]/2
ech[,2] = ech[,2]-rectangle_range[2]/2
init_matrix = initialization(r0,rectangle_range,ech,set.seed = 12345)
dispbool(x_proj = init_matrix,r0 = r0,rectangle_range = rectangle_range,pdf_name = "init_random_Z")
dispbool2(x_proj = init_matrix,r0 = r0,rectangle_range = rectangle_range,pdf_name = "init_random_no_point")
dispbool3(x_proj = init_matrix,ech = ech,rectangle_range = rectangle_range,pdf_name = "init_random_point")

iterative_point = iterative_boolean_sim(tet,r0,a,ech,init_matrix,set.seed = 12345)
dispbool(x_proj = iterative_point[[1]],r0 = r0,rectangle_range = rectangle_range,pdf_name = "iterative_random_Z")
dispbool2(x_proj = iterative_point[[1]],r0 = r0,rectangle_range = rectangle_range,pdf_name = "iterative_random_no_point")
dispbool3(x_proj = iterative_point[[1]],ech = ech,rectangle_range = rectangle_range,pdf_name = "iterative_random_point")
hist_iterative_boolean_sim(tet = tet,r0 = r0,a = a,iterative_point = iterative_point,"iterative_random")
#points line
ech = read.table("pcl")
ech[,1] = ech[,1]-rectangle_range[1]/2
ech[,2] = ech[,2]-rectangle_range[2]/2
init_matrix = initialization(r0,rectangle_range,ech,set.seed = 12345)
dispbool(x_proj = init_matrix,r0 = r0,rectangle_range = rectangle_range,pdf_name = "init_line_Z")
dispbool2(x_proj = init_matrix,r0 = r0,rectangle_range = rectangle_range,pdf_name = "init_line_no_point")
dispbool3(x_proj = init_matrix,ech = ech,rectangle_range = rectangle_range,pdf_name = "init_line_point")

iterative_point = iterative_boolean_sim(tet,r0,a,ech,init_matrix,set.seed = 12345)
dispbool(x_proj = iterative_point[[1]],r0 = r0,rectangle_range = rectangle_range,pdf_name = "iterative_line_Z")
dispbool2(x_proj = iterative_point[[1]],r0 = r0,rectangle_range = rectangle_range,pdf_name = "iterative_line_no_point")
dispbool3(x_proj = iterative_point[[1]],ech = ech,rectangle_range = rectangle_range,pdf_name = "iterative_line_point")
hist_iterative_boolean_sim(tet = tet,r0 = r0,a = a,iterative_point = iterative_point,"iterative_line")

#sequential##############################################

#random point
ech = read.table("pc")
ech[,1] = ech[,1]-rectangle_range[1]/2
ech[,2] = ech[,2]-rectangle_range[2]/2
K=200
#sequential_point = sequential_simulation_particule(tet,r0,a,ech,K)
sequential_point = sequential_simulation(tet,r0,a,ech,K,parallel = FALSE)
#avoiding
dispbool(x_proj = sequential_point[[1]],r0 = r0,rectangle_range = rectangle_range,pdf_name = "sequential_avoiding_random_Z.pdf")
dispbool2(x_proj = sequential_point[[1]],r0 = r0,rectangle_range = rectangle_range,pdf_name = "sequential_avoiding_random_no_point.pdf")
dispbool3(x_proj = sequential_point[[1]],ech = ech,rectangle_range = rectangle_range,pdf_name = "sequential_avoiding_random.pdf")
#hitting
dispbool(x_proj = sequential_point[[2]],r0 = r0,rectangle_range = rectangle_range,pdf_name = "sequential_hitting_random_Z.pdf")
dispbool2(x_proj = sequential_point[[2]],r0 = r0,rectangle_range = rectangle_range,pdf_name = "sequential_hitting_random_no_point.pdf")
dispbool3(x_proj = sequential_point[[2]],ech = ech,rectangle_range = rectangle_range,pdf_name = "sequential_hitting_random.pdf")
#sequential
dispbool(x_proj = rbind(sequential_point[[1]],sequential_point[[2]][,1:3]),r0 = r0,rectangle_range = rectangle_range,pdf_name = "sequential_random_Z.pdf")
dispbool2(x_proj = rbind(sequential_point[[1]],sequential_point[[2]][,1:3]),r0 = r0,rectangle_range = rectangle_range,pdf_name = "sequential_random_no_point.pdf")
dispbool3(x_proj = rbind(sequential_point[[1]],sequential_point[[2]][,1:3]),ech = ech,rectangle_range = rectangle_range,pdf_name = "sequential_random.pdf")


#points line
ech = read.table("pcl")
ech[,1] = ech[,1]-rectangle_range[1]/2
ech[,2] = ech[,2]-rectangle_range[2]/2
K=200
sequential_point = sequential_simulation_particule(tet,r0,a,ech,K)
#avoiding
dispbool(x_proj = sequential_point[[1]],r0 = r0,rectangle_range = rectangle_range,pdf_name = "sequential_avoiding_line_Z.pdf")
dispbool2(x_proj = sequential_point[[1]],r0 = r0,rectangle_range = rectangle_range,pdf_name = "sequential_avoiding_line_no_point.pdf")
dispbool3(x_proj = sequential_point[[1]],ech = ech,rectangle_range = rectangle_range,pdf_name = "sequential_avoiding_line.pdf")
#hitting
dispbool(x_proj = sequential_point[[2]],r0 = r0,rectangle_range = rectangle_range,pdf_name = "sequential_hitting_line_Z.pdf")
dispbool2(x_proj = sequential_point[[2]],r0 = r0,rectangle_range = rectangle_range,pdf_name = "sequential_hitting_line_no_point.pdf")
dispbool3(x_proj = sequential_point[[2]],ech = ech,rectangle_range = rectangle_range,pdf_name = "sequential_hitting_line.pdf")
#sequential
dispbool(x_proj = rbind(sequential_point[[1]],sequential_point[[2]][,1:3]),r0 = r0,rectangle_range = rectangle_range,pdf_name = "sequential_line_Z.pdf")
dispbool2(x_proj = rbind(sequential_point[[1]],sequential_point[[2]][,1:3]),r0 = r0,rectangle_range = rectangle_range,pdf_name = "sequential_line_no_point.pdf")
dispbool3(x_proj = rbind(sequential_point[[1]],sequential_point[[2]][,1:3]),ech = ech,rectangle_range = rectangle_range,pdf_name = "sequential_line.pdf")


