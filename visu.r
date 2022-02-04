#
# Plot a simulation in pdf 
# Gamut for extremes
# Program written by Emilie Chautru
#
# Last update 03.06.20
#
# Remark: don't forget to remove possible headers      
# 
#

#--- FONCTION POUR TRACER LA SIMULATION

# simfile : (chaîne de caractères) nom du fichier source avec la simu e.g. "sim", avec l'extension s'il y en a une
# size : (vecteur) taille de la grille avec abcisses d'abord et ordonnées ensuite e.g. c(300,300)
# plotfile : (chaîne de caractères) nom du fichier pdf à produire e.g. "sim"
# wd : (chaîne de caractères) chemin du dossier courant

plotsim <- function(simfile, size, plotfile, wd = getwd()){
  
  pal <- c("#000000", "#000000", "#000000", "#000000", "#010202", "#020405", "#030608", "#05080B", "#060A0D", "#070C10", "#090F13", "#0A1116", "#0B1318", "#0D151B", "#0E171E", "#0F1921", "#111B23", "#121E26", "#132029", "#15222C", "#16242E", "#172631", "#192834", "#1A2A37", "#1B2D39", "#1D2F3C", "#1E313F", "#1F3342", "#213544", "#223747", "#23394A", "#253C4D", "#263E4F", "#274052", "#294255", "#2A4458", "#2B465A", "#2D485D", "#2E4B60", "#2F4D63", "#314F65", "#325168", "#33536B", "#35556E", "#365770", "#375A73", "#395C76", "#3A5E79", "#3B607B", "#3D627E", "#3E6481", "#3F6684", "#416986", "#426B89", "#436D8C", "#456F8F", "#467191", "#477394", "#497597", "#4A789A", "#4B7A9C", "#4D7C9F", "#4E7EA2", "#4F80A5", "#5182A7", "#5284AA", "#5387AD", "#5589B0", "#568BB2", "#578DB5", "#598FB8", "#5A91BB", "#5B93BD", "#5D96C0", "#5E98C3", "#5F9AC6", "#619CC8", "#629ECB", "#63A0CE", "#65A3D1", "#65A4CE", "#66A5CC", "#67A7CA", "#67A8C8", "#68A9C6", "#69ABC4", "#69ABC4", "#69ACC2", "#69ADC0", "#6AAEBE", "#6AAFBC", "#6BB0BA", "#6BB1B8", "#6CB3B7", "#6CB3B7", "#6CB4B5", "#6DB5B3", "#6DB6B1", "#6EB8AF", "#6EB9AD", "#6FBAAB", "#70BCAA", "#70BCAA", "#70BDA8", "#70BEA6", "#71BFA4", "#71C0A2", "#72C1A0", "#72C29E", "#73C49D", "#73C49D", "#73C59B", "#74C699", "#74C797", "#75C895", "#75C993", "#76CA91", "#77CC90", "#77CC90", "#77CD8E", "#77CE8C", "#78CF8A", "#78D088", "#79D186", "#79D284", "#7AD483", "#7AD483", "#7BD481", "#7CD580", "#7DD57E", "#7FD67D", "#80D77C", "#81D77A", "#82D879", "#84D978", "#84D978", "#86D976", "#88D975", "#8AD974", "#8CDA73", "#8EDA72", "#90DA71", "#93DB70", "#93DB70", "#95DB6E", "#97DB6D", "#99DB6C", "#9CDC6B", "#9EDC6A", "#A0DC69", "#A3DD68", "#A3DD68", "#A5DD66", "#A7DD65", "#A9DD64", "#ACDE62", "#AEDE61", "#B0DE60", "#B3DF5F", "#B3DF5F", "#B5DF5D", "#B7DF5C", "#B9DF5B", "#BBE05A", "#BDE059", "#BFE058", "#C2E157", "#C2E157", "#C4E155", "#C6E154", "#C8E153", "#CBE252", "#CDE251", "#CFE250", "#D2E34F", "#D2E34F", "#D4E34D", "#D6E34C", "#D8E34B", "#DBE44A", "#DDE449", "#DFE448", "#E2E547", "#E2E547", "#E4E545", "#E6E544", "#E8E543", "#EAE642", "#ECE641", "#EEE640", "#F1E73F", "#F1E73F", "#F1E33E", "#F1E03D", "#F1DC3C", "#F2D93B", "#F2D53A", "#F2D239", "#F2CE38", "#F3CB37", "#F3CB37", "#F3C635", "#F3C234", "#F3BE33", "#F3BA32", "#F3B631", "#F3B230", "#F4AE2F", "#F4AE2F", "#F4A92D", "#F4A52C", "#F4A12B", "#F59D2A", "#F59929", "#F59528", "#F69127", "#F69127", "#F68C26", "#F68825", "#F68424", "#F68023", "#F67C22", "#F67821", "#F77420", "#F77420", "#F76F1E", "#F76B1D", "#F7671C", "#F7631B", "#F75F1A", "#F75B19", "#F85718", "#F85718", "#F55216", "#F34E15", "#F04A14", "#EE4613", "#EB4212", "#E93E11", "#E73A10", "#E73A10", "#E4350E", "#E2310D", "#E02D0C", "#DD290B", "#DB250A", "#D92109", "#D71D08", "#D71D08", "#D41806", "#D21405", "#D01004", "#CD0C03", "#CB0802", "#C90401", "#C70000") # Palette
  sim <- as.matrix(read.table(paste(wd, "/", simfile, sep = ""))) # Chargement de la simulation
  sim <- matrix(as.vector(t(sim)), size[1], size[2]) # Formattage de la simulation pour tracé
  
  pdf(paste(wd, "/", plotfile, ".pdf", sep = "")) # Début de l'écriture du pdf
  par(bty = "n", mai =c (0,0,0,0)) # Paramètres graphiques
  image(1:size[1], 1:size[2], sim, asp = 1, main = "", xlab ="", ylab = "", axes = F, col = pal) # Tracé de la simulation
  dev.off() # Fin de l'écriture du pdf
}

plotsim("absdif", c(400,300), "absdif") # Exemple d'utilisation
