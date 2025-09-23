library(RPBCapp)

# Read a conf file
# ===========================

file = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example1-base/example1.rpbc"
db = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/BC52_1/BC52_1 linear file.xlsx"
pb = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/BC52_1/BC52_1_Kinleith_2019_ULS_subsampled.laz"

m <- PlantationModel$new()
c <- PlantationController$new(m)
v <- PlantationView$new(m)

c$set_cloud("/home/jr/Documents/Entreprise/clients/RPBC/Plantations/BC55_2/BC55_2_Kaingaroa_2023_ULS.laz")
c$read_config(file)
v$leaflet()

# View
# ===========================

file = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example2/BC19BP01CKT_EstReport.xlsx"

m <- PlantationModel$new()
c <- PlantationController$new(m)
v <- PlantationView$new(m)

block_size <- c(18.6)
num_trees <- 6

c$set_database(file)
v$summary()

c$set_crs(2193)
c$set_layout_parameter(block_size, num_trees, start = "bl", orientation = "v")

v$rgl()
v$summary()
v$plot_layout()
v$leaflet()
v$compute_tree_stats()
