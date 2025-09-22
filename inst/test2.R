library(RPBCapp)

# Read a conf file
# ===========================

file = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example1/example1.rpbc"

m <- PlantationModel$new()
c <- PlantationController$new(m)
v <- PlantationView$new(m)

c$read_config(file)
c$adjust_layout(2)
c$measure_trees(3)

m$layout
c$get_file_table()
v$leaflet()
v$state()
v$summary()
v$plot_chm()


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
