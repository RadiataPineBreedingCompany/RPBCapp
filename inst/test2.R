library(RPBCapp)

# Read a conf file
# ===========================

file = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example1-base/example1.rpbc"
db = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/BC52_1/BC52_1 linear file.xlsx"
pb = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/BC52_1/BC52_1_Kinleith_2019_ULS_subsampled.laz"

m <- PlantationModel$new()
c <- PlantationController$new(m)
v <- PlantationView$new(m)

c$read_config(file)
c$process_pointcloud(0.3)
c$set_layout_parameter(c(18.6,18.6), c(6,4), start = "bl", orientation = "v")

c$smooth_chm()
terra::plot(m$dtm)
terra::plot(m$chm)
terra::plot(m$schm)
v$plot_layout()
v$ggstats()
v$leaflet( dtm = TRUE, chm = FALSE, schm = TRUE, bound = FALSE,
           bbox = FALSE, trees = FALSE, crowns = FALSE, layout = FALSE)

map = make_base_map()
add_schm_layer(map, m$schm)
map = u$map
u = add_dtm_layer(map, m$dtm)
u
center_on_object(map, c$model$bbox)

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
