library(RPBCapp)

# Read a conf file
# ===========================

file = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/Examples/example3-nonstandard/example3.rpbc"
db = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/BC52_1/BC52_1 linear file.xlsx"
pb = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/BC52_1/BC52_1_Kinleith_2019_ULS_subsampled.laz"
shp = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/Examples/example3-nonstandard/plan.gpkg"


file = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/BC66_1/BC66_1_Kinleith_2023.rpbc"
db = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/BC66_1/BC66_1_DATAFILE.xlsx"

file = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/BC66_2/BC66_2_Kaingaroa.rpbc"

edited = structure(list(`_leaflet_id` = 9736, layerId = 2283, radius = 1,
               geometry = structure(list(structure(c(1917038.9224887, 5737645.90468869
               ), class = c("XY", "POINT", "sfg"))), n_empty = 0L, crs = structure(list(
                 input = "EPSG:2193", wkt = "PROJCRS[\"NZGD2000 / New Zealand Transverse Mercator 2000\",\n    BASEGEOGCRS[\"NZGD2000\",\n        DATUM[\"New Zealand Geodetic Datum 2000\",\n            ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4167]],\n    CONVERSION[\"New Zealand Transverse Mercator 2000\",\n        METHOD[\"Transverse Mercator\",\n            ID[\"EPSG\",9807]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",173,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"Scale factor at natural origin\",0.9996,\n            SCALEUNIT[\"unity\",1],\n            ID[\"EPSG\",8805]],\n        PARAMETER[\"False easting\",1600000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",10000000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"northing (N)\",north,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"easting (E)\",east,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Engineering survey, topographic mapping.\"],\n        AREA[\"New Zealand - North Island, South Island, Stewart Island - onshore.\"],\n        BBOX[-47.33,166.37,-34.1,178.63]],\n    ID[\"EPSG\",2193]]"), class = "crs"), class = c("sfc_POINT",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           "sfc"), precision = 0, bbox = structure(c(xmin = 1917038.9224887,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ymin = 5737645.90468869, xmax = 1917038.9224887, ymax = 5737645.90468869
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ), class = "bbox"))), row.names = 1L, sf_column = "geometry", class = c("sf",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "data.frame"))
m <- PlantationModel$new()
c <- PlantationController$new(m)
v <- PlantationView$new(m)

c$read_config(file)
m$layout_warnings <-NULL
plot(m$layout$tree_layout_oriented$geometry)
v$leaflet( dtm = TRUE, chm = FALSE, schm = TRUE, bound = FALSE, bbox = FALSE, trees = FALSE, crowns = FALSE, layout = TRUE)
c$replace_trees(edited)
plot(m$layout$tree_layout_oriented$geometry)
v$leaflet( dtm = TRUE, chm = FALSE, schm = TRUE, bound = FALSE, bbox = FALSE, trees = FALSE, crowns = FALSE, layout = TRUE)

c$set_database(db)
c$set_layout(shp)

c$smooth_chm()
terra::plot(m$dtm)
terra::plot(m$chm)
terra::plot(m$schm)
v$plot_layout()
v$ggstats()
v$leaflet( dtm = TRUE, chm = FALSE, schm = TRUE, bound = FALSE,
           bbox = FALSE, trees = FALSE, crowns = FALSE, layout = TRUE)

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






file = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/Tutorial/BC52_3/BC53_3.rpbc"

m <- PlantationModel$new()
c <- PlantationController$new(m)
v <- PlantationView$new(m)

c$read_config(file)
c$optim_layout()
c$adjust_layout()
v$leaflet()
