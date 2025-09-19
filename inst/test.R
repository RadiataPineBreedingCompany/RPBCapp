# ========================
# TEST R6 CLASS
# =======================


file = system.file("extdata", "Megaplot.laz", package="lidR")

self = Plantation$new()
self$set_cloud(file)
self$leaflet()
self$read_cloud()
self$compute_chm(2)
self$leaflet()
self$plot()
self$smooth_chm(2)

file = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/19BP01_test/19BP01_ULS_subsampled.las"

self = Plantation$new()
self$set_cloud(file)
self$leaflet()
self$read_cloud()
self$compute_chm()
self$leaflet()
self$smooth_chm(2)
self$plot()

file = "/home/jr/Documents/Entreprise/clients/RPBC/source material/shape of trial and pegs/BP19area.shp"
self$set_boundaries(file)
self$plot()

self = Plantation$new()
self$set_boundaries(file)
self$plot()

chm = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/19BP01_test/19BP01_ULS_subsampled_chm.tif"
self = Plantation$new()
self$set_chm(chm)
self$leaflet()

conf = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example_load_project/19BP01_ULS_subsampled.rpbc"
conf = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example_new_project/19BP01.rpbc"
self = Plantation$new()
self$read_config(conf)
self$get_file_table()
self$leaflet()
file = "/home/jr/Documents/Entreprise/clients/RPBC/source material/shape of trial and pegs/BP19area.shp"
self$set_boundaries(file)
self$leaflet()


# =========================
# TEST TREE LAYOUT
# ========================

file = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/19BP01/BC19BP01CKT_EstReport.xlsx"
file = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example2/BC19BP01CKT_EstReport.xlsx"

block_size <- c(18.6)
num_trees <- 6

self = RPBCapp:::RPBCLayout$new()
self$read_layout(file)
self$build_layout(block_size, num_trees, start = "bl", orientation = "v")
self$plot()
self$set_angle(107.1)
self$set_origin(1916419.46, 5738336.67)
self$plot()

self = Plantation$new()
self$set_layout(file)
self$set_layout_parameter(block_size, num_trees, start = "bl", orientation = "v")
self$layout$set_angle(107.1)
self$layout$set_origin(1916419.46, 5738336.67)
self$layout$set_crs(2193)
self$leaflet()
self$layout$plot()

file = "/home/jr/Documents/Entreprise/clients/RPBC/source material/shape of trial and pegs/BP19area.shp"
self$set_boundaries(file)
self$leaflet()


# =========================
# TEST TREE ALIGNMENT
# ========================

conf = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example1/example1.rpbc"
lay = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example1/BC19BP01CKT_EstReport.xlsx"
schm = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example1/schm.tif"
block_size <- 18.6
num_trees <- 6

self = Plantation$new()
self$read_config(conf)
self$layout$plot()
self$set_crs(2193)

self$layout$set_origin(1916419.40, 5738336.70)
M = self$layout$M
origin = as.numeric(M[1:2,3])
M = layout_alignment_lm(self$layout$tree_layout_raw, self$schm, origin, self$layout$spacing*0.75, self$boundaries)
self$layout$set_matrix(M)
self$layout$tree_layout_adjusted =NULL
self$show_layout()


# =========================
# TEST TREE SEGMENTATION
# =========================

conf = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example_load_project/19BP01_ULS_subsampled.rpbc"
lay = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example_load_project/BC19BP01CKT_EstReport.xlsx"

conf = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/BC66_1 Kinleith/BC66_1_Kinleith.rpbc"
conf = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/BC55_2_Kaingaroa/BC55_2_Kaingaroa_2023_ULS.rpbc"

block_size <- 18.6
num_trees <- 6

self = Plantation$new()
self$read_config(conf)
self$leaflet(trees = FALSE, schm = FALSE)
self$layout$plot()
self$set_crs(2193)
self$layout$set_origin(1917282.6277, 5735288.6283)
res = layout_alignment_angle(self$layout$tree_layout_oriented, self$schm, self$layout$origin, self$layout$spacing*0.75, self$boundaries)
res
angle = res[1]
tx = res[2]
ty = res[3]
origin = self$layout$origin
origin[1] = origin[1] + tx
origin[2] = origin[2] + ty
self$layout$set_angle(-angle)
self$layout$set_origin(origin[1], origin[2])
self$layout$plot()
self$leaflet()

self$adjust_layout(2)
self$leaflet(dtm = FALSE)
self$measure_trees(2.5)
self$leaflet(dtm = FALSE, layout = FALSE)

trees = self$trees
trees$TreeFound[trees$ApexFound == FALSE & is.na(trees$TreeFound)] = TRUE

# Measured
N = nrow(trees)
n = sum(trees$ApexFound)
p = round(n/N*100,1)

# Missing
N = nrow(trees)
n = sum(trees$ApexFound | trees$TreeFound)
p = round((N-n)/N*100,1)

N = nrow(trees)
n = sum(!trees$ApexFound & trees$TreeFound)
p = round(n/N*100,1)



# =========================
# TEST USE GEO LAYOUT
# ========================

conf = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/test/test.rpbc"
lay = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example_load_project/tree_measurements.gpkg"
db = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example_load_project/BC19BP01CKT_EstReport.xlsx"
self = Plantation$new()
self$read_config(conf)
self$set_layout(lay)
self$set_layout_parameter(block_size, num_trees, start = "bl", orientation = "v")
self$set_database(db)
self$layout$plot()
self$set_layout_parameter(block_size, num_trees, start = "bl", orientation = "v")
self$leaflet()
self$adjust_layout(2)
self$measure_trees(4)


flas = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/19BP01/19BP01_ULS.las"
fdb = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/19BP01/BC19BP01CKT_EstReport.xlsx"
fbnd =  "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/19BP01/BP19area.shp"

block_size <- 18.6
num_trees <- 6
self = Plantation$new()
self$set_cloud(flas)
self$crs$Name
self$set_database(fdb)
self$set_layout_parameter(block_size, num_trees, "bl", "v")
self$crs$Name
self$layout$tree_layout_oriented
self$leaflet()
self$read_cloud(0.05)
self$classify_ground()
self$compute_terrain()
self$compute_chm()
self$smooth_chm()
self$set_boundaries()
self$layout$set_origin(1916419.46, 5738336.67)
angle = layout_alignment_angle(self$layout$tree_layout_raw, self$schm, self$layout$origin, self$layout$spacing*0.75, self$boundaries)
angle = res[1]
tx = res[2]
ty = res[3]
origin = self$layout$origin
origin[1] = origin[1] + tx
origin[2] = origin[2] + ty
self$layout$set_angle(-angle)
self$layout$set_origin(origin[1], origin[2])
self$leaflet()



laz = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/BC52_1/BC52_1_Kinleith_2019_ULS.laz"
conf = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/BC52_1/BC52_1 linear file.xlsx"
self = Plantation$new()
self$set_crs(2193)
self$set_boundaries(conf)
self$set_cloud(laz)


self$read_cloud(0.5)
self$leaflet()
self$clip()
res = layout_alignment_lm(self$layout$tree_layout_raw, self$schm, self$layout$origin, self$layout$spacing*0.75, self$boundaries)
res
angle = res[1]
tx = res[2]
ty = res[3]
origin = self$layout$origin
self$leaflet()
origin[1] = origin[1] + tx
origin[2] = origin[2] + ty
self$layout$set_angle(-angle)
self$layout$set_origin(origin[1], origin[2])
self$leaflet()



conf = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/BC66_2/BC66_2_Kaingaroa.rpbc"
self = Plantation$new()
self$read_config(conf)
self$show_layout()
self$set_crs(2193)
self$set_database(conf)
block_size <- 18.6
num_trees <- 6
self$set_layout_parameter(block_size, num_trees, "bl", "v")
self$layout$plot()
self$layout$tree_layout_raw
self$layout$tree_layout_oriented

self$adjust_layout()

self$layout$tree_layout_raw
self$layout$tree_layout_oriented
self$trees
self$measure_trees(4.5)

self$trees

Ml = structure(c(2, 406, 1.5, 1.5), dim = c(2L, 2L), dimnames = list(
  NULL, c("X", "Y")))

Mg = structure(c(1670235.79958831, 1670641.95132854, 6028038.96566213,
            6028035.72226342), dim = c(2L, 2L), dimnames = list(NULL, c("X",
                                                                        "Y")))

plot(RPBCapp:::remove_virtual_trees(self$layout$tree_layout_raw)$geometry, cex = 0.5)
points(Ml)

M = layout_alignment_svd(Ml, Mg)

terra::plot(self$chm)
plot(self$boundaries,add = T)
points(Mg)

self$layout$set_matrix(M)
plot(RPBCapp:::remove_virtual_trees(self$layout$tree_layout_oriented), add = T)
