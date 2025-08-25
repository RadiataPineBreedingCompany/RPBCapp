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

conf = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/19BP01_test/19BP01_ULS_subsampled.rpbc"
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

file = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/19BP01_test/BC19BP01CKT_EstReport.xlsx"

block_size <- 18.6
num_trees <- 6

self = RPBCLayout$new()
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

conf = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/19BP01_test/19BP01_ULS_subsampled.rpbc"
lay = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/19BP01_test/BC19BP01CKT_EstReport.xlsx"

block_size <- 18.6
num_trees <- 6

self = Plantation$new()
self$read_config(conf)
self$layout$plot()
self$set_layout(lay)
self$set_layout_parameter(block_size, 8, start = "bl", orientation = "h")
self$layout$plot()
self$set_layout_parameter(block_size, num_trees, start = "bl", orientation = "v")
self$layout$plot()
self$set_crs(2193)
self$layout$set_origin(1916419.46, 5738336.67)
self$layout$set_origin(1916419.40, 5738336.70)
self$leaflet()

angle = layout_alignment_angle(self$layout$tree_layout, self$schm, self$layout$origin, self$layout$spacing*0.75)
angle
self$layout$set_angle(-angle)
self$leaflet()


# =========================
# TEST TREE SEGMENTATION
# =========================

conf = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/19BP01_test/19BP01_ULS_subsampled.rpbc"
lay = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/19BP01_test/BC19BP01CKT_EstReport.xlsx"

block_size <- 18.6
num_trees <- 6

self = Plantation$new()
self$read_config(conf)
self$layout$plot()
self$set_crs(2193)
self$layout$set_origin(1916419.46, 5738336.67)
angle = layout_alignment_angle(self$layout$tree_layout, self$schm, self$layout$origin, self$layout$spacing*0.75)
angle
self$layout$set_angle(-angle)
self$layout$plot()
self$leaflet()

self$adjust_layout(2)
self$leaflet(dtm = FALSE)
self$measure_trees(6)
self$leaflet(dtm = FALSE, layout = FALSE)

trees = self$trees

database = readxl::read_excel(lay, sheet = "Sorted Linear File")
names(database)[names(database) == "Pset(Block)"] <- "Block"

spatial_db = merge(database, trees, by = c("Tpos", "Block", "Prow", "Pcol"), all.x = TRUE)

library(ggplot2)

ntrees = nrow(spatial_db)
not_found = sum(is.na(spatial_db$Height))
pnotfound = not_found/ntrees*100

# Height vs Block
ggplot(spatial_db, aes(x = as.factor(Block), y = Height, fill = as.factor(Block))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  theme_bw() +
  guides(fill = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Height Distribution by Block",
    x = "Block",
    y = "Height",
    fill = "Block"
  )

# Height vs CloneCode
ggplot(spatial_db, aes(x = as.factor(CloneCode), y = Height, fill = as.factor(CloneCode), col = as.factor(CloneCode))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  theme_bw() +
  guides(fill = "none", col = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Height Distribution by Clone Code",
    x = "Clone Code",
    y = "Height",
    fill = "Clone Code"
  )

# Height vs FamilyCode
ggplot(spatial_db, aes(x = as.factor(FamilyCode), y = Height, fill = as.factor(FamilyCode))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  theme_bw() +
  guides(fill = "none")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Height Distribution by Family Code",
    x = "Family Code",
    y = "Height",
    fill = "Family Code"
  )
