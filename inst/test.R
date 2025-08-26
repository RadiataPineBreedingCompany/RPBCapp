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

conf = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example_load_project/19BP01_ULS_subsampled.rpbc"
lay = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example_load_project/BC19BP01CKT_EstReport.xlsx"

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

conf = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example_load_project/19BP01_ULS_subsampled.rpbc"
lay = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/example_load_project/BC19BP01CKT_EstReport.xlsx"

block_size <- 18.6
num_trees <- 6

self = Plantation$new()
self$read_config(conf)
self$layout$plot()
self$set_crs(2193)
self$layout$set_origin(1916419.46, 5738336.67)
res = layout_alignment_angle(self$layout$tree_layout, self$schm, self$layout$origin, self$layout$spacing*0.75, self$boundaries)
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
self$measure_trees(6)
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

database = readxl::read_excel(self$fdatabase, sheet = "Sorted Linear File")

spatial_db = database
names(spatial_db)[names(spatial_db) == "Pset(Block)"] <- "Block"
spatial_db = merge(database, trees, by = c("Tpos", "Block", "Prow", "Pcol"), all.x = TRUE)

library(ggplot2)

# Height vs Block
p1 = ggplot(spatial_db, aes(x = as.factor(Block), y = Height, fill = as.factor(Block))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  theme_bw() +
  guides(fill = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    breaks = levels(as.factor(spatial_db$Block))[seq(1, length(unique(spatial_db$Block)), 5)]
  ) +
  labs(
    title = "Height Distribution by Block",
    x = "Block",
    y = "Height (m)",
    fill = "Block"
  )

# Height vs CloneCode
p2 = ggplot(spatial_db, aes(x = as.factor(CloneCode), y = Height, fill = as.factor(CloneCode), col = as.factor(CloneCode))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  theme_bw() +
  guides(fill = "none", col = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Height Distribution by Clone Code",
    x = "Clone Code",
    y = "Height (m)",
    fill = "Clone Code"
  )

# Height vs FamilyCode
p3 = ggplot(spatial_db, aes(x = as.factor(FamilyCode), y = Height, fill = as.factor(FamilyCode))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  theme_bw() +
  guides(fill = "none")+
  scale_x_discrete(
    breaks = levels(as.factor(spatial_db$FamilyCode))[seq(1, length(unique(spatial_db$FamilyCode)), 5)]
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Height Distribution by Family Code",
    x = "Family Code",
    y = "Height (m)",
    fill = "Family Code"
  )
