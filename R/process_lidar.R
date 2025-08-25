#' @export
process = function(
    file,
    rigidness = 2,
    cloth_resolution = 0.5,
    ws = 3,
    hmin = 0.5,
    res = 0.1,
    smooth = 1)
{
    fbase  = tools::file_path_sans_ext(file)
    fdtm   = paste0(fbase, "_dtm.tif")
    fchm   = paste0(fbase, "_chm.tif")
    fechm  = paste0(fbase, "_schm.tif")
    fttops = paste0(fbase, "_ttops.gpkg")
    olas   = paste0(fbase, "_classified_normalized.laz")

    ivf = lasR::classify_with_ivf(1)                                    # 1. classify noise
    csf = lasR::classify_with_csf(class_threshold = 0.05, rigidness = rigidness, cloth_resolution = cloth_resolution, filter = drop_noise())
    decgnd = lasR::sampling_pixel(0.5, filter = keep_ground())          # 3. Decimate the ground to reduce number of triangle
    tin = lasR::triangulate(max_edge = 100, filter = keep_ground())     # 4. triangulate ground point
    dtm = lasR::rasterize(0.5, tin, ofile = fdtm)                       # 5. rasterize the TIN mesh
    norm = lasR::transform_with(dtm)                                    # 6. normalize the dataset
    max = lasR::rasterize(res, "max", filter = drop_noise())            # 7. CHM. max per pixel
    chm = lasR::pit_fill(max, ofile = fchm)                             # 8. CHM enhancement (St Onge 2008)
    tmpchm = lasR::focal(chm, smooth, "mean", ofile = "")               # 9. smooth a bit the CHM
    schm = lasR::focal(tmpchm, smooth, "mean", ofile = fechm)           # 10. smooth a bit the CHM
    lmf = lasR::local_maximum_raster(schm, ws, hmin, ofile = fttops)    # 11. find trees on the enhanced CHM
    io = lasR::write_las(olas)                                          # 12. write classified LAS

    pipeline = ivf + csf + ivf + decgnd + tin + dtm + norm + max + chm + tmpchm + echm + lmf# + io# + seg

    ans = lasR::exec(pipeline, on = file, progress = TRUE)
    return(ans)
}

# file = "/home/jr/Documents/Entreprise/clients/RPBC/source material/las/19BP01_ULS_subsampled.las"
# fboundaries = "/home/jr/Documents/Entreprise/clients/RPBC/source material/shape of trial and pegs/BP19area.shp"
#
# library(sf)
# boundaries = st_read(fboundaries)
#
# ttops = ans$local_maximum
# boundaries = st_transform(boundaries, st_crs(ttops))
# res = st_contains(boundaries, ttops)
# ttops = ttops[res[[1]],]
# plot(ttops)
# file.remove(fttops)
# st_write(ttops, fttops)
