context('methods test')

example(read10xVisium)

# test_that("join_features",{
# 
#   pbmc_small %>%
#     join_features("CD3D") %>%
#     slice(1) %>%
#     tidySpatialExperiment::pull(.abundance_counts) %>%
#     expect_equal(4, tolerance=0.1)
# })

test_that("aggregate_cells", {
  
    spe_aggregated <- 
        spe %>%
        aggregate_cells(sample_id, assays = "counts")
    
    spe_aggregated %>%
        ncol() %>%
        expect_equal(2)

    spe_aggregated@assays@data@listData$counts %>%
        sum() %>%
        expect_equal(1355)
})
