context('methods test')

example(read10xVisium)

test_that("join_features long",{

  spe %>%
    join_features(c("ENSMUSG00000051951", "ENSMUSG00000025900"), shape = "long") %>%
    dplyr::pull(.abundance_counts) %>%
    sum() %>%
    expect_equal(8)
})

test_that("join_features wide",{
  
  spe %>%
    join_features(c("ENSMUSG00000051951", "ENSMUSG00000025900"), shape = "wide") %>%
    tidySpatialExperiment::pull(ENSMUSG00000051951) %>%
    sum() %>%
    expect_equal(8)
})

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
