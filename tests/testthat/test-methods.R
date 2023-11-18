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
    pull(ENSMUSG00000051951) %>%
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

test_that("rectangle", {

    spe_filtered <- spe %>%
                    filter(sample_id == "section1") %>%
                    mutate(in_rectangle = rectangle(array_col, array_row, center = c(50, 50), height = 100, width = 20))

    expect_equal(sum(spe_filtered$in_rectangle, na.rm = TRUE), 8)
})