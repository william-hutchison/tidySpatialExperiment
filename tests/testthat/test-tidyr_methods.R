context("tidyr test")

tt <-   pbmc_small %>%   mutate(col2 = "other_col")

test_that("nest_unnest", {
    col_names <- tt %>% colData %>% colnames() %>% c("cell")

    x <- tt %>%
        nest(data = -groups) %>%
        unnest(data) %>%
        scater::logNormCounts() %>%
        scater::runPCA()
    y <- tt %>%
        scater::logNormCounts() %>%
        scater::runPCA()

    expect_equal(
        reducedDims(x)$PCA %>%
            as.data.frame() %>%
            as_tibble(rownames = "cell") %>%
            arrange(cell) %>%
            pull(PC1) %>%
            abs(),
        reducedDims(x)$PCA %>%
            as.data.frame() %>%
            as_tibble(rownames = "cell") %>%
            arrange(cell) %>%
            pull(PC1) %>%
            abs()
    )
})

test_that("unite", {
    spe %>%
        unite("A", array_row:array_col) %>%
        colData() %>%
        ncol() %>%
        expect_equal(5)
})

test_that("extract", {
    spe %>% 
        extract(col = array_row, into = "A", regex = "([[:digit:]]3)") %>%
        pull("A") %>%
        as.numeric() %>%
        sum(na.rm = TRUE) %>%
        expect_equal(404)
})

test_that("separate", {
    spe %>%
        separate(col = sample_id, into = c("A", "B"), sep = "[[:alnum:]]n") %>%
        colData() %>%
        ncol() %>%
        expect_equal(8)
})

test_that("pivot_longer", {
    tt %>%
        pivot_longer(c(orig.ident, groups),
                     names_to = "name",
                     values_to = "value") %>%
        class() %>%
        .[1] %>%
        expect_equal("tbl_df")
})
