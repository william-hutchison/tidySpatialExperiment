context("tidyr test")

example(read10xVisium)

test_that("nest", {
    spe %>%
        nest(data = -sample_id) %>%
        nrow() %>%
        expect_equal(2)
})

test_that("unnest", {
    spe %>%
        nest(data = -sample_id) %>%
        unnest(data) %>%
        ncol() %>%
        expect_equal(99)
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
    spe %>%
        pivot_longer(c(array_row, array_col), names_to = "dimension", values_to = "location") %>%
        class() %>%
        .[1] %>%
        expect_equal("tbl_df")
})
