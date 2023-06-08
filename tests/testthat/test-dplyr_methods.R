context("dplyr test")

library(magrittr)

test_that("arrange", {
    spe_arranged <-
        spe %>%
        arrange(array_row) %>% 
        colData()
      
    spe_arranged[1, 2] %>% 
        expect_equal(0)
    
    spe_arranged[99, 2] %>% 
        expect_equal(73)
})

test_that("bind_rows", {
    spe %>%
        bind_rows(spe) %>%
        ncol() %>%
        expect_equal(198)
})

test_that("bind_cols", {
    spe %>%
        bind_cols(
            spe %>%
                select(array_row)
            ) %>%
        select(array_row...5) %>%
        nrow() %>%
        expect_equal(99)
})

test_that("distinct", {
    spe %>%
        distinct(.cell, in_tissue) %>%
        ncol() %>%
        expect_equal(2)
  
    spe %>%
      distinct(.cell, in_tissue) %>%
      nrow() %>%
      expect_equal(50)
})

test_that("filter", {
    spe %>%
        filter(in_tissue ==  TRUE) %>%
        ncol() %>%
        expect_equal(44)
})

test_that("group_by", {
    spe %>%
        group_by(in_tissue) %>%
        nrow() %>%
        expect_equal(99)
})

test_that("summarise", {
    spe %>%
        summarise(mean(array_row)) %>%
        nrow() %>%
        expect_equal(1)
})

test_that("mutate", {
    spe %>%
        mutate(array_row = 1) %>%
        distinct(array_row) %>%
        nrow() %>%
        expect_equal(1)
})

test_that("rename", {
    spe %>%
        rename(array_row_new = array_row) %>%
        select(array_row_new) %>%
        ncol() %>%
        expect_equal(1)
  
    expect_error(
        spe %>%
            rename(array_row_new = array_row) %>%
            select(array_row_new)
    )
})
  
test_that("left_join", {
    spe %>%
        left_join(spe %>%
              mutate(new_column = 1:2)) %>%
        ncol() %>%
        expect_equal(8)
})

test_that("inner_join", {
    pbmc_small %>%
        inner_join(pbmc_small %>%
                       distinct(groups) %>%
                       mutate(new_column = 1:2) %>%
                       slice(1)) %>%
        ncol() %>%
        expect_equal(36)
})

test_that("right_join", {
    pbmc_small %>%
        right_join(pbmc_small %>%
                       distinct(groups) %>%
                       mutate(new_column = 1:2) %>%
                       slice(1)) %>%
        ncol() %>%
        expect_equal(36)
})

test_that("full_join", {
    pbmc_small %>%
        full_join(tibble::tibble(groups = "g1", other = 1:4)) %>%
        nrow() %>%
        expect_equal(212)
})

test_that("slice", {
    spe %>%
        slice(1) %>%
        ncol() %>%
        expect_equal(1)
})

test_that("select", {
    pbmc_small %>%
        select(cell, orig.ident) %>%
        class() %>%
        as.character() %>%
        expect_equal("SpatialExperiment")

    pbmc_small %>%
        select(orig.ident) %>%
        class() %>%
        as.character() %>%
        .[1] %>%
        expect_equal("tbl_df")
})

test_that("sample_n", {
    set.seed(1)
  
    spe %>%
        sample_n(1) %>%
        ncol() %>%
        expect_equal(1)
    
    spe %>%
        sample_n(200, replace = TRUE) %>%
        nrow() %>%
        expect_equal(200)
})

test_that("sample_frac", {
    set.seed(1)
    
    spe %>%
        sample_frac(0.01) %>%
        ncol() %>%
        expect_equal(1)
    
    spe %>%
        sample_frac(2, replace = TRUE) %>%
        nrow() %>%
        expect_equal(392)
})

test_that("count", {
    spe %>%
        count(array_row) %>%
        nrow() %>%
        expect_equal(36)
})

test_that("add count", {
  pbmc_small %>%
    add_count(groups) %>%
    nrow() %>%
    expect_equal(230)
})

