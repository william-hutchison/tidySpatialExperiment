context("dplyr test")

example(read10xVisium)

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
            select(array_row)
    )
})
  
test_that("left_join", {
    spe %>%
        left_join(spe %>%
            filter(in_tissue == TRUE) %>%
            mutate(new_column = 1)) %>%
        ncol() %>%
        expect_equal(99)
  
    spe %>%
        filter(in_tissue == TRUE) %>%
        mutate(new_column = 1) %>%
        left_join(spe) %>%
        ncol() %>%
        expect_equal(44)
})

test_that("right_join", {
  spe %>%
      right_join(spe %>%
          filter(in_tissue == TRUE) %>%
          mutate(new_column = 1)) %>%
      ncol() %>%
      expect_equal(44)

  spe %>%
      filter(in_tissue == TRUE) %>%
      mutate(new_column = 1) %>%
      right_join(spe) %>%
      ncol() %>%
      expect_equal(99)
})

test_that("inner_join", {
  spe %>%
    inner_join(spe %>%
        filter(in_tissue == TRUE) %>%
        mutate(new_column = 1)) %>%
    ncol() %>%
    expect_equal(44)
  
  spe %>%
    filter(in_tissue == TRUE) %>%
    mutate(new_column = 1) %>%
    inner_join(spe) %>%
    ncol() %>%
    expect_equal(44)
})

test_that("slice", {
    spe %>%
        slice(1) %>%
        ncol() %>%
        expect_equal(1)
})

test_that("select", {
    spe %>%
        select(.cell, array_col) %>%
        colData() %>%
        ncol() %>%
        expect_equal(2)
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
    spe %>%
        add_count(in_tissue) %>%
        pull(n) %>%
        sum() %>%
        expect_equal(4961)
})

