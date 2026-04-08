source("final.R")
set.seed(123)

test_that("la boucle a au moins min_segments", {
  n <- 5
  res <- generer_boucle_slitherlink(n, min_segments = 8)
  
  expect_true(length(res$segments) >= 8)
})


test_that("la boucle est fermée", {
  res <- generer_boucle_slitherlink(5)
  
  expect_true(length(res$segments) > 0)
  
  premier <- res$points_visites[[1]]
  dernier_segment <- res$segments[[length(res$segments)]]
  
  expect_equal(dernier_segment[3:4], premier)
})

test_that("les points restent dans la grille", {
  n <- 5
  res <- generer_boucle_slitherlink(n)
  
  for (p in res$points_visites) {
    expect_true(p[1] >= 1 && p[1] <= n+1)
    expect_true(p[2] >= 1 && p[2] <= n+1)
  }
})