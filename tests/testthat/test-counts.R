test_that("counts are computed correctly", {

    skip_on_cran() 

    # create litter project directory
    tmp_dir <- file.path(tempdir(), "litter-test")
    dir.create(tmp_dir)
    create_litter_project(tmp_dir)

    # run litteR
    result_dir <- litter(file.path(tmp_dir, "settings.yaml"))

    # read results
    result_file <- list.files(
        path = result_dir,
        pattern = "^litteR.+csv$",
        full.names = TRUE)
    d <- readr::read_csv(result_file)
    d$mean <- round(d$mean)
    d <- d[order(d$spatial_code),]

    # check results
    # reference values have been calculated by using spread sheet
    # 'testthat-manual-beach-litter-nl-2012-2017.ods'
    expect_equal(
        d[d$type_name == "TC", "mean"][[1]],
        c(377, 347, 299, 231)
    )
    expect_equal(
        d[d$type_name == "SUP", "mean"][[1]],
        c(91, 82, 61, 44)
    )
    expect_equal(
        d[d$type_name == "FISH", "mean"][[1]],
        c(162, 143, 139, 106)
    )
    expect_equal(
        d[d$type_name == "PLASTIC", "mean"][[1]],
        c(341, 312, 265, 210)
    )

    # replace spatial codes by a single spatial code
    d <- readr::read_csv(file.path(tmp_dir, "beach-litter-nl-2012-2017.csv"))
    d$spatial_code <- "The Netherlands"
    readr::write_csv(d, file.path(tmp_dir, "beach-litter-nl-2012-2017.csv"))
    
    # run litteR
    result_dir <- litter(file.path(tmp_dir, "settings.yaml"))
    
    # read results
    result_file <- list.files(
        path = result_dir,
        pattern = "^litteR.+csv$",
        full.names = TRUE)
    d <- readr::read_csv(result_file)
    d$mean <- round(d$mean)

    # check results
    # reference values have been calculated by using spread sheet
    # 'testthat-manual-beach-litter-nl-2012-2017.ods'
    expect_equal(
        d[d$type_name == "TC", "mean"][[1]],
        314
    )
    expect_equal(
        d[d$type_name == "SUP", "mean"][[1]],
        70
    )
    expect_equal(
        d[d$type_name == "FISH", "mean"][[1]],
        138
    )
    expect_equal(
        d[d$type_name == "PLASTIC", "mean"][[1]],
        282
    )

    # make sure the same dates are used for each beach
    # this should not lead to different results
    d <- readr::read_csv(
        file.path(tmp_dir, "beach-litter-nl-2012-2017.csv"))
    d <- d[order(d$location_name),]
    dates <- d[d$location_name == "Bergen",  "date"][[1]]
    d[d$location_name == "Noordwijk",  "date"] <- dates
    d[d$location_name == "Terschelling",  "date"] <- dates[-length(dates)]
    d[d$location_name == "Veere",  "date"] <- dates
    readr::write_csv(d, file.path(tmp_dir, "beach-litter-nl-2012-2017.csv"))

    # run litteR
    result_dir <- litter(file.path(tmp_dir, "settings.yaml"))
    
    # read results
    result_file <- list.files(
        path = result_dir,
        pattern = "^litteR.+csv$",
        full.names = TRUE)
    d <- readr::read_csv(result_file)
    d$mean <- round(d$mean)
    
    # check results
    # reference values have been calculated by using spread sheet
    # 'testthat-manual-beach-litter-nl-2012-2017.ods'
    expect_equal(
        d[d$type_name == "TC", "mean"][[1]],
        314
    )
    expect_equal(
        d[d$type_name == "SUP", "mean"][[1]],
        70
    )
    expect_equal(
        d[d$type_name == "FISH", "mean"][[1]],
        138
    )
    expect_equal(
        d[d$type_name == "PLASTIC", "mean"][[1]],
        282
    )
})