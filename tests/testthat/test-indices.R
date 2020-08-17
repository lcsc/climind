
context("ClimInd Indices")

dataTest <- function(){
  data.names <- seq(chron("1/1/1990", out.format=c(dates = "m/d/y", times = "h:m:s")), chron("12/31/2010", out.format=c(dates = "m/d/y", times = "h:m:s")))
  data <- array(0, dim=c(length(data.names)))
  names(data) <- data.names
  return(data)
}
data <- dataTest()
# source(file.path("R", "indecis_indices.R"))
# data <- data[1:365]

test_that("gsl_calculate_16", {
  # Annual count of days between the first span of at least 6 days with Tmean >5ºC and first span after 1 July of 6 days with Tmean <5 ºC.
  data[1:length(data)] <- 0
  data[as.character(seq(chron("01/20/90"), chron("01/27/90")))] <- 10
  data[as.character(seq(chron("07/01/90"), chron("12/31/90")))] <- 10
  data[as.character(seq(chron("10/20/90"), chron("10/29/90")))] <- -1
  # length(seq(chron("01/01/90"), chron("01/20/90")))
  # length(seq(chron("01/01/90"), chron("10/19/90")))
  return.length <- length(seq(chron("01/20/90"), chron("10/19/90")))
  value <- calculate_16(data)
  expect_equivalent(value["1990"], return.length)

  data[1:length(data)] <- 0
  data[as.character(seq(chron("01/20/90"), chron("01/27/90")))] <- 10
  # length(seq(chron("01/01/90"), chron("01/20/90")))
  # length(seq(chron("01/01/90"), chron("07/01/90")))
  return.length3 <- length(seq(chron("01/20/90"), chron("07/01/90")))
  value <- calculate_16(data)
  expect_equivalent(value["1990"], return.length3)

  data[1:length(data)] <- 0
  data[as.character(seq(chron("01/20/90"), chron("12/31/90")))] <- 10
  return.length2 <- length(seq(chron("01/20/90"), chron("12/31/90")))
  value <- calculate_16(data)
  expect_equivalent(value["1990"], return.length2)

  data[1:length(data)] <- 10
  value <- calculate_16(data)
  expect_equivalent(value["1990"], 365)

  data[1:length(data)] <- 0
  value <- calculate_16(data)
  expect_success(expect_output(value["1990"], NA))  
})

test_that("cfd_calculate_18", {
  # Maximum number of consecutive frost days.
  data[1:length(data)] <- 0
  data[as.character(seq(chron("01/20/00"), chron("01/27/00")))] <- -1:-8
  data[as.character(seq(chron("03/20/00"), chron("03/26/00")))] <- -1:-7
  expect_equivalent(as.numeric(calculate_18(data)["2000"]), 8)  
})

test_that("csd_calculate_21", {
  # Maximum number of consecutive summer days (TX > 25º).
  data[1:length(data)] <- 0
  data[as.character(seq(chron("01/20/00"), chron("01/27/00")))] <- 28
  data[as.character(seq(chron("03/20/00"), chron("03/26/00")))] <- 28
  expect_equivalent(as.numeric(calculate_21(data)["2000"]), 8)  
})

test_that("wsdi_calculate_29", {
  # Count of days with at least 6 consecutive days when TX > 90th percentile.
  data[1:length(data)] <- 0
  data[as.character(seq(chron("01/20/00"), chron("01/27/00")))] <- 28
  data[as.character(seq(chron("03/20/00"), chron("03/26/00")))] <- 28
  data[as.character(seq(chron("05/20/00"), chron("05/24/00")))] <- 26
  expect_equivalent(as.numeric(calculate_29(data)["2000"]), sum(data==28))
})

test_that("ogs10_calculate_32", {
  data[1:length(data)] <- 0
  data[as.character(seq(chron("01/15/00"), chron("01/27/00")))] <- 28
  data[as.character(seq(chron("03/20/00"), chron("03/26/00")))] <- 28
  data[as.character(seq(chron("05/20/00"), chron("05/24/00")))] <- 26
  expect_equivalent(as.numeric(calculate_32(data)["2000"]), 15)
})

test_that("rx5d_calculate_50", {
  # Maximum consecutive 5-day precipitation
  data[1:length(data)] <- 0
  data[as.character(seq(chron("01/20/00"), chron("01/27/00")))] <- 2
  data[as.character(seq(chron("03/20/00"), chron("03/26/00")))] <- 3
  data[as.character(seq(chron("05/20/00"), chron("05/24/00")))] <- 1
  expect_equivalent(as.numeric(calculate_50(data)["2000"]), 3*5)
})

test_that("bio5_calculate_77", {
  data[1:length(data)] <- 0
  data[as.character(seq(chron("01/01/00"), chron("01/31/00")))] <- 2
  data[as.character(seq(chron("03/01/00"), chron("03/31/00")))] <- 3
  data[as.character(seq(chron("05/20/01"), chron("05/24/01")))] <- 20
  expect_equivalent(as.numeric(calculate_77(data, data)["2000"]), 3)
})

test_that("bio9_calculate_81", {
  pr = taverage = data
  pr[1:length(data)] <- 10
  taverage[1:length(taverage)] <- 1
  pr[as.character(seq(chron("05/01/00"), chron("08/31/00")))] <- 2
  taverage[as.character(seq(chron("05/01/00"), chron("08/31/00")))] <- 4
  expect_equivalent(as.numeric(calculate_81(pr, taverage)["2000"]), 4)
})

test_that("fpsc_calculate_111", {
  data[1:length(data)] <- 0
  data[as.character(seq(chron("01/02/90"), chron("01/12/90")))] <- 2
  data[as.character(seq(chron("03/01/90"), chron("03/31/90")))] <- 3

  data[as.character(seq(chron("01/02/00"), chron("01/12/00")))] <- 2 #length(seq(chron("01/02/00"), chron("01/12/00")))
  data[as.character(seq(chron("03/01/00"), chron("03/31/00")))] <- 3 #length(seq(chron("03/01/00"), chron("03/31/00")))
  # length(seq(chron("10/01/99"), chron("03/01/00"))) #year: 10,11,12 1999 and 1,2,3,4,5,6,7,8,9 2000 

  data[as.character(seq(chron("05/20/01"), chron("05/24/01")))] <- 20

  expect_equivalent(as.numeric(calculate_111(data)["2000"]), length(seq(chron("10/01/99"), chron("03/01/00"))))
})

test_that("lpsc_calculate_112", {
  data[1:length(data)] <- 0
  data[as.character(seq(chron("01/02/90"), chron("01/12/90")))] <- 2
  data[as.character(seq(chron("03/01/90"), chron("03/31/90")))] <- 3

  data[as.character(seq(chron("01/02/00"), chron("01/12/00")))] <- 2 #length(seq(chron("01/02/00"), chron("01/12/00")))
  data[as.character(seq(chron("03/01/00"), chron("03/31/00")))] <- 3 #length(seq(chron("03/01/00"), chron("03/31/00")))
  # length(seq(chron("10/01/99"), chron("03/31/00"))) #year: 10,11,12 1999 and 1,2,3,4,5,6,7,8,9 2000 

  data[as.character(seq(chron("05/20/01"), chron("05/24/01")))] <- 20
  expect_equivalent(as.numeric(calculate_112(data)["2000"]), length(seq(chron("10/01/99"), chron("03/31/00"))))
})


