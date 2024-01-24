
DATA <- read.phlorest("data")

test_that("test fail on not a phlorest object -- require data attribute", {
    expect_error(get_states(rcldf::cldf("data")), "'cldfobject' must inherit from class phlorest")
})


test_that("test fail on bad word", {
    expect_error(get_states(DATA, "COFFEE"), "Invalid word")
    expect_error(get_states(DATA, "NIGHT"), "Invalid word")
})


test_that("test failure on non numeric", {
    phl <- read.phlorest("data")   # Read so we're not changing the cached DATA object
    phl$tables$ParameterTable$ID[2] <- 'COGNATE_A'
    # I don't care that we will also get a warning - the function will stop anyway
    # and that's what I want to test, so use suppressWarnings() to silence this
    # extra warning
    expect_error(
        suppressWarnings(get_states(phl, "NAME")),
        "Parameter IDs are not numeric"
    )
})


test_that("test state recode", {
    expect_equal(recode(c("1")), c("A"))
    expect_equal(recode(c("1", "1", "1")), c("A", "B", "C"))
    expect_equal(recode(c("1", "1", "0")), c("A", "B"))
    expect_equal(recode(c("1", "0", "0")), c("A"))
    expect_equal(recode(c("0", "1", "1")), c("B", "C"))
    expect_equal(recode(c("0", "1", "0")), c("B"))
    expect_equal(recode(c("0", "0", "1")), c("C"))
    expect_equal(recode(c("1", "0", "1")), c("A", "C"))

    expect_equal(recode(c("0")), c("-"))
    expect_equal(recode(c("?")), c("-"))
    expect_equal(recode(c("-")), c("-"))
    expect_equal(recode(c()), c("-"))

    # change alphabet
    expect_equal(recode(c("1", "1", "1"), alphabet=c("1", "2", "3")), c("1", "2", "3"))

    # change missing
    expect_equal(recode(c(), missing_state="?"), c("?"))

})


# 1 100_name_a,
# 2 100_name_b,
# 3 100_name_c,
# 4 100_name_d,
# Khasi       1010
# Khmu        1111
# Lyngngam    0000
# Palaung     0000
# PnarJaintia 0100
# WarLamin    0010
test_that("test get_states NAME", {
    states <- get_states(DATA, "NAME")
    expected <- list(
        'Khasi'       = c("A", "C"),
        'Khmu'        = c("A", "B", "C", "D"),
        'Lyngngam'    = c("-"),
        'Palaung'     = c("-"),
        'PnarJaintia' = c("B"),
        'WarLamin'    = c("C")
    )
    expect_equal(states, expected)
})


# 5 101_narrow_a,
# 6 101_narrow_b,
#
# Khasi        10
# Khmu         01
# Lyngngam     00
# Palaung      00
# PnarJaintia  10
# WarLamin     00
test_that("test get_states NARROW", {
    states <- get_states(DATA, "NARROW")
    expected <- list(
        'Khasi'       = c("A"),
        'Khmu'        = c("B"),
        'Lyngngam'    = c("-"),
        'Palaung'     = c("-"),
        'PnarJaintia' = c("A"),
        'WarLamin'    = c("-")
    )
    expect_equal(states, expected)
})


# 7 102_near_a,
# 8 102_near_b,
# 9 102_near_c,
#
# Khasi       100
# Khmu        001
# Lyngngam    100
# Palaung     010
# PnarJaintia 100
# WarLamin    100
test_that("test get_states NEAR", {
    states <- get_states(DATA, "NEAR")
    expected <- list(
        'Khasi'       = c("A"),
        'Khmu'        = c("C"),
        'Lyngngam'    = c("A"),
        'Palaung'     = c("B"),
        'PnarJaintia' = c("A"),
        'WarLamin'    = c("A")
    )
    expect_equal(states, expected)
})


# 10 103_neck_a,
# 11 103_neck_b,
# 12 103_neck_c,
# 13 103_neck_d,
#
# Khasi       1000
# Khmu        0001
# Lyngngam    0100
# Palaung     0010
# PnarJaintia 1000
# WarLamin    1000
test_that("test get_states NECK", {
    states <- get_states(DATA, "NECK")
    expected <- list(
        'Khasi'       = c("A"),
        'Khmu'        = c("D"),
        'Lyngngam'    = c("B"),
        'Palaung'     = c("C"),
        'PnarJaintia' = c("A"),
        'WarLamin'    = c("A")
    )
    expect_equal(states, expected)
})

# 14 104_new_a,
#
# Khasi       1
# Khmu        1
# Lyngngam    1
# Palaung     1
# PnarJaintia 1
# WarLamin    1
test_that("test get_states NEW", {
    states <- get_states(DATA, "NEW")
    expected <- list(
        'Khasi'       = c("A"),
        'Khmu'        = c("A"),
        'Lyngngam'    = c("A"),
        'Palaung'     = c("A"),
        'PnarJaintia' = c("A"),
        'WarLamin'    = c("A")
    )
    expect_equal(states, expected)
})

# 18 106_nose_a,
# 19 106_nose_b,
#
# Khasi       -0
# Khmu        -0
# Lyngngam    -0
# Palaung     -0
# PnarJaintia -0
# WarLamin    0-
test_that("test get_states NOSE", {
    states <- get_states(DATA, "NOSE")
    expected <- list(
        'Khasi'       = c("-"),
        'Khmu'        = c("-"),
        'Lyngngam'    = c("-"),
        'Palaung'     = c("-"),
        'PnarJaintia' = c("-"),
        'WarLamin'    = c("-")
    )
    expect_equal(states, expected)
})




