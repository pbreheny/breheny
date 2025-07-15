if (requireNamespace("tinytest", quietly = TRUE)) {
  tinytest::test_package("breheny", pattern = "^[^_].*\\.[rR]$")
}
