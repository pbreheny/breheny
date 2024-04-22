if (interactive()) library(tinytest)

0.12345 |> .f(2) |> expect_equal('0.12')
0.10000 |> .f(2) |> expect_equal('0.10')
123456.789 |> .f(1) |> expect_equal('123,456.8')
123456.700 |> .f(2) |> expect_equal('123,456.70')

0.12345 |> .p(1) |> expect_equal('12.3')
0.12000 |> .p(2) |> expect_equal('12.00')
