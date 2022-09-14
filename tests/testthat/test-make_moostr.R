test_that("test output of make_moostr", {
    set.seed(2022)
    expect_identical(make_moostr(type="mchoice",
                                 ans=c("a", "b", "c"),
                                 reward=c(50, 100, 0)),
                     ":MULTICHOICE:%0%c~%100%b~%50%a")
    set.seed(2022)
    expect_identical(make_moostr(type="num",
                                 ans=c(2, 3, 4.5),
                                 reward=c(0, 100, 50)),
                     ":NUMERICAL:%0%2:0.01~%100%3:0.01~%50%4.5:0.01")
    set.seed(2022)
    expect_error(make_moostr(type="num",
                             ans= c(1),
                             reward= c(100)),
                 paste("make_moostr is meant for multiple answers",
                       "so length\\(ans\\) should be greater than one"))
    set.seed(2022)
    expect_error(make_moostr(type="num",
                             ans= c(2, 3, 4.5),
                             reward= c(0, 200, 50)),
                 "at least one reward has to be 100")
    set.seed(2022)
    expect_error(make_moostr(type="shortanswer",
                 ans=c(2, 3, 4.5),
                 reward=c(0, 100, 50)),
                 "type can only be 'mchoice' or 'num'")
    set.seed(2022)
    expect_error(make_moostr(type="num",
                             ans=c(2, 3, 4.5),
                             reward=c(0, 100, 50, 30)),
                 "lengh of ans and reward must match")
})
