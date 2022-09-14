#' Moodle Syntax String for Multichoice and Numerical Question Types
#'
#' @description This package generates a Moodle syntax string for
#'   a given list of possible answers and their corresponding rewards.
#'   It allows the user to conveniently allocate rewards for multiple
#'   correct and partially correct answers For multichoice questions, the
#'   package also randomly shuffles the answer list. For numerical questions,
#'   the package also allows the flexibility to specify different tolerance
#'   levels for each possible answer.
#'
#'   For the package to function properly, it is necessary to set the
#'   question type for both multichoice and numerical questions to vertatim
#'   type.The returned Moodle syntax string can then be directly used as the
#'   solution to a verbatim item in an Embedded Answers (Cloze) exercise
#'   with exams package or Moodle.
#'
#' @param type string; either "mchoice" or "num"
#' @param ans a list of possible answers; for "mchoice" type, each
#'   possible answer is a string; for "num" type, each possible answer is
#'   of numeric type
#' @param reward a list of corresponding rewards for the possible
#'   answers expressed in percentages (0 and 100 inclusive)
#' @param tol a tolerance level for "num" type; this argument is
#'   ignored for "mchoice" type
#' @return a Moodle syntax string
#' @export
#'
#' @examples
#' make_moostr(type="mchoice", ans=c("A two-sample t-test",
#' "A paired t-test", "A one-sample t-test"), reward=c(50, 100, 0))

make_moostr <- function(type, ans, reward, tol = 0.01) {
    if (length(ans) <= 1){
        stop(paste("make_moostr is meant for multiple answers",
                   "so length(ans) should be greater than one"))
    }
    if (max(reward) != 100){
        stop("at least one reward has to be 100")
    }
    if (!(type %in% c("mchoice", "num"))){
        stop("type can only be 'mchoice' or 'num'")
    }
    if (length(ans) != length(reward)){
        stop("lengh of ans and reward must match")
    }
    if (type == "mchoice"){
        s <- make_moostr_multichoice(ans, reward)
    } else {
        s <- make_moostr_numerical(ans, reward, tol = tol)
    }
    return(s)
}

make_moostr_multichoice <- function(ans, reward){
    idx <- sample(seq_along(ans))
    ans <- ans[idx]
    reward <- reward[idx]
    temp <- glue::glue("%{reward}%{ans}")
    multi_reward <- glue::glue_collapse(temp, sep = "~", last= "")
    s <- paste0(":MULTICHOICE:", multi_reward)
    return(s)
}

make_moostr_numerical <- function(ans, reward, tol = 0.01){
    temp <- glue::glue("%{reward}%{ans}:{tol}")
    multi_reward <- glue::glue_collapse(temp, sep = "~", last= "")
    s <- paste0(":NUMERICAL:", multi_reward)
    return(s)
}
