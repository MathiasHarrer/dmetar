#' Print, summary and plot methods for objects created using 'dmetar' functions
#'
#' Print, summary and plot S3 methods for objects of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, and \code{sucra}.
#'
#' @param x An object of class \code{find.outliers}.
#' @param ... Additional arguments.
#'
#' @details
#' A total of four package-specific S3 methods are provided in \code{dmetar}: S3 methods for \code{print},
#' \code{summary}, \code{plot} and \code{forest}. Outputs from \code{print} and \code{summary} are always identical.
#' When both \code{plot} and \code{forest} are available for a class, outputs of both functions are identical (i.e., a forest
#' plot is returned). The \code{forest}/\code{plot} function allows additional arguments of the \code{meta.forest} or
#' \code{metafor}'s code{\link[metafor]{forest}} function (depending on the class of the meta-analysis object on which prior
#' calculations are based on). These can be used for further styling of the forest plot.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#'
#' @importFrom stats as.formula hat influence ks.test optimize pbinom pchisq pf pnorm pt punif qchisq qf qnorm qt reformulate reorder setNames uniroot
#'
#' @export
#' @method print find.outliers


print.find.outliers = function(x, ...){


  # Meta
  if (class(x$m.fixed)[1] %in% c("metagen", "metapropr",
                                 "metacor", "metainc", "metacont",
                                 "metaprop", "metabin", "metabin")){

    if (class(x)[2] == "ftrf"){

      cat("Identified outliers (fixed-effect model)", "\n")
      cat("----------------------------------------", "\n")
      cat(paste(shQuote(x$out.study.fixed, type="cmd"), collapse=", "), "\n", "\n")
      cat("Results with outliers removed", "\n")
      cat("-----------------------------", "\n")
      print(x$m.fixed)

    }

    if (class(x)[2] == "ffrt"){

      cat("Identified outliers (random-effects model)", "\n")
      cat("------------------------------------------", "\n")
      cat(paste(shQuote(x$out.study.random, type="cmd"), collapse=", "), "\n", "\n")
      cat("Results with outliers removed", "\n")
      cat("-----------------------------", "\n")
      print(x$m.random)

    }

    if (class(x)[2] == "ftrt"){

      cat("Identified outliers (fixed-effect model)", "\n")
      cat("----------------------------------------", "\n")
      cat(paste(shQuote(x$out.study.fixed, type="cmd"), collapse=", "), "\n", "\n")
      cat("Results with outliers removed", "\n")
      cat("-----------------------------", "\n")
      print(x$m.fixed)

      cat("\n")

      cat("Identified outliers (random-effects model)", "\n")
      cat("------------------------------------------", "\n")
      cat(paste(shQuote(x$out.study.random, type="cmd"), collapse=", "), "\n", "\n")
      cat("Results with outliers removed", "\n")
      cat("-----------------------------", "\n")
      print(x$m.random)

    }

    if (class(x)[2] == "null.ftrf"){

      cat("No outliers detected (fixed-effect model).")

    }

    if (class(x)[2] == "null.ffrt"){

      cat("No outliers detected (random-effects model).")


    }

    if (class(x)[2] == "null.ftrt"){

      cat("No outliers detected (fixed-effect/random-effects model).")

    }

  } else {

    if (class(x)[2] == "mf.null"){

      cat(paste0("No outliers detected (", class(x)[3],")."))
    }

    # Metafor
    cat(paste0("Identified outliers (", class(x)[3],")"), "\n")
    cat("-------------------------", "\n")
    cat(paste(shQuote(x$out.study, type="cmd"), collapse=", "), "\n", "\n")
    cat("Results with outliers removed", "\n")
    cat("-----------------------------", "\n")
    print(x$m)

  }

}
