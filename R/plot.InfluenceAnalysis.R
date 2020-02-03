#' Print, summary and plot methods for objects created using 'dmetar' functions
#'
#' Print, summary and plot S3 methods for objects of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, and \code{sucra}.
#'
#'
#' @param x An object of class \code{influence.analysis}.
#' @param which Which plot(s) should be generated? Can be either \code{"all"} (default), \code{"baujat"}, \code{"influence"},
#' \code{"ES"} (forest plot sorted by effect size) or \code{"I2"} (forest plot sorted by heterogeneity).
#' @param ... Additional arguments.
#'
#' @details
#' A total of four package-specific S3 methods are provided in \code{dmetar}: S3 methods for \code{print},
#' \code{summary}, \code{plot} and \code{forest}. Outputs from \code{print} and \code{summary} are always identical.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#'
#' @importFrom grid grid.newpage grid.draw
#' @importFrom gridExtra grid.arrange
#' @importFrom forcats fct_reorder
#' @importFrom stats as.formula hat influence ks.test optimize pbinom pchisq pf pnorm pt punif qchisq qf qnorm qt reformulate reorder setNames uniroot
#'
#' @export
#' @method plot InfluenceAnalysis



plot.InfluenceAnalysis = function(x, which = "all", ...){

  if (which == "all"){

    if (class(x)[2] == "rsp"){

      # Baujat plot
      plot(x$BaujatPlot)

      # Influence Characteristics
      grid.newpage()
      grid.draw(x$InfluenceCharacteristics)

      # Forest (ES)
      suppressWarnings(suppressMessages(plot(x$ForestEffectSize)))

      # Forest (I2)
      suppressWarnings(suppressMessages(plot(x$ForestI2)))

    }

    if (class(x)[2] == "rsp.null"){

      title = textGrob("Influence Diagnostics", gp = gpar(fontface = "bold"))
      suppressWarnings(
        suppressMessages(
          gridExtra::grid.arrange(x$BaujatPlot,
                                  x$InfluenceCharacteristics,
                                  x$ForestEffectSize,
                                  x$ForestI2,
                                  ncol = 2) ))

    }

  }

  if (which == "baujat" | which == "Baujat" | which == "baujat plot" | which == "Baujat Plot" | which == "baujatplot" | which == "Baujatplot"){

    # Baujat plot
    plot(x$BaujatPlot)

  }

  if (which == "influence" | which == "influence plot" | which == "Influence" | which == "Influence Plot" | which == "influence diagnostics"){

    # Influence Characteristics
    grid.newpage()
    grid.draw(x$InfluenceCharacteristics)

  }

  if (which == "forest es" | which == "es" | which == "ES"){

    # Forest (ES)
    suppressWarnings(suppressMessages(plot(x$ForestEffectSize)))

  }

  if (which == "forest i2" | which == "i2" | which == "I2"){

    # Forest (I2)
    suppressWarnings(suppressMessages(plot(x$ForestI2)))

  }

}
