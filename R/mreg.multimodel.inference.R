#' Perform multimodel inference with a meta-regression model
#'
#' This function performs multimodel inference to evaluate the importance of predictors
#' with a meta-analytical meta-regression model.
#'
#' @usage multimodel.inference(TE, seTE, data, predictors, method='REML', test='knha',
#'     eval.criterion='AICc', interaction=FALSE, seed=123)
#'
#' @param TE The precalculated effect size for each study. Must be supplied as the name of the effect size
#' column in the dataset (in quotation marks; e.g. \code{TE = "effectsize"}).
#' @param seTE The precalculated standard error of the effect size for each study. Must be supplied as the name of the standard error
#' column in the dataset (in quotation marks; e.g. \code{seTE = "se"}).
#' @param data A \code{data.frame} containing columns for the effect size, standard error and
#' meta-regression predictors of each study/effect.
#' @param predictors A character vector specifying the predictors to be used
#' for multimodel inference. Names of the predictors must be identical to the names of the columns
#' in the \code{data.frame} supplied to \code{data}.
#' @param method Meta-analysis model to use for pooling effect sizes. Use \code{'FE'} for the
#' fixed-effect model. Different random-effect models are available: "\code{DL}", "\code{HE}", "\code{SJ}",
#' "\code{ML}", "\code{REML}", "\code{EB}", "\code{HS}" or "\code{GENQ}.
#' If \code{'FE'} is used, the \code{test} argument is automatically set to \code{'z'}, as the Knapp-Hartung
#' method is not meant to be used with fixed-effect models. Default is \code{'REML'}, and it is strongly advised to remain with
#' this option to use a standard (mixed-effects) meta-regression model.
#' @param test Method to use to compute test statistics and confidence intervals. Default is \code{'knha'}
#' which uses the Knapp-Hartung (Knapp & Hartung, 2003) adjustment method. "Conventional" Wald-type tests and
#' CIs are calculated by setting this argument to \code{'z'}. When \code{method='FE'}, this argument is
#' set to \code{'z'} automatically as the Knapp-Hartung method was not intended to be used with fixed-effect models.
#' @param eval.criterion Evaluation criterion to sort the multiple models by. Can be either \code{'AICc'}
#' (default; corrected Akaike's Information Criterion), \code{'AIC'} (Akaike's Information Criterion) or
#' \code{'BIC'} (Bayesian Information Criterion).
#' @param interaction If set to \code{FALSE} (default), no interactions between predictors are considered. Setting this parameter to
#' \code{TRUE} means that all interactions are modeled (interactions will only be modeled if the number of provided predictors is 4 or less).
#' @param seed Set a seed for the function. Default seed is \code{123}.
#'
#' @details Multimodel methods differ from stepwise regression methods as they do not try to successively build
#' the “best” single (meta-regression) model explaining most of the variance. Instead, in this procedure,
#' all possible combinations of a predefined selection of predictors are modeled, and evaluated using
#' a criterion such as Akaike’s Information Criterion, which rewards simpler models.
#' This enables a full examination of all possible models, and how they perform.
#' A common finding using this procedure is that there are many different kinds of predictor
#' combinations within a model which lead to a good fit. In multimodel inference, the estimated
#' coefficients of predictors can then be synthesized across all possible models to infer how
#' important certain predictors are overall.
#'
#' Multimodel Inference can be a useful way to obtain a comprehensive look on which predictors are
#' more or less important for predicting differences in effect sizes. Despite avoiding some of the
#' problems of stepwise regression methods, it should be noted that this method should still be rather
#' seen as exploratory, and may be used when there is no prior knowledge on how predictors are
#' related to effect sizes in the research field under study.
#'
#' The \code{multimodel.inference} function calls the \code{\link[metafor]{rma.uni}} function internally,
#' results of which are then fed forward to an adapted version of the \code{\link[MuMIn]{dredge}} function
#' internally for multimodel inference.
#' Parts of the computations in this function are adapted from a vignette by Wolfgang Viechtbauer, which can be found
#' \href{http://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti_and_mumin}{here}.
#'
#'
#' @references
#'
#' Harrer, M., Cuijpers, P., Furukawa, T.A, & Ebert, D. D. (2019).
#' \emph{Doing Meta-Analysis in R: A Hands-on Guide}. DOI: 10.5281/zenodo.2551803. \href{https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/smallstudyeffects.html}{Chapter 9.1}
#'
#' Knapp, G., & Hartung, J. (2003). Improved tests for a random effects meta-regression with a single covariate.
#' \emph{Statistics in Medicine, 22}, 2693–2710.
#'
#' Viechtbauer, W. (2019). \emph{Model Selection using the glmulti and MuMIn Packages}. \href{http://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti_and_mumin}{Link}.
#' Last accessed 01-Aug-2019.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @import MuMIn ggplot2
#' @importFrom metafor rma rma.uni
#'
#' @return Returns four tables and a plot:
#' \itemize{
#' \item \strong{Final Results (Summary Table)}: Displays the number of fitted models, model formula,
#' method to calculate test statistics and confidence intervals, interactions, and evaluation criterion used.
#' \item \strong{Best 5 Models}: Displays the top five models in terms of the evaluation criterion used.
#' Predictors are displayed as columns of the table, and models as rows. A number (weight) or \code{+}
#' sign (for categorical predictors) indicates that a predictor/interaction term was used for the
#' model, while empty cells indicate that the predictor was omitted in this model. Other metrics such as the
#' \code{weight}, evaluation metric \code{delta} compared to the best model, log-Likelihood (\code{logLik}) and degrees of freedom
#' are also displayed.
#' \item \strong{Multimodel Inference Coefficients}: Displays the estimated coefficients and statistical significance
#' of each regression term in the model.
#' \item \strong{Predictor Importance}: Displays the estimated importance of each model term. The table is sorted from
#' highest to lowest. A common rule of thumb is to consider a predictor as important when its importance value is above 0.8.
#' \item \strong{Predictor Importance Plot}: A bar plot for the predictor importance data along with a reference line for the
#' 0.8 value often used as a crude threshold to characterize a predictor as important.
#' }
#'
#' @export multimodel.inference
#'
#' @seealso \code{\link[MuMIn]{dredge}}
#'
#' @examples
#' \dontrun{
#' # Example 1: Perform multimodel inference with default settings
#' data('MVRegressionData')
#' library(metafor)
#' mmi = multimodel.inference(TE = 'yi', seTE = 'sei', data = MVRegressionData,
#'                            predictors = c('pubyear', 'quality',
#'                                           'reputation', 'continent'))
#' # Print summary
#' summary(mmi)
#'
#' # Plot predictor importance
#' plot(mmi)
#'
#' # Example 2: Model Interaction terms, set method to 'DL',
#' # change evaluation criterion to bic
#' multimodel.inference(TE = 'yi', seTE = 'sei', data = MVRegressionData,
#'                      predictors = c('pubyear', 'quality',
#'                                     'reputation', 'continent'),
#'                      method='DL', eval.criterion = 'BIC', interaction = TRUE)
#'
#' # Example 3: Use only categorical predictors
#' data('ThirdWave')
#' multimodel.inference(TE = 'TE', seTE = 'seTE', data = ThirdWave,
#'                      predictors = colnames(ThirdWave)[4:7], interaction = FALSE)}

multimodel.inference = function(TE, seTE, data, predictors, method = "REML", test = "knha", eval.criterion = "AICc",
    interaction = FALSE, seed = 123) {


        # Set supplied seed
        seed = seed
        set.seed(seed)

        # Check 'method'; if 'FE', switch test to 'z'.

        if (method %in% c("FE", "DL", "HE", "SJ", "ML", "REML", "EB", "HS", "GENQ")) {
            if (method == "FE" & test != "z") {
                test = "z"
                cat("Knapp-Hartung adjustments are only meant to be used for random-effects models. \n Parameter 'test' has therefore been changed to 'z'. \n")
            } else {

            }
        } else {
            stop("'method' must be either 'FE', 'DL', 'HE', 'SJ', 'ML', 'REML', 'EB', 'HS', or 'GENQ'.")
        }


        # Change supplied df to conform to glmulti
        if (TE %in% colnames(data)) {

        } else {
            stop("Column '", TE, "' not found in dataset.")
        }

        if (seTE %in% colnames(data)) {

        } else {
            stop("Column '", seTE, "' not found in dataset.")
        }

        for (i in 1:length(predictors)) {
            if (predictors[i] %in% colnames(data)) {

            } else {
                stop("Predictor '", predictors[i], "' not found in dataset.")
            }
        }

        if (eval.criterion[1] %in% c("AICc", "BIC", "AIC")) {

        } else {
            stop("'eval.criterion' must be either 'AICc' (default), 'AIC' or 'BIC'.")
        }

        if (length(predictors) > 4 & interaction == TRUE){
          interaction = FALSE
          cat("You entered", length(predictors), "predictors. Interactions can only be modeled for four or less predictors. Therefore, no interactions are modeled.", "\n")

        }


        TE = data[TE]
        seTE = data[seTE]
        preds = data[predictors]
        glm.data = data.frame(TE, seTE)
        colnames(glm.data) = c("TE", "seTE")
        glm.data = cbind(glm.data, as.data.frame(preds))

        # Build the formula
        interaction = interaction
        if (interaction == FALSE) {
            predictor.string = paste(predictors, collapse = "+")
        } else {
            predictor.string = paste(predictors, collapse = "*")
        }
        form = as.formula(paste("~", predictor.string, collapse = ""))


        # Build rma model
        full = suppressMessages(suppressWarnings(metafor::rma(yi = TE, sei = seTE, mods = form, data = glm.data, method = method,
            test = test)))


        # Multimodel Inference
        if (eval.criterion == "AICc") {
            res = suppressMessages(suppressWarnings(dredge2(full, trace = 2, rank = "AICc")))
        }

        if (eval.criterion == "AIC") {
            res = suppressMessages(suppressWarnings(dredge2(full, trace = 2, rank = "AIC")))
        }

        if (eval.criterion == "BIC") {
            res = suppressMessages(suppressWarnings(dredge2(full, trace = 2, rank = "BIC")))
        }

        # Save results for all models: all.models, top5.models
        all.models = res
        top5.models = res[1:5, ]

        # Create Multimodel Inference Coeffient Table and save: multimodel.coef
        multimodel.coef = summary(MuMIn::model.avg(res, revised.var = TRUE))
        multimodel.coef = multimodel.coef$coefmat.full

        # Create importance table and save: predictor.importance
        predictor.importance = data.frame(model = names(sw(res)), importance = as.numeric(sw(res)))

        # Print graph
        ggpredictor = ggplot(predictor.importance, aes(x = reorder(model, importance), y = importance)) +
            geom_bar(stat = "identity") + coord_flip() + geom_hline(yintercept = 0.8, color = "blue") + theme_minimal() +
            theme(axis.title.y = element_blank()) + ylab("Predictor Importance")


        # Return results
        returnlist = list(all.models = all.models, top5.models = top5.models, multimodel.coef = multimodel.coef,
            predictor.importance = predictor.importance, predictor.importance.plot = suppressWarnings(suppressMessages(ggpredictor)),
            formula = form, fitted.models = nrow(all.models), eval.criterion = eval.criterion, type.test = test, interaction = interaction)

        class(returnlist) = "multimodel.inference"

        invisible(returnlist)

        returnlist

}



