# # # # # # # # # # # # #
# makeArgs.rma          #
# # # # # # # # # # # # #

makeArgs.default = utils::getFromNamespace("makeArgs.default", "MuMIn")
makeArgs = utils::getFromNamespace("makeArgs", "MuMIn")

#' @export
#' @method makeArgs rma
makeArgs.rma = function(obj, termNames, comb, opt, ...) {

  return <- makeArgs.default(obj, termNames, comb, opt)
  names(return)[1L] = "mods"
  return

}

# # # # # # # # # # # # #
# coefTable.rma         #
# # # # # # # # # # # # #
.makeCoefTable = utils::getFromNamespace(".makeCoefTable", "MuMIn")

#' @export
#' @method coefTable rma
coefTable.rma = function(model, ...) {
  .makeCoefTable(model$b, model$se, coefNames = rownames(model$b))
}

#' @importFrom stats nobs formula terms

dredge2 = function(global.model, beta = c("none", "sd", "partial.sd"),
                   evaluate = TRUE, rank = "AICc", fixed = NULL, m.lim = NULL,
                   m.min, m.max, subset, trace = FALSE, varying, extra, ct.args = NULL,
                   ...)
{

  # Uses function code from MuMIn
  # Uses code adapted from MuMIn (GPLv2 License)
  # Copyright (C) 2019 Kamil Bartón

  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  asChar <- utils::getFromNamespace("asChar", "MuMIn")
  .getLik <- utils::getFromNamespace(".getLik", "MuMIn")
  isGEE <- utils::getFromNamespace("isGEE", "MuMIn")
  .getRank <- utils::getFromNamespace(".getRank", "MuMIn")
  .checkNaAction <- utils::getFromNamespace(".checkNaAction", "MuMIn")
  fixCoefNames <- utils::getFromNamespace("fixCoefNames", "MuMIn")
  .isREMLFit <- utils::getFromNamespace(".isREMLFit", "MuMIn")
  abbreviateTerms <- utils::getFromNamespace("abbreviateTerms", "MuMIn")
  AICc <- utils::getFromNamespace("AICc", "MuMIn")
  aicloglik_glm_fit <- utils::getFromNamespace("aicloglik_glm_fit", "MuMIn")
  applyrns <- utils::getFromNamespace("applyrns", "MuMIn")
  arm.glm <- utils::getFromNamespace("arm.glm", "MuMIn")
  armWeights <- utils::getFromNamespace("armWeights", "MuMIn")
  beta.weights <- utils::getFromNamespace("beta.weights", "MuMIn")
  BGWeights <- utils::getFromNamespace("BGWeights", "MuMIn")
  CAICF <- utils::getFromNamespace("CAICF", "MuMIn")
  cbindDataFrameList <- utils::getFromNamespace("cbindDataFrameList", "MuMIn")
  checkIsModelDataIdentical <- utils::getFromNamespace("checkIsModelDataIdentical", "MuMIn")
  clusterVExport <- utils::getFromNamespace("clusterVExport", "MuMIn")
  coef.averaging <- utils::getFromNamespace("coef.averaging", "MuMIn")
  coef.geese <- utils::getFromNamespace("coef.geese", "MuMIn")
  coef.model.selection <- utils::getFromNamespace("coef.model.selection", "MuMIn")
  coef.wgee <- utils::getFromNamespace("coef.wgee", "MuMIn")
  coef.yagsResult <- utils::getFromNamespace("coef.yagsResult", "MuMIn")
  prettyEnumStr <- utils::getFromNamespace("prettyEnumStr", "MuMIn")
  formula_margin_check <- utils::getFromNamespace("formula_margin_check", "MuMIn")
  matchCoef <- utils::getFromNamespace("matchCoef", "MuMIn")
  cry <- utils::getFromNamespace("cry", "MuMIn")
  subst <- utils::getFromNamespace("subst", "MuMIn")
  updateDeps <- utils::getFromNamespace("updateDeps", "MuMIn")
  .DebugPrint <- utils::getFromNamespace(".DebugPrint", "MuMIn")
  exprapply0 <- utils::getFromNamespace("exprapply0", "MuMIn")
  exprApply <- utils::getFromNamespace("exprApply", "MuMIn")
  .subst.names.for.items <- utils::getFromNamespace(".subst.names.for.items", "MuMIn")
  evalExprInEnv <- utils::getFromNamespace("evalExprInEnv", "MuMIn")
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  trace <- min(as.integer(trace), 2L)
  strbeta <- betaMode <- NULL
  eval(expression({
    if (is.logical(beta) && beta) {
      betaMode <- as.integer(beta)
      strbeta <- if (beta)
        "sd"
      else "none"
    }
    else if (is.character(beta)) {
      strbeta <- match.arg(beta)
      beta <- strbeta != "none"
      betaMode <- (strbeta != "none") + (strbeta == "partial.sd")
    }
    else {
      betaMode <- 0L
      strbeta <- "none"
    }
  }))
  gmEnv <- parent.frame()
  gmNobs <- nobs(global.model)
  gmCall <- get_call(global.model)
  if (is.null(gmCall)) {
    gmCall <- substitute(global.model)
    if (!is.call(gmCall)) {
      stop("need a 'global.model' with a call component. Consider using ",
           if (inherits(global.model, c("gamm", "gamm4")))
             "'uGamm'"
           else "'updateable'")
    }
    if (!is.function(eval.parent(gmCall[[1L]])))
      cry(, "could not find function '%s'", asChar(gmCall[[1L]]))
  }
  else {
    isDotted <- grep("^\\.\\.", sapply(as.list(gmCall), asChar))
    if (length(isDotted) != 0L) {
      if (is.name(substitute(global.model))) {
        cry(, "call stored in 'global.model' contains dotted names and cannot be updated. \n    Consider using 'updateable' on the modelling function")
      }
      else gmCall[isDotted] <- substitute(global.model)[names(gmCall[isDotted])]
    }
    if (inherits(global.model, "mark") && gmCall[[1L]] ==
        "make.mark.model") {
      gmCall <- call("run.mark.model", model = gmCall,
                     invisible = TRUE)
    }
  }
  thisCall <- sys.call()
  exprApply(gmCall[["data"]], NA, function(expr) {
    if (is.symbol(expr[[1L]]) && all(expr[[1L]] != c("@",
                                                     "$")))
      cry(thisCall, "'global.model' uses \"data\" that is a function value: use a variable instead")
  })
  lik <- .getLik(global.model)
  logLik <- lik$logLik
  rank.custom <- !missing(rank)
  if (!rank.custom && lik$name == "qLik") {
    rank <- "QIC"
    cry(, "using 'QIC' instead of 'AICc'", warn = TRUE)
  }
  rankArgs <- list(...)
  if (any(badargs <- names(rankArgs) == "marg.ex")) {
    cry(, "argument \"marg.ex\" is defunct and has been ignored",
        warn = TRUE)
    rankArgs <- rankArgs[!badargs]
  }
  if (any(names(rankArgs) == "na.action"))
    cry("RTFM", "argument \"na.action\" is inappropriate here",
        warn = FALSE)
  IC <- .getRank(rank, rankArgs)
  if (any(badargs <- is.na(match(names(rankArgs), c(names(formals(get("rank",
                                                                      environment(IC))))[-1L], names(formals()))))))
    cry("RTFM", ngettext(sum(badargs), "argument %s is not a name of formal argument of %s",
                         "arguments %s are not names of formal arguments of %s"),
        prettyEnumStr(names(rankArgs[badargs])), "'dredge' or 'rank'",
        warn = TRUE)
  ICName <- as.character(attr(IC, "call")[[1L]])
  if (length(tryCatch(IC(global.model), error = function(e) {
    stop(simpleError(conditionMessage(e), subst(attr(IC,
                                                     "call"), x = as.name("global.model"))))
  })) != 1L) {
    cry(, "result of '%s' is not of length 1", asChar(attr(IC,
                                                           "call")))
  }
  allTerms <- allTerms0 <- getAllTerms(global.model, intercept = TRUE,
                                       data = eval(gmCall$data, envir = gmEnv))
  interceptLabel <- attr(allTerms, "interceptLabel")
  if (is.null(interceptLabel))
    interceptLabel <- "(Intercept)"
  nIntercepts <- sum(attr(allTerms, "intercept"))
  if (!(gmNaAction <- .checkNaAction(cl = gmCall, what = "'global.model'")))
    cry(, attr(gmNaAction, "message"))
  if (names(gmCall)[2L] == "")
    gmCall <- match.call(gmCall, definition = eval.parent(gmCall[[1L]]),
                         expand.dots = TRUE)
  gmCoefNames <- names(coeffs(global.model))
  if (any(dup <- duplicated(gmCoefNames)))
    cry(, "model cannot have duplicated coefficient names: ",
        prettyEnumStr(gmCoefNames[dup]))
  gmCoefNames <- fixCoefNames(gmCoefNames)
  nVars <- length(allTerms)
  if (isTRUE(rankArgs$REML) || (isTRUE(.isREMLFit(global.model)) &&
                                is.null(rankArgs$REML)))
    cry(, "comparing models fitted by REML", warn = TRUE)
  if ((betaMode != 0L) && is.null(tryCatch(std.coef(global.model,
                                                    betaMode == 2L), error = return_null, warning = return_null))) {
    cry(, "do not know how to standardize coefficients of '%s', argument 'beta' ignored",
        class(global.model)[1L], warn = TRUE)
    betaMode <- 0L
    strbeta <- "none"
  }
  if (nomlim <- is.null(m.lim))
    m.lim <- c(0, NA)
  if (!missing(m.max) || !missing(m.min)) {
    warning("arguments 'm.min' and 'm.max' are deprecated, use 'm.lim' instead")
    if (!nomlim)
      stop("cannot use both 'm.lim' and 'm.min' or 'm.max'")
    if (!missing(m.min))
      m.lim[1L] <- m.min[1L]
    if (!missing(m.max))
      m.lim[2L] <- m.max[1L]
  }
  if (!is.numeric(m.lim) || length(m.lim) != 2L || any(m.lim <
                                                       0, na.rm = TRUE))
    stop("invalid 'm.lim' value")
  m.lim[2L] <- if (!is.finite(m.lim[2L]))
    (nVars - nIntercepts)
  else min(nVars - nIntercepts, m.lim[2L])
  if (!is.finite(m.lim[1L]))
    m.lim[1L] <- 0
  m.min <- m.lim[1L]
  m.max <- m.lim[2L]
  if (!is.null(fixed)) {
    if (inherits(fixed, "formula")) {
      if (fixed[[1L]] != "~" || length(fixed) != 2L)
        cry(, "'fixed' should be a one-sided formula",
            warn = TRUE)
      fixed <- as.vector(getAllTerms(fixed))
    }
    else if (identical(fixed, TRUE)) {
      fixed <- as.vector(allTerms[!(allTerms %in% interceptLabel)])
    }
    else if (!is.character(fixed)) {
      cry(, paste("'fixed' should be either a character vector with",
                  " names of variables or a one-sided formula"))
    }
    if (!all(i <- (fixed %in% allTerms))) {
      cry(, "some terms in 'fixed' do not exist in 'global.model': %s",
          prettyEnumStr(fixed[!i]), warn = TRUE)
      fixed <- fixed[i]
    }
  }
  deps <- attr(allTerms0, "deps")
  fixed <- union(fixed, rownames(deps)[rowSums(deps, na.rm = TRUE) ==
                                         ncol(deps)])
  fixed <- c(fixed, allTerms[allTerms %in% interceptLabel])
  nFixed <- length(fixed)
  if (nFixed != 0L)
    message(sprintf(ngettext(nFixed, "Fixed term is %s",
                             "Fixed terms are %s"), prettyEnumStr(fixed)))
  termsOrder <- order(allTerms %in% fixed)
  allTerms <- allTerms[termsOrder]
  di <- match(allTerms, rownames(deps))
  deps <- deps[di, di, drop = FALSE]
  gmFormulaEnv <- environment(as.formula(formula(global.model),
                                         env = gmEnv))
  if (!missing(varying) && !is.null(varying)) {
    nVarying <- length(varying)
    varyingNames <- names(varying)
    fvarying <- unlist(varying, recursive = FALSE, use.names = FALSE)
    vlen <- vapply(varying, length, 1L)
    nVariants <- prod(vlen)
    variants <- as.matrix(expand.grid(split(seq_len(sum(vlen)),
                                            rep(seq_len(nVarying), vlen))))
    variantsFlat <- unlist(lapply(varying, .makeListNames),
                           recursive = FALSE, use.names = FALSE)
  }
  else {
    variants <- varyingNames <- NULL
    nVariants <- 1L
    nVarying <- 0L
  }
  if (!missing(extra) && length(extra) != 0L) {
    if (any(c("adjR^2", "R^2") %in% extra) && nVariants >
        1L)
      stop("\"R^2\" in 'extra' can be used only with no 'varying'")
    extra <- eval(as.call(list(call("get", ".get.extras",
                                    envir = call("asNamespace", .packageName), inherits = FALSE),
                               substitute(extra), r2nullfit = TRUE)), parent.frame())
    if (any(c("adjR^2", "R^2") %in% names(extra))) {
      nullfit_ <- null.fit(global.model, evaluate = TRUE,
                           envir = gmFormulaEnv)
    }
    applyExtras <- function(x) unlist(lapply(extra, function(f) f(x)))
    extraResult <- applyExtras(global.model)
    if (!is.numeric(extraResult))
      cry(, "function in 'extra' returned non-numeric result")
    nExtra <- length(extraResult)
    extraNames <- names(extraResult)
  }
  else {
    nExtra <- 0L
    extraNames <- character(0L)
  }
  nov <- as.integer(nVars - nFixed)
  ncomb <- (2L^nov) * nVariants
  novMax <- log2(.Machine$integer.max%/%nVariants)
  if (nov > novMax)
    cry(, "number of non-fixed predictors [%d] exceeds the allowed maximum of %d (with %d variants)",
        nov, novMax, nVariants)
  resultChunkSize <- 25L
  if (evaluate) {
    rvNcol <- nVars + nVarying + 3L + nExtra
    rval <- matrix(NA_real_, ncol = rvNcol, nrow = resultChunkSize)
    coefTables <- vector(resultChunkSize, mode = "list")
  }
  if (missing(subset)) {
    hasSubset <- 1L
  }
  else {
    if (!tryCatch(is.language(subset) || is.matrix(subset),
                  error = function(e) FALSE))
      subset <- substitute(subset)
    if (is.matrix(subset)) {
      dn <- dimnames(subset)
      n <- length(allTerms)
      if (is.null(dn) || any(sapply(dn, is.null))) {
        di <- dim(subset)
        if (any(di != n))
          stop("unnamed 'subset' matrix does not have both dimensions",
               " equal to number of terms in 'global.model': %d",
               n)
        dimnames(subset) <- list(allTerms, allTerms)
      }
      else {
        if (!all(unique(unlist(dn)) %in% allTerms))
          warning("at least some dimnames of 'subset' matrix do not ",
                  "match term names in 'global.model'")
        subset0 <- subset
        subset <- matrix(subset[match(allTerms, rownames(subset)),
                                match(allTerms, colnames(subset))], dimnames = list(allTerms,
                                                                                    allTerms), nrow = n, ncol = n)
        nas <- is.na(subset)
        lotri <- lower.tri(subset)
        i <- lotri & nas & !t(nas)
        subset[i] <- t(subset)[i]
        subset[!lotri] <- NA
      }
      if (any(!is.na(subset[!lower.tri(subset)]))) {
        warning("non-missing values exist outside the lower triangle of 'subset'")
        subset[!lower.tri(subset)] <- NA
      }
      mode(subset) <- "logical"
      hasSubset <- 2L
    }
    else {
      if (inherits(subset, "formula")) {
        if (subset[[1L]] != "~" || length(subset) !=
            2L)
          stop("'subset' formula should be one-sided")
        subset <- subset[[2L]]
      }
      subset <- as.expression(subset)
      ssValidNames <- c("comb", "*nvar*")
      tmpTerms <- terms(reformulate(allTerms0[!(allTerms0 %in%
                                                  interceptLabel)]))
      gloFactorTable <- t(attr(tmpTerms, "factors") !=
                            0)
      offsetNames <- sapply(attr(tmpTerms, "variables")[attr(tmpTerms,
                                                             "offset") + 1L], asChar)
      if (length(offsetNames) != 0L) {
        gloFactorTable <- rbind(gloFactorTable, matrix(FALSE,
                                                       ncol = ncol(gloFactorTable), nrow = length(offsetNames),
                                                       dimnames = list(offsetNames, NULL)))
        for (i in offsetNames) gloFactorTable[offsetNames,
                                              offsetNames] <- TRUE
      }
      .DebugPrint(gloFactorTable)
      rownames(gloFactorTable) <- allTerms0[!(allTerms0 %in%
                                                interceptLabel)]
      subsetExpr <- subset[[1L]]
      subsetExpr <- exprapply0(subsetExpr, c("with", "."),
                               .subst.with, gloFactorTable, allTerms, as.name("comb"),
                               gmEnv)
      subsetExpr <- exprapply0(subsetExpr, c("{", "Term"),
                               .subst.term)
      tmp <- updateDeps(subsetExpr, deps)
      subsetExpr <- tmp$expr
      deps <- tmp$deps
      subsetExpr <- exprapply0(subsetExpr, "dc", .subst.vars.for.args)
      subsetExpr <- .subst.names.for.items(subsetExpr,
                                           allTerms, "comb")
      if (nVarying) {
        ssValidNames <- c("cVar", "comb", "*nvar*")
        subsetExpr <- exprapply0(subsetExpr, "V", .subst.v,
                                 as.name("cVar"), varyingNames)
        if (!all(all.vars(subsetExpr) %in% ssValidNames))
          subsetExpr <- .subst.names.for.items(subsetExpr,
                                               varyingNames, "cVar", fun = "[[")
      }
      ssVars <- all.vars(subsetExpr)
      okVars <- ssVars %in% ssValidNames
      if (!all(okVars))
        stop("unrecognized names in 'subset' expression: ",
             prettyEnumStr(ssVars[!okVars]))
      ssEnv <- new.env(parent = parent.frame())
      ssFunc <- setdiff(all.vars(subsetExpr, functions = TRUE),
                        ssVars)
      if ("dc" %in% ssFunc)
        assign("dc", .subset_dc, ssEnv)
      hasSubset <- if (any(ssVars == "cVar"))
        4L
      else 3L
    }
  }
  comb.sfx <- rep(TRUE, nFixed)
  comb.seq <- if (nov != 0L)
    seq_len(nov)
  else 0L
  k <- 0L
  extraResult1 <- integer(0L)
  calls <- vector(mode = "list", length = resultChunkSize)
  ord <- integer(resultChunkSize)
  argsOptions <- list(response = attr(allTerms0, "response"),
                      intercept = nIntercepts, interceptLabel = interceptLabel,
                      random = attr(allTerms0, "random"), gmCall = gmCall,
                      gmEnv = gmEnv, allTerms = allTerms0, gmCoefNames = gmCoefNames,
                      gmDataHead = if (!is.null(gmCall$data)) {
                        if (eval(call("is.data.frame", gmCall$data), gmEnv)) eval(call("head",
                                                                                       gmCall$data, 1L), gmEnv) else gmCall$data
                      } else NULL, gmFormulaEnv = gmFormulaEnv)
  matchCoefCall <- as.call(c(alist(matchCoef, fit1, all.terms = allTerms,
                                   beta = betaMode, allCoef = TRUE), ct.args))
  retColIdx <- if (nVarying)
    -nVars - seq_len(nVarying)
  else TRUE
  if (trace > 1L) {
    progressBar <- if (.Platform$GUI == "Rgui") {
      utils::winProgressBar(max = ncomb, title = "'dredge' in progress")
    }
    else utils::txtProgressBar(max = ncomb, style = 3L)
    setProgressBar <- switch(class(progressBar), txtProgressBar = utils::setTxtProgressBar,
                             winProgressBar = utils::setWinProgressBar, function(...) {
                             })
    on.exit(close(progressBar))
  }
  iComb <- -1L
  while ((iComb <- iComb + 1L) < ncomb) {
    varComb <- iComb%%nVariants
    jComb <- (iComb - varComb)%/%nVariants
    if (varComb == 0L) {
      isok <- TRUE
      comb <- c(as.logical(intToBits(jComb)[comb.seq]),
                comb.sfx)
      nvar <- sum(comb) - nIntercepts
      if (nvar > m.max || nvar < m.min || !formula_margin_check(comb,
                                                                deps) || switch(hasSubset, FALSE, !all(subset[comb,
                                                                                                              comb], na.rm = TRUE), !evalExprInEnv(subsetExpr,
                                                                                                                                                   env = ssEnv, enclos = parent.frame(), comb = comb,
                                                                                                                                                   `*nvar*` = nvar), FALSE)) {
        isok <- FALSE
        next
      }
      newArgs <- makeArgs.rma(global.model, allTerms[comb],
                          argsOptions)
      if (!is.null(attr(newArgs, "problems"))) {
        print.warnings(structure(vector(mode = "list",
                                        length = length(attr(newArgs, "problems"))),
                                 names = attr(newArgs, "problems")))
      }
      cl <- gmCall
      cl[names(newArgs)] <- newArgs
    }
    if (!isok)
      next
    clVariant <- cl
    if (nVarying) {
      cvi <- variants[varComb + 1L, ]
      if (hasSubset == 4L && !evalExprInEnv(subsetExpr,
                                            env = ssEnv, enclos = parent.frame(), comb = comb,
                                            `*nvar*` = nvar, cVar = variantsFlat[cvi]))
        next
      clVariant[varyingNames] <- fvarying[cvi]
    }
    if (trace == 1L) {
      cat(iComb, ": ")
      print(clVariant)
      utils::flush.console()
    }
    else if (trace == 2L) {
      setProgressBar(progressBar, value = iComb, title = sprintf("dredge: %d of %.0f subsets (%d total)",
                                                                 k, (k/iComb) * ncomb, iComb))
    }
    if (evaluate) {
      fit1 <- tryCatch(eval(clVariant, gmEnv), error = function(err) {
        err$message <- paste(conditionMessage(err), "(model",
                             iComb, "skipped)", collapse = "")
        class(err) <- c("simpleError", "warning", "condition")
        warning(err)
        return(NULL)
      })
      if (is.null(fit1))
        next
      if (nExtra != 0L) {
        extraResult1 <- applyExtras(fit1)
        if (length(extraResult1) < nExtra) {
          tmp <- rep(NA_real_, nExtra)
          tmp[match(names(extraResult1), names(extraResult))] <- extraResult1
          extraResult1 <- tmp
        }
      }
      mcoef1 <- eval(matchCoefCall)
      ll1 <- logLik(fit1)
      nobs1 <- nobs(fit1)
      if (nobs1 != gmNobs)
        cry(, "number of observations in model #%d [%d] different from that in global model [%d]",
            iComb, nobs1, gmNobs, warn = TRUE)
      row1 <- c(mcoef1[allTerms], extraResult1, df = attr(ll1,
                                                          "df"), ll = ll1, ic = IC(fit1))
      k <- k + 1L
      rvlen <- nrow(rval)
      if (retNeedsExtending <- k > rvlen) {
        nadd <- min(resultChunkSize, ncomb - rvlen)
        rval <- rbind(rval, matrix(NA_real_, ncol = rvNcol,
                                   nrow = nadd), deparse.level = 0L)
        addi <- seq.int(rvlen + 1L, length.out = nadd)
        coefTables[addi] <- vector("list", nadd)
      }
      rval[k, retColIdx] <- row1
      coefTables[[k]] <- attr(mcoef1, "coefTable")
    }
    else {
      k <- k + 1L
      rvlen <- length(ord)
      if (retNeedsExtending <- k > rvlen) {
        nadd <- min(resultChunkSize, ncomb - rvlen)
        addi <- seq.int(rvlen + 1L, length.out = nadd)
      }
    }
    if (retNeedsExtending) {
      calls[addi] <- vector("list", nadd)
      ord[addi] <- integer(nadd)
    }
    ord[k] <- iComb
    calls[[k]] <- clVariant
  }
  if (k == 0L)
    stop("result is empty")
  ord <- ord + 1L
  names(calls) <- ord
  if (!evaluate)
    return(calls[seq_len(k)])
  if (k < nrow(rval)) {
    i <- seq_len(k)
    rval <- rval[i, , drop = FALSE]
    ord <- ord[i]
    calls <- calls[i]
    coefTables <- coefTables[i]
  }
  if (nVarying) {
    varlev <- ord%%nVariants
    varlev[varlev == 0L] <- nVariants
    rval[, nVars + seq_len(nVarying)] <- variants[varlev,
                                                  ]
  }
  rval <- as.data.frame(rval)
  row.names(rval) <- ord
  tfac <- which(!(allTerms %in% gmCoefNames))
  rval[tfac] <- lapply(rval[tfac], factor, levels = NaN, labels = "+")
  rval[, seq_along(allTerms)] <- rval[, v <- order(termsOrder)]
  allTerms <- allTerms[v]
  colnames(rval) <- c(allTerms, varyingNames, extraNames, "df",
                      lik$name, ICName)
  if (nVarying) {
    variant.names <- vapply(variantsFlat, asChar, "", width.cutoff = 20L)
    vnum <- split(seq_len(sum(vlen)), rep(seq_len(nVarying),
                                          vlen))
    names(vnum) <- varyingNames
    for (i in varyingNames) rval[, i] <- factor(rval[, i],
                                                levels = vnum[[i]], labels = variant.names[vnum[[i]]])
  }
  rval <- rval[o <- order(rval[, ICName], decreasing = FALSE),
               ]
  coefTables <- coefTables[o]
  rval$delta <- rval[, ICName] - min(rval[, ICName])
  rval$weight <- Weights(rval$delta)
  mode(rval$df) <- "integer"
  structure(rval, model.calls = calls[o], global = global.model,
            global.call = gmCall, terms = structure(allTerms, interceptLabel = interceptLabel),
            rank = IC, beta = strbeta, call = match.call(expand.dots = TRUE),
            coefTables = coefTables, nobs = gmNobs, vCols = varyingNames,
            column.types = {
              colTypes <- c(terms = length(allTerms), varying = length(varyingNames),
                            extra = length(extraNames), df = 1L, loglik = 1L,
                            ic = 1L, delta = 1L, weight = 1L)
              column.types <- rep(1L:length(colTypes), colTypes)
              names(column.types) <- colnames(rval)
              lv <- 1L:length(colTypes)
              factor(column.types, levels = lv, labels = names(colTypes)[lv])
            }, class = c("model.selection", "data.frame"))
}


