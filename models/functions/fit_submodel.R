# Fit a two-part model

# Setup ----
suppressWarnings(suppressMessages(library(optparse)))

# Command line interface

options <- list(
  make_option("--data",
              type = "character",
              help = "Path to the data set to be used"),
  make_option("--dist",
              type = "character",
              help = "Distribution assumption of the continuous distribution"),
  make_option("--prior",
              default = "default",
              type = "character",
              help = "Prior distribution assumption of the continuous distribution"),
  make_option("--reg.par",
              default = "mu",
              type = "character",
              help = "Parameter to which regression is added, defaults to mu"),
  make_option("--f",
              type = "character",
              help = "Formula used for regression"),
  make_option("--update.f",
              default = "",
              type = "character",
              help = "Updates to the formula used for regression, defaults to empty string"),
  make_option("--f2",
              default = "",
              type = "character",
              help = "Formula used for regression on another parameter"),
  make_option("--warmup",
              default = 1000,
              type = "integer",
              help = "Number of warmup iterations, defaults to 1000"),
  make_option("--iter",
              default = 2000,
              type = "integer",
              help = "Number of sampling iterations, defaults to 2000"),
  make_option("--chains",
              default = 4,
              type = "integer",
              help = "Number of chains, defaults to 4. The number of cores used corresponds
              to number of chains"),
  make_option("--file",
              type = "character",
              help = "Path to the file to which the result fit is saved"),
  make_option("--overwrite",
              default = TRUE,
              type = "logical",
              help = "By default TRUE and if the file already exists the file is overwritten"),
  make_option("--seed",
              type = "integer",
              help = "Random seed used for model fitting")
)

parser <- OptionParser(usage = "Fits a submodel of a two-part model using brms -package.",
                       option_list = options)

# Give an error if an argument is missing without default
opt <- parse_args(parser)
required_opts <- vapply(options, function(x) x@dest, character(1))
actual_opts <- setdiff(names(opt), "help")
for (req in required_opts) {
  if (!(req %in% actual_opts)) {
    msg <- paste0("Argument '", req, "' missing with no default.", "\n")
    stop(msg)
  }
}

# If overwrite is FALSE, check if the file already exists and quit if it does
if (isFALSE(opt$overwrite) & file.exists(opt$file)) {
  message(paste("File", basename(opt$file), "already exists, model fitting halted."))
  quit()
}

suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(brms)))

# Retrieve necessary arguments for the brm function
newdata <- readRDS(opt$data)
warmup <- opt$warmup
iter <- opt$iter
chains <- opt$chains
cores <- opt$chains
seed <- opt$seed
newformula <- opt$f
if (opt$update.f != "") {
  newformula <- update.formula(newformula, paste(". ~ .", opt$update.f))
}
if (opt$f2 != "") {
  newformula <- paste0("bf(", newformula, ",", opt$f2, ")")
}

switch(
  opt$dist,
  "bernoulli" = {
    newfamily <- brmsfamily(
      "bernoulli",
      link = "logit"
    )
    switch(
      opt$prior,
      "default" = {
        newprior <- NULL
      },
      "informative" = {
        newprior <- prior(normal(-2.41, 2), class = "Intercept") +
          prior(normal(0, 2), class = "b")
      }
    )
  },
  "gamma" = {
    newfamily <- brmsfamily(
      "Gamma",
      link = "log",
      link_shape = "log"
    )
    switch(
      opt$prior,
      "default" = {
        newprior <- NULL
        },
      "informative" = {
        if (opt$f2 == "") { # Prior when no regression on the shape parameter
          newprior <- prior(normal(7.5, 1.5), class = "Intercept") +
            prior(normal(0, 1.5), class = "b") +
            prior(gamma(2, 1), class = "shape")
        } else { # Prior when additional regression set to the shape parameter
          newprior <- prior(normal(7.5, 0.5), class = "Intercept") +
            prior(normal(0, 0.5), class = "b") +
            prior(normal(0, 0.5), class = "Intercept", dpar = "shape") +
            prior(normal(0, 0.5), class = "b", dpar = "shape")
        }
      }
    )
  },
  "weibull" = {
    newfamily <- brmsfamily(
      "weibull",
      link = "log",
      link_shape = "log"
    )
    switch(
      opt$prior,
      "default" = {
        newprior <- NULL
        },
      "informative" = {
        if (opt$f2 == "") { # Prior when no regression on the shape parameter
          newprior <- prior(normal(7.5, 1.5), class = "Intercept") +
            prior(normal(0, 1.5), class = "b") +
            prior(gamma(2, 1), class = "shape")
        } else { # Prior when additional regression set to the shape parameter
          newprior <- prior(normal(7.5, 0.5), class = "Intercept") +
            prior(normal(0, 0.5), class = "b") +
            prior(normal(0, 0.5), class = "Intercept", dpar = "shape") +
            prior(normal(0, 0.5), class = "b", dpar = "shape")
        }
      }
    )
  },
  "lognormal" = {
    newfamily <- brmsfamily(
      "lognormal",
      link = "identity",
      link_sigma = "log"
    )
    switch(
      opt$prior,
      "default" = {
        newprior <- NULL
        },
      "informative" = {
        if (opt$f2 == "") {
          newprior <- prior(normal(7.5, 1.5), class = "Intercept") +
            prior(normal(0, 1.5), class = "b") +
            prior(gamma(2, 1), class = "sigma")
        } else {
          newprior <- prior(normal(7.5, 0.5), class = "Intercept") +
            prior(normal(0, 0.5), class = "b") +
            prior(normal(0, 0.5), class = "Intercept", dpar = "sigma") +
            prior(normal(0, 0.5), class = "b", dpar = "sigma")
        }
      }
    )
  },
  "gen_extreme_value" = {
    newfamily <- brmsfamily(
      "gen_extreme_value",
      link = "log",
      link_sigma = "log"
    )
    switch(
      opt$prior,
      "default" = {
        newprior <- NULL
      },
      "test01" = {
         newprior <- prior(normal(7.5, 0.5), class = "Intercept") +
           prior(normal(0, 0.2), class = "b") +
           prior(gamma(0.1, 10), class = "sigma") +
           prior(uniform(0, 0.499), class = "xi", lb = 0, ub = 0.499)
      }
    )
  },
  "generalized_gamma" = {
    switch(
      opt$reg.par,
      "mu" = {
        source(
          file = file.path("models",
                           "functions",
                           "generalized-gamma_regpar-mu.R"),
          local = TRUE
        )
        newfamily <- generalized_gamma
        switch(
          opt$prior,
          "default" = {
            newprior <- NULL
          },
          "informative" = {
            newprior <- prior(normal(7.6, 1.5), class = "Intercept") +
              prior(normal(0, 1.5), class = "b") +
              prior(gamma(2, 1), class = "alpha") +
              prior(gamma(2, 1), class = "delta")
          }
        )
      },
      "alpha" = {
        source(
          file = file.path("models",
                         "functions",
                         "generalized-gamma_regpar-alpha.R"),
          local = TRUE
        )
        newfamily <- generalized_gamma
        switch(
          opt$prior,
          "default" = {
            newprior <- NULL
          },
          "informative" = {
            newprior <- prior(normal(0.5, 1.5), class = "Intercept") +
              prior(normal(0, 1.5), class = "b") +
              prior(normal(1000, 300), class = "beta", lb = 0) +
              prior(gamma(2, 1), class = "delta")
          }
        )
      }
    )
  },
  "generalized_pareto" = {
    source(
      file = file.path("models",
                       "functions",
                       paste0("gpareto_regpar-", opt$reg.par, ".R")),
      local = TRUE
    )
    newfamily <- generalized_pareto
    switch(
      opt$reg.par,
      "mu" = {
        switch(
          opt$prior,
          "default" = {
            newprior <- NULL
          },
          "informative" = {
            newprior <- prior(normal(7.6, 1), class = "Intercept") +
              prior(normal(0, 0.5), class = "b") +
              prior(gamma(4, 4), class = "ymin", lb = 0) +
              prior(uniform(0, 0.5), class = "k", lb = 0, ub = 1)
          }
        )
      },
      "sigma" = {
        switch(
          opt$prior,
          "default" = {
            newprior <- NULL
          },
          "informative" = {
            newprior <- prior(normal(0, 2), class = "Intercept") +
              prior(normal(0, 0.5), class = "b") +
              prior(gamma(4, 4), class = "ymin", lb = 0) +
              prior(uniform(0, 0.5), class = "k", lb = 0, ub = 1)
          },
          "test01" = {
            newprior <- prior(normal(7.5, 0.5), class = "Intercept") +
              prior(normal(0, 0.5), class = "b") +
              prior(uniform(0, 3), class = "ymin", lb = 0, ub = 3) +
              prior(normal(0, 0.2), class = "k", lb = 0, ub = 1)
          }
        )
      },
      "k" = {
        switch(
          opt$prior,
          "default" = {
            newprior <- NULL
          },
          "informative" = {
            newprior <- prior(normal(-3, 2), class = "Intercept") +
              prior(normal(0, 2), class = "b") +
              prior(gamma(4, 4), class = "ymin", lb = 0) +
              prior(gamma(2, 1), class = "sigma", lb = 0, ub = 1)
          },
          "test01" = {
            newprior <- prior(normal(0, 0.5), class = "Intercept") +
              prior(normal(0, 0.5), class = "b") +
              prior(gamma(2, 1), class = "sigma") +
              prior(uniform(0, 3), class = "ymin", lb = 0, ub = 3)
          }
        )
      }
    )
  }
)

# Model fit
if (newfamily$family != "custom") {
  stanvars <- NULL
}

fit <- brm(
  formula = newformula,
  data = newdata,
  family = newfamily,
  stanvars = stanvars,
  prior = newprior,
  warmup = warmup,
  iter = iter,
  chains = chains,
  cores = getOption("mc.cores", cores),
  seed = seed,
  refresh = 1
)

saveRDS(fit,
        file = opt$file)
