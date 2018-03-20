#' Mostly from Kevin Ushey via 
#' http://kevinushey.github.io/blog/2015/02/02/rprofile-essentials/
.First <- function() {
  #' create .Rprofile env
  .__Rprofile.env__. <- attach(NULL, name = "local:rprofile")

  #' helpers for seeting things in .__Rprofile.env__.
  set <- function(name, value)
    assign(name, value, envir = .__Rprofile.env__.)

  set(".Start.time", as.numeric(Sys.time()))

  NAME <- intToUtf8(c( 71L, 97L, 114L, 114L, 101L, 116L, 116L, 32L,
    77L, 111L, 111L, 110L, 101L, 121L ))
  EMAIL <- intToUtf8( c( 103L, 109L, 111L, 111L, 110L, 101L, 121L,
      64L, 109L, 97L, 105L, 108L, 46L, 98L, 114L, 97L, 100L, 108L,
      101L, 121L, 46L, 101L, 100L, 117L ) )

  options(
    #' CRAN
    repos = c(CRAN = "https://cran.rstudio.org"),

    #' blogdown author name
    blogdown.author = "Garrett Mooney",

    #' warn on partial matches
    # warnPartialMatchAttr = TRUE,
    # warnPartialMatchDollar =  TRUE,
    # warnPartialMatchArgs =  TRUE,

    #' warnings are errors
    # warn = 2,

    #' fancy quotes are annoying and lead to
    #' 'copy + paste' bugs / frustrations
    useFancyQuotes = FALSE,

    #' devtools related options
    devtools.desc = list(
      Author = NAME,
      Maintainer = paste(NAME, " <", EMAIL, ">", sep = ""),
      License = "MIT + file LICENSE",
      Version = "0.0.1"
    ),

    devtools.name = NAME,

    #' rtichoke
    # color scheme see [here](https://help.farbox.com/pygments.html) for a list of supported color schemes, default is `"native"`
    rtichoke.color_scheme = "native",

    # either  `"emacs"` (default) or `"vi"`.
    rtichoke.editing_mode = "vi",

    # auto match brackets and quotes
    rtichoke.auto_match = FALSE,

    # auto indentation for new line and curly braces
    rtichoke.auto_indentation = TRUE,
    rtichoke.tab_size = 4,

    # pop up completion while typing
    rtichoke.complete_while_typing = TRUE,

    # automatically adjust R buffer size based on terminal width
    rtichoke.auto_width = TRUE,

    # insert new line between prompts
    rtichoke.insert_new_line = TRUE,

   # when using history search (ctrl-r/ctrl-s in emacs mode), do not show duplicate results
   rtichoke.history_search_no_duplicates = FALSE,

   # custom prompt for different modes
   rtichoke.prompt = "\033[0;34mr$>\033[0m ",
   rtichoke.shell_prompt = "\033[0;31m#!>\033[0m ",
   rtichoke.browse_prompt = "\033[0;33mBrowse[{}]>\033[0m ",

   # supress the loading message for reticulate
   rtichoke.suppress_reticulate_message = FALSE
  )

  #' enable autocompletions for package naes in
  #' `require()`, `library()`
  utils::rc.settings(ipck = TRUE)

  #' generate some useful aliases, for editing common files
  alias <- function(name, action) {
    placeholder <- structure(list(), class = sprintf("__%s__", name))
    assign(name, placeholder, envir = .__Rprofile.env__.)
    assign(sprintf("print.__%s__", name), action, envir = .__Rprofile.env__.)
  }

  #' open .Rprofile for editing
  alias(".Rprofile", function(...)
    file.edit("~/.Rprofile"))
  alias(".Renviron", function(...)
    file.edit("~/.Renviron"))

  #' open Makevars for editing
  alias("Makevars", function(...) {
    if (!utils::file_test("-d", "~/.R"))
      dir.create("~/.R")
    file.edit("~/.R/Makevars")
  })

  #' simple CLI to git
  git <-
    new.env(parent = emptyenv())
  commands <-
    c(
      "clone",
      "init",
      "add",
      "mv",
      "reset",
      "rm",
      "bisect",
      "grep",
      "log",
      "show",
      "status",
      "stash",
      "branch",
      "checkout",
      "commit",
      "diff",
      "merge",
      "rebase",
      "tag",
      "fetch",
      "pull",
      "push",
      "clean"
    )

  for (command in commands) {
    code <- substitute(system(paste(
      path, "-c color.status=false", command, ...
    )),
    list(path = quote(shQuote(
      normalizePath(Sys.which("git"))
    )),
    command = command))

    fn <-
      eval(call("function", pairlist(... = quote(expr =)), code))

    assign(command, fn, envir = git)

  }

  assign("git", git, envir = .__Rprofile.env__.)

  #' tools for munging the PATH
  PATH <-
    (function() {
      read <- function() {
        strsplit(Sys.getenv("PATH"), .Platform$path.sep, TRUE)[[1]]
      }

      write <-
        function(path) {
          Sys.setenv(PATH = paste(path, collapse = .Platform$path.sep))
          invisible(path)
        }

      prepend <-
        function(dir) {
          dir <- normalizePath(dir, mustWork = TRUE)
          path <-
            c(dir, setdiff(read(), dir))
          write(path)
        }

      append <-
        function(dir) {
          dir <- normalizePath(dir, mustWork = TRUE)
          path <-
            c(setdiff(read(), dir), dir)
          write(path)
        }

      remove <-
        function(dir) {
          path <- setdiff(read(), dir)
          write(path)
        }

      list(
        read = read,
        write = write,
        prepend = prepend,
        append = append,
        remove = remove
      )

    })()
  assign("PATH", PATH, envir = .__Rprofile.env__.)

  #' ensure commonly-used packages are installed, loaded
  quietly <-
    function(expr) {
      status <- FALSE
      suppressWarnings(suppressMessages(utils::capture.output(status <-
                                                                expr)))
      status
    }

  install <-
    function(package) {
      code <- sprintf(
        "utils::install.packages(%s, lib = %s, repos = %s, Ncpus = 8)",
        shQuote(package),
        shQuote(.libPaths()[[1]]),
        shQuote(getOption("repos")[["CRAN"]])
      )

      R <-
        file.path(R.home("bin"),
                  if (Sys.info()[["sysname"]] == "Windows")
                    "R.exe"
                  else
                    "R")

      con <-
        tempfile(fileext = ".R")
      writeLines(code, con = con)
      on.exit(unlink(con), add = TRUE)

      cmd <-
        paste(shQuote(R), "-f", shQuote(con))
      system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    }

  packages <-
    c("devtools", "roxygen2", "knitr", "rmarkdown", "testthat", "fs")
  invisible(lapply(packages, function(package) {
    if (quietly(require(package, character.only = TRUE, quietly = TRUE)))
      return()

    message("Installing '", package, "' ... ", appendLF = FALSE)
    install(package)

    success <-
      quietly(require(package, character.only = TRUE, quietly = TRUE))
    message(if (success)
      "OK"
      else
        "FAIL")

  }))

  #' use lookup::lookup and github to lookup the body of a function
  if (interactive()) {
        suppressPackageStartupMessages(library(lookup))
  }

  #' clean up extra attached envs
  addTaskCallback(function(...) {
    count <- sum(search() == "local:rprofile")
    if (count == 0)
      return(FALSE)

    for (i in seq_len(count - 1))
      detach("local:rprofile")

    return(FALSE)
  })

  #' display startup message(s)
  sanic <- "
      ░░░░░░░░░▄▄▄▄▄
  ░░░░░░░░▀▀▀██████▄▄▄      _______________ 
  ░░░░░░▄▄▄▄▄░░█████████▄ /                 \\
  ░░░░░▀▀▀▀█████▌░▀▐▄░▀▐█ |  Gotta go fast!  |
  ░░░▀▀█████▄▄░▀██████▄██ | _______________ /
  ░░░▀▄▄▄▄▄░░▀▀█▄▀█════█▀ |/
  ░░░░░░░░▀▀▀▄░░▀▀███░▀      ▄▄
  ░░░░░▄███▀▀██▄████████▄░▄▀▀▀██▌
  ░░░██▀▄▄▄██▀▄███▀░▀▀████░░░░░▀█▄
  ▄▀▀▀▄██▄▀▀▌████▒▒▒▒▒▒███░░░░▌▄▄▀
  ▌░░░░▐▀████▐███▒▒▒▒▒▐██▌
  ▀▄░░▄▀░░░▀▀████▒▒▒▒▄██▀
  ░░▀▀░░░░░░▀▀█████████▀
  ░░░░░░░░▄▄██▀██████▀█
  ░░░░░░▄██▀░░░░░▀▀▀░░█
  ░░░░░▄█░░░░░░░░░░░░░▐▌
  ░▄▄▄▄█▌░░░░░░░░░░░░░░▀█▄▄▄▄▀▀▄
  ▌░░░░░▐░░░░░░░░░░░░░░░░▀▀▄▄▄▀ 
  \n\n"
  msg <-
  if (length(.libPaths()) > 1)
      "Using libraries at paths:\n"
  else
    "Using library at path:\n"
  libs <-
    paste("-", .libPaths(), collapse = "\n")
  message(paste0(sanic, msg), libs, sep = "")
}
