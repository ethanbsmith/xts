testXtsPlots <- function(add.missig.controls = FALSE) {
  #generates test images and compares them to control
  #usage:
  # control images are stored and versioned in git in /tests/control-plots
  # comparisons are generated into a temp directory
  # when tests implementations are modified, copy correct output image from to the control directory and commit to git as part of the change
  # add.missig.controls will add any missing control output tp the control dir

  #prepare output dirs
  control.dir <- file.path("..", "tests", "control-plots")
  out.dir <- file.path(tempdir(), "xts-tests-plot")
  actual.dir <- file.path(out.dir, "actual")
  compare.dir <- file.path(out.dir, "compare")
  for (d in c(actual.dir, compare.dir)) {
    unlink(d, recursive = TRUE)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  data(sample_matrix)
  d <- as.xts(sample_matrix)
  #give columns different values to make difference clearer
  d <- cbind(d[,1], d[,2] * 1.05, d[,3] * .95, d[,4] * 1.03)

  tests <- list(
    #1 window
    `1.1.1` = function() {
      print(plot(d[,1], multi.panel = FALSE, main = "1 window, 1 panel, 1 series"))
    },
    `1.1.2` = function() {
      print(plot(d[,1], multi.panel = TRUE, main = "1 window, 1 panel, 1 series"))
    },
    `1.1.3` = function() {
      print(plot(d[,1], multi.panel = 2, main = "1 window, 1 panel, 1 series"))
    },
    `1.1.4` = function() {
      print(plot(d[,1], main = "1 window, 2 panels, Main panel"))
      #FIXME: currently produces a 2nd window
      print(addSeries(d[,2], main = "1 window, 2 panels, Dynamically added panel"))
    },
    `1.1.5` = function() {
      print(plot(d[,1:4], multi.panel = TRUE, main = "1 window, 4 panel, 1 series / panel, same y axis/panel"))
    },
    `1.1.6` = function() {
      print(plot(d[,1:4], multi.panel = TRUE, yaxis.same = FALSE,
                 main = "1 window, 4 panel, 1 series / panel, y axis/panel"))
    },
    `1.2.1` = function() {
      layout(matrix(1:2, ncol = 2))
      print(plot(d[,1], main = "2 windows, 1 panel / window, 1 series / panel, window 1"))
      print(plot(d[,2], main = "2 windows, 1 panel / window, 1 series / panel, window 2"))
    },
    `1.2.2` = function() {
      layout(matrix(1:4, ncol = 2))
      print(plot(d[,1:4], multi.panel = 1, main = "4 windows, 1 panel / window, 1 series / panel"))
    },
    `1.2.3` = function() {
      layout(matrix(1:2, ncol = 2))
      print(plot(d[,1:4], multi.panel = 2, main = "2 windows, 2 panels / window, 1 series / panel"))
      print(plot(d[,1:4], multi.panel = 2, main = "2 windows, 2 panels / window, 1 series / panel"))
    }
  )

  #generate test images using current implementation
  for (i in seq_along(tests)) {
    fn <- file.path(actual.dir, paste(names(tests)[i], "png", sep = "."))
    layout(matrix(1))
    png(fn, width = 1280, height = 1024)
    do.call(tests[[i]], list())
    dev.off()
  }

  if (add.missig.controls) {
    #add missing output to control directory
    fn <- setdiff(paste(names(tests), "png", sep = "."), dir(control.dir))
    if (length(fn) > 0) {
      message("Adding missing control files:")
      file.copy(from = file.path(actual.dir, fn),
                to = file.path(control.dir, fn),
                overwrite = FALSE) # dont overwrite. to repalce, delete original files
    } else {
      message("No missing control files. skipping")
    }
  }

  #compare generated oputput to control
  r <- gdiff::gdiffCompare(control.dir, actual.dir, compare.dir)
  print(r)
  if (!(all(is.na(r$errors)) && all(r$diffs == 0) && all(r$controlInTest) && all(r$testInControl))) {
    stop("Tests failed")
  }
  unlink(out.dir, recursive = TRUE)
}
