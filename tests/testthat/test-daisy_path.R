isolate_daisy_path <- function(code) {
  old_opt <- getOption("daisyr.daisy_exe")
  old_env <- Sys.getenv("DAISY_EXE", unset = NA_character_)
  on.exit({
    options(daisyr.daisy_exe = old_opt)
    if (is.na(old_env)) Sys.unsetenv("DAISY_EXE") else Sys.setenv(DAISY_EXE = old_env)
  }, add = TRUE)
  options(daisyr.daisy_exe = NULL)
  Sys.unsetenv("DAISY_EXE")
  force(code)
}

fake_exe <- function() {
  tmp <- tempfile(fileext = ".exe")
  file.create(tmp)
  normalizePath(tmp, winslash = "/", mustWork = TRUE)
}

test_that("set_daisy_path stores a session path for get_daisy_path()", {
  isolate_daisy_path({
    path <- fake_exe()
    expect_equal(set_daisy_path(path), path)
    expect_equal(get_daisy_path(), path)
  })
})

test_that("set_daisy_path errors when the file is missing", {
  expect_error(set_daisy_path(tempfile()), "not found")
})

test_that("set_daisy_path(NULL) clears the session option", {
  isolate_daisy_path({
    path <- fake_exe()
    set_daisy_path(path)
    set_daisy_path(NULL)
    expect_null(getOption("daisyr.daisy_exe"))
    expect_equal(
      daisyr:::.find_daisy_exe(error = FALSE, candidates = character(0)),
      ""
    )
  })
})

test_that("get_daisy_path prefers set_daisy_path over DAISY_EXE", {
  isolate_daisy_path({
    via_opt <- fake_exe()
    via_env <- fake_exe()
    Sys.setenv(DAISY_EXE = via_env)
    set_daisy_path(via_opt)
    expect_equal(get_daisy_path(), via_opt)

    set_daisy_path(NULL)
    expect_equal(get_daisy_path(), via_env)
  })
})

test_that("get_daisy_path errors when a set path no longer exists", {
  isolate_daisy_path({
    path <- fake_exe()
    set_daisy_path(path)
    unlink(path)
    expect_error(get_daisy_path(), "missing")
  })
})

test_that(".resolve_daisy_exe uses an explicit path without looking up defaults", {
  expect_equal(daisyr:::.resolve_daisy_exe("daisy.exe"), "daisy.exe")
})

test_that(".build_daisy_cmd quotes the local daisy.exe by default", {
  expect_equal(
    as.character(daisyr:::.build_daisy_cmd("run.dai", daisy_exe = "C:/daisy.exe")),
    '"C:/daisy.exe" "run.dai"'
  )
})

test_that(".build_daisy_cmd interpolates a custom template without daisy_exe", {
  cmd <- daisyr:::.build_daisy_cmd(
    "test-optim.dai",
    daisy_exe = NULL,
    cmd = "singularity run --bind $(pwd):$(pwd) ~/daisy/daisy.sif -q $(pwd)/{run_file}",
    working_dir = "/work"
  )
  expect_equal(
    as.character(cmd),
    "singularity run --bind $(pwd):$(pwd) ~/daisy/daisy.sif -q $(pwd)/test-optim.dai"
  )
})

test_that(".build_daisy_cmd substitutes working_dir when asked", {
  cmd <- daisyr:::.build_daisy_cmd(
    "run.dai",
    cmd = "daisy -q {working_dir}/{run_file}",
    working_dir = "/tmp/scenario"
  )
  expect_equal(as.character(cmd), "daisy -q /tmp/scenario/run.dai")
})
