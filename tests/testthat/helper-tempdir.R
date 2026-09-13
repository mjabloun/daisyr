# Test helper: create a temp directory that's cleaned up when the calling
# test finishes, without adding a dependency on withr.
withr_local_tempdir <- function(env = parent.frame()) {
  dir <- tempfile("daisyr-test-")
  dir.create(dir)
  # bquote substitutes the literal path in now, so the deferred call doesn't
  # depend on any variable still existing in `env` when it later runs.
  do.call("on.exit", list(bquote(unlink(.(dir), recursive = TRUE)), add = TRUE), envir = env)
  dir
}
