# helper function to be used with the `expect_snapshot()` `transform` argument
# it takes a string and replaces the memory address of an environment (which is
# {rlang} uses as the environment label) with `"<env-address>"`. The memory
# address usually starts with "0x" or "0X" (as an indication that what follows
# is in hexadecimal) followed by 9, 12 or 16 characters (depending on the OS)
strip_env_label <- function(x) {
  stringr::str_replace_all(
    x,
    # memory addresses have 9 (macOS), 12(linux) or 16(windows) characters
    pattern = "(0[xX]){1}[A-Fa-f0-9]{9,16}",
    replacement = "<env-address>"
  )
}
