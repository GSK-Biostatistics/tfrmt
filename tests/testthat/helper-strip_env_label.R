# x <- "<list_of<quosure>>\n\n[[1]]\n<quosure>\nexpr: ^grp2\nenv:  0x72db165b8\n\n[[2]]\n<quosure>\nexpr: ^lbl\nenv:  0x72db31118\n\n[[3]]\n<quosure>\nexpr: ^ord\nenv:  0x72db403f8\n\n[[4]]\n<quosure>\nexpr: ^`1`\nenv:  0x72db5f4d0\n"

strip_env_label <- function(x) {
  stringr::str_replace_all(
    x,
    pattern = "(0[xX]){1}[A-Fa-f0-9]{9}",
    replacement = "<env-address>"
  )
}
