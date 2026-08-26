# strip_env_label() helper works

    Code
      cat(strip_env_label(
        "<list_of<quosure>>\n\n[[1]]\n<quosure>\nexpr: ^grp2\nenv:  0x72db165b8\n\n[[2]]\n<quosure>\nexpr: ^lbl\nenv:  0x72db31118"))
    Output
      <list_of<quosure>>
      
      [[1]]
      <quosure>
      expr: ^grp2
      env:  <env-address>
      
      [[2]]
      <quosure>
      expr: ^lbl
      env:  <env-address>

