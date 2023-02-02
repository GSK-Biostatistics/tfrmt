# basic tfrmt

    Code
      .
    Output
      {
        "group": [],
        "column": [],
        "label": {},
        "param": {},
        "value": {}
      } 

---

    Code
      .
    Output
      {
        "group": ["group"],
        "column": ["span2", "span1", "my_col"],
        "label": ["label"],
        "param": ["parm"],
        "value": ["val"]
      } 

# Titles and subtitle

    Code
      .
    Output
      {
        "group": [],
        "column": [],
        "label": {},
        "param": {},
        "value": {},
        "title": ["Test Title"],
        "subtitle": ["Also a test"]
      } 

# json row gorup plans

    Code
      .
    Output
      {
        "group": [],
        "column": [],
        "label": {},
        "param": {},
        "value": {},
        "row_grp_plan": {
          "struct_ls": [
            {
              "group_val": ["A", "C"],
              "block_to_apply": {
                "post_space": ["---"],
                "border": ["outline", "bottom"]
              }
            },
            {
              "group_val": ["B"],
              "block_to_apply": {
                "post_space": [""],
                "border": ["outline", "bottom"]
              }
            }
          ],
          "label_loc": {
            "location": ["column"],
            "indent": ["  "]
          }
        }
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "column": [],
        "label": {},
        "param": {},
        "value": {},
        "row_grp_plan": {
          "struct_ls": [
            {
              "group_val": {
                "grp1": ["A"],
                "grp2": ["b"]
              },
              "block_to_apply": {
                "post_space": [""],
                "border": ["outline", "bottom"]
              }
            }
          ],
          "label_loc": {
            "location": ["spanning"],
            "indent": ["  "]
          }
        }
      } 

# Elements with Qusosures (col_plan)

    Code
      .
    Output
      {
        "group": [],
        "column": [],
        "label": {},
        "param": {},
        "value": {},
        "col_plan": {
          "dots": [
            ["col1"],
            ["col2"],
            ["col3"]
          ],
          ".drop": [false]
        }
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "column": [],
        "label": {},
        "param": {},
        "value": {},
        "col_plan": {
          "dots": {
            "foo": ["col1"],
            "": ["col2"],
            "": ["col3"]
          },
          ".drop": [false]
        }
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "column": [],
        "label": {},
        "param": {},
        "value": {},
        "col_plan": {
          "dots": [
            ["starts_with(\"col\")"]
          ],
          ".drop": [false]
        }
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "column": ["span1", "col"],
        "label": {},
        "param": {},
        "value": {},
        "col_plan": {
          "dots": [
            {
              "span1": ["col 4"]
            }
          ],
          ".drop": [false]
        }
      } 

---

    Code
      .
    Output
      {
        "group": ["group"],
        "column": ["span2", "span1", "my_col"],
        "label": ["label"],
        "param": ["parm"],
        "value": ["val"],
        "body_plan": [
          {
            "group_val": [".default"],
            "label_val": [".default"],
            "param_val": [".default"],
            "frmt_to_apply": [
              {
                "expression": ["x"],
                "missing": {},
                "scientific": {}
              }
            ]
          }
        ],
        "col_plan": {
          "dots": {
            "": ["group"],
            "": ["label"],
            "": {
              "span1": ["col 4"]
            },
            "": {
              "span1": ["cols 1,2"],
              "my_col": ["col2", "col1"]
            },
            "": ["everything()"],
            "new_col_3": ["mycol3"],
            "": ["-mycol5"]
          },
          ".drop": [false]
        }
      } 

