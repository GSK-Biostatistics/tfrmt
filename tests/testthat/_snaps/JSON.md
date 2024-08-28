# json basic tfrmt

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": []
      } 

---

    Code
      .
    Output
      {
        "group": ["group"],
        "label": ["label"],
        "param": ["parm"],
        "value": ["val"],
        "column": ["span2", "span1", "my_col"]
      } 

# json Titles and subtitle

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "title": ["Test Title"],
        "subtitle": ["Also a test"]
      } 

# json row group plans

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "row_grp_plan": {
          "struct_list": [
            {
              "group_val": ["A", "C"],
              "block_to_apply": {
                "post_space": ["---"],
                "fill": [true]
              }
            },
            {
              "group_val": ["B"],
              "block_to_apply": {
                "post_space": [" "],
                "fill": [true]
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
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "row_grp_plan": {
          "struct_list": [
            {
              "group_val": {
                "grp1": ["A"],
                "grp2": ["b"]
              },
              "block_to_apply": {
                "post_space": [" "],
                "fill": [true]
              }
            }
          ],
          "label_loc": {
            "location": ["spanning"],
            "indent": ["  "]
          }
        }
      } 

# json body plan

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "body_plan": [
          {
            "group_val": ["group1"],
            "label_val": [".default"],
            "param_val": [".default"],
            "frmt": {
              "expression": ["XXX"],
              "missing": {},
              "scientific": {},
              "transform": {}
            }
          }
        ]
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "body_plan": [
          {
            "group_val": {
              "grp_col1": ["group1"],
              "grp_col2": ["subgroup"]
            },
            "label_val": [".default"],
            "param_val": ["test"],
            "frmt": {
              "expression": ["XXX"],
              "missing": {},
              "scientific": {},
              "transform": {}
            }
          }
        ]
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "body_plan": [
          {
            "group_val": [".default"],
            "label_val": [".default"],
            "param_val": [".default"],
            "frmt_when": {
              "frmt_ls": {
                ">3": {
                  "frmt": {
                    "expression": ["(X.X%)"],
                    "missing": {},
                    "scientific": {},
                    "transform": {}
                  }
                },
                "<=3": {
                  "frmt": {
                    "expression": ["Undetectable"],
                    "missing": {},
                    "scientific": {},
                    "transform": {}
                  }
                }
              },
              "missing": {}
            }
          }
        ]
      } 

---

    Code
      frmt_when_scalar %>% as_json()
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "body_plan": [
          {
            "group_val": [".default"],
            "label_val": [".default"],
            "param_val": [".default"],
            "frmt_when": {
              "frmt_ls": {
                "<1": ["<1"],
                "==100": [""],
                "==0": [""],
                "TRUE": {
                  "frmt": {
                    "expression": ["(xx)"],
                    "missing": {},
                    "scientific": {},
                    "transform": {}
                  }
                }
              },
              "missing": {}
            }
          }
        ]
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "body_plan": [
          {
            "group_val": [".default"],
            "label_val": [".default"],
            "param_val": ["param1", "param2"],
            "frmt_combine": {
              "expression": ["{param1} {param2}"],
              "frmt_ls": {
                "param1": {
                  "frmt": {
                    "expression": ["XXX %"],
                    "missing": {},
                    "scientific": {},
                    "transform": {}
                  }
                },
                "param2": {
                  "frmt": {
                    "expression": ["XX.XXX"],
                    "missing": {},
                    "scientific": {},
                    "transform": {}
                  }
                }
              },
              "missing": {}
            }
          }
        ]
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "body_plan": [
          {
            "group_val": ["group1"],
            "label_val": [".default"],
            "param_val": [".default"],
            "frmt": {
              "expression": ["xx.xx"],
              "missing": {},
              "scientific": ["x10^xx"],
              "transform": {}
            }
          }
        ]
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "body_plan": [
          {
            "group_val": [".default"],
            "label_val": [".default"],
            "param_val": ["param1", "param2"],
            "frmt_combine": {
              "expression": ["{param1} {param2}"],
              "frmt_ls": {
                "param1": {
                  "frmt": {
                    "expression": ["XXX %"],
                    "missing": {},
                    "scientific": {},
                    "transform": {}
                  }
                },
                "param2": {
                  "frmt_when": {
                    "frmt_ls": {
                      ">3": {
                        "frmt": {
                          "expression": ["(X.X%)"],
                          "missing": {},
                          "scientific": {},
                          "transform": {}
                        }
                      },
                      "<=3": {
                        "frmt": {
                          "expression": ["Undetectable"],
                          "missing": {},
                          "scientific": {},
                          "transform": {}
                        }
                      }
                    },
                    "missing": {}
                  }
                }
              },
              "missing": [""]
            }
          },
          {
            "group_val": ["test1"],
            "label_val": [".default"],
            "param_val": ["foo"],
            "frmt": {
              "expression": ["xx.x"],
              "missing": ["--"],
              "scientific": {},
              "transform": {}
            }
          },
          {
            "group_val": [".default"],
            "label_val": [".default"],
            "param_val": [".default"],
            "frmt_when": {
              "frmt_ls": {
                ">0.4": {
                  "frmt": {
                    "expression": ["(X.X%)"],
                    "missing": [""],
                    "scientific": {},
                    "transform": {}
                  }
                },
                "<=0.4": {
                  "frmt_combine": {
                    "expression": ["[{par2m1}-{param2}]"],
                    "frmt_ls": {
                      "par2m1": {
                        "frmt": {
                          "expression": ["XXX"],
                          "missing": {},
                          "scientific": {},
                          "transform": {}
                        }
                      },
                      "param2": {
                        "frmt": {
                          "expression": ["XXX"],
                          "missing": {},
                          "scientific": {},
                          "transform": {}
                        }
                      }
                    },
                    "missing": {}
                  }
                }
              },
              "missing": {}
            }
          }
        ]
      } 

# json big n

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "big_n": {
          "param_val": ["bigN"],
          "n_frmt": {
            "expression": ["\nN = xx"],
            "missing": {},
            "scientific": {},
            "transform": {}
          },
          "by_page": [false]
        }
      } 

# json footnote plan

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "footnote_plan": {
          "struct_list": [
            {
              "column_val": {},
              "group_val": {},
              "label_val": {},
              "footnote_text": ["Source Note"]
            },
            {
              "column_val": ["PL"],
              "group_val": {},
              "label_val": {},
              "footnote_text": ["Placebo"]
            }
          ],
          "marks": ["standard"]
        }
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "footnote_plan": {
          "struct_list": [
            {
              "column_val": {
                "column": ["T1", "T2", "T1&T2"]
              },
              "group_val": {},
              "label_val": {},
              "footnote_text": ["All Treatments"]
            }
          ],
          "marks": ["numbers"]
        }
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "footnote_plan": {
          "struct_list": [
            {
              "column_val": {},
              "group_val": ["group 1"],
              "label_val": ["label 1"],
              "footnote_text": ["Footnote goes here"]
            }
          ],
          "marks": ["numbers"]
        }
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "footnote_plan": {
          "struct_list": [
            {
              "column_val": {
                "span": ["Treatment"],
                "column": ["T1&T2"]
              },
              "group_val": {},
              "label_val": {},
              "footnote_text": ["Footnote goes here"]
            }
          ],
          "marks": ["numbers"]
        }
      } 

# json col_plan

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "col_plan": {
          "col_plan": {
            "dots": [
              ["col1"],
              ["col2"],
              ["col3"]
            ],
            ".drop": [false]
          }
        }
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "col_plan": {
          "col_plan": {
            "dots": {
              "foo": ["col1"],
              "": ["col2"],
              "": ["col3"]
            },
            ".drop": [false]
          }
        }
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "col_plan": {
          "col_plan": {
            "dots": [
              ["starts_with(\"col\")"]
            ],
            ".drop": [false]
          }
        }
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": ["span1", "col"],
        "col_plan": {
          "col_plan": {
            "dots": [
              {
                "span_structure": {
                  "span1": ["col 4"]
                }
              }
            ],
            ".drop": [false]
          }
        }
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": ["span2", "span1", "my_col"],
        "col_plan": {
          "col_plan": {
            "dots": {
              "": ["group"],
              "": ["label"],
              "": {
                "span_structure": {
                  "span1": ["col 4"]
                }
              },
              "": {
                "span_structure": {
                  "span1": ["cols 1,2"],
                  "my_col": ["col2", "col1"]
                }
              },
              "": {
                "span_structure": {
                  "span2": ["Top Label Level 1"],
                  "span1": ["Second Label Level 1.1"],
                  "my_col": ["col_3", "col_4"]
                }
              },
              "": {
                "span_structure": {
                  "span2": ["Top Label Level 1"],
                  "span1": ["col2", "col1"],
                  "my_col": ["starts_with(\"B\")"]
                }
              },
              "": {
                "span_structure": {
                  "span2": ["Top Label Level 1"],
                  "my_col": ["col_5"]
                }
              },
              "": {
                "span_structure": {
                  "span2": ["Top Label Level 2"],
                  "my_col": ["col_6", "col_7"]
                }
              },
              "": ["everything()"],
              "new_col_3": ["mycol3"],
              "": ["-mycol5"]
            },
            ".drop": [false]
          }
        }
      } 

# json col_style_plan

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "col_style_plan": [
          {
            "cols": [
              ["my_var"]
            ],
            "align": ["left"],
            "type": ["char"],
            "width": [100]
          },
          {
            "cols": [
              ["four"]
            ],
            "align": ["right"],
            "type": ["char"],
            "width": {}
          },
          {
            "cols": [
              ["two"],
              ["three"]
            ],
            "align": [".", ",", " "],
            "type": ["char"],
            "width": {}
          },
          {
            "cols": [
              {
                "span_structure": {
                  "span": ["value"],
                  "col": ["val2"]
                }
              }
            ],
            "align": {},
            "type": ["char"],
            "width": [25]
          }
        ]
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "col_style_plan": [
          {
            "cols": [
              ["my var"]
            ],
            "align": ["left"],
            "type": ["char"],
            "width": [100]
          }
        ]
      } 

# json page plan

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "page_plan": {
          "struct_list": [
            {
              "group_val": [".default"],
              "label_val": {}
            },
            {
              "group_val": {},
              "label_val": ["A"]
            }
          ],
          "note_loc": ["source_note"],
          "max_rows": {}
        }
      } 

---

    Code
      .
    Output
      {
        "group": [],
        "label": {},
        "param": {},
        "value": {},
        "column": [],
        "page_plan": {
          "struct_list": [],
          "note_loc": ["noprint"],
          "max_rows": [5]
        }
      } 

