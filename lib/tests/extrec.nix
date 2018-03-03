lib: let inherit (lib) extrec; in

{ testSimpleExtrec =
    { expr =
        let base = extrec."{"{ x = 123;
                               y = self: self.x;
                           }"}";
            overrides = extrec."{"{ x = 456;
                                    z = self: self.y;
                                }"}";
        in extrec.fix (extrec.merge base overrides);
      expected = { x = 456; y = 456; z = 456; };
    };
  testNestedExtrec =
    { expr =
        let base = extrec."{"{ name = "hello";
                               options =
                                 extrec."{"{ enableGUI = extrec.annotate
                                               { type = extrec.types.bool;
                                                 description = "Whether to enable the GUI";
                                               } false;
                                         }"}";
                           }"}";
            overrides = extrec."{"{ options =
                                      extrec."{"{ enableGUI = parent: self: true;
                                              }"}";
                                }"}";
        in extrec.fix (extrec.merge base overrides);
      expected = { name = "hello"; options.enableGUI = true; };
    };
  testGrandparentExtrecReferences =
    { expr = extrec.fix (
        extrec."{"{ a = 1;
                    b = self: self.a;
                    c = extrec."{"{ d = parent: self: self.e.f;
                                    e = extrec."{"{ f = grandparent: parent: self: grandparent.b;
                                                }"}";
                                }"}";
                }"}");
      expected = { a = 1; b = 1; c = { d = 1; e = { f = 1; }; }; };
    };
  testAssign =
    { expr = extrec.fix (extrec.merge
        (extrec."{"{ a = 1;
                     b = extrec."{"{ c = parent: self: parent.a; }"}";
                     d = extrec.assign 1;
                 }"}")
        (extrec."{"{ a = extrec.assign (self: self.a + 1);
                     b = extrec.assign
                           (extrec."{"{ c = parent: self:
                                          parent.b.c + 1;
                                    }"}");
                     d = 2;
                 }"}"));
      expected = { a = 2; b.c = 3; d = 2; };
    };
  testWith =
    { expr = extrec.fix (extrec.merge
        (extrec."{"{ a = 1; b = self: self.a + 1; }"}")
        (extrec."{"{ b = extrec.with' (extrec."{"{ a = 2; }"}"); }"}")
      );
      expected = { a = 1; b = 3; };
    };
}
