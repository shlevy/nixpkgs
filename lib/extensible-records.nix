# This is a pure library implementation of Eelco Dolstra's extensible
# attribute set proposal, currently at https://gist.github.com/edolstra/29ce9d8ea399b703a7023073b0dbc00d.
# This is *not* intended to exist long-term: The goal is to validate
# and, if need be, iterate on Eelco's design by applying it to
# real-world package management and then make the proper language fix.
# As such, parts of the interface are fairly janky. You have been
# warned!
{ lib }:
let isExtrec = x: (x.type or null) == "extrec";
    call = f: if lib.isFunction f then x: call (f x) else f;
    throwIf = pred: msg: if pred then throw msg else call;
    throwUnless = pred: throwIf (!pred);
    checkExtrec = val: name:
      throwUnless (isExtrec val) "${name} is not an extrec";
    inherit (lib) extrec mapAttrs;
    inherit (extrec) merge fix;
in { # Create an extensible record. Uses should look syntactically like:
     #   extrec."{"{ a = 1; b = self: self.c; }"}"
     #
     # If an attribute is a function, its first argument will be the
     # finally fully merged and fixed set (this is where the
     # extensibility comes from). In a nested extensible record*,
     # function attributes will first take all the parent records as
     # arguments in order. So:
     #   fix ({{ a = 1;
     #           b = self: self.a;
     #           c = {{ d = parent: self: self.e.f;
     #                  e = {{ f = grandparent: parent: self: grandparent.b;
     #                      }};
     #               }};
     #        }}) => { a = 1; b = 1; c.d = 1; c.e.f = 1; }
     # In the real implementation, these extraneous arguments will not
     # be necessary as variable scoping will address it.
     #
     # * Nested extensible records can't work right if their *shape*
     # (i.e. whether they are actually extrecs at all or what attrs
     # they are defined with) depends on the final fixpoint, as that
     # would require the nested record attribute itself to take the
     # fixpoint argument which would in turn require delaying merging
     # until the fixpoint to do correctly, which doesn't seem worth it.
     "{" = set: close:
        throwUnless (close == "}") "mis-matched extrec.\"{\"{ bracket: Expected }\"}\", got }${close}"
        { type = "extrec"; inherit set; };

     # // for extrecs.
     # Behaves just like //, except if an attribute is an extrec
     # itself then nested merging occurs.
     merge = lhs: rhs:
       let dups = builtins.intersectAttrs lhs.set rhs.set;
           mergeDup = name: rhs': let lhs' = lhs.set.${name}; in
             if isExtrec lhs'
               then checkExtrec rhs' "nested extrec override ${name}"
                    merge lhs' rhs'
             else rhs';
           merged = mapAttrs mergeDup dups;
       in checkExtrec lhs "LHS of extrec.merge"
          checkExtrec rhs "RHS of extrec.merge"
          { type = "extrec"; set = lhs.set // rhs.set // merged; };

     # Fix an extensible record into a normal attribute set.
     #
     # See the description of "{" for how attributes get access to the
     # result of the fixpoint.
     #
     # Nested extensible records are fixed simultaneously. This allows
     # siblings to properly reference each other through the parent
     # fixpoint.
     fix = set:
       let fixAttr = name: attr:
             if isExtrec attr
               then { type = "extrec"; set = (mapAttrs fixAttr attr.set); }
             else if lib.isFunction attr
               then let res = attr final; in
                 throwIf (isExtrec res)
                   "nested extensible records should access the fixpoint from individual arguments"
                   res
             else attr;
           recurseFixAttr = name: attr:
             let attr' = fixAttr name attr; in
               if isExtrec attr' then fix attr' else attr';
           final = mapAttrs recurseFixAttr set.set;
       in checkExtrec set "extrec fixpoint argument"
          final;
   }
