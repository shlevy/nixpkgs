# This is a pure library implementation of Eelco Dolstra's extensible
# attribute set proposal, currently at https://gist.github.com/edolstra/29ce9d8ea399b703a7023073b0dbc00d.
# This is *not* intended to exist long-term: The goal is to validate
# and, if need be, iterate on Eelco's design by applying it to
# real-world package management and then make the proper language fix.
# As such, parts of the interface are fairly janky. You have been
# warned!
{ lib }:
let isExtrec = x: (x.type or null) == "extrec";
    isAnnotation = x: (x.type or null) == "extrec.annotation";
    call = f: if lib.isFunction f then x: call (f x) else f;
    throwIf = pred: msg: if pred then throw msg else call;
    throwUnless = pred: throwIf (!pred);
    checkExtrec = val: name:
      throwUnless (isExtrec val) "${name} is not an extrec";
    inherit (lib) extrec mapAttrs types;
    inherit (extrec) merge fix;
    mergeLastOption = _: defs:
      (builtins.elemAt defs ((builtins.length defs) - 1)).value;
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

     # Annotate a field of an extensible record.
     #
     # Annotations can be arbitrary name-value pairs. Currently only
     # 'type', via the 'merge' and 'check' fields, actually affects
     # this library, but you should probably also set 'description'.
     #
     # Annotations can only appear the first time a given attribute is
     # defined in a sequence of merges.
     #
     # 'type' values should be of the same form as those in lib.types.
     #
     # TODO lib.types seems more complicated than we need...
     annotate = annotation: value:
       let valid-type = lib.isOptionType type;
           type = annotation.type or types.unspecified;
       in throwUnless valid-type
         "The type given in an annotation must be a proper nixpkgs lib type"
         throwIf (isExtrec value) "Can't annotate an extensible record itself"
         throwIf (isAnnotation value) "Can't annotate an annotation"
         throwUnless (type.check value) "Annotated value doesn't have type ${type.name}"
         { type = "extrec.annotation";
           inherit annotation;
           defs = [ { inherit value; file = "initial annotation definition"; } ];
         };

     # // for extrecs.
     #
     # Behaves just like //, except if an attribute is an extrec
     # itself then nested merging occurs.
     merge = lhs: rhs:
       let dups = builtins.intersectAttrs lhs.set rhs.set;
           mergeDup = name: rhs':
             let lhs' = lhs.set.${name};
                 pos = builtins.unsafeGetAttrPos name rhs.set;
             in
               throwIf (isAnnotation rhs') "can only set annotation on the first occurrence of an attribute in a sequence of merges" (
               if isExtrec lhs'
                 then checkExtrec rhs' "nested extrec override ${name}"
                      merge lhs' rhs'
               else if isAnnotation lhs'
                 then
                   let type = lhs'.annotation.type or types.unspecified;
                       file = "${pos.file}:${toString pos.line}:${toString pos.column}";
                   in throwUnless (type.check rhs') "Value at ${file} does not have type ${type.name}"
                        lhs' //
                          # Ugh, module system types don't have incremental merge.
                         { defs = lhs'.defs ++
                             [ { value = rhs'; inherit file; } ];
                         }
               else rhs');
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
     fix = set: let fix' = set: attrPath:
       let fixAttr = name: attr: let newAttrPath = attrPath ++ [ name ]; in
             if isExtrec attr
               then { type = "extrec"; set = (mapAttrs fixAttr attr.set); }
             else if isAnnotation attr
               then let type = attr.annotation.type or null;
                        inherit (attr) defs;
                        bad-values = builtins.filter (x: !(type.check x.value)) defs;
                        bad-value-pos = (builtins.head bad-values).file;
                    in if type == null
                         then fixAttr name (builtins.elemAt defs ((builtins.length defs) - 1)).value
                       else if bad-values != []
                         then throw "Value ${builtins.concatStringsSep "." newAttrPath} does not have type ${type.name}, at ${bad-value-pos}."
                       else # technically someone could sneak in e.g. a nested extensible record here via the merge function. Don't do that.
                           fixAttr name (type.merge newAttrPath defs)
             else if lib.isFunction attr
               then let res = attr final; in
                 throwIf (isExtrec res)
                   "nested extensible records should access the fixpoint from individual arguments"
                   res
             else attr;
           recurseFixAttr = name: attr:
             let attr' = fixAttr name attr; in
               if isExtrec attr' then fix' attr' (attrPath ++ [ name ]) else attr';
           final = mapAttrs recurseFixAttr set.set;
       in checkExtrec set "extrec fixpoint argument"
          final; in fix' set [];

     # Types more appropriate for extensible records than the module
     # system types.
     types.bool = types.bool // { merge = mergeLastOption; };
   }
