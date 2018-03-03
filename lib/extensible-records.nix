# This is a pure library implementation of Eelco Dolstra's extensible
# attribute set proposal, currently at https://gist.github.com/edolstra/29ce9d8ea399b703a7023073b0dbc00d.
# This is *not* intended to exist long-term: The goal is to validate
# and, if need be, iterate on Eelco's design by applying it to
# real-world package management and then make the proper language fix.
# As such, parts of the interface are fairly janky. You have been
# warned!
{ lib }:
let hasType = t: x: (x.type or null) == t;
    isExtrec = hasType "extrec";
    isAnnotation = hasType "extrec.annotation";
    isAssign = hasType "extrec.assign";
    isWith = hasType "extrec.with";
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
     # Nested extensible records cannot be annotated, but their fields
     # can.
     #
     # 'type' values should be of the same form as those in lib.types.
     #
     # TODO lib.types seems more complicated than we need...
     # TODO Do we need to allow annotating nested extrecs.
     annotate = annotation: value:
       let valid-type = lib.isOptionType type;
           type = annotation.type or types.unspecified;
       in throwUnless valid-type
         "The type given in an annotation must be a proper nixpkgs lib type"
         throwIf (isExtrec value) "Can't annotate an extensible record itself"
         throwIf (isAnnotation value) "Can't annotate an annotation"
         throwIf (isAssign value) "Can't annotate an assignment, assign the annotation instead"
         throwIf (isWith value) "Can't annotate a with, did you want to annotate within the with?"
         throwUnless (type.check value) "Annotated value doesn't have type ${type.name}"
         { type = "extrec.annotation";
           inherit annotation;
           defs = [ { inherit value; file = "initial annotation definition"; } ];
         };

     # assign an attribute to a specified value, ignoring previous definitons.
     #
     # In addition to allowing overriding the merging behavior of a
     # given attribute, assignment also allows access to the previous*
     # value of the assigned field, through the fixpoint at the level
     # the assignment occurred. So:
     #   fix({{ a = 1; b = {{ c = parent: self: parent.a; }}; }} //
     #       {{ a := self: self.a + 1;
     #          b := {{ c = parent: self: parent.b.c + 1; }};
     #       }}
     #      ) => { a = 2; b.c = 3; }
     #
     # Unless they are annotated themselves, assigned values inherit
     # the field's preexisting annotation.
     #
     # * If the previous value is calculated via the fixpoint, it will
     #   see the final fixpoint, *not* what it would have seen had the
     #   final fixpoint occurred before the assignment was merged in!
     assign = value:
      throwIf (isWith value) "can't assign a with, you either want to assign *before* a with or *within* a with"
      { inherit value; type = "extrec.assign"; };

     # Override a sibling for the scope of an attribute.
     #
     # fix ({{ a = 1; b = a + 1; }} // {{ b = {{ with a = 2; }} }}) =>
     #   { a = 1; b = 3; }
     #
     # 'with' overrides are merged as usual with the previous values
     # of the sibling. Use assignment inside the with if you want to
     # ignore them completely. They are evaluated in the same scope
     # as the attribute itself.
     #
     # assignment removes previous 'with's, as presumably you could
     # just reference what you wanted directly if you're assigning.
     # For similar reasons, the first value of an attribute in a merge
     # sequence can't be a 'with'.
     #
     # !!! There are likely weird semantic corner-cases currently!
     with' = withs:
       { inherit withs; type = "extrec.with"; };

     # // for extrecs.
     #
     # If a given field is an extrec, they are recursively merged. If
     # a given field has an annotation with a type, the type's merge
     # function is used. If a given field is assigned on the RHS, it
     # takes priority regardless of merge rules. If a field is a with,
     # its scoping overrides take effect for that field.
     merge = lhs: rhs:
       let dups = builtins.intersectAttrs lhs.set rhs.set;
           mergeDup = name: rhs': let mergeDup' = name: rhs': lhs': topWith:
             let mergeDup'' = name: rhs': lhs':
                   mergeDup' name rhs' lhs' topWith;
                 pos = builtins.unsafeGetAttrPos name rhs.set;
                 annotatedAssignedRhs =
                   if !(isAnnotation rhs'.value) && (isAnnotation lhs')
                     then extrec.annotate lhs'.annotation rhs'.value
                   else rhs'.value;
                 rhs'' = rhs' //
                   { value = annotatedAssignedRhs;
                     previous = lhs';
                   };
             in if isAssign rhs' then rhs'' else
               throwIf (isAnnotation rhs') "can only set annotation on the first occurrence of an attribute in a sequence of merges" (
               if isWith lhs'
                 then let inner = mergeDup' name rhs' lhs'.value false; in
                   if !(lhs' ? value)
                     then throw "you can only use 'with' to override scoping on a previously set value"
                   else if isWith inner
                     then lhs' // inner //
                       { withs = merge lhs'.withs inner.withs; }
                   else lhs' // { value = inner; }
               else if isWith rhs'
                 then { type = "extrec.with";
                        value = if rhs' ? value
                                  then mergeDup'' name rhs'.value lhs'
                                else lhs';
                        withs = if topWith
                                  then merge final rhs'.withs
                                else rhs'.withs;
                      }
               else if isAssign lhs'
                 then lhs' //
                   { value = mergeDup'' name rhs' lhs'.value; }
               else if isExtrec lhs'
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
           in mergeDup' name rhs' lhs.set.${name} true;
           merged = mapAttrs mergeDup dups;
           final = { type = "extrec";
                     set = lhs.set // rhs.set // merged;
                   };
       in checkExtrec lhs "LHS of extrec.merge"
          checkExtrec rhs "RHS of extrec.merge"
          final;

     # Fix an extensible record into a normal attribute set.
     #
     # See the description of "{" for how attributes get access to the
     # result of the fixpoint.
     #
     # Nested extensible records are fixed simultaneously. This allows
     # siblings to properly reference each other through the parent
     # fixpoint.
     fix = set: let fix' = set: attrPath:
       let fixAttr = final: name: attr:
             let newAttrPath = attrPath ++ [ name ]; in
               if isWith attr
                 then if !(attr ? value)
                        then throw "you can only use 'with' to override scoping on a previously set value"
                      else
                           let scope =
                                 mapAttrs
                                   (fixAttr final)
                                   attr.withs.set;
                           in fixAttr scope name attr.value
               else if isAssign attr
                 then fixAttr
                   (if attr ? previous
                     then final //
                       { "${name}" =
                           recurseFixAttr final name attr.previous;
                       }
                   else removeAttrs final [ name ])
                   name attr.value
               else if isExtrec attr
                 then { type = "extrec";
                        set = (mapAttrs (fixAttr final) attr.set);
                      }
               else if isAnnotation attr
                 then let type = attr.annotation.type or null;
                          inherit (attr) defs;
                          bad-values =
                            builtins.filter
                              (x: !(type.check x.value))
                              defs;
                        bad-value-pos =
                          (builtins.head bad-values).file;
                        last =
                          builtins.elemAt
                            defs
                            ((builtins.length defs) - 1);
                    in if type == null
                         then fixAttr final name last.value
                       else if bad-values != []
                         then throw "Value ${builtins.concatStringsSep "." newAttrPath} does not have type ${type.name}, at ${bad-value-pos}."
                       else # technically someone could sneak in e.g. a nested extensible record here via the merge function. Don't do that.
                           fixAttr final name (type.merge newAttrPath defs)
             else if lib.isFunction attr
               then let res = attr final; in
                 throwIf (isExtrec res)
                   "nested extensible records should access the fixpoint from individual arguments"
                   res
             else attr;
           recurseFixAttr = final: name: attr:
             let attr' = fixAttr final name attr; in
               if isExtrec attr' then fix' attr' (attrPath ++ [ name ]) else attr';
           final = mapAttrs (recurseFixAttr final) set.set;
       in checkExtrec set "extrec fixpoint argument"
          final; in fix' set [];

     # Types more appropriate for extensible records than the module
     # system types.
     types.bool = types.bool // { merge = mergeLastOption; };
   }
