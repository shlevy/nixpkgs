{ stdenv, fetchgit, bootPkgs, perl, gmp, ncurses, libiconv, binutils, coreutils
, autoconf, automake, happy, alex, target ? null, cross-tools ? null
}:

let
  inherit (bootPkgs) ghc;

in stdenv.mkDerivation rec {
  version = "8.1.20161109";
  name = "ghc-${version}";
  rev = "2e8463b232054b788b73e6551947a9434aa76009";

  /*
  src = fetchgit {
    url = "git://git.haskell.org/ghc.git";
    inherit rev;
    sha256 = "12nxai5qqnw42syhd0vzl2f9f8z28rc0fsa7g771dyzpqglak90l";
  };*/

  src = builtins.filterSource (name: type: builtins.baseNameOf name != ".git") /Users/shealevy/src/ghc;

  patches = [
    ./ghc-HEAD-dont-pass-linker-flags-via-response-files.patch   # https://github.com/NixOS/nixpkgs/issues/10752
  ];

  postUnpack = ''
    chmod -R +w ghc
    pushd ghc
    echo ${version} >VERSION
    echo ${rev} >GIT_COMMIT_ID
    patchShebangs .
    ./boot
    popd
  '';

  buildInputs = [ ghc perl autoconf automake happy alex ] ++ stdenv.lib.optional (target != null) cross-tools;

  enableParallelBuilding = true;

  preConfigure = ''
    sed -i -e 's|-isysroot /Developer/SDKs/MacOSX10.5.sdk||' configure
  '' + stdenv.lib.optionalString (!stdenv.isDarwin) ''
    export NIX_LDFLAGS="$NIX_LDFLAGS -rpath $out/lib/ghc-${version}"
  '' + stdenv.lib.optionalString stdenv.isDarwin ''
    export NIX_LDFLAGS+=" -no_dtrace_dof"
  '' + stdenv.lib.optionalString (target != null) ''
    sed 's|#BuildFlavour  = quick-cross|BuildFlavour  = quick-cross|' mk/build.mk.sample > mk/build.mk
  '';

  configureFlags = [
    "CC=${if target != null then cross-tools else stdenv.cc}/bin/${if target != null then "${target}-" else ""}cc"
  ] ++ stdenv.lib.optionals (target == null) [
    "--with-gmp-includes=${gmp.dev}/include" "--with-gmp-libraries=${gmp.out}/lib"
    "--with-curses-includes=${ncurses.dev}/include" "--with-curses-libraries=${ncurses.out}/lib"
  ] ++ stdenv.lib.optional (stdenv.isDarwin && target == null) [
    "--with-iconv-includes=${libiconv}/include" "--with-iconv-libraries=${libiconv}/lib"
  ] ++ stdenv.lib.optionals (target != null) [
    "--target=${target}"
  ];

  # required, because otherwise all symbols from HSffi.o are stripped, and
  # that in turn causes GHCi to abort
  stripDebugFlags = [ "-S" ] ++ stdenv.lib.optional (!stdenv.isDarwin) "--keep-file-symbols";

  postInstall = ''
    # Install the bash completion file.
    install -D -m 444 utils/completion/ghc.bash $out/share/bash-completion/completions/ghc

    # Patch scripts to include "readelf" and "cat" in $PATH.
    for i in "$out/bin/"*; do
      test ! -h $i || continue
      egrep --quiet '^#!' <(head -n 1 $i) || continue
      sed -i -e '2i export PATH="$PATH:${stdenv.lib.makeBinPath [ binutils coreutils ]}"' $i
    done
  '';

  passthru = {
    inherit bootPkgs;
  };

  meta = {
    homepage = "http://haskell.org/ghc";
    description = "The Glasgow Haskell Compiler";
    maintainers = with stdenv.lib.maintainers; [ marcweber andres peti ];
    inherit (ghc.meta) license platforms;
  };

}
