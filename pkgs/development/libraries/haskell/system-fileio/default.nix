# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, chell, systemFilepath, temporary, text, time, transformers
}:

cabal.mkDerivation (self: {
  pname = "system-fileio";
  version = "0.3.16";
  sha256 = "1x18ffhas6bhjis0glf0xd6ap8vy7cap8lkmnkn4px83d82yzi8k";
  buildDepends = [ systemFilepath text time ];
  testDepends = [
    chell systemFilepath temporary text time transformers
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/fpco/haskell-filesystem";
    description = "Consistent filesystem interaction across GHC versions";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
