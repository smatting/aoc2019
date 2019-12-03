{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.12";
      identifier = { name = "aoc2019"; version = "0.1.0.0"; };
      license = "NONE";
      copyright = "";
      maintainer = "";
      author = "";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.filepath)
          (hsPkgs.megaparsec)
          (hsPkgs.transformers)
          (hsPkgs.mtl)
          (hsPkgs.optparse-applicative)
          (hsPkgs.parser-combinators)
          (hsPkgs.vector)
          (hsPkgs.yaml)
          (hsPkgs.raw-strings-qq)
          (hsPkgs.optics)
          (hsPkgs.rio)
          ];
        };
      exes = {
        "aoc2019" = {
          depends = [
            (hsPkgs.aoc2019)
            (hsPkgs.base)
            (hsPkgs.filepath)
            (hsPkgs.optparse-applicative)
            ];
          libs = (pkgs.lib).optional (system.isOsx) (pkgs."iconv");
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././../.; }