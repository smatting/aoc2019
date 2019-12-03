{
  pkgs = hackage:
    {
      packages = {
        "optics-core".revision = (((hackage."optics-core")."0.1").revisions).default;
        "exceptions".revision = (((hackage."exceptions")."0.10.3").revisions).default;
        "rio".revision = (((hackage."rio")."0.1.12.0").revisions).default;
        "binary".revision = (((hackage."binary")."0.8.6.0").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.3").revisions).default;
        "split".revision = (((hackage."split")."0.2.3.3").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.0").revisions).default;
        "case-insensitive".revision = (((hackage."case-insensitive")."1.2.1.0").revisions).default;
        "unliftio".revision = (((hackage."unliftio")."0.2.12").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "megaparsec".revision = (((hackage."megaparsec")."7.0.5").revisions).default;
        "megaparsec".flags.dev = false;
        "scientific".revision = (((hackage."scientific")."0.3.6.2").revisions).default;
        "scientific".flags.integer-simple = false;
        "scientific".flags.bytestring-builder = false;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "random".revision = (((hackage."random")."1.1").revisions).default;
        "optics-th".revision = (((hackage."optics-th")."0.1").revisions).default;
        "uuid-types".revision = (((hackage."uuid-types")."1.0.3").revisions).default;
        "optparse-applicative".revision = (((hackage."optparse-applicative")."0.15.0.0").revisions).default;
        "async".revision = (((hackage."async")."2.2.2").revisions).default;
        "async".flags.bench = false;
        "dlist".revision = (((hackage."dlist")."0.8.0.7").revisions).default;
        "conduit".revision = (((hackage."conduit")."1.3.1.1").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.3.0").revisions).default;
        "yaml".revision = (((hackage."yaml")."0.11.1.2").revisions).default;
        "yaml".flags.no-exe = true;
        "yaml".flags.no-examples = true;
        "transformers-compat".revision = (((hackage."transformers-compat")."0.6.5").revisions).default;
        "transformers-compat".flags.five = false;
        "transformers-compat".flags.generic-deriving = true;
        "transformers-compat".flags.two = false;
        "transformers-compat".flags.five-three = true;
        "transformers-compat".flags.mtl = true;
        "transformers-compat".flags.four = false;
        "transformers-compat".flags.three = false;
        "template-haskell".revision = (((hackage."template-haskell")."2.14.0.0").revisions).default;
        "mono-traversable".revision = (((hackage."mono-traversable")."1.0.12.0").revisions).default;
        "vector".revision = (((hackage."vector")."0.12.0.3").revisions).default;
        "vector".flags.unsafechecks = false;
        "vector".flags.internalchecks = false;
        "vector".flags.wall = false;
        "vector".flags.boundschecks = true;
        "primitive".revision = (((hackage."primitive")."0.7.0.0").revisions).default;
        "base-compat".revision = (((hackage."base-compat")."0.10.5").revisions).default;
        "time-compat".revision = (((hackage."time-compat")."1.9.2.2").revisions).default;
        "time-compat".flags.old-locale = false;
        "ansi-terminal".revision = (((hackage."ansi-terminal")."0.10").revisions).default;
        "ansi-terminal".flags.example = false;
        "tagged".revision = (((hackage."tagged")."0.8.6").revisions).default;
        "tagged".flags.transformers = true;
        "tagged".flags.deepseq = true;
        "unliftio-core".revision = (((hackage."unliftio-core")."0.1.2.0").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.0.1").revisions).default;
        "integer-logarithms".revision = (((hackage."integer-logarithms")."1.0.3").revisions).default;
        "integer-logarithms".flags.check-bounds = false;
        "integer-logarithms".flags.integer-gmp = true;
        "bytestring".revision = (((hackage."bytestring")."0.10.8.2").revisions).default;
        "ansi-wl-pprint".revision = (((hackage."ansi-wl-pprint")."0.6.9").revisions).default;
        "ansi-wl-pprint".flags.example = false;
        "optics".revision = (((hackage."optics")."0.1").revisions).default;
        "typed-process".revision = (((hackage."typed-process")."0.2.6.0").revisions).default;
        "raw-strings-qq".revision = (((hackage."raw-strings-qq")."1.1").revisions).default;
        "optics-extra".revision = (((hackage."optics-extra")."0.1").revisions).default;
        "parser-combinators".revision = (((hackage."parser-combinators")."1.2.0").revisions).default;
        "parser-combinators".flags.dev = false;
        "text".revision = (((hackage."text")."1.2.3.1").revisions).default;
        "unordered-containers".revision = (((hackage."unordered-containers")."0.2.10.0").revisions).default;
        "unordered-containers".flags.debug = false;
        "base".revision = (((hackage."base")."4.12.0.0").revisions).default;
        "time".revision = (((hackage."time")."1.8.0.2").revisions).default;
        "vector-algorithms".revision = (((hackage."vector-algorithms")."0.8.0.1").revisions).default;
        "vector-algorithms".flags.unsafechecks = false;
        "vector-algorithms".flags.internalchecks = false;
        "vector-algorithms".flags.llvm = false;
        "vector-algorithms".flags.boundschecks = true;
        "vector-algorithms".flags.bench = true;
        "vector-algorithms".flags.properties = true;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "hashable".revision = (((hackage."hashable")."1.3.0.0").revisions).default;
        "hashable".flags.sse2 = true;
        "hashable".flags.integer-gmp = true;
        "hashable".flags.sse41 = false;
        "hashable".flags.examples = false;
        "attoparsec".revision = (((hackage."attoparsec")."0.13.2.3").revisions).default;
        "attoparsec".flags.developer = false;
        "colour".revision = (((hackage."colour")."2.3.5").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "process".revision = (((hackage."process")."1.6.5.0").revisions).default;
        "libyaml".revision = (((hackage."libyaml")."0.1.1.0").revisions).default;
        "libyaml".flags.system-libyaml = false;
        "libyaml".flags.no-unicode = false;
        "resourcet".revision = (((hackage."resourcet")."1.2.2").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "microlens".revision = (((hackage."microlens")."0.4.11.2").revisions).default;
        "aeson".revision = (((hackage."aeson")."1.4.4.0").revisions).default;
        "aeson".flags.cffi = false;
        "aeson".flags.fast = false;
        "aeson".flags.bytestring-builder = false;
        "aeson".flags.developer = false;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.6.5").revisions).default;
        "base-orphans".revision = (((hackage."base-orphans")."0.8.1").revisions).default;
        "th-abstraction".revision = (((hackage."th-abstraction")."0.3.1.0").revisions).default;
        "array".revision = (((hackage."array")."0.5.3.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        };
      compiler = {
        version = "8.6.5";
        nix-name = "ghc865";
        packages = {
          "binary" = "0.8.6.0";
          "ghc-prim" = "0.5.3";
          "stm" = "2.5.0.0";
          "unix" = "2.7.2.2";
          "mtl" = "2.2.2";
          "rts" = "1.0";
          "deepseq" = "1.4.4.0";
          "directory" = "1.3.3.0";
          "template-haskell" = "2.14.0.0";
          "containers" = "0.6.0.1";
          "bytestring" = "0.10.8.2";
          "text" = "1.2.3.1";
          "base" = "4.12.0.0";
          "time" = "1.8.0.2";
          "transformers" = "0.5.6.2";
          "filepath" = "1.4.2.1";
          "process" = "1.6.5.0";
          "pretty" = "1.1.3.6";
          "ghc-boot-th" = "8.6.5";
          "array" = "0.5.3.0";
          "integer-gmp" = "1.0.2.0";
          };
        };
      };
  extras = hackage:
    { packages = { aoc2019 = ./.plan.nix/aoc2019.nix; }; };
  }