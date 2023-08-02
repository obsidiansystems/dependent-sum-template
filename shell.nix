{ system ? builtins.currentSystem
, ghc ? "ghc810" # or ghc961
}:
(import ./release.nix {}).${system}.${ghc}.env
