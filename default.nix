# default.nix
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  overrides = self: super: {
    reflex-dom-canvas = pkgs.haskell.lib.dontHaddock (self.callCabal2nix "reflex-dom-canvas" (pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo  = "reflex-dom-canvas";
      sha256 = "12ixx1q09pskxszpd67gy5y5zcyxyl5amp1ll35vazcji9q7r299";
      rev = "b3f67cfa8efba208120a5c634ab8c226d3507873";
    }) {});
  };

  packages = {
    common = ./common;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["common" "frontend"];
    ghcjs = ["common" "frontend"];
  };
})
