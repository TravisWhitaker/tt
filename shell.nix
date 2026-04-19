let p = builtins.fetchGit
    {
      url = "https://github.com/nixos/nixpkgs";
      ref = "master";
      rev = "eda1d4afa803c2d26cb820328cd9981556f9e448";
    };
in with import p {};
let hs = haskell.packages.ghc9103.ghcWithPackages
    (p: with p; [cabal-install haskell-language-server]);
in mkShell
{
    packages = [ hs
               ];
}
