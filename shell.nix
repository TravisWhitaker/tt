let p = builtins.fetchGit
    {
        url = "https://github.com/NixOS/nixpkgs";
        ref = "master";
        rev = "c97c47f2bac4fa59e2cbdeba289686ae615f8ed4";
    };
in with import p {};
let hs = haskell.packages.ghc9122.ghcWithPackages
    (p: with p; [cabal-install haskell-language-server]);
in mkShell
{
    packages = [ hs
               ];
}
