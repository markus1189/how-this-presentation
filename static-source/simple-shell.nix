{ pkgs ? import <nixpkgs> {}}: with pkgs;

mkShell {
  buildInputs = [
    (texlive.combine {
      inherit (texlive)
      beamer;
    })
    pythonPackages.pygments
    graphviz
    imagemagick
    (ghc.withPackages (p: with p; [shake dhall]))
  ];
}
