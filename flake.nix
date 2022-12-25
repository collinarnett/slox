{
  description = "Implementation of Lox from the book, Crafting Interpreters by Robert Nystrom.";

  inputs = {
    sbt-derivation.url = "github:zaninime/sbt-derivation";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };
  outputs = {
    self,
    nixpkgs,
    sbt-derivation,
  }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    packageName = "slox";
    version = "0.1";
    build = sbt-derivation.mkSbtDerivation.${system} {
      pname = "${packageName}";
      version = "${version}";
      depsSha256 = "sha256-ZMqRgQueCFwqieLrJw9y8pWYDxKNHTK23vlYJUGT+RQ=";
      src = ./.;
      buildPhase = ''
        sbt assembly
      '';
      installPhase = ''
        mkdir -p $out/bin
        cp target/scala-*/*-assembly-*.jar $out/bin/
      '';
    };
  in {
    packages.${system}.default = build;
    devShells.${system}.default = pkgs.mkShell {
      buildInputs = with pkgs; [sbt jdk scalafmt metals bloop];
    };
  };
}
