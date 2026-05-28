{
  description = "calc development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems
          (system:
            f {
              pkgs = import nixpkgs { inherit system; };
            });
    in
    {
      devShells = forAllSystems ({ pkgs }:
        {
          default = pkgs.mkShell {
            packages = [
              pkgs.babashka
              pkgs.clojure
              pkgs.just
            ];
          };
        });
    };
}
