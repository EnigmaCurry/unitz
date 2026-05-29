{
  description = "calc - unit conversion and calculator";

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
      packages = forAllSystems ({ pkgs }:
        let
          calc = pkgs.writeShellScriptBin "calc" ''
            exec ${pkgs.babashka}/bin/bb --classpath ${self}/src \
              -e "(require '[calc.cli :as cli]) (apply cli/-main *command-line-args*)" \
              -- "$@"
          '';
        in {
          default = calc;
          calc = calc;
        });

      devShells = forAllSystems ({ pkgs }:
        {
          default = pkgs.mkShell {
            packages = [
              pkgs.babashka
              pkgs.clojure
              pkgs.jdk
              pkgs.just
            ];
          };
        });
    };
}
