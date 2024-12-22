{ pkgs, ... }:
{
  projectRootFile = "flake.nix";
  programs.nixpkgs-fmt = {
    enable = true;
    package = pkgs.nixfmt-rfc-style;
  };
}
