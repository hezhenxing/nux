{pkgs, lib, options, ...}:
 let
  inherit (builtins) readFile filter hasAttr;
  inherit (lib) pipe trim splitString mkMerge;
  splitLines = splitString "\n";
  readPkgs = f: pipe f [
    readFile
    splitLines
    (map trim)
    (filter (s: s != ""))
  ];
  addAuto = name:
    if hasAttr name options.programs
      then { programs.${name}.enable = true; }
      else { home.packages = [pkgs.${name}]; };
in pipe ./auto [
  readPkgs
  (map addAuto)
  mkMerge
]
