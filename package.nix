{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "flipChartBasics";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = ''Basic functions to be used by other charting packages (i.e., flipPlots, flipStandardCharts, flipPictographs). flipChartBasics is meant for internal functions. It should not be called directly by users.'';
  propagatedBuildInputs = with pkgs.rPackages; [ 
    flipU
    verbs
    colorRamps
    flipFormat
    colorspace
    RColorBrewer
    flipTransformations
  ];
}
