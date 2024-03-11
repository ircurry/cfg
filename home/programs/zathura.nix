{ config, lib, pkgs, ... }:

let
  cfg = config.nocturne.graphical.zathura;
in
{
  config = lib.mkIf cfg.enable {
    programs.zathura = {
      enable = true;
      extraConfig = ''
        # General settings
        set selection-clipboard clipboard
        
        # Keybindings normal
        map u scroll half-up
        map d scroll half-down
        map D toggle_page_mode
        map r reload
        map R rotate
        map i recolor
        map p print
        map g goto top
        
        # Keybindings fullscreen
        map [fullscreen] u scroll half-up
        map [fullscreen] d scroll half-down
        map [fullscreen] D toggle_page_mode
        map [fullscreen] r reload
        map [fullscreen] R rotate
        map [fullscreen] i recolor
        map [fullscreen] p print
        map [fullscreen] g goto top
        
        # Colors
        set notification-error-bg       "#2E3440"
        set notification-error-fg       "#BF616A"
        set notification-warning-bg     "#2E3440"
        set notification-warning-fg     "#D08770"
        set notification-bg             "#2E3440"
        set notification-fg             "#D8DEE9"
        
        set completion-bg               "#2E3440"
        set completion-fg               "#D8DEE9"
        set completion-group-bg         "#3B4252"
        set completion-group-fg         "#D8DEE9"
        set completion-highlight-bg     "#88C0D0"
        set completion-highlight-fg     "#3B4252"
        
        set index-bg                    "#2E3440"
        set index-fg                    "#8FBCBB"
        set index-active-bg             "#8FBCBB"
        set index-active-fg             "#2E3440"
        
        set inputbar-bg                 "#2E3440"
        set inputbar-fg                 "#E5E9F0"
        
        set statusbar-bg                "#2E3440"
        set statusbar-fg                "#E5E9F0"
        
        set highlight-color             "#D08770"
        set highlight-active-color      "#BF616A"
        
        set default-bg                  "#2E3440"
        set default-fg                  "#D8DEE9"
        set render-loading              "true"
        set render-loading-bg           "#2E3440"
        set render-loading-fg           "#434C5E"
        
        set recolor-lightcolor          "#2E3440"
        set recolor-darkcolor           "#ECEFF4"
        set recolor                     "true"
      '';

    };
  };

}
