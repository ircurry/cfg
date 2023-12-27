{ pkgs, ... }: {
  config = {
    home.packages = [ pkgs.nerdfonts pkgs.swaylock-effects ];

    xdg.configFile."swaylock/config" = {
      enable = true;
      text = ''
        ignore-empty-password
        font="Fira Sans Semibold"
        
        clock
        timestr=%T
        datestr=%a, %e of %b

        indicator
        indicator-radius=125

        # Background Color
        color=2e3440

        # Normal Colors
        text-color=d8dee9
        inside-color=3b4252
        line-color=2e3440
        separator-color=2e3440
        ring-color=434c5e
        key-hl-color=5e81ac
        bs-hl-color=4c566a
        
        # Cleared Colors
        text-clear-color=ebcb8b
        inside-clear-color=3b4252
        line-clear-color=2e3440
        ring-clear-color=ebcb8b
        
        # Verifying Colors
        text-ver-color=d8dee9
        inside-ver-color=3b4252
        line-ver-color=2e3440
        ring-ver-color=5e81ac
        
        # Wrong Colors
        text-wrong-color=bf616a
        inside-wrong-color=3b4252
        line-wrong-color=2e3440
        ring-wrong-color=bf616a

        # Caps-lock Colors
        text-caps-lock-color=bf616a
        inside-caps-lock-color=3b4252
        line-caps-lock-color=2e3440
        ring-caps-lock-color=434c5e
        caps-lock-key-hl-color=bf616a
        caps-lock-bs-hl-color=4c566a
      '';
    };
  };
}
