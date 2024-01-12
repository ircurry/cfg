{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.nocturne.cli.lf.enable {
    home.packages = [ pkgs.nerdfonts pkgs.bzip2 pkgs.gzip pkgs.xz ];
    xdg.configFile."lf/icons".source = ./icons;
    programs.lf = {
      enable = true;
      extraConfig = let
        lf-scope = pkgs.writeShellScriptBin "lf-scope" ''
          ft="$(${pkgs.file}/bin/file -Lb --mime-type "$1")"
          case "$ft" in
          	image/*) ${pkgs.chafa}/bin/chafa --animate off --size "$2x$3" "$1";;
          	*) ${pkgs.pistol}/bin/pistol "$1" ;;
          esac
        '';
      in ''
        set previewer ${lf-scope}/bin/lf-scope
      '';
      settings = {
        preview = true;
        drawbox = true;
        icons = true;
      };
      commands = {
        open = ''
        ''${{
          case $(file --mime-type "$f" -bL) in
            application/pdf|application/vnd.djvu|application/epub*) ${pkgs.zathura}/bin/zathura "$fx" ;;
            text/*|application/json|inode/x-empty|application/x-subrip) ${config.programs.emacs.package}/bin/emacsclient -r "$f" ;;
            # image/*) setsid -f ${pkgs.imv}/bin/imv-dir "$f" >/dev/null 2>&1 ;;
            image/*) ${pkgs.imv}/bin/imv-dir "$f" ;;
            audio/*|video/x-ms-asf) ${pkgs.mpv}/bin/mpv --audio-display=no "$f" ;;
            video/*) ${pkgs.mpv}/bin/mpv "$f" -quiet ;;
            *) xdg-open "$f" ;;
          esac
        }}
        '';
        browser-open = ''$setsid -f ${pkgs.firefox}/bin/firefox "$f" >/dev/null 2>&1'';
        mkdir = ''$mkdir -p "$(echo $* | tr ' ' '\ ')"'';
        mkscript = ''
        %{{
          printf "Name of script: "
          read ans
          touch "$ans" && chmod +x "$ans" && echo "done"
        }}'';
        chmod = ''
        ''${{
          printf "Mode Bits: "
          read ans

          for file in $fx
          do
            chmod "$ans" "$file"
          done

          lf -remote 'send reload'
        }}'';
        opendirimages = ''$setsid -f ${pkgs.imv}/bin/imv "$f" >/dev/null 2>&1'';
        zoxidecd = ''
        ''${{
          dirtocd="$(${pkgs.zoxide}/bin/zoxide query --interactive "$1")"
          if [ -n "$dirtocd" ]; then
              lf -remote "send $id cd \"$dirtocd\"" && ${pkgs.zoxide}/bin/zoxide add "$dirtocd"
          fi 
        }}'';
        zoxideadd = ''
        %{{
          ${pkgs.zoxide}/bin/zoxide add "$f" && echo "Directory added to zoxide" || echo "Error: Failed to add directory"
        }}'';
        trash = ''
        ''${{
          files=$(printf "$fx" | tr '\n' ';')
          while [ "$files" ]; do
            # extract the substring from start of string up to delimiter.
            # this is the first "element" of the string.
            file=''${files%%;*}

            ${pkgs.trashy}/bin/trash put "$(basename "$file")"
            # if there's only one element left, set `files` to an empty string.
            # this causes us to exit this `while` loop.
            # else, we delete the first "element" of the string from files, and move onto the next.
            if [ "$files" = "$file" ]; then
              files='''
            else
              files="''${files#*;}"
            fi
          done
        }}'';
        unarchive = ''
        ''${{
          case "$f" in
              *.zip) ${pkgs.unzip}/bin/unzip "$f" ;;
              *.tar.gz) tar -xzvf "$f" ;;
              *.tar.bz2) tar -xjvf "$f" ;;
              *.tar.xz) tar -xJvf "$f" ;;
              *.tar.zst) tar --zstd -xvf "$f" ;;
              *.tar) tar -xvf "$f" ;;
              *) echo "Unsupported format" ;;
          esac
        }}'';
        zip = ''
        ''${{
          if [ -z "$fs" ]; then
              ${pkgs.zip}/bin/zip -r "$(basename $f).zip" "$(basename $f)"
          else
              for file in "$fx"; do
                  ${pkgs.zip}/bin/zip -r "$(basename $file).zip" "$(basename $file)"
              done
          fi
        }}'';
        tarzstd = ''
        ''${{
          if [ -z "$fs" ]; then
              tar -c -I"${pkgs.zstd}/bin/zstd -19 -T0" -f "$(basename $f).tar.zst" "$(basename $f)"
          else
              for file in "$fx"; do
                  tar -c -I"${pkgs.zstd}/bin/zstd -19 -T0" -f "$(basename $file).tar.zst" "$(basename $file)"
              done
          fi
        }}'';
        tar = ''
        ''${{
          if [ -z "$fs" ]; then
              tar -cvf "$(basename $f).tar" "$(basename $f)"
          else
              for file in "$fx"; do
                  tar -cvf "$(basename $file).tar" "$(basename $file)"
              done
          fi
        }}'';
        targz = ''
        ''${{
          if [ -z "$fs" ]; then
              tar -cvzf "$(basename $f).tar.gz" "$(basename $f)"
          else
              for file in "$fx"; do
                  tar -cvzf "$(basename $file).tar.gz" "$(basename $file)"
              done
          fi
        }}'';
        tarbz2 = ''
        ''${{
          if [ -z "$fs" ]; then
              tar -c -v -I"${pkgs.pbzip2}/bin/pbzip2" -f "$(basename $f).tar.bz2" "$(basename $f)"
          else
              for file in "$fx"; do
                  tar -c -v -I"${pkgs.pbzip2}/bin/pbzip2" -f "$(basename $file).tar.bz2" "$(basename $file)"
              done
          fi
        }}'';
        givedate = ''
        %{{
          printf "Add Date? [y]es [N]o: "
          read ans
          case "$ans" in
            y|yes|Y|Yes)
              if [ -z "$fs" ]; then
                   mv "$(basename $f)" "$(date +%Y-%m-%d)--$(basename $f)"
              else
                   for file in $fx
                   do
                      mv "$(basename $file)" "$(date +%Y-%m-%d)--$(basename $file)"
                   done
              fi
              ;;
            *) echo "$ans: No date added" ;;
          esac
        }}'';
      };
      keybindings = {
        # Unmap bad kbds
        m = null;
        o = null;
        d = null;
        "\"'\"" = null;
        "'\"'" = null;
        e = null;
        c = null;
        f = null;
        
        "." = "set hidden!";
        DD = "delete";
        dd = "trash";
        p = "paste";
        x = "cut";
        y = "copy";
        "<enter>" = "open";
        B = "browser-open";

        z = "zoxidecd";
        ZA = "zoxideadd";

        md = "push :mkdir<space>";
        ms = "mkscript";

        "<c-n>" = ''''$${pkgs.newsboat}/bin/newsboat'';

        R = "reload";
        C = "clear";
        U = "unselect";
        I = "opendirimages";

        aZ = "zip";
        az = "tarzstd";
        at = "tar";
        ag = "targz";
        ab = "tarbz2";
        au = "unarchive";
        ad = "givedate";
      };
    };
  };
  
}
