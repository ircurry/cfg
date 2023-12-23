{ config, pkgs, lib, ... }:

let
  cfg = config.nocturne.cli.scripts.youtubeScripts;
in lib.mkIf cfg.enable {
  home.packages = let
    ytu = pkgs.writeShellScriptBin "ytu" ''
    if [ -z $1 ]; then
	      base=$(echo "$(${pkgs.wl-clipboard}/bin/wl-paste)" | grep -Eo "watch\?v=.{11}")
	      [ -n "$base" ] && ${pkgs.mpv}/bin/mpv "https://youtube.com/$base"
    else 
	      base=$(echo $1 | grep -Eo "watch\?v=.{11}")  
	      ${pkgs.mpv}/bin/mpv "https://youtube.com/$base"
    fi
    exit 0
    '';
    ytd = pkgs.writeShellScriptBin "ytd" ''
    if [ -z $1 ]; then
    	echo "no url provided"
    	exit 1
    fi
    
    case $1 in
    	-i) url=$(${pkgs.ytfzf}/bin/ytfzf -T ${pkgs.chafa}/bin/chafa -t -L)
    	    if [ -z $url ]; then
    		    echo "error, no url selected"
    		    exit 2
    	    fi
    	    ytd "$url" ;;
    	*) base="$(echo $1 | grep -Eo 'watch\?v=.{11}')"
    	   url=$(echo "https://youtube.com/$base")
         mkdir -p "$HOME/dl/intake-vids"
    	   ${pkgs.yt-dlp}/bin/yt-dlp --embed-metadata \
    		  -f 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/22/bestvideo+bestaudio' \
    		  -o "%(uploader)s--%(upload_date)s--%(title)s--%(id)s.%(ext)s" \
    		  -P "$HOME/dl/intake-vids/" "$url" ;;
    esac
    '';
    ytdp = pkgs.writeShellScriptBin "ytdp" ''
    if [ -z $1 ]; then
    	echo "no url provided"
    	exit 1
    fi
    
    case $1 in
    	-i) url=$(${pkgs.ytfzf}/bin/ytfzf -T ${pkgs.chafa}/bin/chafa -t -L --type=playlist)
    	    if [ -z $url ]; then
    		    echo "error, no url selected"
    		    exit 2
    	    fi
    	    ytdp "$url" ;;
    	*) base="$(echo $1 | grep -Eo 'playlist\?list=.{34}')"
    	   url=$(echo "https://youtube.com/$base")
         mkdir -p "$HOME/dl/intake-playlists"
    	   ${pkgs.yt-dlp}/bin/yt-dlp --embed-metadata \
    		  -f 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/22/bestvideo+bestaudio' \
    		  -o "%(playlist_title)s--%(playlist_index)s--%(upload_date)s--%(title)s.%(ext)s" \
    		  -P "$HOME/dl/intake-playlists/" "$url" ;;
    esac
    '';
    
  in [ ytu ytd ytdp ];

}
