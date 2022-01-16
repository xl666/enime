#!/bin/sh
base_url=$(curl -s -L -o /dev/null -w "%{url_effective}\n" https://gogoanime.cm)

get_dpage_link() {
    # get the download page url
    anime_id=$1
    ep_no=$2

    # credits to fork: https://github.com/Dink4n/ani-cli for the fix
    # dub prefix takes the value "-dub" when dub is needed else is empty
    anime_page=$(curl -s "$base_url/$anime_id${dub_prefix}-$ep_no")

    if printf '%s' "$anime_page" | grep -q "404" ; then
	anime_page=$(curl -s "$base_url/$anime_id${dub_prefix}-episode-$ep_no")
    fi

    printf '%s' "$anime_page" |
	sed -n -E 's/^[[:space:]]*<a href="#" rel="100" data-video="([^"]*)".*/\1/p' |
	sed 's/^/https:/g'
}

decrypt_link() {
    ajax_url='https://gogoplay.io/encrypt-ajax.php'

    #get the id from the url
    video_id=$(printf "$1" | cut -d\? -f2 | cut -d\& -f1 | sed 's/id=//g')
    
    #construct ajax parameters
    secret_key='3235373436353338353932393338333936373634363632383739383333323838'
    iv='34323036393133333738303038313335'
    ajax=$(printf "$video_id" | openssl enc -aes256  -K "$secret_key" -iv "$iv" -a)
    
    #send the request to the ajax url
    curl -s -H 'x-requested-with:XMLHttpRequest' "$ajax_url" -d "id=$ajax" -d "time=69420691337800813569" | tr '"' '\n' | sed -n -E 's/.*cdn.*/\0/p' | sed 's/\\//g' 
}

get_video_quality() {
    dpage_url="$1"
    video_links=$(decrypt_link "$dpage_url")
    case $quality in
	best)
	    video_link=$(printf '%s' "$video_links" | head -n 4 | tail -n 1)
	    ;;

	worst)
	    video_link=$(printf '%s' "$video_links" | head -n 1)
	    ;;

	*)
	    video_link=$(printf '%s' "$video_links" | grep -i "${quality}p" | head -n 1)
	    if [ -z "$video_link" ]; then
		err "Current video quality is not available (defaulting to best quality)"
		quality=best
		video_link=$(printf '%s' "$video_links" | head -n 4 | tail -n 1)
	    fi
	    ;;
    esac
    printf '%s' "$video_link"
}


anime_id="$1"
episode="$2"
quality="$3"

dpage_link=$(get_dpage_link "$anime_id" "$episode")
video_url=$(get_video_quality "$dpage_link")

printf "%s;;;%s\n"  "$dpage_link" "$video_url"
