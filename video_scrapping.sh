#!/bin/sh

base_url=$(curl -s -L -o /dev/null -w "%{url_effective}\n" https://gogoanime.cm)

get_dpage_link() {
    # get the download page url
    anime_id="$1"
    ep_no="$2"
    # credits to fork: https://github.com/Dink4n/ani-cli for the fix 
    for params in "-episode-$ep_no" "-$ep_no" "-episode-$ep_no-1" "-camrip-episode-$ep_no"; do
	anime_page=$(curl -s "$base_url/$anime_id$params")
	printf '%s' "$anime_page" | grep -q '<h1 class="entry-title">404</h1>' || break
    done
    printf '%s' "$anime_page" |
	sed -n -E 's/.*class="active" rel="1" data-video="([^"]*)".*/\1/p' | sed 's/^/https:/g'
}

decrypt_link() {
    secret_key='3235373136353338353232393338333936313634363632323738383333323838'
    iv='31323835363732393835323338333933'
    ajax_url="https://gogoplay4.com/encrypt-ajax.php"
    crypto_data=$(curl -s "$1" | sed -nE 's/.*data-value="([^"]*)".*/\1/p')
    id=$(printf '%s' "$crypto_data" | base64 -d | openssl enc -d -aes256 -K "$secret_key" -iv "$iv" | cut -d '&' -f1)

    #encrypt and create the final ajax
    ajax=$(printf "%s\010\016\003\010\t\003\004\t" "$id" | openssl enc -aes256 -K "$secret_key" -iv "$iv" -a)

    #send request and get the data(most lamest way)
    data=$(curl -s -H "X-Requested-With:XMLHttpRequest" "$ajax_url" -d "id=$ajax" | sed -e 's/{"data":"//' -e 's/"}/\n/' -e 's/\\//g')
    
    #decrypt the data to get final links
    printf '%s' "$data" | base64 -d | openssl enc -d -aes256 -K "$secret_key" -iv "$iv" | sed -e 's/\].*/\]/' -e 's/\\//g' |
	grep -Eo 'https:\/\/[-a-zA-Z0-9@:%._\+~#=][a-zA-Z0-9][-a-zA-Z0-9@:%_\+.~#?&\/\/=]*'
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
dub_prefix="$4"

dpage_link=$(get_dpage_link "$anime_id" "$episode")
video_url=$(get_video_quality "$dpage_link")

printf "%s;;;%s\n"  "$dpage_link" "$video_url"
