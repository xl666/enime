#!/bin/sh

base_url="https://gogoplay4.com"

get_dpage_link() {
    anime_id="$1"
    ep_no="$2"

    curl -s "$base_url/videos/${anime_id}${ep_no}" | sed -nE 's_^[[:space:]]*<iframe src="([^"]*)".*_\1_p' |
	sed 's/^/https:/g'
}

decrypt_link() {
    secret_key='3633393736383832383733353539383139363339393838303830383230393037'
    iv='34373730343738393639343138323637'
    ajax_url="https://gogoplay4.com/encrypt-ajax.php"
    id=$(printf "%s" "$1" | sed -nE 's/.*id=(.*)&title.*/\1/p')
    ajax=$(printf '%s' "$id" |openssl enc -e -aes256 -K "$secret_key" -iv "$iv" | base64)
    data=$(curl -s -H "X-Requested-With:XMLHttpRequest" "$ajax_url" -d "id=$ajax" | sed -e 's/{"data":"//' -e 's/"}/\n/' -e 's/\\//g')
    printf '%s' "$data" | base64 -d | openssl enc -d -aes256 -K "$secret_key" -iv "$iv" | sed -e 's/\].*/\]/' -e 's/\\//g' |
	grep -Eo 'https:\/\/[-a-zA-Z0-9@:%._\+~#=][a-zA-Z0-9][-a-zA-Z0-9@:%_\+.~#?&\/\/=]*'
}

# chooses the link for the set quality
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
