#!/bin/sh

base_url="https://goload.pro"

get_dpage_link() {
    anime_id="$1"
    ep_no="$2"

    curl -s "$base_url/videos/${anime_id}${ep_no}" | sed -nE 's_^[[:space:]]*<iframe src="([^"]*)".*_\1_p' |
	sed 's/^/https:/g'
}


decrypt_link() {
    ajax_url="$base_url/encrypt-ajax.php"
    id=$(printf "%s" "$1" | sed -nE 's/.*id=(.*)&title.*/\1/p')
    resp=$(curl -s "$1")
    secret_key=$(printf "%s" "$resp" | sed -nE 's/.*class="container-(.*)">/\1/p' | tr -d "\n" | od -A n -t x1 | tr -d " |\n")
    iv=$(printf "%s" "$resp" | sed -nE 's/.*class="wrapper container-(.*)">/\1/p' | tr -d "\n" | od -A n -t x1 | tr -d " |\n")
    second_key=$(printf "%s" "$resp" | sed -nE 's/.*class=".*videocontent-(.*)">/\1/p' | tr -d "\n" | od -A n -t x1 | tr -d " |\n")
    token=$(printf "%s" "$resp" | sed -nE 's/.*data-value="(.*)">.*/\1/p' | base64 -d | openssl enc -d -aes256 -K "$secret_key" -iv "$iv" | sed -nE 's/.*&(token.*)/\1/p')
    ajax=$(printf '%s' "$id" | openssl enc -e -aes256 -K "$secret_key" -iv "$iv" | base64)
    data=$(curl -s -H "X-Requested-With:XMLHttpRequest" "${ajax_url}?id=${ajax}&alias=${id}&${token}" | sed -e 's/{"data":"//' -e 's/"}/\n/' -e 's/\\//g')
    printf '%s' "$data" | base64 -d | openssl enc -d -aes256 -K "$second_key" -iv "$iv" | sed -e 's/\].*/\]/' -e 's/\\//g' |
	grep -Eo 'https:\/\/[-a-zA-Z0-9@:%._\+~#=][a-zA-Z0-9][-a-zA-Z0-9@:%_\+.~#?&\/\/=]*'
}


# chooses the link for the set quality
get_video_link() {
    dpage_url="$1"
    video_links=$(decrypt_link "$dpage_url")
    if printf '%s' "$video_links" | grep -q "mp4"; then 
	video_url=$(get_video_quality_mp4 "$video_links")
	idx=1
    else
	video_url="$video_links"
	get_video_quality_m3u8
    fi
}	

get_video_quality_mp4() {
    case $quality in
	best)
	    video_url=$(printf '%s' "$1" | head -n 4 | tail -n 1) ;;
	worst)
	    video_url=$(printf '%s' "$1" | head -n 1) ;;
	*)
	    video_url=$(printf '%s' "$1" | grep -i "${quality}p" | head -n 1)
	    if [ -z "$video_url" ]; then
		err "Current video quality is not available (defaulting to best quality)"
		quality=best
		video_url=$(printf '%s' "$1" | head -n 4 | tail -n 1)
	    fi
	    ;;
    esac
    printf '%s' "$video_url"
}

get_video_quality_m3u8() {
    case $quality in
	worst|360)
	    idx=2 ;;
	480)
	    idx=3 ;;
	720)
	    idx=4 ;;
	1080|best)
	    idx=5 ;;
	*)
	    idx=5 ;;
    esac
    printf '%s' "$video_url" | grep -qE "gogocdn.*m3u.*" && idx=$((idx-1))
}


anime_id="$1"
episode="$2"
quality="$3"
dub_prefix="$4"

dpage_link=$(get_dpage_link "$anime_id" "$episode")
get_video_link "$dpage_link"

printf "%s;;;%s\n"  "$dpage_link" "$video_url"
