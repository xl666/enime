#!/bin/sh

base_url="https://animixplay.to"
agent="Mozilla/5.0 (X11; Linux x86_64; rv:99.0) Gecko/20100101 Firefox/100.0"


episode_list () {
    data=$(curl -A "$agent" -s "$base_url/v1/$1" | sed -nE "s/.*malid = '(.*)';/\1/p ; s_.*epslistplace.*>(.*)</div>_\1_p" | tr -d '\r')
    #extract all embed links of all episode from data
    select_ep_result=$(printf "%s" "$data" | head -1 | tr "," "\n" | sed '/extra/d' | sed -nE 's_".*":"(.*)".*_\1_p')
    first_ep_number=1
    [ -z "$select_ep_result" ] && last_ep_number=0 || last_ep_number=$(printf "%s\n" "$select_ep_result" | wc -l)
}

generate_link() {
    case $1 in
	1)
	    provider_name='Mp4upload'
	    refr=$(printf "%s" "$al_links" | grep "mp4upload")
	    [ -z "$refr" ] && refr=$(printf "%s" "$resp" | grep "mp4upload")
	    [ -z "$refr" ] && return 0
	    result_links="$(curl -A "$agent" -s "$refr" -H "DNT: 1" -L |
				sed -nE 's_.*embed\|(.*)\|.*blank.*\|(.*)\|(.*)\|(.*)\|(.*)\|src.*_https://\1.mp4upload.com:\5/d/\4/\3.\2_p')"
	    ;;
	2)
	    provider_name='Doodstream'
	    dood_id=$(printf "%s" "$al_links" | sed -n "s_.*dood.*/e/__p")
	    [ -z "$dood_id" ] && dood_id=$(printf "%s" "$resp" | sed -n "s_.*dood.*/e/__p")
	    refr="https://dood.pm/e/$dood_id"
	    [ -z "$dood_id" ] || dood_md5=$(curl -A "$agent" -s "$refr" --max-time 10 | sed -nE "s|.*'(.*pass_md5.*)', func.*|\1|p")
	    [ -z "$dood_md5" ] && return 0
	    result_links="$(curl -A "$agent" -s "https://dood.pm${dood_md5}" -e "$refr" || true)doodstream?token=$(printf "%s" "$dood_md5" | cut -d'/' -f4 || true)&expiry=$(date +%s)000"
	    ;;
	3)
	    provider_name='Streamlare'
	    lare_id=$(printf "%s" "$al_links" | sed -nE 's_.*streamlare.*/e/(.*)_\1_p')
	    [ -z "$lare_id" ] && lare_id=$(printf "%s" "$dpage_url" | sed -nE 's_.*streamlare.*/e/(.*)_\1_p')
	    refr="https://streamlare.com/e/$lare_id"
	    [ -z "$lare_id" ] && return 0
	    lare_token=$(curl -s -A "$agent" "$refr" -L | sed -nE 's/.*csrf-token.*content="(.*)">/\1/p')
	    [ -z "$lare_token" ] || result_links="$(curl -s -A "$agent" -H "x-requested-with:XMLHttpRequest" -X POST "https://streamlare.com/api/video/download/get" -d "{\"id\":\"$lare_id\"}" \
				-H "x-csrf-token:$lare_token" -H "content-type:application/json;charset=UTF-8" | sed 's/\\//g' | sed -nE 's/.*url":"([^"]*)".*/\1/p')"
	    ;;
	4)
	    provider_name='Okru'
	    ok_id=$(printf "%s" "$al_links" | sed -nE 's_.*ok.*videoembed/(.*)_\1_p')
	    [ -z "$ok_id" ] && ok_id=$(printf "%s" "$dpage_url" | sed -nE 's_.*ok.*videoembed/(.*)_\1_p')
	    refr="https://odnoklassniki.ru/videoembed/$ok_id"
	    [ -z "$ok_id" ] && return 0
	    result_links="$(curl -s "$refr" | sed -nE 's_.*data-options="([^"]*)".*_\1_p' | sed -e 's/&quot;/"/g' -e 's/\u0026/\&/g' -e 's/amp;//g' | sed 's/\\//g' | sed -nE 's/.*videos":(.*),"metadataE.*/\1/p' | tr '{|}' '\n' |
				sed -nE 's/"name":"mobile","url":"(.*)",.*/144p >\1/p ;
						s/"name":"lowest","url":"(.*)",.*/240p >\1/p ;
						s/"name":"low","url":"(.*)",.*/360p >\1/p ;
						s/"name":"sd","url":"(.*)",.*/480p >\1/p ;
						s/"name":"hd","url":"(.*)",.*/720p >\1/p ;
						s/"name":"full","url":"(.*)",.*/1080p >\1/p')"
	    ;;
	5)
	    provider_name='Xstreamcdn'
	    fb_id=$(printf "%s" "$resp" | sed -n "s_.*fembed.*/v/__p")
	    refr="https://fembed-hd.com/v/$fb_id"
	    [ -z "$fb_id" ] && return 0
	    result_links="$(curl -A "$agent" -s -X POST "https://fembed-hd.com/api/source/$fb_id" -H "x-requested-with:XMLHttpRequest" |
				sed -e 's/\\//g' -e 's/.*data"://' | tr "}" "\n" | sed -nE 's/.*file":"(.*)","label":"(.*)","type.*/\2>\1/p')"
	    ;;
	6)
	    provider_name='Animixplay'
	    refr="$base_url"
	    [ -z "$id" ] && return 0
	    enc_id=$(printf "%s" "$id" | base64)
	    ani_id=$(printf "%sLTXs3GrU8we9O%s" "$id" "$enc_id" | base64)
	    result_links="$(curl -s "$base_url/api/live${ani_id}" -A "$agent" -I | sed -nE 's_location: (.*)_\1_p' | cut -d"#" -f2 | base64 -d)"
	    ;;
	*)
	    provider_name='Gogoanime'
	    refr="https://goload.pro"
	    [ -z "$id" ] && return 0
	    secret_key=$(printf "%s" "$resp" | sed -n '2p' | tr -d "\n" | od -A n -t x1 | tr -d " |\n")
	    iv=$(printf "%s" "$resp" | sed -n '3p' | tr -d "\n" | od -A n -t x1 | tr -d " |\n")
	    second_key=$(printf "%s" "$resp" | sed -n '4p' | tr -d "\n" | od -A n -t x1 | tr -d " |\n")
	    token=$(printf "%s" "$resp" | head -1 | base64 -d | openssl enc -d -aes256 -K "$secret_key" -iv "$iv" | sed -nE 's/.*&(token.*)/\1/p')
	    ajax=$(printf '%s' "$id" | openssl enc -e -aes256 -K "$secret_key" -iv "$iv" -a)
	    data=$(curl -A "$agent" -s -H "X-Requested-With:XMLHttpRequest" "https://goload.pro/encrypt-ajax.php?id=${ajax}&alias=${id}&${token}" | sed -e 's/{"data":"//' -e 's/"}/\n/' -e 's/\\//g')
	    result_links="$(printf '%s' "$data" | base64 -d 2>/dev/null | openssl enc -d -aes256 -K "$second_key" -iv "$iv" 2>/dev/null |
				sed -e 's/\].*/\]/' -e 's/\\//g' | grep -Eo 'https:\/\/[-a-zA-Z0-9@:%._\+~#=][a-zA-Z0-9][-a-zA-Z0-9@:%_\+.~#?&\/\/=]*')"
	    ;;
    esac
}

# chooses the link for the set quality
get_video_link() {
    dpage_url="$1"
    id=$(printf "%s" "$dpage_url" | sed -nE 's/.*id=(.*)&title.*/\1/p')
    al_links=$(printf "%s" "$al_data" | sed -e 's_:\[_\n_g' -e 's_:"_\n"_g' | sed -e 's/].*//g' -e '1,2d' | sed -n "${episode}p" | tr -d '"' | tr "," "\n")
    [ -z "$id" ] && id=$(printf "%s" "$al_links" | sed -nE 's/.*id=(.*)&title.*/\1/p')
    #multiple sed are used (regex seperated by ';') for extracting only required data from response of embed url
    resp="$(curl -A "$agent" -s "https://goload.pro/streaming.php?id=$id" |
		sed -nE 's/.*class="container-(.*)">/\1/p ;
			s/.*class="wrapper container-(.*)">/\1/p ;
			s/.*class=".*videocontent-(.*)">/\1/p ;
			s/.*data-value="(.*)">.*/\1/p ;
			s/.*data-status="1".*data-video="(.*)">.*/\1/p')"
    # providers: Doodstream for default, mp4upload for downloading. For best quality use okru, for fallback use goload. Then it's a round robin of which links are returned.
    provider=2
    uname -a | grep -qE '[Aa]ndroid' && provider=3
    [ "$quality" != "best" ] && provider=4
    [ -n "$select_provider" ] && provider="$select_provider"
    i=0
    while [ "$i" -lt 7 ] && [ -z "$result_links" ];do
	generate_link "$provider"
	provider=$((provider % 7 + 1))
	: $((i+=1))
    done
    if printf '%s' "$result_links" | grep -q "m3u8"; then
	get_video_quality_m3u8 "$result_links"
    else
	video_url=$(get_video_quality_mp4 "$result_links")
    fi
    unset result_links
}

get_video_quality_mp4() {
    case $quality in
	best)
	    video_url=$(printf '%s' "$1" | tail -n 1 | cut -d">" -f2) ;;
	worst)
	    video_url=$(printf '%s' "$1" | head -n 1 | cut -d">" -f2) ;;
	*)
	    video_url=$(printf '%s' "$1" | grep -i "${quality}p" | head -n 1 | cut -d">" -f2)
	    if [ -z "$video_url" ]; then
		echo "Current video quality is not available (defaulting to best quality)"
		video_url=$(printf '%s' "$1" | tail -n 1 | cut -d">" -f2)
	    fi
	    ;;
    esac
    printf '%s' "$video_url"
}

get_video_quality_m3u8() {
    printf '%s' "$1" | grep -qE "manifest.*m3u.*" && video_url=$1 && return 0
    m3u8_links=$(curl -A "$agent" -s --referer "$dpage_link" "$1")
    case $quality in
	best)
	    res_selector=$(printf "%s" "$m3u8_links" | sed -nE 's_.*RESOLUTION=.*x([^,]*),.*_\1_p' | sort -nr | head -1);;
	worst)
	    res_selector=$(printf "%s" "$m3u8_links" | sed -nE 's_.*RESOLUTION=.*x([^,]*),.*_\1_p' | sort -nr | tail -1);;
	*)
	    res_selector=$quality
	    if ! (printf '%s' "$m3u8_links" | grep -q "$quality,"); then
		echo "Current video quality is not available (defaulting to best quality)"
		res_selector=$(printf "%s" "$m3u8_links" | sed -nE 's_.*RESOLUTION=.*x([^,]*),.*_\1_p' | sort -nr | head -1)
	    fi
	    ;;
    esac
    video_url=$(printf '%s' "$m3u8_links" | sed -n "/$res_selector,/{n;p;}" | tr -d '\r')
    printf "%s" "$m3u8_links" | grep -q "http" || video_url="$(printf "%s" "$1" | sed 's|[^/]*$||')$video_url" || true
}



anime_id="$1"
episode="$2"
quality="$3"
dub_prefix="$4"


episode_list "$anime_id"

dpage_link=$(printf "%s" "$select_ep_result" | sed -n "${episode}p")
if [ -z "$dpage_link" ];then
    echo "Episode doesn't exist!!"
else
    get_video_link "$dpage_link"
fi

get_video_link "$dpage_link"

printf "%s;;;%s\n"  "$dpage_link" "$video_url"
