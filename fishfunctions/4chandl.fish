#!/usr/bin/env fish

function 4chandl -d "Download media from 4chan thread"
    if ! count $argv > /dev/null
        echo 'No URL specified! Give the URL to thread as the only argument.'
    end
    set url $argv[1]

    set regex_4cdn '\/\/is2\.4chan\.org\/[a-z]+\/[A-Za-z0-9]+\.[A-Za-z]{3,4}'

    set total (curl -ks $url | grep -oE $regex_4cdn | uniq | wc -l)
    echo total: $total
    set counter 1

    for image_url in (curl -k -s $url | grep -Eo $regex_4cdn | uniq | sed 's/^/https:/')
        echo -n Downloading image $counter of $total...
        wget --no-check-certificate -q -nc $image_url
        echo ' Done'
        set counter (math $counter + 1)
    end

end
