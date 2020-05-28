function rainymood
    set volume 50
    getopts $argv | while read -l key option
        switch $key
            case v
                set volume $option
            case volume
                set volume $option
        end
    end
    if [ "$volume" != "" ]
        set FILE (math (random) % 4)
        set URL "https://rainymood.com/audio1112/$FILE.ogg"
        mpv $URL --force-window=no --volume=$volume; and rainymood
    else
        echo "Missing value after -v/--volume option."
        echo "Usage example:"
        printf "\trainymood -v50\n\trainymood --volume 50\n"
        return 1
    end
end
complete -c rainymood -s v -l volume -d 'Volume of the rain (0-100)'
