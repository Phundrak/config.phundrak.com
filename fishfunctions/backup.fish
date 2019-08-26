function backup -d "backs up a file and appends to its name the date and time of backup"
    cp $argv[1] $argv[1].bak.(date +"%Y%m%d%H%M%S")
end
