function we -d "Get weather at location"
    if count $argv > /dev/null
        curl http://v2.wttr.in/~$argv[1]
    else
        curl http://v2.wttr.in/Aubervilliers
    end
end
