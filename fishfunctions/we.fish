function we -d "Get weather at location"
    if count $argv > /dev/null
        curl http://wttr.in/~$argv[1]
    else
        curl http://wttr.in/Aubervilliers
    end
end
