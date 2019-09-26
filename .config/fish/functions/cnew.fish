function cnew -d "Create new C11 project"
    if count $argv > /dev/null
        set projname ""
        for item in $argv
            switch "$item"
                case -h --help
                    man ~/dev/fishfunctions/cnew.man
                    return 0
                case '*'
                    set projname $item
            end
        end
        if [ "$projname" = "" ]
            echo "Missing argument: PROJECT"
            return -1
        end
        cp -r ~/dotfiles/dev/templateC $argv[1]
        cd $argv[1]
        sed -i "s/PROJECTNAME/$argv[1]/g" CMakeLists.txt
        sed -i "s/PROJECTNAME/$argv[1]/g" README.org
        sed -i "s/CPROJECTNAME/$argv[1]/g" doc/Doxyfile
        git init
        git add .
        git commit -m "initial commit"
        cd ..
    else
        echo "Missing argument: PROJECT"
        return -1
    end
end
complete -c cppnew -s h -l help -d 'Print Help'
