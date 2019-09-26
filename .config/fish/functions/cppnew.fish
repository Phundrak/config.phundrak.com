function cppnew -d "Create new C++17 project" --argument-names 'projectname'
    if count $argv > /dev/null
        set conanproj "false"
        set projname ""
        set conanprojname ""
        for item in $argv
            switch "$item"
                case -c --conan
                    set conanproj "true"
                    set conanprojname $value
                case -h --help
                    man ~/dev/fishfunctions/cppnew.man
                    return 0
                case '*'
                    set projname $item
            end
        end
        if [ "$projname" = "" ]
            if [ "$conanprojname" = "" ]
                echo "Missing argument: PROJECT"
                return -1
            end
        end
        if [ "$projname" = "" ]
            set projname $conanprojname
        end
        if [ "$conanproj" = "true" ]
            cp -r ~/dotfiles/dev/conan-project $projname
        else
            cp -r ~/dotfiles/dev/templateC++ $projname
        end
        cd $projname
        sed -i "s/PROJECTNAME/$projname/g" README.org
        sed -i "s/PROJECTNAME/$projname/g" CMakeLists.txt
        if [ "$conanproj" = "true" ]
            sed -i "s/PROJECTNAME/$projname/g" conanfile.py
        end
        sed -i "s/CPPPROJECTNAME/$projname/g" doc/Doxyfile
        git init
        git add .
        git commit -m "initial commit"
        cd ..
    else
        echo "Missing argument: PROJECT"
        return -1
    end
end
complete -c cppnew -s c -l conan -d 'Conan Project'
complete -c cppnew -s h -l help -d 'Print Help'
