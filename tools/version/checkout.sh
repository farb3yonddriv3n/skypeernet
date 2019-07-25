if [ "$#" -ne 1 ]; then
    echo 'Please, specify desired version'
    exit 1
fi

commits=`git rev-list HEAD`
i=`git rev-list HEAD --count`
for c in ${commits[@]}; do
    if [ $i == $1 ]; then
        echo "Checking out to commit #"$i":"$c
        git checkout $c
        break
    fi
    ((i--))
done
