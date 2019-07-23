if [ "$#" -ne 2 ]; then
    echo 'Please, specify output file and size in MB'
    exit 1
fi
dd if=/dev/urandom bs=1024 count=$(($2 * 1024)) of=$1
