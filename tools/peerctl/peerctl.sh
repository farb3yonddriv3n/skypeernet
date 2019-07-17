ACTION_BLOCK="block"
ACTION_ALLOW="allow"

if [ "$#" -ne 2 ]; then
    echo 'Please, specify action and IP address'
    exit 1
fi

if [ $ACTION_BLOCK == $1 ]; then
    echo "Blocking IP "$2
    iptables -I INPUT -p udp -s $2 -j DROP
elif [ $ACTION_ALLOW == $1 ]; then
    echo "Allowing IP "$2
    iptables -D INPUT -p udp -s $2 -j DROP
else
    echo "Action not supported"
fi
