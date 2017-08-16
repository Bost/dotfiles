function start-altcoin-daemons
  # set tststr "aaa inet dlink maty-home Dodo disconnected bbb OEBB-station ccc"
  # echo 'nmcli dev | grep "wifi" | grep --word-regexp "WIFIonICE\|OEBB-station"'
  #       nmcli dev | grep "wifi" | grep --word-regexp "WIFIonICE\|OEBB-station"

  # echo 'nmcli dev | grep "wifi" | grep --word-regexp "inet\|dlink\|maty-home\|Dodo"'
  nmcli dev | grep "wifi" | grep --word-regexp "inet\|dlink\|maty-home\|Dodo"

  # echo $tststr | grep --word-regexp "inet\|dlink\|maty-home\|Dodo"
  if test $status = 0
    /usr/bin/nice -n 20 bitcoind -daemon
    /usr/bin/nice -n 20 $HOME/litecoin-latest/bin/litecoind -daemon
    # echo "bitcoin-cli stop"
    #       bitcoin-cli stop
    # echo "/home/bost/litecoin-latest/bin/litecoin-cli stop"
    #       /home/bost/litecoin-latest/bin/litecoin-cli stop
  end

end
