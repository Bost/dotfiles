function train
  echo 'nmcli dev | grep "wifi" | grep --word-regexp "WIFIonICE"'
        nmcli dev | grep "wifi" | grep --word-regexp "WIFIonICE"

  if test $status = 0
    echo "bitcoin-cli stop"
          bitcoin-cli stop
    echo "/home/bost/litecoin-latest/bin/litecoin-cli stop"
          /home/bost/litecoin-latest/bin/litecoin-cli stop
  else
    echo "status: $status"
  end

end
