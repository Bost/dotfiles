function envp --description "Show PATH variable"
    env | grep '^PATH=.*'
end
