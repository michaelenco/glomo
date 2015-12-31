DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
$DIR/rebuild.sh
erl -noshell -sname hotswap-ejabberd -remsh ejabberd@localhost -eval "rpc:call('ejabberd@localhost',c,l,[$1]),init:stop()."
