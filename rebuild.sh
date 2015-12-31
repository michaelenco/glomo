DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo "Copying custom modules sources to ejabberd environment.."
for f in $DIR/src/*.erl
do
    echo "$f -> $DIR/ejabberd_src/src/"
    cp $f $DIR/ejabberd_src/src/
done

cd $DIR/ejabberd_src
echo "Compiling..."
make
echo "Moving files to their locations..."
make install > /dev/null
