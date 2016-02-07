DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo "Copying custom modules sources to ejabberd environment.."
for f in $DIR/src/*.erl
do
    echo "$f -> $DIR/ejabberd_src/src/"
    cp $f $DIR/ejabberd_src/src/
done

cp $DIR/rebar.config $DIR/ejabberd_src/rebar.config

cd $DIR/ejabberd_src
echo "Compiling..."
make
echo "Moving files to their locations..."
make install > /dev/null

echo "Removing custom modules sources from ejabberd environment..."
cd $DIR/src
for f in *.erl
do
    echo "$f..."
    rm $DIR/ejabberd_src/src/$f
done

$DIR/update_web_admin_files.sh
