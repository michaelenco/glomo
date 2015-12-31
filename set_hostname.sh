DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
if [ ! -f $DIR/ejabberd.yml ];
then
    echo "Config file $DIR/ejabberd.yml is not exists, making one from ejabberd.yml.sample..."
    cp $DIR/ejabberd.yml.sample $DIR/ejabberd.yml
fi

echo "Setting host to $1..."
sed -i "/hosts:$/{N;s/hosts:\n\s*-\s*".*"/hosts:\n  - \"$1\"/}" $DIR/ejabberd.yml
echo "Done."
