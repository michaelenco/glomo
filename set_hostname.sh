DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo "Copy config file $DIR/ejabberd.yml from ejabberd.yml.sample..."
cp $DIR/ejabberd.yml.sample $DIR/ejabberd.yml

echo "Setting host to $1..."
sed -i "/hosts:$/{N;s/hosts:\n\s*-\s*".*"/hosts:\n  - \"$1\"/}" $DIR/ejabberd.yml
echo "Done."
