DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo "Updating web_admin files..."
cp -r $DIR/web_admin /var/lib/ejabberd/
