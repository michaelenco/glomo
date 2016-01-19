DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $DIR
rm -rf ejabberd_src
git clone https://github.com/processone/ejabberd ejabberd_src

cd $DIR/ejabberd_src
./autogen.sh
./configure

cd $DIR
$DIR/rebuild.sh
echo "Making config file symlink..."
cp ejabberd.yml.sample ejabberd.yml
rm /etc/ejabberd/ejabberd.yml
ln -s $DIR/ejabberd.yml /etc/ejabberd/ejabberd.yml 
echo "Done."
