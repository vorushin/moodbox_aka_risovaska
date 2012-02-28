rm -r -f MoodBox_Lin
mkdir MoodBox_Lin
ln -s `pwd`/Avatars `pwd`/MoodBox_Lin/Avatars
ln -s `pwd`/Clipart `pwd`/MoodBox_Lin/Clipart
ln -s `pwd`/Sound `pwd`/MoodBox_Lin/Sound
ln -s `pwd`/MoodBox.bin `pwd`/MoodBox_Lin/MoodBox.bin
tar czvfh MoodBox.tar.gz MoodBox_Lin/ --exclude=.svn
# vhbit: may be we need 2 packing options?
#tar cjvfh MoodBox.tar.bz2 MoodBox_Lin/ --exclude=.svn

