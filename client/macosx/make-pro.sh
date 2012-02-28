qmake -project -o MoodBox_Mac.pro
cat MoodBox_Mac.pro MacOS.pro > tmp.pro
mv tmp.pro MoodBox_Mac.pro
qmake -spec macx-xcode MoodBox_Mac.pro