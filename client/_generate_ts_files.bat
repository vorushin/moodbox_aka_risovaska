qmake.exe -project -o 1.pro
copy 1.pro + AddOn.pro MoodBox.pro
del 1.pro
lupdate.exe MoodBox.pro