qmake -project -o MoodBox_lin_tmpl.pro
cat MoodBox_lin_tmpl.pro Linux.pro > MoodBox_lin.pro
qmake -unix MoodBox_lin.pro
