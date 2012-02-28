qmake -project -o Velasquez.pro
cat Velasquez.pro AddOn.pro > tmp.pro
mv tmp.pro Velasquez.pro
qmake -spec macx-xcode Velasquez.pro