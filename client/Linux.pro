RESOURCES += moodbox.qrc
TRANSLATIONS += moodbox_en.ts moodbox_ru.ts
QT += network xml svg
QTPLUGIN += qjpeg qgif qmng qsvg qtiff
TARGET = MoodBox.bin

INCLUDEPATH += ../../Velasquez/Qt /usr/local/include/ImageMagick
LIBS += -lMagick++ -lMagickCore -lMagickWand -lgomp -lz -lssl
LIBS += -L/usr/local/lib -lz -lMagickCore -L/usr/X11/lib -lXext -lX11 -lm -lpthread -ldl

HEADERS += ../../Velasquez/Qt/transformableelement.h \
           ../../Velasquez/Qt/hoverpoints.h \
           ../../Velasquez/Qt/textelement.h \
	   ../../Velasquez/Qt/debug.h
		

HEADERS -= _PaletteConverter/stdafx.h \
           _PaletteConverter/targetver.h \
           mactools.h

SOURCES -= mactools.cpp \
           qtsingleapplication_mac.cpp \
           qtsingleapplication_win.cpp \ 
           _PaletteConverter/PaletteConverter.cpp \
           http_window.cpp

HEADERS += ../../Velasquez/Qt/backgroundelement.h \
           ../../Velasquez/Qt/backgroundtool.h \
           ../../Velasquez/Qt/brushdrawingelement.h \
           ../../Velasquez/Qt/brushdrawingtool.h \
           ../../Velasquez/Qt/brushstroke.h \
           ../../Velasquez/Qt/debug.h \
           ../../Velasquez/Qt/drawingelement.h \
           ../../Velasquez/Qt/drawingtool.h \
           ../../Velasquez/Qt/drawingutils.h \
           ../../Velasquez/Qt/editorscene.h \
           ../../Velasquez/Qt/eraserelement.h \
           ../../Velasquez/Qt/erasertool.h \
           ../../Velasquez/Qt/eyedroppertool.h \
           ../../Velasquez/Qt/hoverpoints.h \
           ../../Velasquez/Qt/imageelement.h \
           ../../Velasquez/Qt/imagetool.h \
           ../../Velasquez/Qt/metainfoprovider.h \
           ../../Velasquez/Qt/mousedrawingelement.h \
           ../../Velasquez/Qt/mousedrawingtool.h \
           ../../Velasquez/Qt/mtrandom.h \
           ../../Velasquez/Qt/oilbrushelement.h \
           ../../Velasquez/Qt/oilbrushtool.h \
           ../../Velasquez/Qt/penelement.h \
           ../../Velasquez/Qt/pentool.h \
           ../../Velasquez/Qt/scenecursorlayer.h \
           ../../Velasquez/Qt/settingsprovider.h \
           ../../Velasquez/Qt/simplebrushelement.h \
           ../../Velasquez/Qt/simplebrushtool.h \
           ../../Velasquez/Qt/sprayelement.h \
           ../../Velasquez/Qt/spraytool.h \
           ../../Velasquez/Qt/svgelement.h \
           ../../Velasquez/Qt/svgtool.h \
           ../../Velasquez/Qt/textcursorpointer.h \
           ../../Velasquez/Qt/textelement.h \
           ../../Velasquez/Qt/texttool.h \
           ../../Velasquez/Qt/toolbox.h \
           ../../Velasquez/Qt/transformableelement.h \
           ../../Velasquez/Qt/transformabletool.h \
           ../../Velasquez/Qt/transformation.h \
           ../../Velasquez/Qt/undocommands.h \
           ../../Velasquez/Qt/varianthash.h \
           ../../Velasquez/Qt/vcommon.h

SOURCES += ../../Velasquez/Qt/backgroundelement.cpp \
           ../../Velasquez/Qt/backgroundtool.cpp \
           ../../Velasquez/Qt/brushdrawingelement.cpp \
           ../../Velasquez/Qt/brushdrawingtool.cpp \
           ../../Velasquez/Qt/brushstroke.cpp \
           ../../Velasquez/Qt/drawingelement.cpp \
           ../../Velasquez/Qt/drawingtool.cpp \
           ../../Velasquez/Qt/drawingutils.cpp \
           ../../Velasquez/Qt/editorscene.cpp \
           ../../Velasquez/Qt/eraserelement.cpp \
           ../../Velasquez/Qt/erasertool.cpp \
           ../../Velasquez/Qt/eyedroppertool.cpp \
           ../../Velasquez/Qt/hoverpoints.cpp \
           ../../Velasquez/Qt/imageelement.cpp \
           ../../Velasquez/Qt/imagetool.cpp \
           ../../Velasquez/Qt/metainfoprovider.cpp \
           ../../Velasquez/Qt/mousedrawingelement.cpp \
           ../../Velasquez/Qt/mousedrawingtool.cpp \
           ../../Velasquez/Qt/mtrandom.cpp \
           ../../Velasquez/Qt/oilbrushelement.cpp \
           ../../Velasquez/Qt/oilbrushtool.cpp \
           ../../Velasquez/Qt/penelement.cpp \
           ../../Velasquez/Qt/pentool.cpp \
           ../../Velasquez/Qt/scenecursorlayer.cpp \
           ../../Velasquez/Qt/settingsprovider.cpp \
           ../../Velasquez/Qt/simplebrushelement.cpp \
           ../../Velasquez/Qt/simplebrushtool.cpp \
           ../../Velasquez/Qt/sprayelement.cpp \
           ../../Velasquez/Qt/spraytool.cpp \
           ../../Velasquez/Qt/svgelement.cpp \
           ../../Velasquez/Qt/svgtool.cpp \
           ../../Velasquez/Qt/textcursorpointer.cpp \
           ../../Velasquez/Qt/textelement.cpp \
           ../../Velasquez/Qt/texttool.cpp \
           ../../Velasquez/Qt/toolbox.cpp \
           ../../Velasquez/Qt/transformableelement.cpp \
           ../../Velasquez/Qt/transformabletool.cpp \
           ../../Velasquez/Qt/transformation.cpp \
           ../../Velasquez/Qt/undocommands.cpp \
           ../../Velasquez/Qt/varianthash.cpp
