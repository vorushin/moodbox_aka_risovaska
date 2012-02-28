TRANSLATIONS = moodbox_en.ts \
		    moodbox_ru.ts

QT += network xml svg
QTPLUGIN += qjpeg qgif qmng qsvg qtiff           
ICON = moodbox-dock.icns      
TARGET = MoodBox

INCLUDEPATH += /sw/include/ImageMagick /sw/include/ImageMagick/Magick++
LIBS += -lMagick++ -lMagickCore -lMagickWand
LIBS += -L/sw/lib/

HEADERS -= _PaletteConverter/stdafx.h \
           _PaletteConverter/targetver.h  

DEFINES += DEBUG
                        
# install into app bundle
data.path = Contents/Resources
data.files = $$(PWD)/Avatars $$(PWD)/Clipart $$(PWD)/Sound $$(PWD)/catalogue.pal
QMAKE_BUNDLE_DATA += data     
                                      
# framework setup
LIBS += -framework Sparkle -framework Growl

private_frameworks.path = Contents/Frameworks
private_frameworks.files = /Library/Frameworks/Growl.framework /Library/Frameworks/Sparkle.framework
QMAKE_BUNDLE_DATA += private_frameworks     

# customized plist file
QMAKE_INFO_PLIST = MoodBox_Info.plist

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
           ../../Velasquez/Qt/vcommon.h \
			autoupdater_mac.h

SOURCES -= _PaletteConverter/PaletteConverter.cpp \
           _PaletteConverter/stdafx.cpp \
           qtsingleapplication_win.cpp \
           qtsingleapplication_x11.cpp

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
                                     
# objective++ files need additional separated section, otherwise they will have problems on compile
OBJECTIVE_SOURCES = autoupdater_mac.mm \
					mactools.mm