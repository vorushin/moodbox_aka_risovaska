#ifndef VCOMMON_H
#define VCOMMON_H

// Common constants and defines
namespace Velasquez
{

// Math
#ifndef PI
#define PI	3.14159265358979323846
#endif

#ifndef TWOPI
#define TWOPI	PI*2
#endif

// Handy macro
#ifndef DELETE_AND_NULL
#define DELETE_AND_NULL(x)	delete x; x = NULL
#endif

// Extensions
#define SVG_EXT                         "*.svg"

#define SUPPORTED_IMAGE_FORMATS         "*.jpg *.jpeg *.png *.gif *.bmp *.svg *.mng *.tiff"
#define SUPPORTED_TEXT_FORMATS          "*.txt *.html"

// Scene size
#define SCENE_WIDTH					388
#define SCENE_HEIGHT				240

// Background
#define BACKGROUND_COLOR_SETTING	100
#define DEFAULT_BACKGROUND_COLOR	0xFFFFFF
#define BACKGROUND_COLOR_TYPE		"image/background-color"

}

#endif // VCOMMON_H
