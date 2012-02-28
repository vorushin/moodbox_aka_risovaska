// PaletteConverter.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include <io.h>
#include <string>
#include <fstream>
#include <iostream>

using namespace std;



void AddPalette(fstream& o, string paletteFileName)
{
	fstream f(paletteFileName.c_str());
	
	char buf[10000];
	f.read(buf, 10000);
	int read = f.gcount();

	string paletteName = paletteFileName.substr(paletteFileName.find_last_of("\\") + 1, 
		paletteFileName.find_last_of(".") - paletteFileName.find_last_of("\\") - 1);

	o << "<Palette Name=\"" << paletteName << "\">";
	
	int cur = 0;
	int colorNum = 0;
	while (cur < read) {
		if (buf[cur] == '#' && isalnum(buf[cur + 1])) {
			string color;
			color += buf[cur + 1];
			color += buf[cur + 2];
			color += buf[cur + 3];
			color += buf[cur + 4];
			color += buf[cur + 5];
			color += buf[cur + 6];
			cout << color << endl;
			o << "<Color Value=\"#" << color << "FF\"/>";
			cur += 7;
			colorNum++;
		}
		else cur++;
	}

	while (colorNum < 16) {
		o << "<Color Value=\"#FFFFFFFF\"/>";
		colorNum++;
	}

	o << "</Palette>";
}


int _tmain(int argc, _TCHAR* argv[])
{
	fstream o("C:\\Palettes\\custom.pal", ios_base::out);
	o << "<Palettes>";

	struct _finddata_t info;
	int files = _findfirst("C:\\Palettes\\*.cs", &info);
	if (files == -1)
		return 1;
	
	AddPalette(o, string("C:\\Palettes\\") + info.name); 

	while (!_findnext(files, &info))
		AddPalette(o, string("C:\\Palettes\\") + info.name);

	o << "</Palettes>";

	return 0;
}


