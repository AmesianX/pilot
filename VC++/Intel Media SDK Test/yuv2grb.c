/* yuv to rgb conversion stuff 

   Tridge, July 2000
*/
/* 
   Copyright (C) Andrew Tridgell 2000
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include "capture.h"


/* convert a YUV set to a rgb set - thanks to MartinS and
   http://www.efg2.com/lab/Graphics/Colors/YUV.htm */
static void yuvtorgb(int Y, int U, int V, u8 *rgb)
{
	int r, g, b;
	static short L1[256], L2[256], L3[256], L4[256], L5[256];
	static int initialised;

	if (!initialised) {
		int i;
		initialised=1;
		for (i=0;i<256;i++) {
			L1[i] = 1.164*(i-16);
			L2[i] = 1.596*(i-128);
			L3[i] = -0.813*(i-128);
			L4[i] = 2.018*(i-128);
			L5[i] = -0.391*(i-128);
		}
	}
#if 0
	r = 1.164*(Y-16) + 1.596*(V-128);
	g = 1.164*(Y-16) - 0.813*(U-128) - 0.391*(V-128);
	b = 1.164*(Y-16) + 2.018*(U-128);
#endif

	r = L1[Y] + L2[V];
	g = L1[Y] + L3[U] + L5[V];
	b = L1[Y] + L4[U];

	if (r < 0) r = 0;
	if (g < 0) g = 0;
	if (b < 0) b = 0;
	if (r > 255) r = 255;
	if (g > 255) g = 255;
	if (b > 255) b = 255;

	rgb[0] = r;
	rgb[1] = g;
	rgb[2] = b;
}

/* convert yuv to rgb */
void yuv_convert(u8 *buf, u8 *rgb, int xsize, int ysize)
{
	int i;

	for (i=0;i<xsize*ysize;i+=2) {
		int Y1, Y2, U, V;

		Y1 = buf[2*i+0];
		Y2 = buf[2*i+2];
		U = buf[2*i+1];
		V = buf[2*i+3];

		yuvtorgb(Y1, U, V, &rgb[3*i]);
		yuvtorgb(Y2, U, V, &rgb[3*(i+1)]);
	}
}


/* this is _really_ weird */
void yuv_block_convert(u8 *buf, u8 *rgb, int xsize, int ysize)
{
	u8 Y[128], U[64], V[64];
	int i, j, x, y, xx, yy;

	memset(Y, 32, sizeof(Y));
	memset(U, 32, sizeof(U));
	memset(V, 32, sizeof(V));

#define YINC 8
#define XINC 16

	for (y=0;y<ysize;y+=YINC) {
		for (x=0;x<xsize;x+=XINC) {

			memcpy(Y, buf, 128); 
			memcpy(U, buf+128, 64);
			memcpy(V, buf+192, 64);
			buf += 256;

			for (i=0;i<128;i+=2) {
				xx = i%16;
				yy = i/16;
				if (xx < 8 && yy < 8) {
					j = yy*8 + xx;
				} else if (xx >= 8 && yy < 8) {
					j = 64 + yy*8 + (xx-8);
				} else if (xx < 8 && yy >= 8) {
					j = yy*8 + xx;
				} else {
					j = 64 + yy*8 + xx-8;
				}
				yuvtorgb(Y[j], U[i>>1], V[i>>1],
					 &rgb[3*((y+yy)*xsize+(x+xx))]);
				yuvtorgb(Y[j+1], U[i>>1], V[i>>1],
					 &rgb[3*((y+yy)*xsize+(x+xx+1))]);
			}
		}
	}
}

