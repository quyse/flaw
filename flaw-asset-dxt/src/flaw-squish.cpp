#include "alpha.h"
#include "clusterfit.h"
#include "colourset.h"
#include "singlecolourfit.h"
#include <cstring>
using namespace squish;

extern "C" void flaw_squish_compress_bc1(u8 const* input, int inputLinePitch, u8* output)
{
	// make rgba input block
	u8 rgbaInput[64];
	for(int i = 0; i < 16; ++i)
	{
		int k = (i / 4) * inputLinePitch + (i % 4) * 3;
		rgbaInput[i * 4 + 0] = input[k + 0];
		rgbaInput[i * 4 + 1] = input[k + 1];
		rgbaInput[i * 4 + 2] = input[k + 2];
		rgbaInput[i * 4 + 3] = 255;
	}

	int flags = kDxt1 | kColourIterativeClusterFit;

	// calculate color set
	ColourSet colours(rgbaInput, 0xffff, flags);
	// if there's just one color, do single color fit
	if(colours.GetCount() == 1)
	{
		SingleColourFit fit(&colours, flags);
		fit.Compress(output);
	}
	else
	{
		ClusterFit fit(&colours, flags, NULL);
		fit.Compress(output);
	}
}

// Compress 16-byte alpha block to 8 bytes using BC4 format.
// BC4 is used in DXT5 for alpha.
extern "C" void flaw_squish_compress_bc4(u8 const* input, int inputLinePitch, u8* output)
{
	// make rgba input block
	u8 rgbaInput[64];
	for(int i = 0; i < 16; ++i)
		rgbaInput[i * 4 + 3] = input[(i / 4) * inputLinePitch + i % 4];
	// compress
	CompressAlphaDxt5(rgbaInput, 0xffff, output);
}
