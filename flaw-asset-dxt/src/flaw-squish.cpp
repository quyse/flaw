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

	const int flags = kDxt1 | kColourIterativeClusterFit;

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

extern "C" void flaw_squish_compress_bc2(u8 const* input, int inputLinePitch, u8* output)
{
	// make rgba input block
	u8 rgbaInput[64];
	for(int i = 0; i < 4; ++i)
		memcpy(rgbaInput + i * 16, input + i * inputLinePitch, 16);

	const int flags = kDxt1 | kColourIterativeClusterFit;

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

	// compress alpha
	CompressAlphaDxt5(rgbaInput, 0xffff, output + 8);
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

// Compress 32-byte RG block to 16 bytes using BC5 format.
// BC5 format is simply two BC4 blocks.
extern "C" void flaw_squish_compress_bc5(u8 const* input, int inputLinePitch, u8* output)
{
	u8 rgbaInput[64];
	// compress first block
	for(int i = 0; i < 16; ++i)
		rgbaInput[i * 4 + 3] = input[(i / 4) * inputLinePitch + (i % 4) * 2 + 0];
	CompressAlphaDxt5(rgbaInput, 0xffff, output);
	// compress second block
	for(int i = 0; i < 16; ++i)
		rgbaInput[i * 4 + 3] = input[(i / 4) * inputLinePitch + (i % 4) * 2 + 1];
	CompressAlphaDxt5(rgbaInput, 0xffff, output + 8);
}
