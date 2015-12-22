#include "alpha.h"
using namespace squish;

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
