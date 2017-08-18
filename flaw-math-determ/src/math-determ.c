#include <stdint.h>
#include <xmmintrin.h>

static const uint32_t g_pi = 0x40490fdb;
static const uint32_t g_pi2 = 0x40c90fdb;
static const uint32_t g_pi_2 = 0x3fc90fdb;

uint32_t determ_float_from(float a)
{
	return *(uint32_t*)&a;
}

float determ_float_to(uint32_t a)
{
	return *(float*)&a;
}

int determ_eq_float(uint32_t a, uint32_t b)
{
	return !!_mm_comieq_ss(_mm_load_ss((float*)&a), _mm_load_ss((float*)&b));
}

int determ_neq_float(uint32_t a, uint32_t b)
{
	return !!_mm_comineq_ss(_mm_load_ss((float*)&a), _mm_load_ss((float*)&b));
}

int determ_lt_float(uint32_t a, uint32_t b)
{
	return !!_mm_comilt_ss(_mm_load_ss((float*)&a), _mm_load_ss((float*)&b));
}

int determ_le_float(uint32_t a, uint32_t b)
{
	return !!_mm_comile_ss(_mm_load_ss((float*)&a), _mm_load_ss((float*)&b));
}

int determ_gt_float(uint32_t a, uint32_t b)
{
	return !!_mm_comigt_ss(_mm_load_ss((float*)&a), _mm_load_ss((float*)&b));
}

int determ_ge_float(uint32_t a, uint32_t b)
{
	return !!_mm_comige_ss(_mm_load_ss((float*)&a), _mm_load_ss((float*)&b));
}

uint32_t determ_add_float(uint32_t a, uint32_t b)
{
	uint32_t r;
	_mm_store_ss((float*)&r, _mm_add_ss(_mm_load_ss((float*)&a), _mm_load_ss((float*)&b)));
	return r;
}

uint32_t determ_subtract_float(uint32_t a, uint32_t b)
{
	uint32_t r;
	_mm_store_ss((float*)&r, _mm_sub_ss(_mm_load_ss((float*)&a), _mm_load_ss((float*)&b)));
	return r;
}

uint32_t determ_multiply_float(uint32_t a, uint32_t b)
{
	uint32_t r;
	_mm_store_ss((float*)&r, _mm_mul_ss(_mm_load_ss((float*)&a), _mm_load_ss((float*)&b)));
	return r;
}

uint32_t determ_divide_float(uint32_t a, uint32_t b)
{
	uint32_t r;
	_mm_store_ss((float*)&r, _mm_div_ss(_mm_load_ss((float*)&a), _mm_load_ss((float*)&b)));
	return r;
}

int determ_isneg_float(uint32_t a)
{
	return !!(a & 0x80000000);
}

uint32_t determ_negate_float(uint32_t a)
{
	return a ^ 0x80000000;
}

uint32_t determ_abs_float(uint32_t a)
{
	return a & 0x7FFFFFFF;
}

uint32_t determ_floor_float(uint32_t a)
{
	__m128 p = _mm_load_ss((float*)&a);
	__m128 t = _mm_cvtepi32_ps(_mm_cvttps_epi32(p));
	uint32_t r;
	_mm_store_ss((float*)&r, _mm_sub_ps(t, _mm_and_ps(_mm_cmplt_ps(p, t), _mm_set_ss(1.0f))));
	return r;
}

uint32_t determ_ceil_float(uint32_t a)
{
	__m128 p = _mm_load_ss((float*)&a);
	__m128 t = _mm_cvtepi32_ps(_mm_cvttps_epi32(p));
	uint32_t r;
	_mm_store_ss((float*)&r, _mm_add_ps(t, _mm_and_ps(_mm_cmpgt_ps(p, t), _mm_set_ss(1.0f))));
	return r;
}

uint32_t determ_trunc_float(uint32_t a)
{
	uint32_t r;
	_mm_store_ss((float*)&r, _mm_cvtepi32_ps(_mm_cvttps_epi32(_mm_load_ss((float*)&a))));
	return r;
}

int32_t determ_trunc_float_int(uint32_t a)
{
	return _mm_cvtt_ss2si(_mm_load_ss((float*)&a));
}

uint32_t determ_sqrt_float(uint32_t a)
{
	uint32_t r;
	_mm_store_ss((float*)&r, _mm_sqrt_ss(_mm_load_ss((float*)&a)));
	return r;
}

static uint32_t determ_sin_float_impl(__m128 a)
{
	// reduce to range [0, pi]
	__m128 n;
	__m128 pi = _mm_load_ss((float*)&g_pi);
	if(_mm_comige_ss(a, pi))
	{
		n = _mm_cvtepi32_ps(_mm_cvttps_epi32(_mm_div_ss(a, pi)));
		a = _mm_sub_ss(a, _mm_mul_ss(n, pi));
	}
	// reduce to range [0, pi/2]
	__m128 pi_2 = _mm_load_ss((float*)&g_pi_2);
	if(_mm_comige_ss(a, pi_2)) a = pi - a;

	static const uint32_t
		c_k1 = 0x3f7ff052, //  0.99976073735983227f
		c_k3 = 0xbe29c7cc, // -0.16580121984779175f
		c_k5 = 0x3bf7d14a; //  0.00756279111686865f

	__m128 a2 = _mm_mul_ss(a, a);
	__m128 a3 = _mm_mul_ss(a2, a);
	__m128 a5 = _mm_mul_ss(a3, a2);

	a = _mm_add_ss(
		_mm_add_ss(
			_mm_mul_ss(a, _mm_load_ss((float*)&c_k1)),
			_mm_mul_ss(a3, _mm_load_ss((float*)&c_k3))
			),
		_mm_mul_ss(a5, _mm_load_ss((float*)&c_k5))
		);

	int nn = _mm_cvtt_ss2si(n);

	uint32_t r;
	_mm_store_ss((float*)&r, a);
	if(nn % 2) r ^= 0x80000000;
	return r;
}

uint32_t determ_sin_float(uint32_t a)
{
	// handle negative values
	if(determ_isneg_float(a))
	{
		a = determ_negate_float(a);
		return determ_negate_float(determ_sin_float_impl(_mm_load_ss((float*)&a)));
	}
	else
	{
		return determ_sin_float_impl(_mm_load_ss((float*)&a));
	}
}

uint32_t determ_cos_float(uint32_t a)
{
	return determ_sin_float(determ_add_float(a, g_pi_2));
}
