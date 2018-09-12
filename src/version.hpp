#pragma once

#include <inttypes.h>
#include <string>
#ifdef DONATE_LEVEL
#include "donate-level.hpp"
#endif

extern const char ver_long[];
extern const char ver_short[];
extern const char ver_html[];

inline std::string get_version_str()
{
#ifdef DONATE_LEVEL
	return std::string(ver_long) + std::to_string(uint32_t(fDevDonationLevel * 1000)) ;
#else
	return std::string(ver_long);
#endif
}

inline std::string get_version_str_short()
{
	return std::string(ver_short);
}
