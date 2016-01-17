#include "stdafx.h"
#include "snappy-c.h"

extern "C" __declspec(dllexport) snappy_status compress(const char* input,
                              size_t input_length,
                              char* compressed,
                              size_t* compressed_length) 
{
	return snappy_compress(input, input_length,compressed, compressed_length);
}


extern "C" __declspec(dllexport) snappy_status uncompress(const char* compressed,
                                size_t compressed_length,
                                char* uncompressed,
                                size_t* uncompressed_length)
{
	return snappy_uncompress(compressed, compressed_length, uncompressed, uncompressed_length);
}


extern "C" __declspec(dllexport) size_t max_compressed_length(size_t source_length)
{
	return snappy_max_compressed_length(source_length);
}


extern "C" __declspec(dllexport) snappy_status uncompressed_length(const char* compressed,
                                         size_t compressed_length,
                                         size_t* result)
{
	return snappy_uncompressed_length(compressed, compressed_length, result);
}


extern "C" __declspec(dllexport) snappy_status validate_compressed_buffer(const char* compressed,
                                                size_t compressed_length)
{
	return snappy_validate_compressed_buffer(compressed, compressed_length);
}