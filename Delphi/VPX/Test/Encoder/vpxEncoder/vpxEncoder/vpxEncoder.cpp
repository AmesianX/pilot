// vpxEncoder.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include "vpx/vpx_codec.h"
#include "vpx/vpx_encoder.h"
#include "vpx/vp8cx.h"

#define interface (vpx_codec_vp8_cx())

int _tmain(int argc, _TCHAR* argv[])
{
    printf("Using %s\n",vpx_codec_iface_name(interface));

	int width = 320;
	int height = 240;
	int gop_size = 0;

	vpx_codec_err_t result;
    struct vpx_codec_enc_cfg cfg;
    vpx_codec_ctx_t codec;
	vpx_image_t	raw;

	if (!vpx_img_alloc(&raw, VPX_IMG_FMT_RGB32 , width, height, 1)) {
        printf("Failed to allocate image\n");
		return -1;
	}

    if ((result = vpx_codec_enc_config_default(interface, &cfg, 0)) != VPX_CODEC_OK) {
        printf("Failed to get config: %s\n", vpx_codec_err_to_string(result));
		return -1;
	}

	cfg.g_w = width;
	cfg.g_h = height;
	cfg.kf_max_dist = gop_size;
	if (vpx_codec_enc_init(&codec, interface, &cfg, 0)) {
		printf("Failed to initialize encoder");
		return -1;
	}

	//vpx_codec_control(codec, VP8E_SET_CPUUSED, 16);

	scanf("\n");
	return 0;
}

