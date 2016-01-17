/*
 * Copyright (c) 2001 Fabrice Bellard
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * avcodec API use example.
 *
 * Note that this library only handles codecs (mpeg, mpeg4, etc...),
 * not file formats (avi, vob, etc...). See library 'libavformat' for the
 * format handling
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef HAVE_AV_CONFIG_H
#undef HAVE_AV_CONFIG_H
#endif

#include "libavutil/opt.h"
#include "libavcodec/avcodec.h"
#include "libavutil/mathematics.h"

#define INBUF_SIZE 4096
#define AUDIO_INBUF_SIZE 20480
#define AUDIO_REFILL_THRESH 4096

/*
 * Audio encoding example
 */
static void audio_encode_example(const char *filename)
{
    AVCodec *codec;
    AVCodecContext *c= NULL;
    int frame_size, i, j, out_size, outbuf_size;
    FILE *f;
    short *samples;
    float t, tincr;
    uint8_t *outbuf;

    printf("Audio encoding\n");

    /* find the MP2 encoder */
    codec = avcodec_find_encoder(CODEC_ID_MP2);
    if (!codec) {
        fprintf(stderr, "codec not found\n");
        exit(1);
    }

    c= avcodec_alloc_context();

    /* put sample parameters */
    c->bit_rate = 64000;
    c->sample_rate = 44100;
    c->channels = 2;

    /* open it */
    if (avcodec_open(c, codec) < 0) {
        fprintf(stderr, "could not open codec\n");
        exit(1);
    }

    /* the codec gives us the frame size, in samples */
    frame_size = c->frame_size;
    samples = malloc(frame_size * 2 * c->channels);
    outbuf_size = 10000;
    outbuf = malloc(outbuf_size);

    f = fopen(filename, "wb");
    if (!f) {
        fprintf(stderr, "could not open %s\n", filename);
        exit(1);
    }

    /* encode a single tone sound */
    t = 0;
    tincr = 2 * M_PI * 440.0 / c->sample_rate;
    for(i=0;i<200;i++) {
        for(j=0;j<frame_size;j++) {
            samples[2*j] = (int)(sin(t) * 10000);
            samples[2*j+1] = samples[2*j];
            t += tincr;
        }
        /* encode the samples */
        out_size = avcodec_encode_audio(c, outbuf, outbuf_size, samples);
        fwrite(outbuf, 1, out_size, f);
    }
    fclose(f);
    free(outbuf);
    free(samples);

    avcodec_close(c);
    av_free(c);
}

/*
 * Audio decoding.
 */
static void audio_decode_example(const char *outfilename, const char *filename)
{
    AVCodec *codec;
    AVCodecContext *c= NULL;
    int out_size, len;
    FILE *f, *outfile;
    uint8_t *outbuf;
    uint8_t inbuf[AUDIO_INBUF_SIZE + FF_INPUT_BUFFER_PADDING_SIZE];
    AVPacket avpkt;

    av_init_packet(&avpkt);

    printf("Audio decoding\n");

    /* find the mpeg audio decoder */
    codec = avcodec_find_decoder(CODEC_ID_MP2);
    if (!codec) {
        fprintf(stderr, "codec not found\n");
        exit(1);
    }

    c= avcodec_alloc_context();

    /* open it */
    if (avcodec_open(c, codec) < 0) {
        fprintf(stderr, "could not open codec\n");
        exit(1);
    }

    outbuf = malloc(AVCODEC_MAX_AUDIO_FRAME_SIZE);

    f = fopen(filename, "rb");
    if (!f) {
        fprintf(stderr, "could not open %s\n", filename);
        exit(1);
    }
    outfile = fopen(outfilename, "wb");
    if (!outfile) {
        av_free(c);
        exit(1);
    }

    /* decode until eof */
    avpkt.data = inbuf;
    avpkt.size = fread(inbuf, 1, AUDIO_INBUF_SIZE, f);

    while (avpkt.size > 0) {
        out_size = AVCODEC_MAX_AUDIO_FRAME_SIZE;
        len = avcodec_decode_audio3(c, (short *)outbuf, &out_size, &avpkt);
        if (len < 0) {
            fprintf(stderr, "Error while decoding\n");
            exit(1);
        }
        if (out_size > 0) {
            /* if a frame has been decoded, output it */
            fwrite(outbuf, 1, out_size, outfile);
        }
        avpkt.size -= len;
        avpkt.data += len;
        if (avpkt.size < AUDIO_REFILL_THRESH) {
            /* Refill the input buffer, to avoid trying to decode
             * incomplete frames. Instead of this, one could also use
             * a parser, or use a proper container format through
             * libavformat. */
            memmove(inbuf, avpkt.data, avpkt.size);
            avpkt.data = inbuf;
            len = fread(avpkt.data + avpkt.size, 1,
                        AUDIO_INBUF_SIZE - avpkt.size, f);
            if (len > 0)
                avpkt.size += len;
        }
    }

    fclose(outfile);
    fclose(f);
    free(outbuf);

    avcodec_close(c);
    av_free(c);
}

static void set_conf(AVCodecContext *c)
{
	c->coder_type			= FF_CODER_TYPE_VLC;
//	c->refs				= 3;
	c->flags 			|= CODEC_FLAG_LOOP_FILTER;
	c->deblockalpha			= 0;
	c->deblockbeta			= 0;
	c->partitions			= X264_PART_I4X4
					| X264_PART_P8X8
					| X264_PART_B8X8;
//	c->me_method 			= ME_HEX; 
//	c->me_subpel_quality 		= 7; 
//	c->flags2 			|= CODEC_FLAG2_PSY;
//	c->psy_rd			= 1.0;
//	c->psy_trellis			= 0.0;
//	c->flags2 			|= CODEC_FLAG2_MIXED_REFS;
//	c->me_range 			= 16; 
//	c->me_cmp 			= FF_CMP_CHROMA;
//	c->trellis 			= 0;	// 0: speed up!
//	c->flags2 			&= ~CODEC_FLAG2_8X8DCT;
//	c->flags2 			|= CODEC_FLAG2_FASTPSKIP;
//	c->chromaoffset 		= -2;
//	c->thread_count 		= 1;
//	c->slices			= 0;
//	c->noise_reduction		= 0;
//    	c->flags			|= CODEC_FLAG_INTERLACED_DCT;
//	c->max_b_frames 		= 0;
//	c->weighted_p_pred 		= 0;
	c->gop_size 			= 25;
//	c->keyint_min 			= 25;
//	c->scenechange_threshold 	= 40; 	
//	c->flags2 			&= ~CODEC_FLAG2_INTRA_REFRESH;
//	c->rc_lookahead 		= 40;
//	c->crf				= 23.0;	// ABR
//	c->cqp				= -1;	// ABR
//	c->flags2 			|= CODEC_FLAG2_MBTREE;
	c->bit_rate 			= 10 * 1000 * 1000; 
//	c->bit_rate_tolerance 		= 1.0;	//not config
//	c->qcompress 			= 0.6; 
	c->qmin 			= 10; 
	c->qmax 			= 51;
	c->max_qdiff 			= 4;
//	c->i_quant_factor 		= 10/14; // i_qfactor=0.71
//	c->aq_mode			= 1;
//	c->aq_strength			= 1.00;

//	c->rc_max_rate			= 200 * 1000;
//	c->rc_min_rate			= 300 * 1000;
//	c->rc_buffer_size		= c->rc_max_rate * 100;
	c->flags			|= CODEC_FLAG_GLOBAL_HEADER;
//	c->flags 			|= CODEC_FLAG_PASS1;

	/* */
	c->pix_fmt 			= PIX_FMT_YUV420P;
	c->width 			= 384; // 320; //176;
	c->height			= 288; //240; // 144;
	c->time_base.num 		= 1;
	c->time_base.den 		= 25; //15;
}

static void *alloc_priv_context(int size, const AVClass *class)
{
    void *p = av_mallocz(size);
    if (p) {
        *(const AVClass **)p = class;
        av_opt_set_defaults(p);
    }
    return p;
}
/*
 * Video encoding example
 */
static void video_encode_example(const char *filename)
{
    AVCodec *codec;

    AVCodecContext *c= NULL;
    int i, out_size, size, x, y, outbuf_size;
    FILE *f;
    AVFrame *picture;
    uint8_t *outbuf, *picture_buf;

    printf("Video encoding\n");

    /* find the mpeg1 video encoder */
//    codec = avcodec_find_encoder(CODEC_ID_MPEG1VIDEO);
    codec = avcodec_find_encoder(CODEC_ID_H264);
    if (!codec) {
        fprintf(stderr, "codec not found\n");
        exit(1);
    }

//    c= avcodec_alloc_context();
    c= avcodec_alloc_context3(codec);

#if 0
    char buf[256];
    const AVOption *opt;
    const char *str = "600K";
//    str = av_get_string(c, "b", &opt, buf, sizeof(buf));
    printf("str = %s\n", str);
    if (av_set_string3(c, "b", str, 1, NULL) < 0) {
	    fprintf(stderr, "Invalid value '%s' for option '%s'\n",
			    "b", "200K");
	    exit(1);
    }
#endif
    printf("codec->priv_data_size = %d\n", codec->priv_data_size);
    c->priv_data = alloc_priv_context(codec->priv_data_size, codec->priv_class);
    int flags = AV_OPT_FLAG_VIDEO_PARAM | AV_OPT_FLAG_ENCODING_PARAM;
    if (av_find_opt(c->priv_data, "profile", NULL, flags, flags)) {
	    if (av_set_string3(c->priv_data, "profile", "baseline", 1, NULL) < 0) {
		    fprintf(stderr, "Invalid value '%s' for option '%s'\n",
				    "profile", "baseline");
		    exit(1);
	    }
    }
	    //    c= avcodec_alloc_context3(codec);

    set_conf(c);
    //    c= avcodec_alloc_context3(codec);
    printf("profile = %d\n", c->profile);

    picture= avcodec_alloc_frame();

#if 0
//	c->codec_type = CODEC_TYPE_VIDEO;
//	c->codec_id = CODEC_ID_H264;

	/* presets */
//    	c->profile = FF_PROFILE_H264_CONSTRAINED_BASELINE;
    	c->profile = FF_PROFILE_H264_BASELINE;
    printf("profile = %d\n", c->profile);
// 	c->level = -1;
#if 1
    	/* Frame-type option */
	c->gop_size 			= 250;	// def: 250
	c->keyint_min 			= c->gop_size / 10;	// def: 25
//	c->scenechange_threshold 	= 40; 	// def: 40
//	c->max_b_frames 		= 0;	// def: 3 not support B-frames.
//	c->b_frame_strategy		= 0;	// def: 1
//	c->bframebias			= 0;	// def: 0
//	c->flags2 			|= CODEC_FLAG2_BPYRAMID;
	c->coder_type			= FF_CODER_TYPE_VLC;	// def: CABAC
//	c->refs				= 3;	// def: 3
//	c->flags 			|= CODEC_FLAG_4MV;
	c->flags 			|= CODEC_FLAG_LOOP_FILTER; // def: enabled
	c->deblockalpha			= 0;	// def: 0
	c->deblockbeta			= 0;	// def: 0

	/* Ratecontrol */
//	c->cqp				= 51;
	c->cqp				= -2;
	c->bit_rate 			= 192 * 1000; // (int) (32.f);
	/*
	 * number of bits the bitstream is allowed to diverge from the reference. 
	 * the reference can be CBR (for CBR pass1) or VBR (for pass2)
	 */
//	c->bit_rate_tolerance = (int)(c->bit_rate * 0.20f); // (int) (32.f * 0.20f);
	c->rc_max_rate			= 256 * 1000;
	c->slices			= 0;
//	c->rc_min_rate			= 10000;
	c->rc_buffer_size		= c->rc_max_rate;
//	c->crf_max			= 25.0;
//	c->rc_initial_cplx		= 0.9;
	c->crf				= 0.0; //23.0;	// def 23.0
//	c->rc_lookahead 		= 40;	// def: 40

	c->qmin 			= 10; 	// def: 10
	c->qmax 			= 51; 	// def: 51
	c->max_qdiff 			= 4; 	// def: 4

	c->qcompress 			= 0.6; 	// 0.3 	// qcomp=0.6

//	c->i_quant_factor 		= 0.71; // i_qfactor=0.71

	/* Analysis */
	c->partitions			= X264_PART_I4X4 | X264_PART_I8X8 
					| X264_PART_P8X8 | X264_PART_P4X4
					| X264_PART_B8X8;
//	c->partitions			= X264_PART_I8X8 | X264_PART_P8X8;
//	c->partitions = 0;
#endif					
	/* */
	c->pix_fmt 			= PIX_FMT_YUV420P;
	c->width 			= 320; //176;
	c->height			= 240; // 144;
	c->time_base.num 		= 1;
	c->time_base.den 		= 15;
	c->thread_count 		= 1;

///*	
//    	c->me_cmp |= FF_CMP_CHROMA; // 1; // cmp=+chroma, where CHROMA = 1
//	c->me_method = ME_HEX; // me_method=hex
//	c->me_subpel_quality = 7; //0; // subq=7
  // 	c->flags2 |= CODEC_FLAG2_MIXED_REFS;
   //	c->flags2 |= CODEC_FLAG2_PSY;
//	c->psy_rd = 1;
  //  	c->psy_trellis = 0.0;
//	c->me_range = 16; // me_range=16
//	c->i_quant_factor = 0.71; // i_qfactor=0.71
//	c->flags2 |= CODEC_FLAG2_FASTPSKIP; // set b_fast_pskip
//	c->flags = c->flags & ~CODEC_FLAG_PASS1; // clear param->rc.b_stat_write
//	c->flags = c->flags & ~CODEC_FLAG_PASS2; // clear param->rc.b_stat_read
//*/
//	c->flags2 &= ~CODEC_FLAG2_WPRED; 
//	c->flags2 &= ~CODEC_FLAG2_8X8DCT;
//	c->weighted_p_pred = 0; // wpredp=2
//	c->trellis = 1; // 
 //   	c->chromaoffset = 1;
//	c->directpred = 0; // directpred=1
/*
  1 coder=0
    2 bf=0
      3 flags2=-wpred-dct8x8
        4 wpredp=0
	*/
	               
    /* */
#endif
#if 0
#if 1
    /*
     * H.264 Profile
     02253 #define FF_PROFILE_UNKNOWN -99
     02254 #define FF_PROFILE_RESERVED -100
     02255 
     02256 #define FF_PROFILE_AAC_MAIN 0
     02257 #define FF_PROFILE_AAC_LOW  1
     02258 #define FF_PROFILE_AAC_SSR  2
     02259 #define FF_PROFILE_AAC_LTP  3
     02260 
     02261 #define FF_PROFILE_DTS         20
     02262 #define FF_PROFILE_DTS_ES      30
     02263 #define FF_PROFILE_DTS_96_24   40
     02264 #define FF_PROFILE_DTS_HD_HRA  50
     02265 #define FF_PROFILE_DTS_HD_MA   60
     02266 
     02267 #define FF_PROFILE_MPEG2_422    0
     02268 #define FF_PROFILE_MPEG2_HIGH   1
     02269 #define FF_PROFILE_MPEG2_SS     2
     02270 #define FF_PROFILE_MPEG2_SNR_SCALABLE  3
     02271 #define FF_PROFILE_MPEG2_MAIN   4
     02272 #define FF_PROFILE_MPEG2_SIMPLE 5
     02273 
     02274 #define FF_PROFILE_H264_CONSTRAINED  (1<<9)  // 8+1; constraint_set1_flag
     02275 #define FF_PROFILE_H264_INTRA        (1<<11) // 8+3; constraint_set3_flag
     02276 
     02277 #define FF_PROFILE_H264_BASELINE             66
     02278 #define FF_PROFILE_H264_CONSTRAINED_BASELINE (66|FF_PROFILE_H264_CONSTRAINED)
     02279 #define FF_PROFILE_H264_MAIN                 77
     02280 #define FF_PROFILE_H264_EXTENDED             88
     02281 #define FF_PROFILE_H264_HIGH                 100
     02282 #define FF_PROFILE_H264_HIGH_10              110
     02283 #define FF_PROFILE_H264_HIGH_10_INTRA        (110|FF_PROFILE_H264_INTRA)
     02284 #define FF_PROFILE_H264_HIGH_422             122
     02285 #define FF_PROFILE_H264_HIGH_422_INTRA       (122|FF_PROFILE_H264_INTRA)
     02286 #define FF_PROFILE_H264_HIGH_444             144
     02287 #define FF_PROFILE_H264_HIGH_444_PREDICTIVE  244
     02288 #define FF_PROFILE_H264_HIGH_444_INTRA       (244|FF_PROFILE_H264_INTRA)
     02289 #define FF_PROFILE_H264_CAVLC_444            44
     02290 
     02291 #define FF_PROFILE_VC1_SIMPLE   0
     02292 #define FF_PROFILE_VC1_MAIN     1
     02293 #define FF_PROFILE_VC1_COMPLEX  2
     02294 #define FF_PROFILE_VC1_ADVANCED 3
     02295 
     02296 #define FF_PROFILE_MPEG4_SIMPLE                     0
     02297 #define FF_PROFILE_MPEG4_SIMPLE_SCALABLE            1
     02298 #define FF_PROFILE_MPEG4_CORE                       2
     02299 #define FF_PROFILE_MPEG4_MAIN                       3
     02300 #define FF_PROFILE_MPEG4_N_BIT                      4
     02301 #define FF_PROFILE_MPEG4_SCALABLE_TEXTURE           5
     02302 #define FF_PROFILE_MPEG4_SIMPLE_FACE_ANIMATION      6
     02303 #define FF_PROFILE_MPEG4_BASIC_ANIMATED_TEXTURE     7
     02304 #define FF_PROFILE_MPEG4_HYBRID                     8
     02305 #define FF_PROFILE_MPEG4_ADVANCED_REAL_TIME         9
     02306 #define FF_PROFILE_MPEG4_CORE_SCALABLE             10
     02307 #define FF_PROFILE_MPEG4_ADVANCED_CODING           11
     02308 #define FF_PROFILE_MPEG4_ADVANCED_CORE             12
     02309 #define FF_PROFILE_MPEG4_ADVANCED_SCALABLE_TEXTURE 13
     02310 #define FF_PROFILE_MPEG4_SIMPLE_STUDIO             14
     02311 #define FF_PROFILE_MPEG4_ADVANCED_SIMPLE           15

     */
    c->profile = FF_PROFILE_H264_BASELINE;
    c->level = 1;

    /*
     *ENTROPY CODING MODE
     *	
     02000 #define FF_CODER_TYPE_VLC       0	==> CAVLC
     02001 #define FF_CODER_TYPE_AC        1	==> CABAC
     02002 #define FF_CODER_TYPE_RAW       2
     02003 #define FF_CODER_TYPE_RLE       3
     02004 #define FF_CODER_TYPE_DEFLATE   4
     *
     */
    c->coder_type = FF_CODER_TYPE_VLC; // coder = 1

    /*
    00572 #define CODEC_FLAG_QSCALE 0x0002  
    00573 #define CODEC_FLAG_4MV    0x0004  
    00574 #define CODEC_FLAG_QPEL   0x0010  
    00575 #define CODEC_FLAG_GMC    0x0020  
    00576 #define CODEC_FLAG_MV0    0x0040  
    00577 #define CODEC_FLAG_PART   0x0080  
    00578 
    00583 #define CODEC_FLAG_INPUT_PRESERVED 0x0100
    00584 #define CODEC_FLAG_PASS1           0x0200   
    00585 #define CODEC_FLAG_PASS2           0x0400   
    00586 #define CODEC_FLAG_EXTERN_HUFF     0x1000   
    00587 #define CODEC_FLAG_GRAY            0x2000   
    00588 #define CODEC_FLAG_EMU_EDGE        0x4000   
    00589 #define CODEC_FLAG_PSNR            0x8000   
    00590 #define CODEC_FLAG_TRUNCATED       0x00010000 
    00592 #define CODEC_FLAG_NORMALIZE_AQP  0x00020000 
    00593 #define CODEC_FLAG_INTERLACED_DCT 0x00040000 
    00594 #define CODEC_FLAG_LOW_DELAY      0x00080000 
    00595 #define CODEC_FLAG_ALT_SCAN       0x00100000 
    00596 #define CODEC_FLAG_GLOBAL_HEADER  0x00400000 
    00597 #define CODEC_FLAG_BITEXACT       0x00800000 
    00598 // Fx : Flag for h263+ extra options 
    00599 #define CODEC_FLAG_AC_PRED        0x01000000 
    00600 #define CODEC_FLAG_H263P_UMV      0x02000000 
    00601 #define CODEC_FLAG_CBP_RD         0x04000000 
    00602 #define CODEC_FLAG_QP_RD          0x08000000 
    00603 #define CODEC_FLAG_H263P_AIV      0x00000008 
    00604 #define CODEC_FLAG_OBMC           0x00000001 
    00605 #define CODEC_FLAG_LOOP_FILTER    0x00000800 
    00606 #define CODEC_FLAG_H263P_SLICE_STRUCT 0x10000000
    00607 #define CODEC_FLAG_INTERLACED_ME  0x20000000 
    00608 #define CODEC_FLAG_SVCD_SCAN_OFFSET 0x40000000 
    00609 #define CODEC_FLAG_CLOSED_GOP     0x80000000

     */
//    c->flags |= CODEC_FLAG_LOOP_FILTER; // flags=+loop

    /*
     * Motion Estimation

    01861 #define FF_CMP_SAD    0
    01862 #define FF_CMP_SSE    1
    01863 #define FF_CMP_SATD   2
    01864 #define FF_CMP_DCT    3
    01865 #define FF_CMP_PSNR   4
    01866 #define FF_CMP_BIT    5
    01867 #define FF_CMP_RD     6
    01868 #define FF_CMP_ZERO   7
    01869 #define FF_CMP_VSAD   8
    01870 #define FF_CMP_VSSE   9
    01871 #define FF_CMP_NSSE   10
    01872 #define FF_CMP_W53    11
    01873 #define FF_CMP_W97    12
    01874 #define FF_CMP_DCTMAX 13
    01875 #define FF_CMP_DCT264 14
    01876 #define FF_CMP_CHROMA 256
     */
//    c->me_cmp |= FF_CMP_CHROMA; // 1; // cmp=+chroma, where CHROMA = 1

    /*
    02506 #define X264_PART_I4X4 0x001  // Analyze i4x4 
    02507 #define X264_PART_I8X8 0x002  // Analyze i8x8 (requires 8x8 transform)
    02508 #define X264_PART_P8X8 0x010  // Analyze p16x8, p8x16 and p8x8 
    02509 #define X264_PART_P4X4 0x020  // Analyze p8x4, p4x8, p4x4 
    02510 #define X264_PART_B8X8 0x100  // Analyze b16x8, b8x16 and b8x8 

     */
//    c->partitions |= X264_PART_I4X4 | X264_PART_I8X8 | X264_PART_P8X8 | X264_PART_P4X4 | X264_PART_B8X8; // partitions=+parti8x8+parti4x4+partp8x8+partb8x8
	c->partitions = 0;

    /*
	    00454 enum Motion_Est_ID {
	    00455     ME_ZERO = 1,    
	    00456     ME_FULL,
	    00457     ME_LOG,
	    00458     ME_PHODS,
	    00459     ME_EPZS,        
	    00460     ME_X1,          
	    00461     ME_HEX,         
	    00462     ME_UMH,         
	    00463     ME_ITER,        
	    00464     ME_TESA,        
	    00465 };

    1 (zero), 2 (full), 3 (log), 4 (phods), 5 (epzs), 
    6 (x1), 7 (hex), 8 (umh), 9 (iter), 10 (tesa) 
    [7, 8, 10 are x264 specific, 9 is snow specific]
     */
    c->me_method = ME_ZERO; //ME_HEX; // me_method=hex

    /*
     * subpel ME quality
     */
    c->me_subpel_quality = 7; //0; // subq=7

    /*
     * maximum motion estimation search range in subpel units If 0 then no limit.
     */
    c->me_range = 16; // me_range=16

    /* 
     * the number of pictures in a group of pictures, or 0 for intra_only
     */
    c->gop_size = 30*3; // g=250

    /* 
     * minimum GOP size
     */
    c->keyint_min = 30; // keyint_min=25
    
    /*
     * scene change detection threshold 0 is default, larger means fewer detected scene changes. 
     */
    c->scenechange_threshold = 0; //40; // sc_threshold=40

    /*
     * qscale factor between P and I-frames If > 0 then the last p frame quantizer will be used (q= lastp_q*factor+offset). 
     * If < 0 then normal ratecontrol will be done (q= -normal_q*factor+offset).
     */
    c->i_quant_factor = 0.71; // i_qfactor=0.71

    /*
     *
     */
//    c->b_frame_strategy = 0; // 1; // b_strategy=1

    /*
     * amount of qscale change between easy & hard scenes (0.0-1.0) 
     */
    c->qcompress = 0.6; // qcomp=0.6

    /*
     * minimum quantizer
     */
   c->qmin = 10; //0; // qmin=10

    /*
     * maximum quantizer
     */
   c->qmax = 51; //69; // qmax=51

    /*
     * maximum quantizer difference between frames
     */
    c->max_qdiff = 4; // qdiff=4

    /*
     * maximum number of B-frames between non-B-frames 
     * Note: The output will be delayed by max_b_frames+1 relative to the input. 
     */
    c->max_b_frames = 2; // 3; // bf=3
    
    /* 
     * number of reference frames
     */
    c->refs = 0; // refs=3

    /*
     * direct MV prediction mode - 0 (none), 1 (spatial), 2 (temporal), 3 (auto)
     */
    c->directpred = 0; // directpred=1
    
    /*
     * trellis RD quantization
     */
    c->trellis = 0; // trellis=1

    /*
     00610 #define CODEC_FLAG2_FAST          0x00000001 
     00611 #define CODEC_FLAG2_STRICT_GOP    0x00000002 
     00612 #define CODEC_FLAG2_NO_OUTPUT     0x00000004 
     00613 #define CODEC_FLAG2_LOCAL_HEADER  0x00000008 
     00614 #define CODEC_FLAG2_BPYRAMID      0x00000010 
     00615 #define CODEC_FLAG2_WPRED         0x00000020 
     00616 #define CODEC_FLAG2_MIXED_REFS    0x00000040 
     00617 #define CODEC_FLAG2_8X8DCT        0x00000080 
     00618 #define CODEC_FLAG2_FASTPSKIP     0x00000100 
     00619 #define CODEC_FLAG2_AUD           0x00000200 
     00620 #define CODEC_FLAG2_BRDO          0x00000400 
     00621 #define CODEC_FLAG2_INTRA_VLC     0x00000800 
     00622 #define CODEC_FLAG2_MEMC_ONLY     0x00001000 
     00623 #define CODEC_FLAG2_DROP_FRAME_TIMECODE 0x00002000 
     00624 #define CODEC_FLAG2_SKIP_RD       0x00004000 
     00625 #define CODEC_FLAG2_CHUNKS        0x00008000 
     00626 #define CODEC_FLAG2_NON_LINEAR_QUANT 0x00010000 
     00627 #define CODEC_FLAG2_BIT_RESERVOIR 0x00020000 
     00628 #define CODEC_FLAG2_MBTREE        0x00040000 
     00629 #define CODEC_FLAG2_PSY           0x00080000 
     00630 #define CODEC_FLAG2_SSIM          0x00100000 
     00631 #define CODEC_FLAG2_INTRA_REFRESH 0x00200000 
     */
    c->flags2|=CODEC_FLAG2_FASTPSKIP; // flags2=+bpyramid+mixed_refs+wpred+dct8x8+fastpskip

    /* 
     * explicit P-frame weighted prediction analysis method 
     * 0: off 
     * 1: fast blind weighting (one reference duplicate with -1 offset) 
     * 2: smart weighting (full fade detection analysis)
     */
    c->weighted_p_pred = 0; // wpredp=2

    /*
     * the average bitrate
     */
    c->bit_rate = 32000;

    /*
     * picture width / height. 
     */
    c->width = 320; //clip_width;

    /*
     * picture width / height. 
     */
    c->height = 240; //clip_height;

    /*
     * rational number numerator/denominator 
     *
     * This is the fundamental unit of time (in seconds) in terms of which frame timestamps are represented. 
     * For fixed-fps content, timebase should be 1/framerate and timestamp increments should be identically 1.
     */
    c->time_base.num = 1;
    c->time_base.den = 30;
 
    /*
     * Pixel format, see PIX_FMT_xxx. 
     * May be set by the demuxer if known from headers. May be overriden by the decoder if it knows better.
     */
    c->pix_fmt = PIX_FMT_YUV420P; 

    /* 
     * dsp_mask could be add used to disable unwanted CPU features CPU features (i.e. MMX, SSE. ...)
     * With the FORCE flag you may instead enable given CPU features. 
     * (Dangerous: Usable in case of misdetection, improper usage however will result into program crash.) 
     */
//    c->dsp_mask = (FF_MM_MMX | FF_MM_MMXEXT | FF_MM_SSE);

    /*
     * RC lookahead Number of frames for frametype and ratecontrol lookahead
     *
     * warning: lookaheadless mb-tree requires intra refresh or infinite keyint
     */
 //   c->rc_lookahead = 0;

    /*
     * chroma qp offset from luma
     */
    c->chromaoffset = 0;

    /*
     * thread count is used to decide how many independent tasks should be passed to execute()
     */
    c->thread_count = 1;
//    c->bit_rate = (int)(128000.f * 0.80f);

    /*
     * number of bits the bitstream is allowed to diverge from the reference. 
     * the reference can be CBR (for CBR pass1) or VBR (for pass2)
     */
//    c->bit_rate_tolerance = (int) (128000.f * 0.20f);

//    c->gop_size = 30*3; // Each 3 seconds
#else
    c->refs = 3; // i_frame_reference
    c->me_range = 16; // i_me_range
    c->qmin = 10; // i_qp_min
    c->qmax = 51; // i_qp_max
    c->gop_size = 250; // i_keyint_max
    c->keyint_min = 6; // i_keyint_min
    c->max_qdiff = 4; // i_qp_step

    /*
     * 02319 #define FF_LEVEL_UNKNOWN -99
     */
    c->level = -1; // if(avctx->level > 0) x4->params.i_level_idc = avctx->level;

    c->scenechange_threshold = 40; // i_scenecut_threshold
    c->b_frame_strategy = 1; // i_bframe_adaptive

    /*
     * Influences how often B-frames are used. 
     */
    c->bframebias = 0; // i_bframe_bias

    /*
     * CODEC_FLAG2_*
     */
    c->flags2 = c->flags2 & ~CODEC_FLAG2_BPYRAMID; // clear param->b_bframe_pyramid
    c->flags |= CODEC_FLAG_LOOP_FILTER; // set param->b_deblocking_filter

    /*
     * in-loop deblocking filter alphac0 parameter alpha is in the range -6...6
     */
    c->deblockalpha = 0; // i_deblocking_filter_alphac0

    /*
     * in-loop deblocking filter beta parameter beta is in the range -6...6
     */
    c->deblockbeta = 0; // i_deblocking_filter_beta

    c->coder_type = FF_CODER_TYPE_AC; // b_cabac

    /*
     * constant rate factor - quality-based VBR - values ~correspond to qps
     */
    c->crf = 23.0; // f_rf_constant

    c->i_quant_factor = (float)(-1.0/1.4); // x4->params.rc.f_ip_factor
    c->b_quant_factor = (float)1.3; // x4->params.rc.f_pb_factor

    /*
     *
     */
    c->flags = c->flags & ~CODEC_FLAG_PASS1; // clear param->rc.b_stat_write
    c->flags = c->flags & ~CODEC_FLAG_PASS2; // clear param->rc.b_stat_read

    c->max_b_frames = 0;
    c->partitions = X264_PART_I4X4 | X264_PART_I8X8 | X264_PART_P8X8 | X264_PART_B8X8;

    /*
     *
     */
    c->flags2 = c->flags2 & ~CODEC_FLAG2_8X8DCT; // set b_transform_8x8 true

    c->me_subpel_quality = 7; // i_subpel_refine
    c->flags2 = c->flags2 | CODEC_FLAG2_MIXED_REFS; // set b_mixed_references
    c->trellis = 0; // i_trellis
    c->flags2 = c->flags2 | CODEC_FLAG2_FASTPSKIP; // set b_fast_pskip
    c->chromaoffset = 0; // i_chroma_qp_offset
    c->me_cmp |= FF_CMP_CHROMA; // set b_chroma_me
//    c->directpred = X264_DIRECT_PRED_SPATIAL;
    c->me_method = ME_HEX; // set x4->params.analyse.i_me_method = X264_ME_HEX;
    c->flags = c->flags & ~CODEC_FLAG_PASS1; // clear param->rc.b_stat_write
    c->flags2 = c->flags2 | CODEC_FLAG2_WPRED; // set b_weighted_bipred
    c->flags2 = c->flags2 | CODEC_FLAG2_8X8DCT; // set b_transform_8x8
    c->trellis = 1; // set i_trellis
    c->flags = c->flags & ~CODEC_FLAG_PSNR; // clear b_psnr
    c->qcompress = (float)0.6;
    c->flags2 = c->flags2 & ~CODEC_FLAG2_AUD; // clear b_aud
    //c->flags = c->flags & ~CODEC_FLAG_GLOBAL_HEADER; // b_repeat_headers=1
    c->flags = c->flags | CODEC_FLAG_GLOBAL_HEADER; // b_repeat_headers=0

    // End make X264 happy code

    c->pix_fmt = PIX_FMT_YUV420P;
    c->time_base.num = 1;
    c->time_base.den = 10; // fps

#endif
#endif

    /* open it */
    if (avcodec_open(c, codec) < 0) {
        fprintf(stderr, "could not open codec\n");
        exit(1);
    }

	printf("!@#!@#!@# = %d\n", c->bit_rate);

    f = fopen(filename, "wb");
    if (!f) {
        fprintf(stderr, "could not open %s\n", filename);
        exit(1);
    }

    /* alloc image and output buffer */
    outbuf_size = 300000;
    outbuf = malloc(outbuf_size);
    size = c->width * c->height;
    picture_buf = malloc((size * 3) / 2); /* size for YUV 420 */

    picture->data[0] = picture_buf;
    picture->data[1] = picture->data[0] + size;
    picture->data[2] = picture->data[1] + size / 4;
    picture->linesize[0] = c->width;
    picture->linesize[1] = c->width / 2;
    picture->linesize[2] = c->width / 2;
#if 1

    FILE *read_fp = fopen("../../example.yuv", "rb");
    int total_size = 0;
    /* encode 1 second of video */
    for(i=0;i<51 /*c->time_base.den * 10*/;i++) {
        fflush(stdout);
        /* prepare a dummy image */
        /* Y */
#if 1
        for(y=0;y<c->height;y++) {
            for(x=0;x<c->width;x++) {
                picture->data[0][y * picture->linesize[0] + x] = x + y + i * 3;
            }
        }

        /* Cb and Cr */
        for(y=0;y<c->height/2;y++) {
            for(x=0;x<c->width/2;x++) {
                picture->data[1][y * picture->linesize[1] + x] = 128 + y + i * 2;
                picture->data[2][y * picture->linesize[2] + x] = 64 + x + i * 5;
            }
        }
#else
	int read_size = 0;
	fread(picture->data[0], 1, c->height * c->width, read_fp);
	fread(picture->data[1], 1, c->height * c->width / 4, read_fp);
	read_size = fread(picture->data[2], 1, c->height * c->width / 4, read_fp);

	if (0 >= read_size) {
		goto end;
	}
#endif
//    	picture->pts = 0.7 * i;
    	picture->pts = i;
//   	picture->pts = av_q2d(c->time_base);
    	picture->pts = AV_NOPTS_VALUE;

        /* encode the image */
        out_size = avcodec_encode_video(c, outbuf, outbuf_size, picture);
        printf("encoding frame %3d (size=%5d)\n", i, out_size);
        fwrite(outbuf, 1, out_size, f);

	total_size += out_size;
    }
end:
    /* get the delayed frames */
    for(out_size = 1; out_size; i++) {
        fflush(stdout);

        out_size = avcodec_encode_video(c, outbuf, outbuf_size, NULL);
        printf("write frame %3d (size=%5d)\n", i, out_size);
        fwrite(outbuf, 1, out_size, f);
	total_size += out_size;
    }

    printf("total_size = %d Bytes/ %d bps\n", total_size, total_size * 8 / 1);
#endif
    /* add sequence end code to have a real mpeg file */
    outbuf[0] = 0x00;
    outbuf[1] = 0x00;
    outbuf[2] = 0x01;
    outbuf[3] = 0xb7;
    fwrite(outbuf, 1, 4, f);
    fclose(f);
    free(picture_buf);
    free(outbuf);

    avcodec_close(c);
    av_free(c);
    av_free(picture);
    printf("\n");
}

/*
 * Video decoding example
 */

static void pgm_save(unsigned char *buf, int wrap, int xsize, int ysize,
                     char *filename)
{
    FILE *f;
    int i;

    f=fopen(filename,"w");
    fprintf(f,"P5\n%d %d\n%d\n",xsize,ysize,255);
    for(i=0;i<ysize;i++)
        fwrite(buf + i * wrap,1,xsize,f);
    fclose(f);
}

static void video_decode_example(const char *outfilename, const char *filename)
{
    AVCodec *codec;
    AVCodecContext *c= NULL;
    int frame, got_picture, len;
    FILE *f;
    AVFrame *picture;
    uint8_t inbuf[INBUF_SIZE + FF_INPUT_BUFFER_PADDING_SIZE];
    char buf[1024];
    AVPacket avpkt;

    av_init_packet(&avpkt);

    /* set end of buffer to 0 (this ensures that no overreading happens for damaged mpeg streams) */
    memset(inbuf + INBUF_SIZE, 0, FF_INPUT_BUFFER_PADDING_SIZE);

    printf("Video decoding\n");

    /* find the mpeg1 video decoder */
    codec = avcodec_find_decoder(CODEC_ID_MPEG1VIDEO);
    if (!codec) {
        fprintf(stderr, "codec not found\n");
        exit(1);
    }

    c= avcodec_alloc_context();
    picture= avcodec_alloc_frame();

    if(codec->capabilities&CODEC_CAP_TRUNCATED)
        c->flags|= CODEC_FLAG_TRUNCATED; /* we do not send complete frames */

    /* For some codecs, such as msmpeg4 and mpeg4, width and height
       MUST be initialized there because this information is not
       available in the bitstream. */

    /* open it */
    if (avcodec_open(c, codec) < 0) {
        fprintf(stderr, "could not open codec\n");
        exit(1);
    }

    /* the codec gives us the frame size, in samples */

    f = fopen(filename, "rb");
    if (!f) {
        fprintf(stderr, "could not open %s\n", filename);
        exit(1);
    }

    frame = 0;
    for(;;) {
        avpkt.size = fread(inbuf, 1, INBUF_SIZE, f);
        if (avpkt.size == 0)
            break;

        /* NOTE1: some codecs are stream based (mpegvideo, mpegaudio)
           and this is the only method to use them because you cannot
           know the compressed data size before analysing it.

           BUT some other codecs (msmpeg4, mpeg4) are inherently frame
           based, so you must call them with all the data for one
           frame exactly. You must also initialize 'width' and
           'height' before initializing them. */

        /* NOTE2: some codecs allow the raw parameters (frame size,
           sample rate) to be changed at any frame. We handle this, so
           you should also take care of it */

        /* here, we use a stream based decoder (mpeg1video), so we
           feed decoder and see if it could decode a frame */
        avpkt.data = inbuf;
        while (avpkt.size > 0) {
            len = avcodec_decode_video2(c, picture, &got_picture, &avpkt);
            if (len < 0) {
                fprintf(stderr, "Error while decoding frame %d\n", frame);
                exit(1);
            }
            if (got_picture) {
                printf("saving frame %3d\n", frame);
                fflush(stdout);

                /* the picture is allocated by the decoder. no need to
                   free it */
                snprintf(buf, sizeof(buf), outfilename, frame);
                pgm_save(picture->data[0], picture->linesize[0],
                         c->width, c->height, buf);
                frame++;
            }
            avpkt.size -= len;
            avpkt.data += len;
        }
    }

    /* some codecs, such as MPEG, transmit the I and P frame with a
       latency of one frame. You must do the following to have a
       chance to get the last frame of the video */
    avpkt.data = NULL;
    avpkt.size = 0;
    len = avcodec_decode_video2(c, picture, &got_picture, &avpkt);
    if (got_picture) {
        printf("saving last frame %3d\n", frame);
        fflush(stdout);

        /* the picture is allocated by the decoder. no need to
           free it */
        snprintf(buf, sizeof(buf), outfilename, frame);
        pgm_save(picture->data[0], picture->linesize[0],
                 c->width, c->height, buf);
        frame++;
    }

    fclose(f);

    avcodec_close(c);
    av_free(c);
    av_free(picture);
    printf("\n");
}

int main(int argc, char **argv)
{
    const char *filename;

    /* must be called before using avcodec lib */
    avcodec_init();

    /* register all the codecs */
    avcodec_register_all();

    if (argc <= 1) {
//        audio_encode_example("/tmp/test.mp2");
//        audio_decode_example("/tmp/test.sw", "/tmp/test.mp2");

        video_encode_example("./tmp/test.h264");
        filename = "./tmp/test.h264";
    } else {
        filename = argv[1];
    }

    //    audio_decode_example("/tmp/test.sw", filename);
//    video_decode_example("./tmp/test%d.pgm", filename);

    return 0;
}
