HD Photo Device Porting Kit v1.0 - November 2006
---------------------------------------------------------------------------------

Windows Media™ Photo is Renamed HD Photo
----------------------------------------
This device porting kit supports the still image format previously released under 
the name Windows Media™ Photo.

Throughout the development of the HD Photo still image file format, the project 
was first known (internal to Microsoft) by its code name “Photon”.  Throughout 
it’s Beta release, and when publicly introduced at WinHEC 2006, the file format 
was named “Windows Media™ Photo.”  With the introduction of the released version 
of the Device Porting Kit, the format name has been officially changed to 
"HD Photo.”

“Windows Media™ Photo” is still used as the name for the Windows implementation 
of the “HD Photo” format, in both Windows Vista and the Windows Image Component 
(WIC) and .NET Frameworks 3.0 redistributable packages for down-level versions of 
Windows.  “Windows Media™ Photo” may still be used internally with this release 
of the Device Porting Kit.

Windows Media™ Photo and HD Photo refer to exactly the same file format.  There 
are no features or changes that differentiate one from the other.  They are two 
names for the same format.

We have extended the specification to allow two different file extensions – 
either .wdp or .hdp – to be used to identify an HD Photo (or Windows Media™ 
Photo) file.  With the 1.0 release of Windows Vista, only the .wdp extension is 
recognized.  This will be updated at some point in the future.  Applications that 
support the HD Photo file format should recognize either extension, and ideally 
offer the option to create files with either extension.

While we recognize the potential confusion and issues that this name change can 
create, we believe the new name, initiated in part based on feedback from our 
partners, provides a better identification for broad, cross-platform support for 
this new still image file format.


Device Porting Kit Contents
---------------------------
This device porting kit contains documentation, reference source code, sample 
applications and utilities for the evaluation and implementation of the HD Photo
file format and compression technology.

If you have used an zip file utility to extract all the files to a single 
directory, stop now.  Delete the files and run the self-installing executable
file to install the individual files in the correct directory structure.

Assuming the installation dir is C:\HDPHOTO, all the path mentioned below are 
relative to this base path.

   The Visual Studio 2005 main solution is:
   Systems\tools\SampleCodeNew\WMPEncDecoder\WMP.sln

   Build WMP.sln Debug Configuration, you will get:
   Systems\tools\SampleCodeNew\WMPEncDecoder\Debug\WMPDecApp\WMPDecApp.exe
   Systems\tools\SampleCodeNew\WMPEncDecoder\Debug\WMPEncApp\WMPEncApp.exe

The xplat subdirectory contains a Unix/Linux compatible make file for building the
encoder and decoder, including support for big endian or little endian processor
architecture.  It is the developer's responsibility to properly organize all the 
source files according to the paths defined in this make file for its correct 
operation.  This is provided as a convenience for cross-platform developers and 
to demonstrate the correct operation of the encoder and decoder on big endian 
systems.

"HDPhoto_Feature_Spec_1.0.doc" documents the file container format, metadata tags 
and Windows API's, and other general information about the file format.

"HD_Photo_Bitstream_Spec_1.0.doc" provides a detailed specification of the 
compression encoder and decoder algorithms plus the detailed structure of the 
compressed data (elementary) bit stream.  This document is designed to be used in 
conjunction with the included source code.  If you find instances where the code 
differs from the documentation, the code implementation should be used as the 
reference.

"HD_Photo_DPK_1.0.doc" documents the contents of this porting kit, the usage of 
the command line file conversion utilities (WMPEncApp.exe and WMPDecApp.exe), and 
technical details of the API's and data structures between these sample command 
line applications and the core codec.

The code and documentation in this release represent the final design and 
specification of the 1.0 bit stream, and can be used as the reference for final 
implementations of encoders and decoders for HD Photo.  This version corrects some 
bugs that were present in the previous RC version.

This release of the DPK has received extensive testing of all the various pixel 
formats, encoder options and modes of operation.  We are confident that most errors 
and other bugs have been resolved.  Any code bugs, documentation errors or other 
discrepancies found in this release candidate should be reported to Microsoft as 
promptly as possible.  These can be submitted to hdphoto@microsoft.com.

This DPK provides basic support for big endian architectures.  We have 
successfully tested the encoder and decoder using a big endian processor.  This 
support is provided as a starting reference to be adapted to the specific 
platform and hardware architecture of the target system.

Contact Information
-------------------
For any and all technical questions or feedback about any part of this Device
Porting Kit, including the documentation, please send email to:

  HDphoto@microsoft.com

We will respond as promptly as possible with answers to your questions.

Additional information, best practices, tools, utilities, sample code, sample 
image content, links to additional resources and community discussion can 
currently be found at http://blogs.msdn.com/billcrow.

 - The Microsoft HD Photo Development Team

---------------------------------------------------------------------------------

Change History

V.07  September 2005      First Beta Distribution

V.08  November  2005      Documentation Update

        - There are NO CHANGES to the bit stream.  Files encoded with the 0.7
          release of the porting kit are fully compatible with this release, and 
          vice versa.  However, due to various minor fixes, encoded bit streams 
          may not match exactly between the two versions.
        - Major rewrite of the bit stream specification.  This document is still 
          not complete but this version contains significant improvements, 
          including the removal of extraneous implementation information not 
          relevant to the bit stream specification.
        - Updates to the feature specification including an important correction 
          on the use of the encoder property parameters, using TIFF-compatible 
          descriptive metadata tags, a significant correction to the formatting 
          (which had caused chapters 2 & 3 to be inadvertently merged together), 
          and numerous other minor corrections and clean-up.
        - Minor changes to the Porting Kit spec to sync with the other documents
        - A number of minor bug fixes to the codec code and a fix to the glue 
          code to insure the encoder was writing correct metadata to the output 
          file.

V.09  February 2006      Documentation Update, Bitstream Update, New Features

	- The bitstream has changed to accommodate new features, correct some 
          inconsistencies and respond to feedback we have received to date.  
          Files encoded with previous versions of the porting kit will not decode
          with this version of the decoder.  Likewise, files encoded with this 
          decoder will no decode with previous versions.

        - Significant expansion and improvements to the bit stream specification.
          This version should help answer a number of questions and more 
          completely describe the algorithm and the associated bit stream 
          elements.

        - The feature specification includes numerous corrections, plus updates 
          describing the operation of the new compressed domain operations. 
          These new features are still being finalized and have not had extensive 
          testing at this time.

        - The porting kit spec has been updated to document the new command line 
          options in the sample apps for compressed domain operations as well as 
          correct some other minor problems.

V1.0RC May 2006 Documentation Update, Bitstream Update, New Features, New Tools

	- There are a couple minor (but breaking) changes to the bit stream from 
          v.09 to support some requested new capabilities.  This is now the FINAL
          bit stream for Windows Media Photo Version 1.0.  This version of the 
          bit stream is compatible with the implementation of Windows Media Photo 
          in Windows Vista Beta 2.

        - The Feature Specification was expanded to include a new chapter with 
          detailed descriptions of the different numerical encodings. pixel 
          formats and default color spaces supported by Windows Media Photo.

        - The specification for including an embedded thumbnail/preview was added
          to the Feature Specification.  Numerous other changes, corrections and 
          minor additions were also made to the feature specification.

        - Compressed domain transformation operations have been implemented.

        - Many issues with different pixel formats have been resolved.  Most of 
          these were bugs in the sample applications and did not require changes 
          to the encoder or decoder.

        - Encoder and decoder support for big endian architecture has been added.  
          There is also a Unix/Linux compatible make file for building the porting 
          kit code on a big endian processor.

        - The Porting Kit Spec has been updated to include updated documentation 
          on the sample application command line options.

        - An unsupported utility to convert from compressed .hdr files to 
          uncompressed .hdr files is included.  Adobe Photoshop CS2 saves .hdr 
          files in compressed mode only.  This utility can be used to convert 
          these to uncompressed mode, which is required by the wmpencapp utility.  
          While we have tested this utility (we use it regularly for our own 
          testing), we cannot offer any support if there are problems with it's 
          use.

V1.0 RTM November 2006   Name Change, Float/Alpha Fixes, Doc Updates, New Tools

	- Three separate problems with floating point pixel formats were 
          identified and corrected, including a quantization bug, a problem with 
          renormalization of the base and with the use of the block transform.  
          This may result in changes to the compressed bit stream for floating 
          point images, so any previously created images that use one of the 
          various floating point pixel formats may need to be re-generated 
          with this corrected version.

        - A problem with interleaved alpha channel encoding was fixed.  
          Interleaved alpha channel images should now work properly.

        - The default restriction in the RC release that prevented certain tiling 
          modes (to insure compatibility with Windows Vista Beta 1) has been 
          removed.

        - The version code written to the header of the container has been changed 
          from the development value of 00 to the release value of 01.

        - There were other bug fixes and cleanup work implemented throughout the 
          code.

        - All documentation was updated to reflect the name change to HD Photo.  
          Please note that to avoid unnecessary delays in releasing this version 
          of the Device Porting Kit, no changes have been made to any code related 
          to this name change.  These changes will be integrated in a future 
          release.

        - The Feature Specification was updated to describe the new name, provide 
          information on additional meta data tags, clarify a number of sections 
          based on feedback we have received, and correct a number of other minor 
          typos and syntax problems.

        - The DPK Specification was updated to reflect the new format name, 
          provide a more detailed description of the use of the sample 
          applications (and describe the use of the various encoder parameters), 
          document the new utility programs, and correct a number of other typos, 
          errors, omissions and formatting issues.

        - The Bitstream Specification was updated to reflect the new format name, 
          significantly expand and improve the technical description of the 
          compressed bit stream, and correct a number of typos, errors and 
          formatting issues.

        - WMPEncApp was enhanced to properly default the alpha mode and add 
          support for setting the quality of planar alpha channels independent 
          from the image quality.

        - A new utility, IMAGECOMP, was added for uncompressed image quality and 
          difference measurements. While we have tested this utility (we use it 
          regularly for our own testing), we cannot offer any support if there 
          are problems with its use.
