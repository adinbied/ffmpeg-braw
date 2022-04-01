FFmpeg-Braw
=============
Super hacky workaround enabling blackmagic raw decode support into ffmpeg based on the reverse engineered 2019 implementation patch floating around,
compiled and merged to form a working codebase; the caveat is the 2019 patch is semi broken with the output being super dark and green tinted; looks very similar to a problem Magic Lantern had a while back (something about the rggb16le using 12bits in a 16 bit container maybe?).

So to duct tape a solution I just added a bunch of filters and offsets to the output pipeline and a custom gamma curve to approximate gen 5 color science.
These are the commands I used with differing r,g,and b values based on the color temp and tint:
### For 5250K 30T
./ffmpeg -i /mnt/l/input.braw -vf colorchannelmixer=rr=0.98:gg=0.62:bb=0.8,tonemap=linear:param=300,tonemap=gamma:param=1.4,curves=psfile='BRAWHLRolloff.acv' -acodec aac -vcodec prores_ks -pix_fmt yuv444p outputProRes.mov

### For 3950K 16T
./ffmpeg -i /mnt/i/input.braw -vf colorchannelmixer=rr=0.78:gg=0.58:bb=0.86,tonemap=linear:param=300,tonemap=gamma:param=1.38,curves=psfile='BRAWHLRolloff.acv' -acodec aac -vcodec prores_ks -pix_fmt yuv444p outputProRes.mov

NOTE: I am not actively developing this, just a random side project thrown together in an afternoon
===============================

It's super slow and inefficient and doesnt use the actual BRAW SDK pipeline for getting frames, but hey it vaguely works.
--

FFmpeg is a collection of libraries and tools to process multimedia content
such as audio, video, subtitles and related metadata.

## Libraries

* `libavcodec` provides implementation of a wider range of codecs.
* `libavformat` implements streaming protocols, container formats and basic I/O access.
* `libavutil` includes hashers, decompressors and miscellaneous utility functions.
* `libavfilter` provides a mean to alter decoded Audio and Video through chain of filters.
* `libavdevice` provides an abstraction to access capture and playback devices.
* `libswresample` implements audio mixing and resampling routines.
* `libswscale` implements color conversion and scaling routines.

## Tools

* [ffmpeg](https://ffmpeg.org/ffmpeg.html) is a command line toolbox to
  manipulate, convert and stream multimedia content.
* [ffplay](https://ffmpeg.org/ffplay.html) is a minimalistic multimedia player.
* [ffprobe](https://ffmpeg.org/ffprobe.html) is a simple analysis tool to inspect
  multimedia content.
* Additional small tools such as `aviocat`, `ismindex` and `qt-faststart`.

## Documentation

The offline documentation is available in the **doc/** directory.

The online documentation is available in the main [website](https://ffmpeg.org)
and in the [wiki](https://trac.ffmpeg.org).

### Examples

Coding examples are available in the **doc/examples** directory.

## License

FFmpeg codebase is mainly LGPL-licensed with optional components licensed under
GPL. Please refer to the LICENSE file for detailed information.

## Contributing

Patches should be submitted to the ffmpeg-devel mailing list using
`git format-patch` or `git send-email`. Github pull requests should be
avoided because they are not part of our review process and will be ignored.
