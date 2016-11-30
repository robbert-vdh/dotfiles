import pyopencl as cl
import numpy
import sys

if __name__ == "__main__":
    platforms = cl.get_platforms()
    if sys.argv[1] == "-p":
        i = 0
        for p in platforms:
            print("Platform " + str(i) + ":")
            print("  Vendor:   " + p.vendor)
            print("  Name:     " + p.name)
            print("  Version : " + p.version)
            i = i + 1
    else:
        ctx       = cl.Context(
            dev_type   = cl.device_type.ALL,
            properties = [(cl.context_properties.PLATFORM,
                           platforms[int(sys.argv[1])])]
        )
        f         = open(sys.argv[2], 'r')
        fstr      = "".join(f.readlines())
        program   = cl.Program(ctx, fstr).build()
