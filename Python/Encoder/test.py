import sys,os;

setup_args = str.join(" ",sys.argv[1:])
build_cmd = '';

if (setup_args):
    if (' --debug' in setup_args):
        build_cmd += ' CFLAGS="$CFLAGS -UNDEBUG"' #python setup.py build --debug runs with -DNDEBUG! !@#
    build_cmd += ' python setup.py ' + setup_args

if (build_cmd):
    print "----------------------:"
    print build_cmd
    if (0 != os.system(build_cmd)):
        print "Build: Error\n"
        exit(1)
    else:
        print "Build: OK"
        print "----------------------:\n"

sys.path.insert(0, 'build/lib.macosx-10.6-universal-2.6/')
import srlencoder

jonas="gunnar"
print "About to encode string: %s" % jonas
srlencoder.encode(jonas,
                  no_shared_hashkeys=True,
                  snappy=1,
                  stringify_unknown=False,
                  undef_unknown=True,
                  max_recursion_depth=1,
                  sort_keys=True
                  )

exit(0)
