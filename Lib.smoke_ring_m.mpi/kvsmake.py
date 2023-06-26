#!/usr/bin/python

import sys
import os

LIB_NAME = "smoke_ring_m.mpi"

#=============================================================================
#  Executes kvsmake command.
#=============================================================================
def KVSMake( option ):

    s = ";"
    if os.name == 'nt': s = "&"

    make_option = ''
    if   option == 'build': make_option += "kvsmake lib" + s
    elif option == 'clean': make_option += "kvsmake clean" + s
    elif option == 'distclean': make_option += "kvsmake distclean" + s
#    elif option == 'rebuild': make_option += "kvsmake clean" + s + "kvsmake lib" + s
    elif option == 'debug_build': make_option += "kvsmake lib DEBUG=1" + s
#    elif option == 'debug_rebuild': make_option += "kvsmake clean DEBUG=1" + s + "kvsmake lib DEBUG=1" + s
    else:
        print( "Error: Unknown option '" + option +"'" )
#        print( "Usage: python kvsmake.py [clean | distclean | rebuild]" )
        print( "Usage: python kvsmake.py [clean | distclean]" )
        sys.exit()

    command = ''
    command += "kvsmake -g " + LIB_NAME + " -use_mpi" + s
    command += make_option
    os.system( command )

def MakeSliceSVG( option ):

    s = ";"
    if os.name == 'nt': s = "&"

    make_option = ''
    if   option == 'build': make_option = "make"
    elif option == 'clean': make_option = "make clean"
    elif option == 'distclean': make_option = "make clean"
#    elif option == 'rebuild': make_option = "make clean" + s + "make"
    elif option == 'debug_build': make_option = "make" + s
#    elif option == 'debug_rebuild': make_option = "make clean" + s + "make"
    else:
        print( "Error: Unknown option '" + option +"'" )
#        print( "Usage: python kvsmake.py [clean | distclean | rebuild]" )
        print( "Usage: python kvsmake.py [clean | distclean]" )
        sys.exit()

    command = "cd slice-svg/src/" + s
    command += make_option
    os.system( command )

def MakeSmokeRing( option ):

    s = ";"
    if os.name == 'nt': s = "&"

    make_option = ''
    if   option == 'build': make_option = "make"
    elif option == 'clean': make_option = "make clean"
    elif option == 'distclean': make_option = "make clean"
#    elif option == 'rebuild': make_option = "make clean" + s + "make"
    elif option == 'debug_build': make_option = "make" + s
#    elif option == 'debug_rebuild': make_option = "make clean" + s + "make"
    else:
        print( "Error: Unknown option '" + option +"'" )
#        print( "Usage: python kvsmake.py [clean | distclean | rebuild]" )
        print( "Usage: python kvsmake.py [clean | distclean]" )
        sys.exit()


    command = "cd smoke-ring/src/" + s
    command += make_option
    if 'build' in option: command += s + "cp *.F90 ../../" + s + "rm ../../main.F90"
    os.system( command )

#=============================================================================
#  Main process.
#=============================================================================
if __name__=='__main__':
    argc = len( sys.argv )
    argv = sys.argv
    if   argc == 1:
        #MakeSliceSVG( 'build' )
        MakeSmokeRing( 'build' )
        KVSMake( 'build' )
    elif argc == 2:
        #MakeSliceSVG( argv[1] )
        MakeSmokeRing( argv[1] )
        KVSMake( argv[1] )
