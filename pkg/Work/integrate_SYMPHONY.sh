## Usage integrate_SYMPHONY.sh [SYMPHONY.tgz]
## Integrates the latest SYMPHONY sources
## theussl, 2008-12-18

## where to put source files and headers
URL="http://www.coin-or.org/download/source/SYMPHONY/"
latest="SYMPHONY-5.1.8.tgz"
DESTINATION=../src/SYMPHONY

## --------------------------------------------
## Usage

usage() {
cat << "USAGE_END"
Usage: integrate_SYMPHONY.sh [-g ]
       integrate_SYMPHONY.sh [-i SYMPHONY_version.tgz]
       integrate_SYMPHONY.sh [-c]
Get, integrate or clean SYMPHONY sources

Options:
  -g, --get           get latest Version of SYMPHONY
  -i, --integrate     integrate given SYMPHONY sources
  -c, --clean         clean the R package's src directory

USAGE_END
        exit $1
}

## --------------------------------------------
## Read command line arguments

for x in "$@" ; do
    case "${x}" in
        -i|--integrate)        # integrate sources
             integrate=true
             ;;
        -c|--clean)            # clean sources
             clean=true
             ;;
        -g|--get)            # clean sources
             get=true
	     sources=$latest # sources gets latest
             ;;
        -*)  # invalid option
             echo "$0: Invalid switch \"${x}\"!  Run $0 without parameters for help."
             exit 1
             ;;
        *)   # this should be the tarball of glpk sources
             if [[ ! -z "${sources}" ]] ; then
                 echo "$0: Only one source file allowed: \"${sources}\"!  Run $0 without parameters for help."
                 exit 1
             fi
             sources="${x}"
             ;;
    esac
done

## --------------------------------------------
## input validation

if [[ ! ( ${integrate} || ${clean} || ${get}) ]] ; then
    echo "$0: No option given; nothing to do!"
    usage 1
    exit 1
fi

if [[ ( ${integrate} && ${clean} ) || ( ${get} && ${clean} )]] ; then
    echo "$0: --clean can only be used alone!  Run $0 without parameters for help."
    exit 1
fi

if [[ -z "${sources}" && $integrate ]] ; then
    echo "$0: No source file to integrate given!"
    usage 1
    exit 1
fi


## --------------------------------------------
## integrate Symphony sources to package

if [[ $get ]] ; then
    if [[ ! -s "${sources}" ]] ; then
	wget $URL/$sources
    else
	echo "$sources already available."
    fi
fi

if [[ $integrate ]] ; then
    
    if [[ ! -s "${sources}" ]] ; then
	echo "$0: Selected source file \"$sources\" is not available or zero!"
	usage 1
	exit 1
    fi
    SYMPHONY=`basename $sources .tgz`
    SOURCEDIR=${SYMPHONY}

    tar xzf $sources
    
    if [[ ! -d $DESTINATION ]] ; then
	mkdir $DESTINATION
    fi

    cp -r $SOURCEDIR/* $DESTINATION
    if [[ -d $SOURCEDIR ]] ; then
	rm -rf $SOURCEDIR
    fi
	
fi


if [[ $clean ]] ; then
    if [[ -d $DESTINATION ]] ; then
	rm -rf $DESTINATION
    fi
fi

echo "done."

exit 0
