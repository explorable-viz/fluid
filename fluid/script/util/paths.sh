# Library layout, shared by build and website scripts
LIB_ROOT="lib" # source root of standard library
DIST_LIB_ROOT="dist/fluid/$LIB_ROOT" # copy of root shipped in npm package
WEBSITE_LIB_ROOT="static/$LIB_ROOT" # website's link to standard library
WEBSITE_SHARED="website/src/lib" # shared components and CSS, target of each site's $shared alias
