major=0
minor=1
commits=`git rev-list HEAD --count`
echo "#ifndef SPN_VERISON_H_
#define SPN_VERSION_H_
#define SPN_VERSION \"$major.$minor.$commits\"
#endif" > src/include/spn_version.h
echo "$major.$minor.$commits" > VERSION
