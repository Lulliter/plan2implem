#=========================================== (Render site Locally) ================================================#
#=====  Go To Root ./ folder
cd ./


#=========================================== (Push to Github repo) ================================================#
# check status
git status

# Add changes to git Index.
git add docs/* # specific
git add -A # ALL
git add -u # tracked

# Create Std commit "message"....
msg="rebuilt on `date`"
if [ $# -eq 1 ]
  then msg="$1"
fi
# ... Commit Those changes.
git commit -m "$msg"
		# git commit -m "loaded 3 datasets"
		# git commit -m " output in ./docs/"

# Push source and build repos.
git push origin master


